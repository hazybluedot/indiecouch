% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(micropub_httpd).

-export([handle_req/3]).

-include("couch_db.hrl").

%-import(couch_httpd, [send_json/2, send_json/3, send_json/4]).
-import(webmention_validate, [validate_init/2]).

log(T) ->
  ?LOG_INFO("[micropub_http] ~p ~n", [T]).

send_json(Req, Value) ->
    send_json(Req, 200, Value).
send_json(Req, Code, Value) ->
    send_json(Req, Code, [], Value).
send_json(Req, Code, Headers, Value) ->
    Body = [?JSON_ENCODE(Value), $\n],
    DefaultHeaders = [ 
                       {"Content-Type", "application/json"} 
                     ],
    couch_httpd:send_response(Req, Code, DefaultHeaders ++ Headers, Body).


send_error(_Req, {already_sent, Resp, _Error}) ->
    {ok, Resp};

send_error(Req, Error) ->
    {Code, ErrorStr, ReasonStr} = error_info(Error),
    %{Code1, Headers} = error_headers(Req, Code, ErrorStr, ReasonStr),
    couch_httpd:send_error(Req, Code, [], ErrorStr, ReasonStr).

handle_req(#httpd{
              path_parts=[_DbName, _Design, DesignName, _ ]
             } = Req, Db, _DDoc) ->
    case get_indie_config(Db, DesignName) of
        { error, Reason } ->
            send_error(Req, {bad_config, Reason});
        { ok, Indie } -> 
            handle_indie_req(Req, Db, Indie)
    end.
    
handle_indie_req(Req, Db, Indie) ->
    case validate(Req, Indie) of
        { ok, Ident } ->
            handle_auth_req(Req, Db, Indie, Ident);
        { error, Error } ->
            send_error(Req, Error)
    end.

handle_auth_req(#httpd{method='GET',
                       mochi_req=MochiReq
                      } = Req, _Db, Indie, _Ident) ->
    Query = mochiweb_request:parse_qs(MochiReq),
    handle_query(Req, Query, Indie);
handle_auth_req(#httpd{method = 'POST'} = Req, Db, _Indie, Ident) ->
    MPDoc = case content_type(Req) of
                "application/x-www-form-urlencoded" ->
                    parse_form(Req);
                "application/json" ->
                    parse_json(Req)
            end,
    Json = micropub:create(MPDoc, Ident),
    Doc = couch_doc:from_json_obj(Json),
    log({update_doc, Doc}),
    case couch_db:update_doc(Db, Doc, []) of 
        { ok, _NewRevs } ->
            send_json(Req, 201, new_doc_response_headers(Doc), {[{<<"location">>, <<"see header">>}]} );
        _Other ->
            log(_Other),
            send_error(Req, "something went wrong with document update")
    end;
handle_auth_req(Req, _Db, _Indie, _Ident) ->
    couch_httpd:send_method_not_allowed(Req, "GET,POST").

handle_config_req(Req, IndieDoc) -> 
    Config = proplists:get_value(<<"micropub">>, IndieDoc),
    %log( {"indiedoc config", Config } ),
    send_json(Req, 200, Config).

handle_query(Req, Query, Indie) ->
    {Config} = proplists:get_value(<<"micropub">>, Indie),
                                                %log( {"indiedoc config", Config } ),
    case proplists:get_value("q", Query) of
        "config" ->
            send_json(Req, {Config});
        "syndicate-to" ->
            send_json(Req, {[{ <<"syndicate-to">>, proplists:get_value(<<"syndicate-to">>, Config) }]});
        undefined ->
            send_error(Req, {"no query", "GET request must have a query parmeter"}); 
        Unknown ->
            send_error(Req, {"unknown query", string:join(["The query of", Unknown, "is not handled"], " ")})
    end.

new_doc_response_headers(#doc{}=Doc) ->
    {Props}=couch_doc:to_json_obj(Doc, []),
    log({props, Props}),
    [ {"Location", string:concat("https://hazyblue.me/posts/", erlang:binary_to_list(proplists:get_value(<<"_id">>, Props))) } ].

content_type(Req) ->
    case couch_httpd:header_value(Req, "Content-Type") of
        undefined ->
            throw({bad_ctype, "Request header must contain Content-Type"});
        ReqCType ->
            case string:tokens(ReqCType, ";") of
                [CType] ->
                    CType;
                [CType|_Rest] ->
                    CType
            end
    end.

parse_form(#httpd{mochi_req=MochiReq}) ->
    %mochiweb_request:parse_post(MochiReq).
    MochiReq:parse_post().

parse_json(#httpd{mochi_req=MochiReq}=Req) ->
    couch_httpd:json_body(Req).

validate(Req, Indie) ->
    Token = couch_httpd:header_value(Req, "Authorization"),
    TokenEndpoint = proplists:get_value(<<"token_endpoint">>, Indie),
    {Owner} = proplists:get_value(<<"owner">>, Indie),
    Me = proplists:get_value(<<"url">>, Owner),
    case indieauth_validate:validate(Token, TokenEndpoint) of
        { ok, Ident } ->
            IdentMe = proplists:get_value("me", Ident),
            case hosts_match(Me, IdentMe) of
                true ->
                    { ok, Ident};
                false ->
                    { error, {unauthorized, 
                              string:join(["Identity ", IdentMe, " does not have permission to use this endpoint"], " ")} }
            end;
        { error, Reason } ->
            { error, {indieauth_error, Reason} }
    end.

hosts_match(Url1, Url2) ->
    {_, Netloc1, _, _, _} = mochiweb_util:urlsplit(couch_util:to_list(Url1)),
    {_, Netloc2, _, _, _} = mochiweb_util:urlsplit(couch_util:to_list(Url2)),
    Netloc1 =:= Netloc2.

get_indie_config(Db, DesignName) ->
    DesignId = <<"_design/", DesignName/binary>>,
    #doc{body = {Body} } = couch_httpd_db:couch_doc_open(Db, DesignId, nil, [ejson_body]),
    case proplists:get_value(<<"indieweb">>, Body) of
        undefined -> 
            { error, <<"design doc must have an indieweb property">>};
        {Indie} ->     
            { ok, Indie }
    end.

error_info({Error, Reason}) when is_list(Reason) ->
    error_info({Error, ?l2b(Reason)});
error_info({forbidden, Reason}) ->
    { 403, <<"forbidden">>, Reason };
error_info({indieauth_error, Reason}) ->
    { 403, <<"indieauth error">>, Reason };
error_info({bad_config, Reason}) ->
    { 501, <<"bad config">>, Reason };
error_info({Error, Reason}) ->
    {500, couch_util:to_binary(Error), couch_util:to_binary(Reason)};
error_info(Error) ->
    {500, <<"unknown_error">>, couch_util:to_binary(Error)}.

