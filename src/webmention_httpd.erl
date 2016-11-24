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

-module(webmention_httpd).

-export([handle_req/3]).

-include("couch_db.hrl").

-import(couch_httpd, [send_json/2, send_json/3, send_json/4]).
-import(webmention_validate, [validate_init/2]).

log(T) ->
  ?LOG_INFO("[micropub_http] ~p ~n", [T]).

send_error(_Req, {already_sent, Resp, _Error}) ->
    {ok, Resp};

send_error(Req, Error) ->
    {Code, ErrorStr, ReasonStr} = error_info(Error),
    %{Code1, Headers} = error_headers(Req, Code, ErrorStr, ReasonStr),
    couch_httpd:send_error(Req, Code, [], ErrorStr, ReasonStr).

handle_req(#httpd{method = 'GET'} = Req, _Db, _DDoc) ->
    Managed_hosts = couch_config:get("indieweb", "managed_hosts", []),
    %{200, {[ {status, <<"not implemented">>}]}
    send_json(Req, 200, {[ {hello, <<"a happy webmention endpoint">>},
                           {managed_hosts, list_to_binary(Managed_hosts)}
                         ]});
handle_req(#httpd{method = 'POST'} = Req, Db, _DDoc) ->
    io:format("Got a POST: ~p ~n", [Req]),
    PostData = Req:parse_post(),
    SourceUrl = proplists:get_value("source", PostData),
    TargetUrl = proplists:get_value("target", PostData),
    case validate_init(SourceUrl, TargetUrl) of
        { ok, Json } -> 
            create_and_send(Req, Db, Json);
        { error, Reason } -> 
            send_json(Req, 400, {[
                                  {status, <<"failed">>},
                                  {reason, Reason}
                                 ]});
        _ -> 
            send_json(Req, 500, {[ {status, <<"not implemented, booya">>} ]})
    end;
handle_req(Req, _Db, _DDoc) ->
    couch_httpd:send_method_not_allowed(Req, "GET,POST").

create_and_send(Req, Db, Json) ->
    log({mpjson, Json}),
    Doc = couch_doc:from_json_obj(Json),
    case couch_db:update_doc(Db, Doc, []) of 
        { ok, NewRevs } ->
            Location = uri_for_doc(Doc),
            send_json(Req, 201, 
                      [
                       { <<"Location">>, Location }
                      ]
                     , {[{<<"location">>, Location }]} );
        _Other ->
            log(_Other),
            send_error(500, {"unknown", "something went wrong with document update"})
    end.

uri_for_doc(#doc{}=Doc) ->
    {Props}=couch_doc:to_json_obj(Doc, []),
    Id = proplists:get_value("<<_id>>", Props),
    string:concat("https://hazyblue.me/mentions/", couch_util:to_list(Id)).

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
