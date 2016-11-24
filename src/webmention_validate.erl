-module(webmention_validate).

-export([validate_init/2]).

%-behavior(gen_server).

validate_init(Source, Target) ->
    Valid = case validate_urls([Source, Target]) of
                { ok, Result } -> { ok, Result };
                { error, Reason } -> { error, Reason }
            end,
    case Valid of
        { ok, Status } ->
            Location = webmention_doc:make_status_doc(Source, Target, Status),
            do_validate(Source, Target, Location);
        { error, Reason0 } -> { error, Reason0 }
    end.
                                                 
validate_scheme(Parts) ->
    case lists:member(erlang:element(1, Parts), [ http, https ]) of
        true -> { ok, Parts };
        false -> { error, {"Invalid scheme", Parts} }
    end.
        
validate_urls(Urls) ->
    Result = lists:map(fun(U) ->
                               chain:chain([fun http_uri:parse/1, fun validate_scheme/1], U) end,
                       Urls),
    io:format("validate_urls Result0: ~p ~n", [Result]),
    case lists:any(fun({error, _Reason}) -> true;
                      (_) -> false end, Result) of
        false -> { ok, Urls };
        true -> { error, lists:filter(fun({ error, _Reason }) -> true;
                                          (_) -> false 
                                       end, Result ) }
    end.
    
                           
do_validate(Source, Target, Location) ->
    %% async call
    ssl:start(),
    application:start(inets),

    case chain:chain([fun validate_resolve/1, 
                      fun validate_target_host/1, 
                      fun validate_source_link/1
                     ], {Source, Target}) of
        { ok, Result } -> { ok, Result};
        { error, Reason } -> { error, Reason }
    end.

validate_resolve({Source, Target}) ->
    Resolved = lists:map(fun httpfollow:httpfollow/1, [Source, Target]),
    case lists:all(fun([H|_Rest]) ->
                           {Url, Status, Code} = H,
                           Code =:= 200;
                      (_) -> false
                   end, Resolved) of
        true -> { ok, erlang:list_to_tuple(Resolved) };
        false -> { error, erlang:list_to_tuple(Resolved) }
    end.
    %validate_source(Source, Target).
                                                %{ ok, queued }.
validate_target_host({Sources, [Target|_] = Targets}) ->
    {Url, Status, Code} = Target,
    {ok, Parts} = http_uri:parse(Url),
    Host = erlang:element(3, Parts),
    io:format("Checking if I manage ~p ~n", [Host]),
    case lists:member(Host, [ "www.hazyblue.me", "hazyblue.me" ]) of
        true -> { ok, {Sources, Targets} };
        false -> { error, string:concat("I don't manage ", Host) }
    end.

% does Source link to target?
validate_source_link({[Source|_]=Sources, Targets}) ->
    { SourceUrl, _, _ } = Source,
    { TargetUrl, _, _ } = lists:last(Targets),
    io:format("Checking if ~p links to ~p ~n", [SourceUrl,TargetUrl]),
    case get_body(SourceUrl) of
        { ok, Body } ->
            case find_link(TargetUrl, Body) of
                { true, {_Link,Tree} } ->  { ok, Tree };
                false -> { error, <<"source does not link to target">> }
            end;
        { error, Reason } -> { error, Reason }
    end.

get_body(Url) ->
    case httpc:request(Url) of 
        { ok, { _Status, _Headers, Body } } ->
            {ok, Body};
        { error, Reason } -> { error, Reason }
    end.
                             
find_link(Link, Html) when erlang:is_list(Link) ->
                              find_link(erlang:list_to_binary(Link), Html);
find_link(Link, Html) ->
    Tree = mochiweb_html:parse(Html),
    Hrefs = mochiweb_xpath:execute("//@href", Tree),
    io:format("Looking for ~s in ~p ~n", [Link, "Tree"]),
    case lists:any(fun(Elem) -> 
                      Elem =:= Link
              end, Hrefs) of
        true ->
            io:format("found it!~n"),
            { true, {Link, Tree} };
        false -> 
            io:format("did not find ~p in hrefs ~n", [Link]),
            false
    end.
    
