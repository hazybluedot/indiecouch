-module(indieauth_validate).

-export([validate/2]).

log(T) ->
    io:format("[indieauth_validate:validate] ~p ~n", [T]).

validate(Token, Endpoint) when erlang:is_binary(Endpoint) ->
    validate(Token, erlang:binary_to_list(Endpoint));
validate(Token, Endpoint) ->
    ssl:start(),
    application:start(inets),
    log({ "token and endppoint", {Token, Endpoint} }),
    Headers = [ {"Content-type", "application/x-www-form-urlencoded"},
                {"Authorization", Token}
              ],
    case httpc:request(get,
                       { Endpoint, Headers }, 
                       [{autoredirect, true}], []) 
    of
        { ok, {{_,Code,_}, _, Body}} -> 
            validate_response(httpdata:decode(Body));
        { error, Reason } -> {error, Reason}
    end.

validate_response(ResponseBody) ->
    case proplists:is_defined("me", ResponseBody) of
        true ->
            { ok, ResponseBody };
        false ->
            { error, "could not determine identity from token server response" }
    end.
