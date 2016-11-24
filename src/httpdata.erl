-module(httpdata).

-export([decode/1]).

decode(Body) ->
    lists:map(fun(Token) ->
                      decode_token(Token)
              end, string:tokens(Body, "&")).

decode_token(Token) ->
    [ K, V ] = string:tokens(Token, "="),
    {K, erlang:list_to_binary(http_uri:decode(V))}.
