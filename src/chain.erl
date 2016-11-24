-module(chain).

-export([chain/2]).

chain([], Arg) ->
    {ok, Arg};
chain([Fun|Funs], Arg) ->
    case Fun(Arg) of
        {ok, V} -> chain(Funs, V);
        {error, E} -> { error, E }
    end.
