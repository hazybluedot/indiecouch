-module(httpfollow).

-export([httpfollow/1, httpfollow/2]).

httpfollow(Url) -> httpfollow(Url, 10).
httpfollow(Url, Depth) -> httpfollow(Url, Depth, []).
httpfollow("", _, Acc) -> Acc;
httpfollow(_, Depth, Acc) when Depth < 0 -> throw({ maxdepth, Acc });
httpfollow(Url, Depth, Acc) -> 
	ssl:start(),
	application:start(inets),

	case httpc:request(head, { Url, [] }, [{autoredirect, false}], []) of
		{ ok, {{_,Code,_}, Headers, _}} when Code >= 300, Code < 400 ->
			{_,U} = lists:keyfind("location", 1, Headers),
			httpfollow(U, Depth-1, [{Url, ok, Code}|Acc]);
		{ ok, {{_,Code,_}, _, _}} -> [{Url, ok, Code}|Acc];
		{ error, Reason } -> [{Url, error, Reason}|Acc]
	end.
