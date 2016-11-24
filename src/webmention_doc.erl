-module(webmention_doc).

-import(urlutil, [normalize/1]).
-export([make_status_doc/3]).

log(T) ->
  io:format("[micropub_http] ~p ~n", [T]).

doc_id(Source, Target) ->
    slugify:slugify(string:join([
                                 "mention",
                                 Source,
                                 Target
                                ]), "-").

make_status_doc(Source, Target, Valid) ->
    NormSource = normalize(Source),
    NormTarget = normalize(Target),
    Location = <<"/mentions/">>,
    Json = {[
             {<<"_id">> , doc_id(NormSource, NormTarget)},
             {<<"type">>, [<<"mention">>]},
             {<<"properties">>, {[]}},
             {<<"source">>, NormSource},
             {<<"target">>, NormTarget},
             {<<"validate">>, {Valid} }
            ]}.
