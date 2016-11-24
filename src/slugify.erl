-module(slugify).

-include_lib("eunit/include/eunit.hrl").

-export([slugify/1]).
-compile([export_all]).

%% this isn't used yet
-define(CHAR_ENTITY_PATTERN, "&(aring|gt|sup|Ntilde|upsih|Yacute|Atilde|radic|otimes|aelig|Psi|Uuml|Epsilon|Icirc|Eacute|Lambda|Prime|Kappa|sigmaf|lrm|cedil|kappa|AElig|prime|Tau|lceil|dArr|ge|sdot|lfloor|lArr|Auml|brvbar|Otilde|Theta|Pi|OElig|Scaron|egrave|sub|iexcl|ordf|sum|ntilde|atilde|theta|nsub|hArr|Oslash|THORN|yuml|Mu|thinsp|ecirc|bdquo|Aring|nabla|permil|Ugrave|eta|Agrave|forall|eth|rceil|iuml|Egrave|divide|igrave|otilde|pound|frasl|ETH|lowast|chi|Aacute|cent|Beta|perp|there4|pi|empty|euml|notin|uuml|icirc|bull|upsilon|Oacute|ensp|ccedil|cap|mu|deg|tau|emsp|hellip|ucirc|ugrave|cong|Iota|quot|rarr|Rho|uacute|acirc|sim|phi|diams|Euml|Ccedil|Eta|Gamma|euro|thetasym|sect|ldquo|hearts|oacute|zwnj|yen|ograve|Chi|trade|xi|nbsp|tilde|lsaquo|oelig|equiv|le|auml|cup|Yuml|lt|Upsilon|ndash|yacute|real|psi|rsaquo|darr|Alpha|not|amp|oslash|acute|zwj|laquo|rdquo|Igrave|micro|shy|supe|szlig|clubs|agrave|Ocirc|harr|larr|frac12|prop|circ|ocirc|asymp|uml|prod|reg|rlm|infin|Sigma|mdash|uarr|times|rArr|or|gamma|lambda|rang|sup3|dagger|Ouml|image|alefsym|sube|alpha|Nu|plusmn|sup1|sup2|frac34|oline|Delta|loz|iota|iacute|para|ordm|epsilon|weierp|part|delta|omicron|copy|Iuml|Xi|Dagger|Ograve|Ucirc|scaron|lsquo|isin|Zeta|minus|and|ang|curren|int|rfloor|crarr|exist|oplus|Acirc|piv|ni|Phi|Iacute|Uacute|Omicron|ne|iquest|sbquo|Ecirc|zeta|Omega|nu|macr|frac14|aacute|uArr|beta|fnof|rho|eacute|omega|middot|lang|spades|rsquo|thorn|ouml|raquo|sigma);").
-define(DECIMAL_PATTERN, "&#([0-9]+);").
-define(HEX_PATTERN, "&#x([0-9a-fA-F]+);").
-define(ALLOWED_CHARS_PATTERN, "[^-a-z0-9]+").
-define(DUPLICATE_DASH_PATTERN, "-{2,}").
-define(NUMBERS_PATTERN, "(?<=[0-9]),(?=[0-9])").
-define(QUOTE_PATTERN, "[']+").

match_to_str(String, MatchObj) when is_list(String) and is_list(MatchObj) ->
    lists:map(fun({Idx,Span}) ->
                      string:substr(String, Idx+1, Span)
              end, MatchObj).

%% Convert match objet index to string index
%% match_to_index({Start, Span}) ->
%%     {Start+1, Span};
%% match_to_index([]) -> [];
%% match_to_index([Match|Rest]) ->
%%     [match_to_index(Match)|match_To_index(Rest)].

match2codepoint([_|Groups]) ->
    [Group|_] = Groups,
    unicode:characters_to_list(Group).

splice(Subject, {Start,Span}, Repl) ->
    Head = case Start-2 > 0 of
               true -> string:substr(Subject, 1, Start-1);
               false -> ""
           end,
    Tail = case Start+Span > string:len(Subject) of
               true -> [];
               false -> string:substr(Subject, Start+Span)
           end,
    string:join([
                 Head,
                 Repl,
                 Tail
                 ], "").

%% sub(Subject, RE, Fun) when is_function(Fun) ->
%%     case re:run(Subject, RE, [global]) of 
%%         {match, Captured} -> 
%%             lists:foldl(fun([M|_Groups] = Match, S) ->
%%                                 splice(Subject, M, Fun(match_to_str(Subject, Match)))
%%                         end, Subject, Captured);
%%         nomatch -> Subject
%%     end;
%% sub(Subject, RE, Rep) when is_list(Rep) ->
%%     case re:run(Subject, RE, [global]) of
%%         {match, Captured} ->
%%             lists:foldl(fun([M|_Groups], S) ->
%%                                 splice(S, match_to_index(M), Rep)
%%                         end, Subject, Captured);
%%         nomatch -> Subject
%%     end.

                                  
slugify(String) when is_list(String) ->
    %%CharacterRe = re:compile(CHAR_ENTITY_PATTER),
    
    %%TODO: unicode check

    SlugifyRE = [ %% Unicode normalization here
                  fun(S) -> string:to_lower(S) end,
                            {?QUOTE_PATTERN, ""},
                  {?NUMBERS_PATTERN, ""},
                  {?ALLOWED_CHARS_PATTERN,"-"},
                  {?DUPLICATE_DASH_PATTERN, "-"} 
                ],
    Slug0 = lists:foldl(fun({RE, Replacement}, Text) ->
                                re:replace(Text, RE, Replacement, [global]);
                           (Fun, Text) when is_function(Fun) ->
                                Fun(Text)
                        end, unicode:characters_to_list(String), SlugifyRE),
    Slug1 = binary_to_list(unicode:characters_to_binary(Slug0)),
    string:to_lower(Slug1).

    %% Replace quotes with dashes
    
    %% Character entity reference

%% Tests
slugify_test_() ->
    [
     ?_assertEqual("hello-world", slugify("Hello World"))
    ].
