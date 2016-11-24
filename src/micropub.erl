-module(micropub).

-include("couch_db.hrl").

-export([create/2]).
-import(slugify, [slugify/1]).
-define(ENTRY_PROPERTIES, ["name", "summary", "content", "published", "updated", "category", "location", "in-reply-to", "like-of", "repost-of", "syndication"]).
-define(MP_PROPERTIES, ["mp-syndicate-to"]).
%-define(MP_MF_ALIASES, [{<<"published">>, <<"created_at">>}]).

published() ->
    {Date, Time} = calendar:now_to_datetime(erlang:now()),
    [format_date(Date), "T", format_time(Time), "Z"].
format_date(Date) ->
    format(Date, "-").
format_time(Time) ->
    format(Time, ":").
format(Tuple, Sep) ->
    string:join(lists:map(fun padded_int/1, erlang:tuple_to_list(Tuple)), Sep).
padded_int(Int) ->
    SInt = erlang:integer_to_list(Int),
    case string:len(SInt) =< 2 of
        true ->
            string:right(erlang:integer_to_list(Int), 2, $0);
        false ->
            SInt
    end.

make_id(Data) ->
    Stub = case proplists:get_value(<<"name">>, Data) of
               undefined ->
                   ["note-", date_to_id(get_value_as_string(<<"published">>, Data))];
               Name ->
                   [slugify(Name), "-", pub_date(Data)]
           end,
    erlang:list_to_binary(Stub).

date_to_id(DateStr) when erlang:is_list(DateStr) ->
    string:join(string:tokens(DateStr, ": "), "").

pub_date(Data) ->
    [Date|_] = string:tokens(get_value_as_string(<<"published">>, Data), "T "),
    Date.

get_value_as_string(Key, PropList) ->
    case proplists:get_value(Key, PropList) of
        [Value|_] when erlang:is_binary(Value) ->
            erlang:binary_to_list(Value);
        [Value|_] when erlang:is_list(Value) ->
            Value;
        undefined ->
            undefined
    end.

create({_}=Json, Ident) ->
    create_from_json(Json, Ident);
create(FormData, Ident) ->
    create_from_form(FormData, Ident).

create_from_json({Entry}, Ident) ->
    {Props0} = proplists:get_value(<<"properties">>, Entry),
    Props = merge_props(Props0, Ident),
    {[{<<"_id">>, make_id(Props)} | replace_properties(Entry, Props) ]}.

replace_properties(Entry, NewProps) ->
    [ {<<"properties">>, {NewProps} } | proplists:delete(<<"properties">>, Entry) ].

merge_props(Props, Ident) ->
    Author = proplists:get_value("me", Ident),
    Defaults = [ {<<"author">>, [Author] },
                 {<<"published">>, [erlang:list_to_binary(published())] }
               ],
    lists:foldl(fun({Key, _ } = Prop, Acc) ->
                        case proplists:get_value(Key, Acc) of
                            undefined ->
                                [Prop|Acc];
                            _ ->
                                Acc
                        end
                end, Props, Defaults).

   
create_from_form(Data, Ident) ->
    Data1 = lists:map(fun ({Key, Value}) ->
                              {Key, erlang:list_to_binary(Value)}
                      end, Data),
    Props = merge_props(make_props(Data1), Ident),
    %Doc2 = proplists:substitute_aliases(?MP_MF_ALIASES, Doc1),
    {[{<<"_id">>, make_id(Props)}|make_htype(Props, Data1)]}.

make_htype(Doc, Data) ->
    [{ <<"type">>, [ normalize_htype(proplists:get_value("h", Data)) ] } | [{ <<"properties">>, {Doc} }]].

normalize_htype(Type) when erlang:is_binary(Type) ->
    TString = erlang:binary_to_list(Type),
    case string:substr(TString, 1, 2) =:= "h-" of
        true ->
            Type;
        false ->
            erlang:list_to_binary(string:concat("h-", erlang:binary_to_list(Type)))
    end.

make_props(Data) ->
    lists:foldl(fun (X, Sum) ->
                        case proplists:get_all_values(X, Data) of
                            [] ->
                                Sum;
                            Value ->
                                [{X, Value}|Sum]
                        end
                end, [], ?ENTRY_PROPERTIES).
