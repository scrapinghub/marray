-module(marray).

-export([new/1]).
-export([get/2]).
-export([set/3]).
-export([count/1]).
-export([to_list/1]).


-define(MIN, 0).
-define(MAX, 65535).

-type uint16() :: 0..65535.


-spec new(List) -> Result when
      List    :: [uint16()],
      Result  :: {ok, term()}
               | {error, overflow}
               | {error, {bad_element, Pos, Element}}
               | {error, {index_out_of_range, Pos, Count}},
      Pos     :: uint16(),
      Element :: non_neg_integer(),
      Count   :: non_neg_integer().
new(List) ->
    Len = length(List),
    case (length(List) =< ?MAX + 1) of
        true ->
            {ok, Array} = marray_nif:new(Len),
            FoldFun = fun(Element, Pos) ->
                case set(Pos, Element, Array) of
                    ok    -> ok;
                    Error -> throw(Error)
                end,
                Pos + 1
            end,
            case catch lists:foldl(FoldFun, 0, List) of
                {error, _} = Err -> Err;
                _                -> {ok, Array}
            end;
        false ->
            {error, overflow}
    end.


-spec set(Pos, Value, Arr) -> Result when
      Pos    :: uint16(),
      Value  :: uint16(),
      Arr    :: term(),
      Result :: ok
              | {error, index_out_of_range}.
set(Pos, Value, Arr) when
      (is_integer(Pos) andalso Pos >= ?MIN andalso Pos =< ?MAX) andalso
      (is_integer(Value) andalso Value >= ?MIN andalso Value =< ?MAX)
->
    Count = count(Arr),
    case Pos >= Count of
        true ->
            {error, {index_out_of_range, Pos, Count}};
        false ->
            marray_nif:set(Pos, Value, Arr),
            ok
    end;
set(Pos, Value, _Arr) ->
    {error, {badarg, Pos, Value}}.


-spec get(Pos, Arr) -> Result when
      Pos    :: uint16(),
      Arr    :: term(),
      Result :: {ok, uint16()}
              | {error, {index_out_of_range, Pos, Count}},
      Pos    :: non_neg_integer(),
      Count  :: non_neg_integer().
get(Pos, Arr) ->
    Count = count(Arr),
    case Pos >= Count of
        true  -> {error, {index_out_of_range, Pos, Count}};
        false -> {ok, marray_nif:get(Pos, Arr)}
    end.


-spec count(Arr) -> Result when
      Arr    :: term(),
      Result :: uint16().
count(Arr) ->
    marray_nif:count(Arr).


-spec to_list(Arr) -> List when
      Arr  :: term(),
      List :: [uint16()].
to_list(Arr) ->
    Count = count(Arr),
    case Count of
        0 ->
            [];
        _ ->
            lists:map(
                fun(Pos) -> {ok, Elem} = get(Pos, Arr), Elem end,
                lists:seq(0, Count - 1)
            )
    end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

marray_test_() ->
    [
        {"new", fun() ->
            Seq = lists:seq(?MIN, ?MAX),
            {ok, Array} = marray:new(Seq),
            ?assertEqual(
                Seq,
                lists:map(fun(Pos) -> {ok, Elem} = marray:get(Pos, Array), Elem end, Seq)
            )
        end},
        {"size overflow", fun() ->
            ?assertEqual({error, overflow}, marray:new(lists:seq(?MIN, ?MAX * 2)))
        end},
        {"bad elements (element is less than 0 or more than 65535)", fun() ->
            ?assertEqual({error, {badarg, 0, ?MIN - 1}}, marray:new([?MIN - 1])),
            ?assertEqual({error, {badarg, 0, ?MAX + 1}}, marray:new([?MAX + 1]))
        end},
        {"set at bad pos", fun() ->
            {ok, Array} = marray:new([]),
            ?assertEqual({error, {index_out_of_range, 1, 0}}, marray:set(1, 1, Array))
        end},
        {"get at bad pos", fun() ->
            {ok, Array} = marray:new([]),
            ?assertEqual({error, {index_out_of_range, 1, 0}}, marray:get(1, Array))
        end},
        {"to_list", fun() ->
            {ok, Array} = marray:new([]),
            ?assertEqual([], marray:to_list(Array)),
            Seq = lists:seq(?MIN, ?MAX),
            {ok, Array1} = marray:new(Seq),
            ?assertEqual(Seq, marray:to_list(Array1))
        end}
    ].

-endif.
