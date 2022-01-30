-module(marray).

-export([new/1]).
-export([get/2]).
-export([set/3]).
-export([count/1]).
-export([to_list/1]).


-define(MIN, 0).
-define(MAX, 4294967295).

-type uint32() :: 0..4294967295.


-spec new(List) -> Result when
      List    :: [uint32()],
      Result  :: {ok, term()}
               | {error, overflow}
               | {error, {bad_element, Pos, Element}}
               | {error, {index_out_of_range, Pos, Count}},
      Pos     :: uint32(),
      Element :: non_neg_integer(),
      Count   :: non_neg_integer().
new(List) ->
    Len = length(List),
    case (Len =< ?MAX) of
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
      Pos    :: uint32(),
      Value  :: uint32(),
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
      Pos    :: uint32(),
      Arr    :: term(),
      Result :: {ok, uint32()}
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
      Result :: uint32().
count(Arr) ->
    marray_nif:count(Arr).


-spec to_list(Arr) -> List when
      Arr  :: term(),
      List :: [uint32()].
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
