-module(marray_nif).

-on_load(load_nif/0).
-export([new/1]).
-export([get/2]).
-export([set/3]).
-export([count/1]).


load_nif() ->
    PrivDir = case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end,
    erlang:load_nif(filename:join(PrivDir, "marray"), 0).


new(_Size) ->
    exit(nif_not_loaded).


set(_Pos, _Value, _Arr) ->
    exit(nif_not_loaded).


get(_Pos, _Arr) ->
    exit(nif_not_loaded).


count(_Hash) ->
    exit(nif_not_loaded).
