-module(druuid).

-export([new/0,
         uuid/1]).

-on_load(init/0).

-define(nif_stub, nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    SoName = case code:priv_dir(?MODULE) of
                 {error, bad_name} ->
                     case code:which(?MODULE) of
                         Filename when is_list(Filename) ->
                             filename:join([filename:dirname(Filename),"../priv", "druuid"]);
                         _ ->
                             filename:join("../priv", "druuid")
                     end;
                 Dir ->
                     filename:join(Dir, "druuid")
             end,
    erlang:load_nif(SoName, 0).

new() ->
    ?nif_stub.

uuid(_Ref) ->
    ?nif_stub.

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = new(),
    ?assertEqual(ok, uuid(Ref)).

-endif.
