%% -------------------------------------------------------------------
%% Copyright (C) 2011 by Kelly L. McLaughlin
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------

-module(druuid).

-export([v1/0,
         v2/0,
         v3/0,
         v4/0]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    SoName = case code:priv_dir(?MODULE) of
                 {error, bad_name} ->
                     case code:which(?MODULE) of
                         Filename when is_list(Filename) ->
                             filename:join([filename:dirname(Filename), "../priv", "druuid"]);
                         _ ->
                             filename:join("../priv", "druuid")
                     end;
                 Dir ->
                     filename:join(Dir, "druuid")
             end,
    erlang:load_nif(SoName, 0).

v1() ->
    %% @TODO Not yet implemented
    erlang:nif_error({error, not_loaded}).

v2() ->
    %% @TODO Not yet implemented
    erlang:nif_error({error, not_loaded}).

v3() ->
    %% @TODO Not yet implemented
    erlang:nif_error({error, not_loaded}).

v4() ->
    erlang:nif_error({error, not_loaded}).

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    ?assertEqual(36, size(v4())).

-endif.
