%%% Copyright (c) 2021 Olle Mattsson <rymdolle@gmail.com>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.

-module(rodent_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, App} = application:get_application(),
    Host = application:get_env(App, host, "localhost"),
    Port = application:get_env(App, port, 70),
    %% Sort with longest selector first
    Routes = lists:sort(fun(#{selector := A}, #{selector := B}) ->
                                length(A) > length(B)
                        end, [Route#{selector => re:split(maps:get(selector, Route), "/")} ||
                                 Route <- application:get_env(App, routes, [])]),
    Options = #{host => Host, port => Port, routes => Routes},
    Listener = ranch:child_spec(App,
                                ranch_tcp, [{port, Port}],
                                rodent_protocol, Options),
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},
    Procs = [Listener],
    {ok, {SupFlags, Procs}}.

%% internal functions
