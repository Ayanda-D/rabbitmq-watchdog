%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ Watchdog.
%%
%% The Developer of this component is Erlang Solutions, Ltd.
%% Copyright (c) 2017-2018 Erlang Solutions, Ltd.  All rights reserved.
%%

-module(rabbit_watchdog_controller).
-behaviour(gen_server2).

-include_lib("rabbit_watchdog.hrl").

-export([start_link/0, stop_watchdog/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% ------------------------------------------------------------
-spec start_link()          -> rabbit_types:ok_pid_or_error().
-spec stop_watchdog(atom()) -> 'ok'.
-spec stop()                -> 'ok'.
% ------------------------------------------------------------

-define(WATCHDOG_CTL,  ?MODULE).

-record(state,
                {parent_sup,    %% parent supervisor pid
                 watchdogs,     %% watchdog definitions
                 watchdog_pids  %% watchdog proc supervisors
                }).

% ----
% API
% ----
start_link() ->
    gen_server2:start_link({local, ?WATCHDOG_CTL}, ?MODULE, [self()], []).

stop_watchdog(WDMod) ->
    gen_server2:call(?WATCHDOG_CTL, {stop_watchdog, WDMod}).

stop() ->
    gen_server2:stop(?WATCHDOG_CTL).

% ----------
% Callbacks
% ----------
init([PSup]) ->
    State = #state{watchdogs = rabbit_watchdog:get_env(watchdogs, [])},
    self() ! '$start_watchdogs',
    {ok, State#state{parent_sup = PSup}}.

handle_call({stop_watchdog, WDMod}, _From, State = #state{watchdog_pids = WDPids}) ->
    case rabbit_misc:pget(WDMod, WDPids) of
            WDSPid when is_pid(WDSPid) ->
                %% terminate's parent watchdog-sup
                exit(WDSPid, normal);
            _ -> void
    end,
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info('$start_watchdogs', S = #state{parent_sup = PSup,
                                           watchdogs  = Watchdogs}) ->
    WDPids = start_watchdogs(PSup, Watchdogs),
    {noreply, S#state{watchdog_pids = WDPids}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{watchdog_pids = WDPids}) ->
    ok = stop_watchdogs(WDPids).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% ---------
% Internal
% ---------
start_watchdogs(Sup, Watchdogs) ->
    lists:foldl(fun({Name, Mod, Interval, UData}, Procs) ->
    [{Mod, begin
               {ok, Mod}       = validate_mod(Mod),
               {ok, Interval0} = validate_interval(Interval),
               {ok, ProcPID}   = supervisor2:start_child(Sup,
                    {Mod,
                       {rabbit_watchdog_proc_sup, start_link,
                           [Name, Mod, Interval0, UData]},
                       transient, 16#ffffffff, supervisor,
                       [rabbit_watchdog_proc_sup]}),
               ProcPID
           end} | Procs ] end, [], Watchdogs).

validate_mod(Mod) when is_atom(Mod) ->
    case code:which(Mod) of
        non_existing -> throw({bad_config, {non_existing, Mod}});
        _exists      -> {ok, Mod}
    end;
validate_mod(Mod) -> validate_mod(rabbit_data_coercion:to_atom(Mod)).

validate_interval(Interval) when is_integer(Interval) -> {ok, Interval};
validate_interval(Interval) -> {ok, rabbit_data_coercion:to_integer(Interval)}.

stop_watchdogs(WDPids) ->
    [exit(WDSPid, normal) || WDSPid <- WDPids].
