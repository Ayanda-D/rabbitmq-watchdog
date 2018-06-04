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

-module(rabbit_watchdog_proc).

-behaviour(gen_server2).

-include_lib("rabbit_watchdog.hrl").

-export([start_link/4, info/1, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
                 {parent_sup,
                  ref,
                  tref,
                  callback,
                  callback_s,
                  title,
                  interval
                 }).

% ----
% API
% ----
start_link(Title, CBMod, Interval, UData) ->
    gen_server2:start_link({local, CBMod}, ?MODULE,
        [self(), Title, CBMod, Interval, UData], []).

info(Name) when is_atom(Name) ->
    gen_server2:call(Name, info).

stop(Name) when is_atom(Name) ->
    gen_server2:stop(Name).

% ----------
% Callbacks
% ----------
init([PSup, Title, CBMod, Interval, UData]) ->
    {ok, CBstate} = case catch CBMod:init(UData) of
                        {ok, _CBstate0} = Return -> Return;
                        Other -> exit(?WATCHDOG_ERR(CBMod, init, Other))
                    end,
    {ok, {TRef, Ref}} = schedule_watchdog(CBMod, Interval),
    ?INFO_MSG("~p starting", [CBMod]),
    {ok, #state{parent_sup = PSup,
                callback   = CBMod,
                callback_s = CBstate,
                title      = Title,
                interval   = Interval,
                ref        = Ref,
                tref       = TRef}}.

handle_call(info, _From, State = #state{callback   = CBMod,
                                        callback_s = CBstate,
                                        title      = Title,
                                        interval   = Interval}) ->
    I = [{watchdog, CBMod},{title, Title},{state, CBstate},{interval, Interval}],
    {reply, {ok, I}, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({Ref, watchdog}, S = #state{ref        = Ref,
                                        callback   = CBMod,
                                        callback_s = CBstate,
                                        interval   = Interval}) ->
    {ok, CBstate2} =
        case catch CBMod:validate(CBstate) of
            {noaction, CBstate0} -> {ok, CBstate0};
            {action,   CBstate0} ->
                case catch CBMod:action(CBstate0) of
                    {ok, _CBstate1} = Return ->
                        ?WARN_MSG("action applied"),
                        Return;
                    Other ->
                        %% If there's a problem, terminate watchdog proc
                        exit(?WATCHDOG_ERR(CBMod, action, Other))
                end;
            Other ->
                exit(?WATCHDOG_ERR(CBMod, validate, Other))
        end,
    {ok, {TRef, Ref0}} = schedule_watchdog(CBMod, Interval),
    {noreply, S#state{ref        = Ref0,
                      tref       = TRef,
                      callback_s = CBstate2}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State = #state{callback = CBMod, callback_s = CBstate}) ->
    ?INFO_MSG("terminating"),
    catch CBMod:terminate(CBstate),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

schedule_watchdog(Proc, T) ->
    {ok, TRef} = timer:send_after(T, Proc, {Ref = make_ref(), watchdog}),
    {ok, {TRef, Ref}}.
