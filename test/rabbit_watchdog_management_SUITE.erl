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
%% Copyright (c) 2017-2018 Erlang Solutions, Ltd. All rights reserved.
%%

-module(rabbit_watchdog_management_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("rabbit_watchdog.hrl").

-define(COLLECT_INTERVAL, 100).
-define(RABBITMQ_MGMT_STARTUP_TIMEOUT, 10000).
-define(WATCHDOG_INTERVAL, 200).

-define(LOW_LIMIT_WATERMARK, 0.0001).

-compile(export_all).

all() ->
    [
      {group, non_parallel_tests}
    ].

groups() ->
    [
      {non_parallel_tests, [], [
          watchdog_with_memory_alarm_detected
        ]}
    ].

%% -------------------------------------------------------------------
%% Testsuite setup/teardown.
%% -------------------------------------------------------------------

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    Config1 = rabbit_ct_helpers:set_config(
                Config, [{rmq_nodename_suffix, ?MODULE}]),
    Config2 = rabbit_ct_helpers:merge_app_env(Config1,
        {rabbit, [{collect_statistics_interval, ?COLLECT_INTERVAL}]}),
    rabbit_ct_helpers:run_setup_steps(Config2,
      rabbit_ct_broker_helpers:setup_steps() ++
      rabbit_ct_client_helpers:setup_steps()).

end_per_suite(Config) ->
    rabbit_ct_helpers:run_teardown_steps(Config,
      rabbit_ct_client_helpers:teardown_steps() ++
      rabbit_ct_broker_helpers:teardown_steps()).

init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_started(Config, Testcase),
    ok = rabbit_ct_broker_helpers:rpc(Config, 0,
      application, stop, [rabbitmq_watchdog]),
    Config.

end_per_testcase(Testcase, Config) ->
    rabbit_ct_helpers:testcase_finished(Config, Testcase).

%% -----------
%% Test Cases
%% -----------

watchdog_with_memory_alarm_detected(Config) ->
    ok = setup_watchdog(Config, 0, ?WATCHDOG_INTERVAL, 40),

    OrigManagementPid = rabbitmq_management_pid(Config),

    ok = trigger_memory_alarm(Config),

    case rabbitmq_management_pid(Config) of
        OrigManagementPid ->
            error({not_restarted, rabbitmq_management});
        _Other ->
            ok
    end,

    passed.

%% ---------
%% Internal
%% ---------

trigger_memory_alarm(Config) ->
    OriginalVmMemHighWatermark = get_vm_memory_high_watermark(Config),
    set_vm_memory_high_watermark(Config, ?LOW_LIMIT_WATERMARK),
    rabbit_watchdog:delay(?WATCHDOG_INTERVAL * 2),
    set_vm_memory_high_watermark(Config, OriginalVmMemHighWatermark),
    rabbit_watchdog:delay(?WATCHDOG_INTERVAL * 2),
    ok.

setup_watchdog(Config, Node, WD_Interval, Delay) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, Node,
           ?MODULE, init_rabbit_watchdog_remote, [WD_Interval, Delay]),
    rabbit_watchdog:delay(?WATCHDOG_INTERVAL * 2).

init_rabbit_watchdog_remote(WD_Interval, Delay) ->
    application:set_env(rabbitmq_watchdog, watchdogs,
        [{"Rabbit Management WD", rabbit_watchdog_management, WD_Interval,
          [{delay, Delay}]}]),
    ok = application:start(rabbitmq_watchdog).

rabbitmq_management_pid(Config) ->
    rabbit_ct_broker_helpers:rpc(Config, 0,
      ?MODULE, rabbitmq_management_pid_remote, []).

rabbitmq_management_pid_remote() ->
    TimerChecker =
        spawn_link(
            fun () -> receive
                          {timeout, _TRef, _Msg} ->
                              error({not_running, rabbitmq_management});
                          stop ->
                              ok
                      end
            end),

    TRef = erlang:start_timer(?RABBITMQ_MGMT_STARTUP_TIMEOUT, TimerChecker, ""),

    rabbitmq_management_pid_remote(TRef, TimerChecker).

rabbitmq_management_pid_remote(TRef, TimerChecker) ->
    RunningApps = proplists:get_value(running, application:info(), []),
    case proplists:get_value(rabbitmq_management, RunningApps) of
        undefined ->
            rabbitmq_management_pid_remote(TRef, TimerChecker);
        Pid ->
            unlink(TimerChecker),
            erlang:cancel_timer(TRef),
            TimerChecker ! stop,
            Pid
    end.

get_vm_memory_high_watermark(Config) ->
    rabbit_ct_broker_helpers:rpc(Config, 0,
      vm_memory_monitor, get_vm_memory_high_watermark, []).

set_vm_memory_high_watermark(Config, Percentage) ->
    rabbit_ct_broker_helpers:rpc(Config, 0,
      vm_memory_monitor, set_vm_memory_high_watermark, [Percentage]).
