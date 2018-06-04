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

-module(rabbit_watchdog_rabbit_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("rabbit_watchdog.hrl").

-compile(export_all).

all() ->
    [
      {group, non_parallel_tests}
    ].

groups() ->
    [
      {non_parallel_tests, [], [
          % watchdog_with_action_application_terminated,
          % watchdog_with_action_resource_limit_failure,
          watchdog_with_no_action
        ]}
    ].

%% -------------------------------------------------------------------
%% Testsuite setup/teardown.
%% -------------------------------------------------------------------

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    Config1 = rabbit_ct_helpers:set_config(Config, [
        {rmq_nodename_suffix, ?MODULE}
      ]),
    rabbit_ct_helpers:run_setup_steps(Config1,
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
% watchdog_with_action_application_terminated(Config) ->
%    ok = setup_watchdog(Config, 0, 200, 40),
%    ok = verify_terminated_rabbit(Config),
%    rabbit_watchdog:delay(300),
%    ok = verify_running_rabbit(Config),
%    passed.

% watchdog_with_action_resource_limit_failure(Config) ->
%    ok = setup_watchdog(Config, 0, 200, 40),
%    ok = raise_vm_memory_highwater_mark_alarm(Config),
%    ok = verify_suspended_rabbit(Config),
%    rabbit_watchdog:delay(300),
%    ok = verify_cleared_vm_memory_highwater_mark_alarm(Config),
%    ok = verify_running_rabbit(Config),
%    passed.

watchdog_with_no_action(Config) ->
    ok = setup_watchdog(Config, 0, 200, 40),
    rabbit_watchdog:delay(300),
    ok = verify_running_rabbit(Config),
    passed.

%% ---------
%% Internal
%% ---------
setup_watchdog(Config, Node, WD_Interval, Delay) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, Node,
            ?MODULE, init_rabbit_watchdog_remote, [WD_Interval, Delay]).

init_rabbit_watchdog_remote(WD_Interval, Delay) ->
    application:set_env(rabbitmq_watchdog, watchdogs,
        [{"Rabbit WD", rabbit_watchdog_rabbit, WD_Interval, [{delay, Delay}]}]),
    ok = application:start(rabbitmq_watchdog).

verify_running_rabbit(Config) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, 0,
      ?MODULE, verify_running_rabbit1, []).

verify_running_rabbit1() ->
    case [A || {A = rabbit, _, _} <- application:which_applications()] of
        [_] -> ok;
        _   -> throw({error, {not_running, rabbit}})
    end.
