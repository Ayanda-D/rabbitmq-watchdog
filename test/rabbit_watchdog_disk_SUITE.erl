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

-module(rabbit_watchdog_disk_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("rabbit_watchdog.hrl").

-define(COLLECT_INTERVAL, 100).
-define(RABBITMQ_MGMT_STARTUP_TIMEOUT, 10000).
-define(WATCHDOG_INTERVAL, 200).

-define(NR_TMP_DIRS, 10).

-compile(export_all).

all() ->
    [
      {group, non_parallel_tests}
    ].

groups() ->
    [
      {non_parallel_tests, [], [
          watchdog_with_disk_free_limit_reached,
          watchdog_with_disk_free_limit_reached_forbidden_dirs,
          watchdog_with_disk_free_limit_reached_forbidden_dirs_forced,
          watchdog_with_disk_free_limit_reached_forbidden_dirs_hard,
          watchdog_with_disk_free_limit_reached_forbidden_dirs_other_mod
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

watchdog_with_disk_free_limit_reached(Config) ->
    TempDirs = lists:map(fun (_) -> create_temp_dir(Config) end,
                         lists:seq(1, ?NR_TMP_DIRS)),
    TempDirs = collect_existing_dirs(Config, TempDirs),

    ok = setup_watchdog(Config, 0, ?WATCHDOG_INTERVAL, TempDirs),

    ok = trigger_disk_free_alarm(Config),

    check_dirs_deleted(Config, TempDirs),

    passed.

watchdog_with_disk_free_limit_reached_forbidden_dirs(Config) ->
    forbidden_dir_test(Config).

watchdog_with_disk_free_limit_reached_forbidden_dirs_forced(Config) ->
    % Forced dirs are delted
    forbidden_dir_test(Config, force).

watchdog_with_disk_free_limit_reached_forbidden_dirs_hard(Config) ->
    % Hard dirs are delted
    forbidden_dir_test(Config, hard).

watchdog_with_disk_free_limit_reached_forbidden_dirs_other_mod(Config) ->
    % Other modifiers are ignored
    forbidden_dir_test(Config, other_mod).

%% ---------
%% Internal
%% ---------

forbidden_dir_test(Config) ->
    forbidden_dir_test(Config, undefined).
forbidden_dir_test(Config, Mod) ->
    case os:type() of
        {unix, _} ->
            Home = os:getenv("HOME"),
            TempDirs = lists:map(fun (_) -> create_temp_dir(Config, Home) end,
                                 lists:seq(1, ?NR_TMP_DIRS)),
            TempDirs = collect_existing_dirs(Config, TempDirs),

            {TempDirsWithMod, ExpectedToRemain} = case Mod of
                undefined ->
                    % No modifiers
                    {TempDirs, TempDirs};
                ForceOrHard when ForceOrHard =:= force; ForceOrHard =:= hard ->
                    {add_modifier(TempDirs, Mod), []};
                _Other ->
                    {add_modifier(TempDirs, Mod), TempDirs}
            end,

            ok = setup_watchdog(Config, 0, ?WATCHDOG_INTERVAL, TempDirsWithMod),

            ok = trigger_disk_free_alarm(Config),

            check_dirs_deleted(Config, TempDirs, ExpectedToRemain);

        _Else ->
            ok
    end,
    passed.

trigger_disk_free_alarm(Config) ->
    OrigDiskFreeLimit = get_disk_free_limit(Config),
    DiskFree = disk_free(Config),

    ok = set_disk_free_limit(Config, io_lib:format("~w", [100 * DiskFree])),
    rabbit_watchdog:delay(?WATCHDOG_INTERVAL * 2),
    set_disk_free_limit(Config, OrigDiskFreeLimit).

setup_watchdog(Config, Node, WD_Interval, TempDirs) ->
    ok = rabbit_ct_broker_helpers:rpc(Config, Node,
           ?MODULE, init_rabbit_watchdog_remote, [WD_Interval, TempDirs]),
    rabbit_watchdog:delay(?WATCHDOG_INTERVAL * 2).

init_rabbit_watchdog_remote(WD_Interval, TempDirs) ->
    application:set_env(rabbitmq_watchdog, watchdogs,
        [{"Disk Watchdog", rabbit_watchdog_disk, WD_Interval,
          [{dirs, TempDirs}]}]),
    ok = application:start(rabbitmq_watchdog).

set_disk_free_limit(Config, Limit) ->
    rabbit_ct_broker_helpers:rpc(
      Config, 0, rabbit_disk_monitor, set_disk_free_limit, [Limit]).

get_disk_free_limit(Config) ->
    rabbit_ct_broker_helpers:rpc(
      Config, 0, rabbit_disk_monitor, get_disk_free_limit, []).

disk_free(Config) ->
    RabbitStatus = rabbit_status(Config),
    rabbit_misc:pget(disk_free, RabbitStatus).

rabbit_status(Config) ->
    rabbit_ct_broker_helpers:rpc(Config, 0, rabbit, status, []).

create_temp_dir(Config) ->
    rabbit_ct_broker_helpers:rpc(
      Config, 0, ?MODULE, create_temp_dir_remote, []).
create_temp_dir(Config, ParDir) when is_list(ParDir) ->
    rabbit_ct_broker_helpers:rpc(
      Config, 0, ?MODULE, create_temp_dir_remote, [ParDir]).

create_temp_dir_remote() ->
    ParDir = get_tmp_dir(),
    create_temp_dir_remote(ParDir).
create_temp_dir_remote(ParDir) ->
    DirName = lists:flatten(
                io_lib:format("~s/tmp~p/",
                              [ParDir, erlang:unique_integer([positive])])),
    ok = filelib:ensure_dir(DirName),
    DirName.

get_tmp_dir() ->
    case {os:getenv("TEMP"), os:getenv("TMP")} of
        {false, false} ->
            "/tmp";
        {Other, false} when is_list(Other) ->
            Other;
        {_DontCare, Other} when is_list(Other) ->
            Other
    end.

cleanup_tmp_dirs([]) ->
    ok;
cleanup_tmp_dirs(Dirs) ->
    lists:foreach(fun (Dir) -> os:cmd("rm -rf " ++ Dir) end, Dirs).

collect_existing_dirs(Config, DirNames) ->
    rabbit_ct_broker_helpers:rpc(
      Config, 0, ?MODULE, collect_existing_dirs, [DirNames]).

collect_existing_dirs(DirNames) ->
    lists:filter(fun (Dir) -> filelib:is_dir(Dir) end, DirNames).

check_dirs_deleted(Config, TempDirs) ->
    check_dirs_deleted(Config, TempDirs, []).
check_dirs_deleted(Config, TempDirs, ExpectedToRemain) ->
    case collect_existing_dirs(Config, TempDirs) of
        ExpectedToRemain ->
            cleanup_tmp_dirs(ExpectedToRemain),
            ok;
        Dirs when is_list(Dirs) ->
            cleanup_tmp_dirs(Dirs),
            error({unexpected_dirs_found, Dirs -- ExpectedToRemain})
    end.

add_modifier(Dirs, Mod) ->
    lists:map(fun (Dir) -> {Dir, Mod} end, Dirs).
