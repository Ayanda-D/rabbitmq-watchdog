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

-module(rabbit_watchdog_disk).

-behaviour(rabbit_watchdog).

-include_lib("rabbit_watchdog.hrl").

-export([init/1, validate/1, action/1, terminate/1]).

-define(FORBIDDEN_UNIX_DIRS,
   ["/usr", "/etc", "/opt", "/bin", "/home", "/opt", "/mnt", "/sbin", "/bin",
    "/dev", "/Volumes", "/Users", "/System", "/Applications", "/Library"
    "/Network", "/private" ]).
-define(FORBIDDEN_WINDOWS_DIRS,
    ["C:/Windows", "C:/Users", "C:\\Windows", "C:\\Users"]).

-define(FORBIDDEN_REMOVAL_WARNING(Dir),
        ?WARN_MSG("removing directory ~s is forbidden, to force "
                  "removal specify as {~s, force | hard} in the config",
                  [Dir, Dir])).

-record(disk_wd_state,
            {os,
             udata  = [] }).

% ----------
% Callbacks
% ----------
init(UData) ->
    {ok, #disk_wd_state{os = os:type(), udata = UData}}.

validate(State) ->
    {case rabbit_alarm:get_alarms() of
         []     -> noaction;
         Alarms when is_list(Alarms) ->
             IsResLimit = is_disk_alarm(Alarms),
             if IsResLimit ->
                    ?ERR_MSG("disk alarm detected"),
                    action;
                true -> noaction
             end
     end, State}.

action(State = #disk_wd_state{os = OS, udata = UData}) ->
    case rabbit_misc:pget(dirs, UData) of
        Dirs when length(Dirs) > 1 ->
            lists:foreach(fun (Dir) -> try_remove_dir(OS, Dir) end, Dirs);
        _Other ->
            ok
    end,
    {ok, State}.

try_remove_dir(OS, {Dir, Mod}) when Mod =:= force; Mod =:= hard ->
    case filelib:is_dir(Dir) of
        true ->
            remove_dir_with_notice(OS, Dir);
        _Else ->
            ?INFO_MSG("~s does not exist", [Dir])
    end;
try_remove_dir(OS, {Dir, _UnknownMod}) ->
    try_remove_dir(OS, Dir);
try_remove_dir(OS, Dir) ->
    case {filelib:is_dir(Dir), is_forbidden(OS, Dir)} of
        {true, false} ->
            remove_dir_with_notice(OS, Dir);
        {true, true} ->
            ?FORBIDDEN_REMOVAL_WARNING(Dir);
        _Else ->
            ?INFO_MSG("~s does not exist", [Dir])
    end.

remove_dir_with_notice(OS, Dir) ->
    ?WARN_MSG("removing directory " ++ Dir),
    rm_dir(OS, Dir).

terminate(_State) ->
    ok.

% --------
% Private
% --------
is_disk_alarm([]) -> false;
is_disk_alarm([{{resource_limit, disk, _Node}, _Info} | _Rem]) -> true;
is_disk_alarm([_|Rem]) -> is_disk_alarm(Rem).

is_forbidden({unix, _},  Dir) ->
    check_forbidden(Dir, ?FORBIDDEN_UNIX_DIRS);
is_forbidden({win32, _}, Dir) ->
    check_forbidden(Dir, ?FORBIDDEN_WINDOWS_DIRS).

check_forbidden(Dir, FDirs) ->
    try
        [check(Dir, FDir) || FDir <- FDirs ],
         false
    catch
        throw:{forbidden_dir, Dir} -> true
    end.

check(Dir, FDir) ->
    case re:run(Dir, FDir, [{capture, none}]) of
        match -> throw({forbidden_dir, Dir});
        _     -> ok
    end.

rm_dir(_OS, "/")        ->
    ?WARN_MSG("removing \"/\" directory is forbidden"),
    ok;
rm_dir({unix,  _OSname}, Dir) -> ?CMD("rm -rf "   ++ Dir);
rm_dir({win32, _OSname}, Dir) -> ?CMD("rd /s /q " ++ Dir).
