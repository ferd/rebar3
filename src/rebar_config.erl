%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
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
-module(rebar_config).

-export([consult/1
        ,consult_app_file/1
        ,consult_file/1
        ,consult_lock_file/1
        ,write_lock_file/3
        ,verify_config_format/1
        ,format_error/1

        ,merge_locks/2]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").
-define(LOCKVSN, "1.0.0").

%% ===================================================================
%% Public API
%% ===================================================================

-spec consult(file:name()) -> [any()].
consult(Dir) ->
    consult_file(filename:join(Dir, ?DEFAULT_CONFIG_FILE)).

consult_app_file(File) ->
    consult_file_(File).

-spec consult_lock_file(iodata()) -> {Locks, PluginLocks} when
      Locks :: [term()],
      PluginLocks :: [term()].
consult_lock_file(File) ->
    Terms = consult_file_(File),
    case Terms of
        [] ->
            {[], []};
        [Locks] when is_list(Locks) -> % beta lock file, single list
            {Locks, []};
        [{Vsn, Locks}|Attrs] when is_list(Locks) -> % maybe single term
            %% Make sure the warning above is to be shown whenever a version
            %% newer than the current one is being used, as we can't parse
            %% all the contents of the lock file properly.
            case Vsn of
                ?LOCKVSN ->
                    supported;
                _ ->
                    ?WARN("Rebar3 detected a lock file from a newer version. "
                          "It will be loaded in compatibility mode, but important "
                          "information may be missing or lost. It is recommended to "
                          "upgrade Rebar3.", [])
            end,
            read_attrs(Vsn, Locks, Attrs)
    end.

write_lock_file(LockFile, Locks, Attrs) ->
    {NewLocks, NewAttrs} = write_attrs(Locks, Attrs),
    case format_attrs(NewAttrs) of
        "" ->
            file:write_file(LockFile, io_lib:format("~p.~n", [NewLocks]));
        FormattedAttrs ->
            file:write_file(LockFile,
                            [io_lib:format("~p.~n", [{?LOCKVSN, NewLocks}]),
                             FormattedAttrs])
    end.

%% We expect all versions past ?LOCKVSN to support plugin locks
%% and care about no other attribute
read_attrs(_Vsn, Locks, Attrs) ->
    PluginLocks = proplists:get_value(plugin_locks, Attrs, []),
    {Locks, PluginLocks}.

write_attrs(Locks, Attrs) ->
    %% No attribute known that needs to be taken out of the structure,
    %% just return terms as is.
    {Locks, Attrs}.

format_attrs(Attrs) ->
    %% Only print attributes that are non-null, and in a {key, val} format
    [io_lib:format("~p.~n", [Attr]) || Attr = {_,V} <- Attrs,
                                       V =/= []].

consult_file(File) ->
    Terms = consult_file_(File),
    true = verify_config_format(Terms),
    Terms.

-spec consult_file_(file:name()) -> [any()].
consult_file_(File) when is_binary(File) ->
    consult_file_(binary_to_list(File));
consult_file_(File) ->
    case filename:extension(File) of
        ".script" ->
            {ok, Terms} = consult_and_eval(remove_script_ext(File), File),
            Terms;
        _ ->
            Script = File ++ ".script",
            case filelib:is_regular(Script) of
                true ->
                    {ok, Terms} = consult_and_eval(File, Script),
                    Terms;
                false ->
                    rebar_file_utils:try_consult(File)
            end
    end.

verify_config_format([]) ->
    true;
verify_config_format([{_Key, _Value} | T]) ->
    verify_config_format(T);
verify_config_format([Term | _]) ->
    throw(?PRV_ERROR({bad_config_format, Term})).

%% no lockfile
merge_locks(Config, []) ->
    Config;
%% lockfile with entries
merge_locks(Config, Locks) ->
    ConfigDeps = proplists:get_value(deps, Config, []),
    %% We want the top level deps only from the lock file.
    %% This ensures deterministic overrides for configs.
    %% Then check if any new deps have been added to the config
    %% since it was locked.
    Deps = [X || X <- Locks, element(3, X) =:= 0],
    NewDeps = find_newly_added(ConfigDeps, Locks),
    [{{locks, default}, Locks}, {{deps, default}, NewDeps++Deps} | Config].

format_error({bad_config_format, Term}) ->
    io_lib:format("Unable to parse config. Term is not in {Key, Value} format:~n~p", [Term]);
format_error({bad_dep_name, Dep}) ->
    io_lib:format("Dependency name must be an atom, instead found: ~p", [Dep]).

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec consult_and_eval(File::file:name_all(), Script::file:name_all()) ->
                              {ok, Terms::[term()]} |
                              {error, Reason::term()}.
consult_and_eval(File, Script) ->
    ?DEBUG("Evaluating config script ~p", [Script]),
    StateData = rebar_file_utils:try_consult(File),
    %% file:consult/1 always returns the terms as a list, however file:script
    %% can (and will) return any kind of term(), to make consult_and_eval
    %% work the same way as eval we ensure that when no list is returned we
    %% convert it in a list.
    case file:script(Script, bs([{'CONFIG', StateData}, {'SCRIPT', Script}])) of
        {ok, Terms} when is_list(Terms) ->
            {ok, Terms};
        {ok, Term} ->
            {ok, [Term]};
        Error ->
            Error
    end.

remove_script_ext(F) ->
    filename:rootname(F, ".script").

bs(Vars) ->
    lists:foldl(fun({K,V}, Bs) ->
                        erl_eval:add_binding(K, V, Bs)
                end, erl_eval:new_bindings(), Vars).

%% Find deps that have been added to the config after the lock was created
find_newly_added(ConfigDeps, LockedDeps) ->
    [D || {true, D} <- [check_newly_added(Dep, LockedDeps) || Dep <- ConfigDeps]].

check_newly_added({_, _}=Dep, LockedDeps) ->
    check_newly_added_(Dep, LockedDeps);
check_newly_added({_, _, {pkg, _}}=Dep, LockedDeps) ->
    check_newly_added_(Dep, LockedDeps);
check_newly_added({Name, _, Source}, LockedDeps) ->
    check_newly_added_({Name, Source}, LockedDeps);
check_newly_added(Dep, LockedDeps) ->
    check_newly_added_(Dep, LockedDeps).

check_newly_added_({Name, Vsn, Source}, LockedDeps) ->
    case check_newly_added_(Name, LockedDeps) of
        {true, Name1} ->
            {true, {Name1, Vsn, Source}};
        false ->
            false
    end;
check_newly_added_({Name, Source}, LockedDeps) ->
    case check_newly_added_(Name, LockedDeps) of
        {true, Name1} ->
            {true, {Name1, Source}};
        false ->
            false
    end;
check_newly_added_(Dep, LockedDeps) when is_atom(Dep) ->
    Name = ec_cnv:to_binary(Dep),
    case lists:keyfind(Name, 1, LockedDeps) of
        false ->
            {true, Name};
        Match ->
            case element(3, Match) of
                0 ->
                    {true, Name};
                _ ->
                    ?WARN("Newly added dep ~s is locked at a lower level. "
                          "If you really want to unlock it, use 'rebar3 upgrade ~s'",
                          [Name, Name]),
                    false
            end
    end;
check_newly_added_(Dep, _) ->
    throw(?PRV_ERROR({bad_dep_name, Dep})).
