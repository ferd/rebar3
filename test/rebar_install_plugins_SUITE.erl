-module(rebar_install_plugins_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%% TODO: add stuff to differentiate both plugin types
all() -> [{group, git}, {group, pkg}, {group, mixed}].

groups() ->
    [{unique, [], [flat, pick_highest_left, pick_highest_right,
                   pick_smallest1, pick_smallest2,
                   circular1, circular2, circular_skip,
                   fail_conflict, default_profile, nondefault_profile,
                   nondefault_pick_highest]},
     {git, [], [{group, unique}]},
     {pkg, [], [{group, unique}]},
     {mixed, [], [
        m_flat1, m_flat2, m_circular1, m_circular2,
        m_pick_source1, m_pick_source2, m_pick_source3,
        m_pick_source4, m_pick_source5, m_source_to_pkg,
        m_pkg_level1, m_pkg_level2, m_pkg_level3, m_pkg_level3_alpha_order
     ]}
    ].

init_per_suite(Config) ->
    application:start(meck),
    Config.

end_per_suite(_Config) ->
    application:stop(meck).

init_per_group(git, Config) ->
    [{plugins_type, git} | Config];
init_per_group(pkg, Config) ->
    [{plugins_type, pkg} | Config];
init_per_group(mixed, Config) ->
    [{plugins_type, mixed} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(Case, Config) when is_atom(Case) ->
    DepsType = ?config(plugins_type, Config),
    init_per_testcase({DepsType, Case}, Config);
init_per_testcase({mixed, Case}, Config) ->
    {Plugins, Warnings, Expect} = mplugins(Case),
    Expected = case Expect of
        {ok, List} -> {ok, format_expected_mplugins(List)};
        Other -> Other
    end,
    mock_warnings(),
    [{expect, Expected},
     {warnings, format_expected_mixed_warnings(Warnings)}
    | setup_project(Case, Config, rebar_test_utils:expand_deps(mixed, Plugins))];
init_per_testcase({PluginsType, Case}, Config) ->
    {Plugins, Warnings, Expect} = plugins(Case),
    Expected = case Expect of
        {ok, List} -> {ok, format_expected_plugins(List)};
        Other -> Other
    end,
    mock_warnings(),
    [{expect, Expected},
     {warnings, Warnings}
    | setup_project(Case, Config, rebar_test_utils:expand_deps(PluginsType, Plugins))].

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

format_expected_plugins(Plugins) ->
    lists:append([case Plugin of
        {N,V} -> [{plugin, N, V}, {plugin_lock, N, V}];
        N -> [{plugin, N}, {plugin_lock, N}]
    end || Plugin <- Plugins]).

format_expected_mplugins(Plugins) ->
    %% for mixed deps, lowercase is a package, uppercase is source.
    %% We can't check which was used from the dep, but the lock contains
    %% the type and we can use that information.
    lists:append([
    case Plugin of
        {N,V} when hd(N) >= $a, hd(N) =< $z ->
             UN = string:to_upper(N),
             [{plugin, UN, V}, {plugin_lock, pkg, UN, V}];
        {N,V} when hd(N) >= $A, hd(N) =< $Z ->
             [{plugin, N, V}, {plugin_lock, src, N, V}];
        N when hd(N) >= $a, hd(N) =< $z ->
             UN = string:to_upper(N),
             [{plugin, UN}, {plugin_lock, pkg, UN, "0.0.0"}];
        N when hd(N) >= $A, hd(N) =< $Z ->
             [{plugin, N}, {plugin_lock, src, N, "0.0.0"}]
    end || Plugin <- Plugins]).

format_expected_mixed_warnings(Warnings) ->
    [case W of
        {N, Vsn} when hd(N) >= $a, hd(N) =< $z -> {pkg, string:to_upper(N), Vsn};
        {N, Vsn} when hd(N) >= $A, hd(N) =< $Z -> {git, N, Vsn};
        N when hd(N) >= $a, hd(N) =< $z -> {pkg, string:to_upper(N), "0.0.0"};
        N when hd(N) >= $A, hd(N) =< $Z -> {git, N, "0.0.0"}
     end || W <- Warnings].

%% format:
%% {Spec,
%%  [Warning],
%%  {ok, Result} | {error, Reason}}
%%
%% Spec is a list of levelled dependencies of two possible forms:
%% - {"Name", Spec}
%% - {"Name", "Vsn", Spec}
%%
%% Warnings are going to match on mocked ?WARN(...)
%% calls to be evaluated. An empty list means we do not care about
%% warnings, not that no warnings will be printed. This means
%% the list of warning isn't interpreted to be exhaustive, and more
%% warnings may be generated than are listed.
plugins(flat) ->
    {[{"B", []},
      {"C", []}],
     [],
     {ok, ["B", "C"]}};
plugins(pick_highest_left) ->
    {[{"B", [{"C", "2", []}]},
      {"C", "1", []}],
     [{"C","2"}],
     {ok, ["B", {"C", "1"}]}};
plugins(pick_highest_right) ->
    {[{"B", "1", []},
      {"C", [{"B", "2", []}]}],
     [{"B","2"}],
     {ok, [{"B","1"}, "C"]}};
plugins(pick_smallest1) ->
    {[{"B", [{"D", "1", []}]},
      {"C", [{"D", "2", []}]}],
     [{"D","2"}],
     %% we pick D1 because B < C
     {ok, ["B","C",{"D","1"}]}};
plugins(pick_smallest2) ->
    {[{"C", [{"D", "2", []}]},
      {"B", [{"D", "1", []}]}],
     [{"D","2"}],
     %% we pick D1 because B < C
     {ok, ["B","C",{"D","1"}]}};
plugins(circular1) ->
    {[{"B", [{"A", []}]}, % A is the top-level app
      {"C", []}],
     [],
     {error, {rebar_prv_install_deps, {cycles, [[<<"A">>,<<"B">>]]}}}};
plugins(circular2) ->
    {[{"B", [{"C", [{"B", []}]}]},
      {"C", []}],
     [],
     {error, {rebar_prv_install_deps, {cycles, [[<<"B">>,<<"C">>]]}}}};
plugins(circular_skip) ->
    %% Never spot the circular dep due to being to low in the deps tree
    %% in source deps
    {[{"B", [{"C", "2", [{"B", []}]}]},
      {"C", "1", [{"D",[]}]}],
     [{"C","2"}],
     {ok, ["B", {"C","1"}, "D"]}};
plugins(fail_conflict) ->
    {[{"B", [{"C", "2", []}]},
      {"C", "1", []}],
     [{"C","2"}],
     rebar_abort};
plugins(default_profile) ->
    {[{"B", []},
      {"C", []}],
     [],
     {ok, ["B", "C"]}};
plugins(nondefault_profile) ->
    {[{"B", []},
      {"C", []}],
     [],
     {ok, ["B", "C"]}};
plugins(nondefault_pick_highest) ->
    %% This is all handled in setup_project
    {[],[],{ok,[]}}.

%% format:
%% Same as `deps/1' except "A" is a source dep
%% and "a" is a package dep.
mplugins(m_flat1) ->
    {[{"c", []},
      {"B", []}],
     [],
     {ok, ["B","c"]}};
mplugins(m_flat2) ->
    {[{"B", []},
      {"c", []}],
     [],
     {ok, ["B","c"]}};
mplugins(m_circular1) ->
    {[{"b", [{"a",[]}]}], % "A" is the top app
     [],
     {error, {rebar_prv_install_deps, {cycles, [[<<"A">>,<<"B">>]]}}}};
mplugins(m_circular2) ->
    {[{"B", [{"c", [{"b", []}]}]}],
     [],
     {error, {rebar_prv_install_deps, {cycles, [[<<"B">>,<<"C">>]]}}}};
mplugins(m_pick_source1) ->
    {[{"B", [{"D", []}]},
      {"c", [{"d", []}]}],
     ["d"],
     {ok, ["B", "c", "D"]}};
mplugins(m_pick_source2) ->
    %% The order of declaration is important.
    {[{"b", []},
      {"B", []}],
     [],
     {ok, ["b"]}};
mplugins(m_pick_source3) ->
    {[{"B", []},
      {"b", []}],
     [],
     {ok, ["B"]}};
mplugins(m_pick_source4) ->
    {[{"b", [{"d", "1", []}]},
      {"C", [{"D", "1", []}]}],
     [{"D", "1"}],
     {ok, ["b", "C", {"d", "1"}]}};
mplugins(m_pick_source5) ->
    {[{"B", [{"d", "1", []}]},
      {"C", [{"D", "1", []}]}],
     [{"D", "1"}],
     {ok, ["B", "C", {"d", "1"}]}};
mplugins(m_source_to_pkg) ->
    {[{"B", [{"c",[{"d", []}]}]}],
     [],
     {ok, ["B", "c", "d"]}};
mplugins(m_pkg_level1) ->
    {[{"B", [{"D", [{"e", "2", []}]}]},
      {"C", [{"e", "1", []}]}],
     [{"e","2"}],
     {ok, ["B","C","D",{"e","1"}]}};
mplugins(m_pkg_level2) ->
    {[{"B", [{"e", "1", []}]},
      {"C", [{"D", [{"e", "2", []}]}]}],
     [{"e","2"}],
     {ok, ["B","C","D",{"e","1"}]}};
mplugins(m_pkg_level3_alpha_order) ->
    {[{"B", [{"d", [{"f", "1", []}]}]},
      {"C", [{"E", [{"f", "2", []}]}]}],
     [{"f","2"}],
     {ok, ["B","C","d","E",{"f","1"}]}};
mplugins(m_pkg_level3) ->
    {[{"B", [{"d", [{"f", "1", []}]}]},
      {"C", [{"E", [{"G", [{"f", "2", []}]}]}]}],
     [{"f","2"}],
     {ok, ["B","C","d","E","G",{"f","1"}]}}.

setup_project(fail_conflict, Config0, Plugins) ->
    PluginsType = ?config(plugins_type, Config0),
    Config = rebar_test_utils:init_rebar_state(
            Config0,
            "fail_conflict_"++atom_to_list(PluginsType)++"_"
    ),
    AppDir = ?config(apps, Config),
    rebar_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    TopPlugins = rebar_test_utils:top_level_deps(Plugins),
    %% Same option for deps and plugins to fail fast? Probably no
    %% choice if they share implementations
    RebarConf = rebar_test_utils:create_config(AppDir, [{plugins, TopPlugins},
                                                        {deps_error_on_conflict, true}]),
    {SrcPlugins, PkgPlugins} = rebar_test_utils:flat_deps(Plugins),
    mock_git_resource:mock([{deps, SrcPlugins}]),
    mock_pkg_resource:mock([{pkgdeps, PkgPlugins}]),
    [{rebarconfig, RebarConf} | Config];
setup_project(nondefault_profile, Config0, Plugins) ->
    PluginsType = ?config(plugins_type, Config0),
    Config = rebar_test_utils:init_rebar_state(
            Config0,
            "nondefault_profile_"++atom_to_list(PluginsType)++"_"
    ),
    AppDir = ?config(apps, Config),
    rebar_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    TopPlugins = rebar_test_utils:top_level_deps(Plugins),
    RebarConf = rebar_test_utils:create_config(AppDir, [{profiles, [
                                                            {nondef, [{plugins, TopPlugins}]}
                                                       ]}]),
    {SrcPlugins, PkgPlugins} = rebar_test_utils:flat_deps(Plugins),
    mock_git_resource:mock([{deps, SrcPlugins}]),
    mock_pkg_resource:mock([{pkgdeps, PkgPlugins}]),
    [{rebarconfig, RebarConf} | Config];
setup_project(nondefault_pick_highest, Config0, _) ->
    PluginsType = ?config(plugins_type, Config0),
    Config = rebar_test_utils:init_rebar_state(
            Config0,
            "nondefault_pick_highest_"++atom_to_list(PluginsType)++"_"
    ),
    AppDir = ?config(apps, Config),
    rebar_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    DefaultPlugins = rebar_test_utils:expand_deps(PluginsType, [{"B", [{"C", "1", []}]}]),
    ProfilePlugins = rebar_test_utils:expand_deps(PluginsType, [{"C", "2", []}]),
    DefaultTop = rebar_test_utils:top_level_deps(DefaultPlugins),
    ProfileTop = rebar_test_utils:top_level_deps(ProfilePlugins),
    RebarConf = rebar_test_utils:create_config(
            AppDir,
            [{plugins, DefaultTop},
             {profiles, [{nondef, [{plugins, ProfileTop}]}]}]
    ),
    case PluginsType of
        git ->
            {SrcPlugins, _} = rebar_test_utils:flat_deps(DefaultPlugins++ProfilePlugins),
            mock_git_resource:mock([{deps, SrcPlugins}]);
        pkg ->
            {_, PkgPlugins} = rebar_test_utils:flat_deps(DefaultPlugins++ProfilePlugins),
            mock_pkg_resource:mock([{pkgdeps, PkgPlugins}])
    end,
    [{rebarconfig, RebarConf} | Config];
setup_project(Case, Config0, Plugins) ->
    PluginsType = ?config(plugins_type, Config0),
    Config = rebar_test_utils:init_rebar_state(
            Config0,
            atom_to_list(Case)++"_installplugins_"++atom_to_list(PluginsType)++"_"
    ),
    AppDir = ?config(apps, Config),
    rebar_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    TopPlugins = rebar_test_utils:top_level_deps(Plugins),
    RebarConf = rebar_test_utils:create_config(AppDir, [{plugins, TopPlugins}]),
    {SrcPlugins, PkgPlugins} = rebar_test_utils:flat_deps(Plugins),
    mock_git_resource:mock([{deps, SrcPlugins}]),
    mock_pkg_resource:mock([{pkgdeps, PkgPlugins}]),
    [{rebarconfig, RebarConf} | Config].

mock_warnings() ->
    %% just let it do its thing, we check warnings through
    %% the call log.
    meck:new(rebar_log, [no_link, passthrough]).

%%% TESTS %%%
flat(Config) -> run(Config).
pick_highest_left(Config) -> run(Config).
pick_highest_right(Config) -> run(Config).
pick_smallest1(Config) -> run(Config).
pick_smallest2(Config) -> run(Config).
circular1(Config) -> run(Config).
circular2(Config) -> run(Config).
circular_skip(Config) -> run(Config).

fail_conflict(Config) ->
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["lock"], ?config(expect, Config)
    ),
    check_warnings(error_calls(), ?config(warnings, Config), ?config(deps_type, Config)).

default_profile(Config) ->
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    AppDir = ?config(apps, Config),
    {ok, Apps} = Expect = ?config(expect, Config),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["lock"], Expect
    ),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["as", "profile", "lock"], Expect
    ),
    check_warnings(error_calls(), ?config(warnings, Config), ?config(deps_type, Config)),
    BuildDir = filename:join([AppDir, "_build"]),
    [?assertMatch({ok, #file_info{type=directory}},
                  file:read_file_info(filename:join([BuildDir, "default", "plugins", App])))
     || {plugin, App} <- Apps],
    [?assertMatch({ok, #file_info{type=directory}}, % somehow symlinks return dirs
                  file:read_file_info(filename:join([BuildDir, "profile", "plugins", App])))
     || {plugin, App} <- Apps],
    %% A second run to another profile also links default to the right spot
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["lock"], Expect
    ),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["as", "other", "lock"], Expect
    ),
    [?assertMatch({ok, #file_info{type=directory}}, % somehow symlinks return dirs
                  file:read_file_info(filename:join([BuildDir, "other", "plugins", App])))
     || {plugin, App} <- Apps].

nondefault_profile(Config) ->
    %% The dependencies here are saved directly to the
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    AppDir = ?config(apps, Config),
    {ok, AppLocks} = ?config(expect, Config),
    try
        rebar_test_utils:run_and_check(
            Config, RebarConfig, ["as", "nondef", "lock"], {ok, AppLocks}
        ),
        error(generated_locks)
    catch
        error:generated_locks -> error(generated_locks);
        _:_ -> ok
    end,
    Apps = [App || App = {plugin, _} <- AppLocks],
    Expect = {ok, Apps},
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["as", "nondef", "lock"], Expect
    ),
    check_warnings(error_calls(), ?config(warnings, Config), ?config(deps_type, Config)),
    BuildDir = filename:join([AppDir, "_build"]),
    [?assertMatch({error, enoent},
                  file:read_file_info(filename:join([BuildDir, "default", "plugins", App])))
     || {plugin, App} <- Apps],
    [?assertMatch({ok, #file_info{type=directory}},
                  file:read_file_info(filename:join([BuildDir, "nondef", "plugins", App])))
     || {plugin, App} <- Apps],
    %% A second run to another profile doesn't link dependencies
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["as", "other", "lock"], Expect
    ),
    [?assertMatch({error, enoent},
                  file:read_file_info(filename:join([BuildDir, "default", "plugins", App])))
     || {plugin, App} <- Apps].

nondefault_pick_highest(Config) ->
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["lock"],
        {ok, [{plugin, "B"}, {plugin_lock, "B"},
              {plugin_lock, "C", "1"}, {plugin, "C", "1"}], "default"}
    ),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["as", "nondef", "lock"],
        {ok, [{plugin, "B"}, {plugin_lock, "B"},
              {plugin_lock, "C", "1"}, {plugin, "C", "2"}], "nondef"}
    ),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["lock"],
        {ok, [{plugin, "B"}, {plugin_lock, "B"},
              {plugin, "C", "1"}, {plugin_lock, "C", "1"}], "default"}
    ),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["as", "nondef", "lock"],
        {ok, [{plugin, "B"}, {plugin_lock, "B"},
              {plugin_lock, "C", "1"}, {plugin, "C", "2"}], "nondef"}
    ).

m_flat1(Config) -> run(Config).
m_flat2(Config) -> run(Config).
m_circular1(Config) -> run(Config).
m_circular2(Config) -> run(Config).
m_pick_source1(Config) -> run(Config).
m_pick_source2(Config) -> run(Config).
m_pick_source3(Config) -> run(Config).
m_pick_source4(Config) -> run(Config).
m_pick_source5(Config) -> run(Config).
m_source_to_pkg(Config) -> run(Config).
m_pkg_level1(Config) -> run(Config).
m_pkg_level2(Config) -> run(Config).
m_pkg_level3(Config) -> run(Config).
m_pkg_level3_alpha_order(Config) -> run(Config).

run(Config) ->
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["lock"], ?config(expect, Config)
    ),
    check_warnings(warning_calls(), ?config(warnings, Config), ?config(plugins_type, Config)).

warning_calls() ->
    History = meck:history(rebar_log),
    [{Str, Args} || {_, {rebar_log, log, [warn, Str, Args]}, _} <- History].

error_calls() ->
    History = meck:history(rebar_log),
    [{Str, Args} || {_, {rebar_log, log, [error, Str, Args]}, _} <- History].

check_warnings(_, [], _) ->
    ok;
check_warnings(Warns, [{Type, Name, Vsn} | Rest], mixed) ->
    ct:pal("Checking for warning ~p in ~p", [{Name,Vsn},Warns]),
    ?assert(in_warnings(Type, Warns, Name, Vsn)),
    check_warnings(Warns, Rest, mixed);
check_warnings(Warns, [{Name, Vsn} | Rest], Type) ->
    ct:pal("Checking for warning ~p in ~p", [{Name,Vsn},Warns]),
    ?assert(in_warnings(Type, Warns, Name, Vsn)),
    check_warnings(Warns, Rest, Type).

in_warnings(git, Warns, NameRaw, VsnRaw) ->
    Name = iolist_to_binary(NameRaw),
    1 =< length([1 || {_, [AppName, {git, _, {_, Vsn}}]} <- Warns,
                      AppName =:= Name, Vsn =:= VsnRaw]);
in_warnings(pkg, Warns, NameRaw, VsnRaw) ->
    Name = iolist_to_binary(NameRaw),
    Vsn = iolist_to_binary(VsnRaw),
    1 =< length([1 || {_, [AppName, {pkg, _, AppVsn}]} <- Warns,
                      AppName =:= Name, AppVsn =:= Vsn]).

