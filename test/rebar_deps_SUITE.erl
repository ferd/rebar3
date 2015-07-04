-module(rebar_deps_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [sub_app_deps, newly_added_dep, http_proxy_settings, https_proxy_settings, {group, git}, {group, pkg}].

groups() ->
    [{all, [], [flat, pick_highest_left, pick_highest_right,
                pick_smallest1, pick_smallest2,
                circular1, circular2, circular_skip]},
     {git, [], [{group, all}]},
     {pkg, [], [{group, all}]}].

init_per_suite(Config) ->
    application:start(meck),
    Config.

end_per_suite(_Config) ->
    application:stop(meck).

init_per_group(git, Config) ->
    [{deps_type, git} | Config];
init_per_group(pkg, Config) ->
    [{deps_type, pkg} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(newly_added_dep, Config) ->
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(sub_app_deps, Config) ->
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(http_proxy_settings, Config) ->
    %% Create private rebar.config
    Priv = ?config(priv_dir, Config),
    GlobalDir = filename:join(Priv, "global"),
    GlobalConfigDir = filename:join([GlobalDir, ".config", "rebar3"]),
    GlobalConfig = filename:join([GlobalDir, ".config", "rebar3", "rebar.config"]),

    meck:new(rebar_dir, [passthrough]),
    meck:expect(rebar_dir, global_config, fun() -> GlobalConfig end),
    meck:expect(rebar_dir, global_cache_dir, fun(_) -> GlobalDir end),

    %% Insert proxy variables into config
    rebar_test_utils:create_config(GlobalConfigDir,
                                   [{http_proxy, "http://localhost:1234"}
    ]),
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(https_proxy_settings, Config) ->
    SupportsHttpsProxy = case erlang:system_info(otp_release) of
        "R16"++_ -> true;
        "R"++_ -> false;
        _ -> true % 17 and up don't have a "R" in the version
    end,
    if not SupportsHttpsProxy ->
            {skip, https_proxy_unsupported_before_R16};
       SupportsHttpsProxy ->
            %% Create private rebar.config
            Priv = ?config(priv_dir, Config),
            GlobalDir = filename:join(Priv, "global"),
            GlobalConfigDir = filename:join([GlobalDir, ".config", "rebar3"]),
            GlobalConfig = filename:join([GlobalDir, ".config", "rebar3", "rebar.config"]),

            meck:new(rebar_dir, [passthrough]),
            meck:expect(rebar_dir, global_config, fun() -> GlobalConfig end),
            meck:expect(rebar_dir, global_cache_dir, fun(_) -> GlobalDir end),

            %% Insert proxy variables into config
            rebar_test_utils:create_config(GlobalConfigDir,
                                           [{https_proxy, "http://localhost:1234"}
            ]),
            rebar_test_utils:init_rebar_state(Config)
    end;
init_per_testcase(Case, Config) ->
    {Deps, Warnings, Expect} = deps(Case),
    Expected = case Expect of
        {ok, List} -> {ok, format_expected_deps(List)};
        {error, Reason} -> {error, Reason}
    end,
    DepsType = ?config(deps_type, Config),
    mock_warnings(),
    [{expect, Expected},
     {warnings, Warnings}
    | setup_project(Case, Config, rebar_test_utils:expand_deps(DepsType, Deps))].

end_per_testcase(https_proxy_settings, Config) ->
    meck:unload(rebar_dir),
    Config;
end_per_testcase(http_proxy_settings, Config) ->
    meck:unload(rebar_dir),
    Config;
end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

format_expected_deps(Deps) ->
    [case Dep of
        {N,V} -> {dep, N, V};
        N -> {dep, N}
     end || Dep <- Deps].

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
deps(flat) ->
    {[{"B", []},
      {"C", []}],
     [],
     {ok, ["B", "C"]}};
deps(pick_highest_left) ->
    {[{"B", [{"C", "2", []}]},
      {"C", "1", []}],
     [{"C","2"}],
     {ok, ["B", {"C", "1"}]}};
deps(pick_highest_right) ->
    {[{"B", "1", []},
      {"C", [{"B", "2", []}]}],
     [{"B","2"}],
     {ok, [{"B","1"}, "C"]}};
deps(pick_smallest1) ->
    {[{"B", [{"D", "1", []}]},
      {"C", [{"D", "2", []}]}],
     [{"D","2"}],
     %% we pick D1 because B < C
     {ok, ["B","C",{"D","1"}]}};
deps(pick_smallest2) ->
    {[{"C", [{"D", "2", []}]},
      {"B", [{"D", "1", []}]}],
     [{"D","2"}],
     %% we pick D1 because B < C
     {ok, ["B","C",{"D","1"}]}};
deps(circular1) ->
    {[{"B", [{"A", []}]}, % A is the top-level app
      {"C", []}],
     [],
     {error, {rebar_prv_install_deps, {cycles, [[<<"A">>,<<"B">>]]}}}};
deps(circular2) ->
    {[{"B", [{"C", [{"B", []}]}]},
      {"C", []}],
     [],
     {error, {rebar_prv_install_deps, {cycles, [[<<"B">>,<<"C">>]]}}}};
deps(circular_skip) ->
    %% Never spot the circular dep due to being to low in the deps tree
    %% in source deps
    {[{"B", [{"C", "2", [{"B", []}]}]},
      {"C", "1", [{"D",[]}]}],
     [{"C","2"}],
     {ok, ["B", {"C","1"}, "D"]}}.

setup_project(Case, Config0, Deps) ->
    DepsType = ?config(deps_type, Config0),
    Config = rebar_test_utils:init_rebar_state(
               Config0,
               atom_to_list(Case)++"_"++atom_to_list(DepsType)++"_"
              ),
    AppDir = ?config(apps, Config),
    rebar_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    TopDeps = rebar_test_utils:top_level_deps(Deps),
    RebarConf = rebar_test_utils:create_config(AppDir, [{deps, TopDeps}]),
    case DepsType of
        git ->
            mock_git_resource:mock([{deps, rebar_test_utils:flat_deps(Deps)}]);
        pkg ->
            mock_pkg_resource:mock([{pkgdeps, flat_pkgdeps(Deps)}])
    end,
    [{rebarconfig, RebarConf} | Config].

flat_pkgdeps([]) -> [];
flat_pkgdeps([{{pkg, Name, Vsn}, Deps} | Rest]) ->
    [{{iolist_to_binary(Name),iolist_to_binary(Vsn)}, rebar_test_utils:top_level_deps(Deps)}]
    ++
    flat_pkgdeps(Deps)
    ++
    flat_pkgdeps(Rest).

app_vsn([]) -> [];
app_vsn([{Source, Deps} | Rest]) ->
    {Name, Vsn} = case Source of
        {pkg, N, V} -> {N,V};
        {N,V,_Ref} -> {N,V}
    end,
    [{Name, Vsn}] ++ app_vsn(Deps) ++ app_vsn(Rest).

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

%% Test that the deps of project apps that have their own rebar.config
%% are included, but that top level rebar.config deps take precedence
sub_app_deps(Config) ->
    AppDir = ?config(apps, Config),
    Deps = rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                             ,{"b", "1.0.0", []}
                                             ,{"b", "2.0.0", []}]),
    mock_git_resource:mock([{deps, rebar_test_utils:flat_deps(Deps)}]),

    Name = rebar_test_utils:create_random_name("sub_app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),

    SubAppsDir = filename:join([AppDir, Name]),
    SubDeps = rebar_test_utils:top_level_deps(rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                              ,{"b", "2.0.0", []}])),
    rebar_test_utils:create_app(SubAppsDir, Name, Vsn, [kernel, stdlib]),
    rebar_test_utils:create_config(SubAppsDir, [{deps, SubDeps}]),

    TopDeps = rebar_test_utils:top_level_deps(rebar_test_utils:expand_deps(git, [{"b", "1.0.0", []}])),
    {ok, RebarConfig} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, TopDeps}])),

    rebar_test_utils:run_and_check(
      Config, RebarConfig, ["compile"],
      {ok, [{app, Name}, {dep, "a"}, {dep, "b", "1.0.0"}]}).

%% Newly added dependency after locking
newly_added_dep(Config) ->
    AppDir = ?config(apps, Config),
    Deps = rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                            ,{"b", "1.0.0", [{"c", "1.0.0", []}]}
                            ,{"c", "2.0.0", []}]),
    mock_git_resource:mock([{deps, rebar_test_utils:flat_deps(Deps)}]),

    Name = rebar_test_utils:create_random_name("app_"),
    Vsn = rebar_test_utils:create_random_vsn(),

    SubAppsDir = filename:join([AppDir, Name]),
    rebar_test_utils:create_app(SubAppsDir, Name, Vsn, [kernel, stdlib]),

    TopDeps = rebar_test_utils:top_level_deps(rebar_test_utils:expand_deps(git, [{"b", "1.0.0", []}])),
    {ok, RebarConfig} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, TopDeps}])),
    rebar_test_utils:run_and_check(
      Config, RebarConfig, ["compile"],
      {ok, [{app, Name}, {dep, "b", "1.0.0"}, {dep, "c", "1.0.0"}]}),

    %% Add a and c to top level
    TopDeps2 = rebar_test_utils:top_level_deps(rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                               ,{"c", "2.0.0", []}
                                               ,{"b", "1.0.0", []}])),
    {ok, RebarConfig2} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, TopDeps2}])),
    LockFile = filename:join(AppDir, "rebar.lock"),
    RebarConfig3 = rebar_config:merge_locks(RebarConfig2,
                                           rebar_config:consult_lock_file(LockFile)),

    %% a should now be installed and c should not change
    rebar_test_utils:run_and_check(
      Config, RebarConfig3, ["compile"],
      {ok, [{app, Name}, {dep, "a"}, {dep, "b", "1.0.0"}, {dep, "c", "1.0.0"}]}).


http_proxy_settings(_Config) ->
    %% Load config
    rebar_utils:set_httpc_options(),
    rebar3:init_config(),

    %% Assert variable is right
    ?assertEqual({ok,{{"localhost", 1234}, []}},
                 httpc:get_option(proxy, rebar)).

https_proxy_settings(_Config) ->
    %% Load config
    rebar_utils:set_httpc_options(),
    rebar3:init_config(),

    %% Assert variable is right
    ?assertEqual({ok,{{"localhost", 1234}, []}},
                 httpc:get_option(https_proxy, rebar)).


run(Config) ->
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["install_deps"], ?config(expect, Config)
    ),
    check_warnings(warning_calls(), ?config(warnings, Config), ?config(deps_type, Config)).

warning_calls() ->
    History = meck:history(rebar_log),
    [{Str, Args} || {_, {rebar_log, log, [warn, Str, Args]}, _} <- History].

check_warnings(_, [], _) ->
    ok;
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
    1 =< length([1 || {_, [AppName, AppVsn]} <- Warns,
                      AppName =:= Name, AppVsn =:= Vsn]).
