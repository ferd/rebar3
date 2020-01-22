%%% @doc
%%% Unit tests for internal compiler utils.
%%% Make it easier to validate internal behaviour of compiler data and
%%% handling of module parsing without having to actually set up
%%% entire projects.
%%% @end
-module(rebar_compiler_lib_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).

all() ->
    [{group, module}, {group, digraph}].

groups() ->
    [{module, [], [
        analyze, analyze_old_behaviour, analyze_old_behavior,
        analyze_empty, analyze_bad_mod,
        change_marker, maybe_hash, maybe_hash_deleted, compare_markers,
        resolve_module
     ]},
     {digraph, [], [
        find_marker, update_vertices, explode_edges,
        find_old, find_changed
     ]}
    ].

%% SAMPLE APP PLAN
%%
%%    A                    B                    C                  D
%%  - out:Af,Al            Bl,Bb,Dt             Cb,Cl,Ci           Dl,Dt
%%  - in:Af,Al,Bl,Cl,Dl    Cl,Bb,Cb             Dl
%%
%% - change one or more of them, see that repercussions hold in order
%%    - define: "unchanged file with no changed dep not recompiled"
%%    - define: "changed file recompiled"
%%    - define: "unchanged file with changed dep recompiled"

%% To check: dropping beam files and re-building missing ones
%%           vs. shource change or not
%% To check: erl_first_files -- possibly no longer needed?
%% To check: include a file from another lib fails
%% To check: deps are ignored except checkouts?
%% To check: circular dependencies are ok for include files, but can't work
%%           for parse transforms or behaviour definitions (maybe?)
%% To check: extra src dirs and recursion
%% To try: parallelize dep check since they can all change the same
%%         digraph?

init_per_group(module, Config) ->
    to_file(Config, {"direct.hrl", "-direct(val). "}),
    Config;
init_per_group(digraph, Config) ->
    [{start_digraph, true} | Config].

end_per_group(_, Config) ->
    Config.

init_per_testcase(_, Config) ->
    case ?config(start_digraph, Config) of
        true -> [{digraph, digraph:new()} | Config];
        _ -> Config
    end.

end_per_testcase(_, Config) ->
    ?config(start_digraph, Config) =:= true
      andalso digraph:delete(?config(digraph, Config)),
    Config.

%%%%%%%%%%%%%%%%%%%%%
%%% digraph group %%%
%%%%%%%%%%%%%%%%%%%%%
find_marker() ->
    [{doc, "using a digraph's state, find about file updates"}].
find_marker(Config) ->
    Path = to_file(Config, fake_mod()),
    G = ?config(digraph, Config),
    {new, Marker0} = rebar_compiler_lib:find_marker(G, Path),
    {new, Marker0} = rebar_compiler_lib:find_marker(G, Path),
    digraph:add_vertex(G, Path, Marker0),
    unchanged = rebar_compiler_lib:find_marker(G, Path),
    ok = file:change_time(Path, {{1970,1,1},{1,0,0}}),
    {unchanged_hash, Marker1} = rebar_compiler_lib:find_marker(G, Path),
    digraph:add_vertex(G, Path, Marker1),
    unchanged = rebar_compiler_lib:find_marker(G, Path),
    ok = file:write_file(Path, <<"random garbage">>, [sync]),
    {changed, _Marker2} = rebar_compiler_lib:find_marker(G, Path),
    ok.

update_vertices() ->
    [{doc, "using a digraph state, update according to a marker's "
           "change state."}].
update_vertices(Config) ->
    Path1 = to_file(Config, fake_mod()),
    Path2 = to_file(Config, old_behaviour_mod()),
    Path3 = to_file(Config, empty_mod()),
    Path4 = to_file(Config, bad_mod()),
    G = ?config(digraph, Config),
    {ok, Marker1} = rebar_compiler_lib:change_marker(Path1),
    {ok, Marker2} = rebar_compiler_lib:change_marker(Path2),
    {ok, Marker3} = rebar_compiler_lib:change_marker(Path3),
    digraph:add_vertex(G, Path1, Marker1),
    digraph:add_vertex(G, Path2, Marker2),
    digraph:add_vertex(G, Path3, Marker3),
    ok = file:change_time(Path3, {{1970,1,1},{1,0,0}}),
    ok = file:write_file(Path1, <<"random garbage">>, [sync]),
    ok = file:write_file(Path2, <<"random garbage">>, [sync]),
    {ok, Marker4} = rebar_compiler_lib:change_marker(Path1),
    {ok, Marker5} = rebar_compiler_lib:change_marker(Path2),
    {ok, Marker6} = rebar_compiler_lib:change_marker(Path3),
    Res = rebar_compiler_lib:update_vertices_markers(G, [Path1, Path2, Path3, Path4]),
    ?assertEqual([{changed, Path1}, {changed, Path2}, {new, Path4}], Res),
    ?assertEqual({Path1, Marker4}, digraph:vertex(G, Path1)),
    ?assertEqual({Path2, Marker5}, digraph:vertex(G, Path2)),
    ?assertEqual({Path3, Marker6}, digraph:vertex(G, Path3)),
    ok.

explode_edges() ->
    [{doc, "moving from file dependency records to actual edges."}].
explode_edges(Config) ->
    G = ?config(digraph, Config),
    digraph:add_vertex(G, "f1", m1a),
    digraph:add_vertex(G, "f2", m2a),
    digraph:add_vertex(G, "f3", m3a),
    digraph:add_vertex(G, "f4", m4a),
    Res = rebar_compiler_lib:explode_edges(
        G,
        [{"f1", ["f2","f3"], [f4]},
         {"f2", [], [f4]},
         {"f3", ["f4"], [f1, f4]}],
        [{"f1", unchanged}, {"f4", {new, m4b}}],
        [{"f2", {changed, m2c}}, {"f3", {unchanged_hash, m3c}}],
        [{f1, "f1"}, {f4, "f4"}]
    ),
    ?assertEqual(
       [{"f1", "f2", m2c}, {"f1", "f3", m3c}, {"f1", "f4", m4b},
        {"f2", "f4", m4b},
        {"f3", "f1", m1a}, {"f3", "f4", m4a}, {"f3", "f4", m4b}],
       lists:sort(Res)
    ),
    ok.

find_old() ->
    [{doc, "find edges that have been dropped from a digraph"}].
find_old(_Config) ->
    ?assertEqual(
       [id2, id3],
       rebar_compiler_lib:find_old(
         [{id1, "f1", "f2", m}, {id2, "f1", "f3", m2}, {id3, "f3", "f2", m3},
          {id4, "f1", "f4", m}],
         [{"f1", "f2", m}, {"f1", "f4", m2}]
       )
    ),
    ok.

find_changed() ->
    [{doc, "find edges that have been modified from a digraph"}].
find_changed(_Config) ->
    ?assertEqual(
       [{"f1", "f4", m2}, {"f1", "f5", m}],
       lists:sort(rebar_compiler_lib:find_changed(
         [{id1, "f1", "f2", m}, {id2, "f1", "f3", m2}, {id3, "f3", "f2", m3},
          {id4, "f1", "f4", m}],
         [{"f1", "f2", m}, {"f1", "f5", m}, {"f1", "f4", m2}]
       ))
    ),
    ok.

%%%%%%%%%%%%%%%%%%%%
%%% module group %%%
%%%%%%%%%%%%%%%%%%%%
analyze() ->
    [{docs, "Analyzing a module returns all the "
            "parseable dependencies for it in a map."}].
analyze(Config) ->
    ?assert(check_analyze(
       #{include => [
           "eunit-.*/include/eunit.hrl$",
           "stdlib-.*/include/assert.hrl$",
           "direct.hrl$"
         ],
         %% missing includes
         missing_include_file => [
            "^false.hrl$"
         ],
         missing_include_lib => [
            "^some_app/include/lib.hrl$"
         ],
         parse_transform => [erl_id_trans, missing_parse_trans],
         behaviour => [gen_server, gen_statem],
         is_behaviour => true
       },
       rebar_compiler_lib:deps(
         to_file(Config, fake_mod()),
         [{includes, []}, {macros, []}]
       )
    )),
    ok.

analyze_old_behaviour() ->
    [{docs, "Analyzing old-style behaviour annotation"}].
analyze_old_behaviour(Config) ->
    ?assert(check_analyze(
       #{include => [],
         missing_include_file => [],
         missing_include_lib => [],
         parse_transform => [],
         behaviour => [],
         is_behaviour => true
       },
       rebar_compiler_lib:deps(
         to_file(Config, old_behaviour_mod()),
         [{includes, []}, {macros, []}]
       )
    )),
    ok.

analyze_old_behavior() ->
    [{docs, "Analyzing old-style behavior annotation"}].
analyze_old_behavior(Config) ->
    ?assert(check_analyze(
       #{include => [],
         missing_include_file => [],
         missing_include_lib => [],
         parse_transform => [],
         behaviour => [],
         is_behaviour => true
       },
       rebar_compiler_lib:deps(
         to_file(Config, old_behavior_mod()),
         [{includes, []}, {macros, []}]
       )
    )),
    ok.

analyze_empty() ->
    [{docs, "Making sure empty files are properly handled as valid but null "
            "and let some other compiler phase handle this. We follow "
            "what EPP handles."}].
analyze_empty(Config) ->
    ?assert(check_analyze(
       #{include => [],
         missing_include_file => [],
         missing_include_lib => [],
         parse_transform => [],
         behaviour => [],
         is_behaviour => false
       },
       rebar_compiler_lib:deps(
         to_file(Config, empty_mod()),
         [{includes, []}, {macros, []}]
       )
    )),
    ok.

analyze_bad_mod() ->
    [{docs, "Errors for bad modules that don't compile are skipped "
            "by EPP and so we defer that to a later phase of the "
            "compilation process"}].
analyze_bad_mod(Config) ->
    ?assert(check_analyze(
       #{include => [],
         missing_include_file => [],
         missing_include_lib => [],
         parse_transform => [],
         behaviour => [],
         is_behaviour => false
       },
       rebar_compiler_lib:deps(
         to_file(Config, bad_mod()),
         [{includes, []}, {macros, []}]
       )
    )),
    ok.

change_marker() ->
    [{doc, "Force scan a file to get its content marker"}].
change_marker(Config) ->
    Path = to_file(Config, fake_mod()),
    BadPath = filename:join([Path, "fakefile"]),
    ?assertEqual({error, enotdir}, rebar_compiler_lib:change_marker(BadPath)),
    ?assertEqual({error, enoent}, rebar_compiler_lib:change_marker(Path++"rand-garb")),
    {ok, Marker} = rebar_compiler_lib:change_marker(Path),
    %% expect an ordered list format, useful for faster checks
    ?assertMatch(
       [{hash, _}, {mtime, _}, {size, _}, {inode, _}, {mode, _},
        {uid, _}, {gid, _}],
       Marker
    ),
    ok.

maybe_hash() ->
    [{doc, "the change_marker/2 function takes a path and the old hash "
           "for it, and if any of the non-hash attributes changed, "
           "it re-hashes it."}].
maybe_hash(Config) ->
    Path = to_file(Config, fake_mod()),
    {ok, Marker} = rebar_compiler_lib:change_marker(Path),
    ?assertEqual({ok, Marker}, rebar_compiler_lib:change_marker(Path)),
    ok = file:write_file(Path, <<"random garbage">>),
    {ok, NewMarker} = rebar_compiler_lib:change_marker(Path, Marker),
    %% Hard-code on the hash being the first element
    ?assertNotEqual(hd(Marker), hd(NewMarker)),
    ok.

maybe_hash_deleted() ->
    [{doc, "a file that is being deleted just returns the posix error."}].
maybe_hash_deleted(Config) ->
    Path = to_file(Config, fake_mod()),
    {ok, Marker} = rebar_compiler_lib:change_marker(Path),
    ok = file:delete(Path),
    ?assertEqual({error, enoent}, rebar_compiler_lib:change_marker(Path, Marker)),
    ok.

compare_markers() ->
    [{doc, "only consider markers to be unequal if the hash changes"}].
compare_markers(Config) ->
    Path = to_file(Config, fake_mod()),
    {ok, Marker} = rebar_compiler_lib:change_marker(Path),
    ?assertEqual({ok, Marker}, rebar_compiler_lib:change_marker(Path)),
    ok = file:change_mode(Path, 8#0764),
    {ok, NewMarker} = rebar_compiler_lib:change_marker(Path, Marker),
    %% Hard-code on the hash being the first element
    ?assertNotEqual(Marker, NewMarker),
    ?assertNot(rebar_compiler_lib:file_changed(Marker, NewMarker)),
    ok.

resolve_module() ->
    [{doc, "given a module name and a bunch of paths, find "
           "the first path that matches the module"}].
resolve_module(Config) ->
    Path1 = to_file(Config, fake_mod()),
    Path2 = to_file(Config, old_behaviour_mod()),
    Path3 = to_file(Config, empty_mod()),
    ?assertEqual(
       {ok, Path2},
       rebar_compiler_lib:resolve_module(
         old_behaviour,
         [Path1, Path2, Path3]
       )
    ),
    ok.

%%%%%%%%%%%%%%%
%%% HELPERS %%%
%%%%%%%%%%%%%%%

%% check each field of `Map' and validate them against `CheckMap'.
%% This allows to check each value in the map has a matching assertion.
%% Then check each field of `CheckMap' against `Map' to find if
%% any missing value exists.
check_analyze(CheckMap, Map) ->
    maps:fold(fun(K,V,Acc) -> check(CheckMap, K, V) and Acc end,
              true, Map)
    andalso
    maps:fold(
      fun(K,_,Acc) ->
          check(CheckMap, K, maps:get(K, Map, make_ref())) and Acc
      end,
      true,
      Map
    ).

check(Map, K, V) ->
    case maps:is_key(K, Map) of
        false -> false;
        true ->
            #{K := Val} = Map,
            compare_val(Val, V)
    end.

%% two identical values always works
compare_val(V, V) ->
    true;
%% compare lists of strings; each string must be checked individually
%% because they are assumed to be regexes.
compare_val(V1, V2) when is_list(hd(V1)) ->
    lists:all(fun(X) -> X end,
              [compare_val(X,Y) || {X,Y} <- zip(V1,V2)]);
%% strings as regexes
compare_val(V1, V2) when is_list(V1) ->
    re:run(V2, V1) =/= nomatch;
%% anything else is not literally the same and is bad
compare_val(_, _) ->
    false.

%% custom zip function that causes value failures (by using make_ref()
%% that will never match in compare_val/2) rather than crashing because
%% of lists of different lengths.
zip([], []) -> [];
zip([], [H|T]) -> [{make_ref(),H} | zip([], T)];
zip([H|T], []) -> [{H,make_ref()} | zip(T, [])];
zip([X|Xs], [Y|Ys]) -> [{X,Y} | zip(Xs, Ys)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module specifications %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% turn a module string to a file that will live in CT's scratch dir
to_file(Config, {Name,Contents}) ->
    Path = filename:join([?config(priv_dir, Config), Name]),
    file:write_file(Path, Contents, [sync]),
    Path.

%% base module with all the interesting includes and attributes
%% we want to track
fake_mod() ->
    {"somemod.erl", "
-module(somemod).
-export([f/1]).
-include(\"direct.hrl\").
-include(\"direct.hrl\").
-include_lib(\"some_app/include/lib.hrl\").
-include_lib(\"eunit/include/eunit.hrl\").
-compile({parse_transform, {erl_id_trans, []}}).
-compile([{parse_transform, {missing_parse_trans, []}}]).
-behaviour(gen_server).
-behavior(gen_statem).
-callback f() -> ok.
-ifdef(OPT).
-include(\"true.hrl\").
-else.
-include(\"false.hrl\").
-endif.
f(X) -> X.
    "}.

%% variations for attributes that can't be checked in the
%% same base module
old_behaviour_mod() ->
    {"old_behaviour.erl", "
-module(old_behaviour).
-export([f/1, behaviour_info/1]).
f(X) -> X.
behaviour_info(callbacks) -> [{f,1}].
    "}.

old_behavior_mod() ->
    {"old_behaviour.erl", "
-module(old_behaviour).
-export([f/1, behaviour_info/1]).
f(X) -> X.
behavior_info(callbacks) -> [{f,1}].
    "}.

empty_mod() ->
    {"empty.erl", ""}.

bad_mod() ->
    {"badmod.erl", "
-module(bad_mod). % wrong name!
f(x) -> X+1. % bad vars
f((x)cv) -> bad syntax.
    "}.

