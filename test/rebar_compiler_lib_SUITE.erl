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

init_per_suite(Config) ->
    to_file(Config, {"direct.hrl", "-direct(val). "}),
    Config.

end_per_suite(Config) ->
    Config.

all() ->
    [analyze, analyze_old_behaviour, analyze_old_behavior,
     analyze_empty, analyze_bad_mod,
     change_marker, maybe_hash, maybe_hash_deleted, compare_markers].

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
    file:write_file(Path, Contents),
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

