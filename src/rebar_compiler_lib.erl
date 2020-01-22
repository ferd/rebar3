%%% @doc
%%% Common functions to analyze erlang-related files and compilation data
%%% for rebar3.
%%% This module should be considered private, and unless indicated otherwise
%%% plugins and custom compiler modules should not rely on this functionality.
%%% @end
-module(rebar_compiler_lib).
-export([find_marker/2, update_vertices_markers/2, explode_edges/5,
         find_old/2, find_changed/2]).
-export([deps/2, change_marker/1, change_marker/2, file_changed/2,
         resolve_module/2]).
-include_lib("kernel/include/file.hrl").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% High Level Graph Handling %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Given a compile digraph and a filename, go look for the filename as a
%% vertex, and return whether the file has changed. If the file has changed,
%% return `{changed, NewMarker}'; if it hasn't changed but its metadata
%% was modified, return `{unchanged_hash, NewMarker}'.
%% If the file didn't exist, returns `{new, Marker}'. `unchanged' is returned
%% in case the file is the same.
find_marker(G, File) ->
    case digraph:vertex(G, File) of
        {_, OldMarker} ->
            {ok, Marker} = change_marker(File, OldMarker),
            if Marker =:= OldMarker ->
                unchanged;
            true ->
                case file_changed(OldMarker, Marker) of
                    false -> {unchanged_hash, Marker};
                    true -> {changed, Marker}
                end
            end;
        false ->
            {ok, Marker} = change_marker(File),
            {new, Marker}
    end.

%% @doc Given a digraph and a bunch of paths, update all the vertices
%% matching the file paths, and return the ones that changed.
update_vertices_markers(G, Paths) ->
    [{Keyword, Path}
     || Path <- Paths,
        {Keyword, M} <- [find_marker(G, Path)],
        begin
            % always update the definition
            digraph:add_vertex(G, Path, M),
            % only return changes worth compiling for
            Keyword =:= changed orelse Keyword =:= new
        end].

%% @doc Take a digraph, file dependencies, all markers and a lookup list of
%% parse transform mods, and generate a list of edges for a digraph that
%% represents dependencies with markers.
explode_edges(_, [], _, _, _) ->
    [];
explode_edges(G, [{File, Incls, Mods} | T], FMarkers, InclMarkers, DepMods) ->
    [{File, MPath, pick_marker(G, MPath, FMarkers)}
     || Mod <- Mods,
        {_, MPath} <- [lists:keyfind(Mod, 1, DepMods)]]
    ++
    [{File, IPath, pick_marker(G, IPath, InclMarkers)} || IPath <- Incls]
    ++ explode_edges(G, T, FMarkers, InclMarkers, DepMods).

find_old(Olds, News) ->
    NewMap = maps:from_list([{{A,B}, true} || {A,B,_} <- News]),
    [K || {K,A,B,_} <- Olds, not maps:is_key({A,B}, NewMap)].

find_changed(Olds, News) ->
    OldMap = maps:from_list([{{A,B}, M} || {_,A,B,M} <- Olds]),
    [R || R = {A,B,M} <- News, maps:find({A,B}, OldMap) =/= {ok, M}].

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic File Handling %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Find all Erlang code dependencies for a given file
deps(File, Opts) ->
    {ok, Forms} = epp:parse_file(File, Opts),
    normalize(handle_forms(Forms, default_attrs())).

%% Create a change marker out of a given file. This is based on
%% file_info metadata and a hash of the file contents.
%% The hash is required because in the case of docker files and copying,
%% all the metadata will change for all files, but not the hash; this
%% acts as a last resort safety to prevent recompiling all the things all
%% the time.
change_marker(File) ->
    case file:read_file_info(File, [raw]) of
        {error, Reason} ->
            {error, Reason};
        {ok, FInfo} ->
            {ok, file_info_to_list(FInfo, read_hash(File))}
    end.

%% Given a file with a known change marker, update the change marker;
%% this function is to be used specifically to avoid re-computing an
%% expensive hash when none of the data has changed before.
change_marker(File, [{hash, _}, {mtime, Mtime}, {size, Size}, {inode, Inode},
                     {mode, Mode}, {uid, Uid}, {gid, Gid}] = Marker) ->
    case file:read_file_info(File, [raw]) of
        {error, Reason} ->
            {error, Reason};
        {ok, #file_info{
               mtime = Mtime, size = Size, inode = Inode,
               mode = Mode, uid = Uid, gid = Gid
        }} -> % same file info, don't recalculate hash
            {ok, Marker};
        {ok, FInfo} ->
            {ok, file_info_to_list(FInfo, read_hash(File))}
    end.

%% check whether a file actually changed based on its hash, by looking
%% at markers.
file_changed([{hash, H1} | _], [{hash, H2} | _]) ->
    H1 =/= H2.

%% Find the path matching a given erlang module
resolve_module(Mod, Paths) ->
    ModStr = atom_to_list(Mod),
    try
        [throw(P) || P <- Paths, ModStr =:= filename:basename(P, ".erl")],
        {error, not_found}
    catch
        Path -> {ok, Path}
    end.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

default_attrs() ->
    #{include => [],
      missing_include_file => [],
      missing_include_lib => [],
      behaviour => [],
      parse_transform => [],
      is_behaviour => false}.

normalize(Map) ->
    #{include := Incl,
      missing_include_file := InclF,
      missing_include_lib := InclL,
      behaviour := Behaviour,
      parse_transform := PTrans} = Map,
    Map#{include => lists:usort(Incl),
         missing_include_file => lists:usort(InclF),
         missing_include_lib => lists:usort(InclL),
         behaviour => lists:usort(Behaviour),
         parse_transform => lists:usort(PTrans)}.

handle_forms([File|Forms], Map) ->
    lists:foldl(fun handle_form/2, Map, drop_self_file(File, Forms)).

drop_self_file(_, []) ->
    [];
drop_self_file({attribute, _, file, {Path,_}} = File,
               [{attribute,_, file, {Path,_}} | Rest]) ->
    drop_self_file(File, Rest);
drop_self_file(File, [Keep|Rest]) ->
    [Keep | drop_self_file(File, Rest)].

%% Included files (both libs and direct includes);
%% There are also references to the module's own file declaration
%% in there, but this is dropped by `drop_self_file/2' and assumed
%% to be gone here.
handle_form({attribute, _Line, file, {Path, _Ln}}, Map) ->
    maps:update_with(include, fun(L) -> [Path|L] end, [Path], Map);
%% Include files that EPP couldn't resolve
handle_form({error, {_Line, epp, {include, file, Name}}}, Map) ->
    maps:update_with(missing_include_file, fun(L) -> [Name|L] end, [Name], Map);
handle_form({error, {_Line, epp, {include, lib, Path}}}, Map) ->
    maps:update_with(missing_include_lib, fun(L) -> [Path|L] end, [Path], Map);
%% Behaviour implementation declaration
handle_form({attribute, _Line, behaviour, Name}, Map) ->
    maps:update_with(behaviour, fun(L) -> [Name|L] end, [Name], Map);
handle_form({attribute, _Line, behavior, Name}, Map) ->
    maps:update_with(behaviour, fun(L) -> [Name|L] end, [Name], Map);
%% Extract parse transforms
handle_form({attribute, Line, compile, Attr}, Map) when not is_list(Attr) ->
    handle_form({attribute, Line, compile, [Attr]}, Map);
handle_form({attribute, _Line, compile, Attrs}, Map) ->
    Mods = [M || {_, {M,_}} <- proplists:lookup_all(parse_transform, Attrs)],
    maps:update_with(parse_transform, fun(L) -> Mods++L end, Mods, Map);
%% Current style behaviour specification declaration
handle_form({attribute, _Line, callback, _}, Map) ->
    Map#{is_behaviour => true};
%% Old style behaviour specification, both spellings supported
%% The function needs to be exported, but we skip over that logic
%% for now.
handle_form({function, _Line, behaviour_info, 1, _}, Map) ->
    Map#{is_behaviour => true};
handle_form({function, _Line, behavior_info, 1, _}, Map) ->
    Map#{is_behaviour => true};
%% Skip the rest
handle_form(_, Map) ->
    Map.

%% @private Turn a file_info record to a marker list (only missing the hash)
file_info_to_list(#file_info{
        mtime = Mtime, size = Size, inode = Inode,
        mode = Mode, uid = Uid, gid = Gid
    }, Hash) ->
    [{hash, Hash}, {mtime, Mtime}, {size, Size}, {inode, Inode}, {mode, Mode},
     {uid, Uid}, {gid, Gid}].

%% Optimize for something faster (raw mode files maybe?)
read_hash(Path) ->
    {ok, Data} = file:read_file(Path),
    erlang:md5(Data).

pick_marker(G, Path, Markers) ->
    case lists:keyfind(Path, 1, Markers) of
        {_, {_, Marker}} ->
            Marker;
        _ ->
            {_, Marker} = digraph:vertex(G, Path),
            Marker
    end.

