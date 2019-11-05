%%% @doc
%%% Common functions to analyze erlang-related files and compilation data
%%% for rebar3.
%%% This module should be considered private, and unless indicated otherwise
%%% plugins and custom compiler modules should not rely on this functionality.
%%% @end
-module(rebar_compiler_lib).
-export([deps/2, change_marker/1, change_marker/2, file_changed/2]).
-include_lib("kernel/include/file.hrl").

deps(File, Opts) ->
    {ok, Forms} = epp:parse_file(File, Opts),
    normalize(handle_forms(Forms, default_attrs())).

change_marker(File) ->
    case file:read_file_info(File, [raw]) of
        {error, Reason} ->
            {error, Reason};
        {ok, FInfo} ->
            {ok, file_info_to_list(FInfo, read_hash(File))}
    end.

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

file_changed([{hash, H1} | _], [{hash, H2} | _]) ->
    H1 =/= H2.

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


