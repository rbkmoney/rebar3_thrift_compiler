-module(rebar3_thrift_compiler_utils).

-export([assert_file/1]).
-export([assert_dir/1]).
-export([ensure_dir/1]).
-export([realpath/1]).

%%

-spec assert_file(filelib:filename()) -> filelib:filename().

assert_file(Path) ->
    true = filelib:is_file(Path) orelse rebar_api:abort("Not regular file: ~s", [Path]),
    Path.

-spec assert_dir(filelib:dirname()) -> filelib:dirname().

assert_dir(Dir) ->
    true = filelib:is_dir(Dir) orelse rebar_api:abort("Not directory: ~s", [Dir]),
    Dir.

-spec ensure_dir(filelib:dirname()) -> ok.

ensure_dir(Dir) ->
    {ok, RealDir} = rebar3_thrift_compiler_utils:realpath(Dir),
    case filelib:ensure_dir(filename:join(RealDir, "dummy")) of
        ok -> assert_dir(RealDir);
        {error, eexist} -> assert_dir(RealDir)
    end.

-spec realpath(filelib:filename()) -> {ok, filelib:filename()} | {error, invalid | notsup}.

realpath(Path) ->
    get_realpath(rebar_file_utils:canonical_path(Path), []).

get_realpath(Path, Visited) ->
    case file:read_link(Path) of
        {ok, RealPath} ->
            get_linkpath(Path, RealPath, Visited);
        {error, _} ->
            get_realpath(filename:dirname(Path), filename:basename(Path), Visited)
    end.

get_realpath(Root, [], _) ->
    {ok, Root};
get_realpath(Path, Rest, Visited) ->
    case get_realpath(Path, Visited) of
        {ok, Real} -> {ok, filename:join(Real, Rest)};
        Error -> Error
    end.

get_linkpath(Path, RealPath, Visited) ->
    case filename:pathtype(RealPath) of
        absolute ->
            follow_path(RealPath, Visited);
        relative ->
            Dir = filename:dirname(Path),
            follow_path(rebar_file_utils:canonical_path(filename:join(Dir, RealPath)), Visited);
        _ ->
            {error, notsup}
    end.

follow_path(Path, Visited) ->
    case lists:member(Path, Visited) of
        false -> get_realpath(Path, [Path | Visited]);
        true -> {error, invalid}
    end.
