-module(rebar3_thrift_compiler_prv).

-export([compile/3]).
-export([clean/2]).

%%

-type command_flags() :: [{atom(), string() | atom() | boolean()}].
-type command_args() :: {command_flags(), [string()]}.

-spec compile(rebar_app_info:t(), command_args(), rebar_state:t()) -> ok.

compile(AppInfo, CmdOpts, State) ->
    Opts = get_all_opts(AppInfo, CmdOpts, State),
    _ = rebar_api:debug("Thrift compiler opts: ~p", [Opts]),
    AppDir = rebar_app_info:dir(AppInfo),
    InFiles = get_in_files(AppDir, Opts),
    _ = rebar_api:debug("Thrift compiler input files: ~p", [InFiles]),
    AppOutDir = rebar_app_info:out_dir(AppInfo),
    OutDirs = get_out_dirs(AppOutDir, Opts),
    ok = ensure_dirs(OutDirs),
    try
        ok = compile_files(InFiles, OutDirs, Opts),
        ok = distribute_files(OutDirs)
    catch C:E ->
        % is it ok for now to depend on the fact that rebar aborts via exceptions?
        _ = cleanup(OutDirs),
        erlang:raise(C, E, erlang:get_stacktrace())
    end.

-spec clean(rebar_app_info:t(), command_args()) -> ok.

clean(AppInfo, CmdOpts) ->
    Opts = get_all_opts(AppInfo, CmdOpts),
    AppOutDir = rebar_app_info:out_dir(AppInfo),
    OutDirs = get_out_dirs(AppOutDir, Opts),
    cleanup(OutDirs).

%%

compile_files(InFiles, OutDirs, Opts) ->
    rebar_base_compiler:run(
        Opts, [], InFiles,
        fun (InFile, _) -> compile_one(InFile, OutDirs, Opts) end
    ).

compile_one(InFile, {OutErlDir, _OutHrlDir}, Opts) ->
    Cmd = construct_command(InFile, OutErlDir, Opts),
    case rebar_utils:sh(Cmd, [return_on_error]) of
        {ok, Lines} ->
            _ = Lines == [] orelse rebar_api:warn("~s", [lists:droplast(Lines)]),
            ok;
        {error, {_, Lines}} ->
            {error, [Lines], []}
    end.

construct_command(InFile, OutErlDir, Opts) ->
    CmdFrags = ["thrift", "-r", "--gen", get_opt(gen, Opts) | get_includes(Opts)] ++ ["--out", OutErlDir, InFile],
    string:join(CmdFrags, " ").

distribute_files({OutErlDir, OutHrlDir}) ->
    Paths = find_generated_files([OutErlDir]),
    _ = rebar_api:debug("Thrift compiler generated files: ~p", [Paths]),
    case OutHrlDir of
        OutErlDir ->
            % the same directory, we can skip safely
            ok;
        _Different ->
            HrlPaths = lists:filter(fun is_hrl_file/1, Paths),
            lists:foreach(fun (Path) -> rebar_file_utils:mv(Path, OutHrlDir) end, HrlPaths)
    end.

cleanup({OutErlDir, OutHrlDir}) ->
    Files = find_generated_files([OutErlDir, OutHrlDir]),
    lists:foreach(fun rebar_file_utils:rm_rf/1, Files).

%%

find_generated_files(Dirs) ->
    Paths0 = lists:flatmap(fun (Dir) -> rebar_utils:find_files(Dir, "^.*\.?rl$") end, Dirs),
    lists:foldl(fun (P, Acc) -> guess_generated_files(P, Paths0) ++ Acc end, [], Paths0).

guess_generated_files(Path, Paths) ->
    IsErl = is_erl_file(Path),
    case is_generated_file(Path) of
        true when IsErl ->
            % try to find accompanying hrl file
            HrlName = erl_file_to_hrl(filename:basename(Path)),
            HrlPaths = lists:filter(fun (P) -> filename:basename(P) == HrlName end, Paths),
            [Path | HrlPaths];
        true ->
            [Path];
        false ->
            []
    end.

is_generated_file(Path) ->
    % guess there's the marker somewhere in the first three strings
    {ok, FD} = file:open(Path, [raw, {read_ahead, 1024}]),
    Marker = "Autogenerated by Thrift Compiler",
    try
        Lines = [L || _ <- [1, 2, 3], {ok, L} <- [file:read_line(FD)]],
        lists:any(fun (L) -> string:str(L, Marker) > 0 end, Lines)
    after
        file:close(FD)
    end.

is_erl_file(Path) ->
    filename:extension(Path) == ".erl".

is_hrl_file(Path) ->
    filename:extension(Path) == ".hrl".

erl_file_to_hrl(Path) ->
    filename:rootname(Path) ++ ".hrl".

%%

get_all_opts(AppInfo, CmdOpts, State) ->
    Opts       = get_all_opts(AppInfo, CmdOpts),
    RootDir    = rebar_dir:root_dir(State),
    DepsDir    = rebar_dir:deps_dir(State),
    AbsDepsDir = filename:join(RootDir, DepsDir),
    DepNames   = rebar_app_info:deps(AppInfo),
    DepPaths   = [
        rebar_app_info:dir(App) ||
        App <- rebar_state:all_deps(State),
        true == lists:member(rebar_app_info:name(App), DepNames)
    ],
    append_list(include_dirs, [AbsDepsDir | DepPathes], Opts).

get_all_opts(AppInfo, {CmdOpts0, InFiles}) ->
    DefOpts = get_default_opts(),
    AppOpts = get_app_opts(AppInfo),
    CmdOpts = case InFiles of
        [_ | _] -> [{in_files, InFiles} | CmdOpts0];
        [] -> CmdOpts0
    end,
    merge_opts(DefOpts, merge_opts(AppOpts, CmdOpts)).

get_app_opts(AppInfo) ->
    case dict:find(thrift_compiler_opts, rebar_app_info:opts(AppInfo)) of
        {ok, Value} -> Value;
        error -> []
    end.

merge_opts(Opts0, Opts1) ->
    lists:foldl(fun merge_opt/2, Opts0, Opts1).

merge_opt({Key, _} = Opt, Acc) ->
    lists:keystore(Key, 1, Acc, Opt);
merge_opt(_Opt, Acc) ->
    Acc.

get_default_opts() ->
    [
        {in_dir, "proto"},
        {in_files, all},
        {out_erl_dir, "src"},
        {out_hrl_dir, "include"},
        {include_dirs, []},
        {gen, "erl"}
    ].

get_in_files(Root, Opts) ->
    Dir = rebar3_thrift_compiler_utils:assert_dir(filename:join(Root, get_opt(in_dir, Opts))),
    case get_opt(in_files, Opts) of
        all ->
            get_canonical_paths(rebar_utils:find_files(Dir, "^.*\.thrift$"));
        List ->
            lists:map(
                fun rebar3_thrift_compiler_utils:assert_file/1,
                get_canonical_paths(lists:map(fun (Path) -> filename:join(Dir, Path) end, List))
            )
    end.

get_out_dirs(Root, Opts) ->
    OutErl = filename:join(Root, get_opt(out_erl_dir, Opts)),
    OutHrl = filename:join(Root, get_opt(out_hrl_dir, Opts)),
    {OutErl, OutHrl}.

get_opt(K, Opts) ->
    {_, V} = lists:keyfind(K, 1, Opts),
    _ = validate_opt(K, V) orelse rebar_api:abort("Invalid `~p` value: ~p", [K, V]),
    V.

get_includes(Opts) ->
    Dirs = get_opt(include_dirs, Opts),
    [ string:join(["-I", Dir], " ") || Dir <- Dirs ].

validate_opt(K, V) when K == in_dir orelse K == out_erl_dir orelse K == out_hrl_dir ->
    validate_path(V);
validate_opt(in_files, all) ->
    true;
validate_opt(in_files, V) ->
    is_list(V) andalso lists:all(fun validate_path/1, V);
validate_opt(include_dirs, V) ->
    is_list(V) andalso lists:all(fun validate_path/1, V);
validate_opt(gen, V) ->
    io_lib:printable_list(V);
validate_opt(_, _) ->
    false.

validate_path(V) ->
    io_lib:printable_unicode_list(V) orelse is_binary(V).

ensure_dirs(Dirs) when is_tuple(Dirs) ->
    lists:foreach(fun rebar3_thrift_compiler_utils:ensure_dir/1, tuple_to_list(Dirs)).

get_canonical_paths(Paths) ->
    lists:map(fun rebar_file_utils:canonical_path/1, Paths).

append_list(Key, Value, Opts) ->
    {Key, Old} = lists:keyfind(Key, 1, Opts),
    lists:keystore(Key, 1, Opts, {Key, Old ++ Value}).
