%%%====================================
%%% @Module     : resource_manager.erl
%%% @Author     : chris
%%% @Email      : cablsbs@live.com
%%% @Created    : 2014.08.22
%%% @Description: 客户端版本资源管理
%%%====================================

-module(resource_manager).

-include("common.hrl").
-include("resource_manager.hrl").

-export([gen/0]).

gen() ->
    [ReposDir, NewVersionDir, NewVersionStr] = init:get_plain_arguments(),
    Version = list_to_float(NewVersionStr),
    [VersionStr] = io_lib:format("~.2f", [Version]),
    FinalVersion = list_to_float(VersionStr),
    ?ASSERT(filelib:is_dir(ReposDir), {reposDirMustBeDirectory, ReposDir}),
    ?ASSERT(filelib:is_dir(NewVersionDir), {newVersionDirMustBeDirectory, NewVersionDir}),
    ?ASSERT(ReposDir=/=NewVersionDir, {reposDirMustNotNewVersionDir, {ReposDir, NewVersionDir}}),
    gen(ReposDir, NewVersionDir, FinalVersion).

gen(ReposDir, NewVersionDir, NewVersion) ->
    VersionDir = io_lib:format("~s/Version", [ReposDir]),
    VersionFile = io_lib:format("~s/version.txt", [ReposDir]),
    LastestVersion = case file:read_file(VersionFile) of
                         {error, enoent} -> 
                             first_version_init(VersionDir);
                         {ok, BinaryValue} -> 
                             ListValue = binary:bin_to_list(BinaryValue, {0, size(BinaryValue)-1}),
                             list_to_float(ListValue)
                     end,
    ?ASSERT(NewVersion > LastestVersion, {"NewVersion Must Lager Than LatestVersion", {LastestVersion, NewVersion}}),

    FormatedVsn = io_lib:format("~.2f", [NewVersion]),
    util:sleep_ms(200),

    ?TRACE_W("~n版本库目录: ~p, ~n新版本目录: ~p,~n新版本号: ~s~n", [ReposDir, NewVersionDir, FormatedVsn]),
    ?TRACE_W("~n开始生成版本操作. . . "),

    ok = copy_new_version_to_version(VersionDir, NewVersionDir, FormatedVsn),
    ResourceDir = io_lib:format("~s/Resource", [ReposDir]),
    put_resource_dir(ResourceDir),
    ok = copy_new_version_to_resource(NewVersionDir, ResourceDir),
    UpdateDir = io_lib:format("~s/Update", [ReposDir]),
    ok = gen_version_update(VersionDir, UpdateDir),
    ok = update_version(VersionFile, FormatedVsn),

    ?TRACE_W("所有操作成功，2 秒后自动退出!"),
    util:countdown_exit(2).

first_version_init(VersionDir) ->
    FirstVersionDir = io_lib:format("~s/0.00", [VersionDir]),
    lib_file:make_dir(FirstVersionDir),
    0.00.

copy_new_version_to_version(VersionDir, NewVersionDir, FormatedVsn) ->
    ?TRACE_W("开始拷贝新版本资源到版本库，位置:[Version/~s]~n", [FormatedVsn]),
    DestDir = io_lib:format("~s/~s", [VersionDir, FormatedVsn]),
    try 
        lib_file:copy_dir(NewVersionDir, DestDir)
    catch
        _:Reason ->
            ?TRACE_W("拷贝新版本资源到版本库的[Version/~s]失败，原因:~p~n", [FormatedVsn, Reason]),
            ?TRACE_W("中止生成版本操作, 自动退出！"),
            halt()
    end,
    ?TRACE_W("拷贝新版本资源到版本库的[Version/~s]成功！~n", [FormatedVsn]).

copy_new_version_to_resource(NewVersionDir, ResourceDir) ->
    ?TRACE_W("~n开始拷贝新版本资源到版本库，位置:[Resource]"),
    try
        lib_file:del_dir(ResourceDir),
        lib_file:copy_dir(NewVersionDir, ResourceDir)
    catch
        _:Reason ->
            ?TRACE_W("拷贝新版本资源到版本库的[Resource]失败，原因:~p~n", [Reason]),
            ?TRACE_W("中止生成版本操作, 自动退出！"),
            halt()
    end,
    ?TRACE_W("拷贝新版本资源到版本库的[Resource]成功！").

update_version(VersionFile, FormatedVsn) ->
    ?TRACE_W("~n开始更新版本号文件，位置:[version.txt]"),
    try
        lib_file:write_to_file(VersionFile, FormatedVsn)
    catch
        _:Reason ->
            ?TRACE_W("更新版本号文件失败，原因:~p~n", [Reason]),
            ?TRACE_W("中止生成版本操作, 自动退出！"),
            halt()
    end,
    ?TRACE_W("更新版本资号文件[version.txt]成功！~n").

gen_version_update(VersionDir, UpdateDir) ->
    ?TRACE_W("~n开始生成旧版本到最新版本的增量资源包..."),
    {ok, Versions} = file:list_dir(VersionDir),
    SortedVersions = lists:sort(Versions),

    CurVersion = lists:max(SortedVersions),
    CurVsnUpdateDir = io_lib:format("~s/~s", [UpdateDir, CurVersion]),

    OldVsns = lists:delete(CurVersion, SortedVersions),
    [gen_version_update_info(VersionDir, CurVsnUpdateDir, CurVersion, OldVsn) || OldVsn <- OldVsns],
    ?TRACE_W("所有增量资源包生成成功!"),
    ok.

gen_version_update_info(VersionDir, CurVsnUpdateDir, CurVsn, OldVsn) ->
    ?TRACE_W("开始生成从版本[~s]到版本[~s]的增量资源~n", [OldVsn, CurVsn]),
    ResourceDir = get_resource_dir(),
    OldVsnDir = io_lib:format("~s/~s", [VersionDir, OldVsn]),
    OldToCurVsnUpdateDir = io_lib:format("~s/~s", [CurVsnUpdateDir, OldVsn]),
    lib_file:make_dir(OldToCurVsnUpdateDir),
    try
        compare_dir(OldVsnDir, ResourceDir),
        handle_gen_updates(OldToCurVsnUpdateDir)
    catch
        _:Reason ->
            ?TRACE_W("生成从版本[~s]到版本[~s]的增量资源失败，原因: R:~p~n", [OldVsn, CurVsn, Reason]),
            ?TRACE_W("中止生成版本操作, 自动退出！"),
            halt()
    end,
    ?TRACE_W("生成从版本[~s]到版本[~s]的增量资源成功!~n", [OldVsn, CurVsn]).

handle_gen_updates(UpdateInfoDir) ->
    DiffInfos = get_diff_infos(),
    UpdateListFileName = io_lib:format("~s/UpdateList.txt", [UpdateInfoDir]),
    GenFun = fun(#diff_info{file_name=RelativePath, diff_type=DiffType}) ->
                     case DiffType of
                         ?DEL -> 
                             lib_file:append_to_file(UpdateListFileName, RelativePath);
                         _Other -> 
                             SourceFullPath = get_source_full_path(RelativePath),
                             DestFullPath = get_dest_full_path(UpdateInfoDir, RelativePath),
                             copy_to_update_info_dir(SourceFullPath, DestFullPath)
                     end
             end,
    lists:foreach(GenFun, DiffInfos),

    {ok, ModFileNames} = file:list_dir(UpdateInfoDir),
    file:set_cwd(UpdateInfoDir),
    ZipList = [Name || Name <- ModFileNames, Name=/="UpdateList.txt"],
    ZipUpdateFileName = "update.zip",
    lib_file:create_zip_file(ZipUpdateFileName, ZipList),
    ZipUpdateListFileName = "updatelist.zip",
    lib_file:create_zip_file(ZipUpdateListFileName, ["UpdateList.txt"]),
    % 420 is rw-r--r--
    file:change_mode(ZipUpdateFileName, 420),
    file:change_mode(ZipUpdateListFileName, 420),
    put_diff_infos([]).

get_source_full_path(RelativePath) ->
    ResourceDir = get_resource_dir(),
    FullPath = io_lib:format("~s~s", [ResourceDir, lists:flatten(RelativePath)]),
    lists:flatten(FullPath).

get_dest_full_path(UpdateInfoDir, RelativePath) ->
    FullPath = io_lib:format("~s~s", [UpdateInfoDir, lists:flatten(RelativePath)]),
    lists:flatten(FullPath).
    
copy_to_update_info_dir(SourceFullPath, DestFullPath) ->
    case lib_file:is_file(SourceFullPath) of
        true -> file:copy(SourceFullPath, DestFullPath);
        false -> lib_file:copy_dir(SourceFullPath, DestFullPath)
    end.

compare_dir(OldVsnDir, CurVsnDir) ->
    {ok, FileList1} = file:list_dir(OldVsnDir),
    {ok, FileList2} = file:list_dir(CurVsnDir),
    WholeFileList = lists:append(FileList1, FileList2),
    NoRepeatFileList = util:remove_same_list_element(WholeFileList),
    [gen_diff_infos(OldVsnDir, CurVsnDir, FileName) || FileName <- NoRepeatFileList].

gen_diff_infos(OldVsnDir, CurVsnDir, FileName) ->
    OldFileName = lists:flatten(io_lib:format("~s/~s", [OldVsnDir, FileName])),
    NewFileName = lists:flatten(io_lib:format("~s/~s", [CurVsnDir, FileName])),
    IsDirOld = filelib:is_dir(OldFileName),
    IsDirNew = filelib:is_dir(NewFileName),
    case IsDirOld andalso IsDirNew of
        true -> 
            compare_dir(OldFileName, NewFileName);
        false -> 
            if
                IsDirOld -> 
                    RelativePath = get_relative_path(NewFileName),
                    DiffInfo = #diff_info{file_name=RelativePath, diff_type=?DEL},
                    add_diff_info(DiffInfo);
                IsDirNew -> 
                    RelativePath = get_relative_path(NewFileName),
                    DiffInfo = #diff_info{file_name=RelativePath, diff_type=?ADD},
                    add_diff_info(DiffInfo);
                true -> 
                    compare_file(OldFileName, NewFileName)
            end
    end.

compare_file(OldFileName, NewFileName) ->
    case lib_file:is_file(OldFileName) andalso lib_file:is_file(NewFileName) of
        true -> 
            case lib_file:is_file_differ(OldFileName, NewFileName) of
                true -> 
                    RelativePath = get_relative_path(NewFileName),
                    DiffInfo = #diff_info{file_name=RelativePath, diff_type=?MOD},
                    add_diff_info(DiffInfo);
                false ->
                    ok
            end;
        false ->
            DiffType = case filelib:is_file(OldFileName) of
                           true -> ?DEL;
                           false -> ?ADD
                       end,
            RelativePath = get_relative_path(NewFileName),
            DiffInfo = #diff_info{file_name=RelativePath, diff_type=DiffType},
            add_diff_info(DiffInfo)
    end.

add_diff_info(DiffInfo) ->
    OldDiffInfos = get_diff_infos(),
    put_diff_infos([DiffInfo | OldDiffInfos]).

get_relative_path(FileName) ->
    Offset = get_relative_path_offset(),
    FullName = lists:flatten(FileName),
    lists:nthtail(Offset, FullName).

get_resource_dir() ->
    get(resource_dir).

put_resource_dir(ResourceDir) ->
    RelativePathOffset = length(lists:flatten(ResourceDir)),
    put_relative_path_offset(RelativePathOffset),
    put(resource_dir, ResourceDir).

get_diff_infos() ->
    case get(diff_infos) of
        undefined -> [];
        DiffInfos -> DiffInfos
    end.

put_diff_infos(DiffInfos) ->
    put(diff_infos, DiffInfos).

get_relative_path_offset() ->
    get(relative_path_offset).

put_relative_path_offset(Offset) ->
    put(relative_path_offset, Offset).
