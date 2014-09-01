-module(lib_file).

-export([is_file/1,
         del_dir/1,
         make_dir/1,
         copy_dir/2,
         copy_file/2,
         write_to_file/2,
         append_to_file/2,
         is_file_differ/2,
         create_zip_file/2,
         create_zip_file/3
        ]).

is_file(FileName) ->
    case filelib:is_file(FileName) of
        true ->
            case filelib:is_dir(FileName) of
                true -> false;
                false -> true
            end;
        false ->
            false
    end.

del_dir(Dir) ->
    Cmd = io_lib:format("rm -rf ~s", [Dir]),
    os:cmd(Cmd).

copy_dir(SourceDir, DestDir) ->
    filelib:ensure_dir(DestDir),
    Cmd = io_lib:format("cp -r ~s ~s", [SourceDir, DestDir]),
    os:cmd(Cmd).

make_dir(DirName) ->
    case filelib:is_dir(DirName) of
        true -> 
            ok;
        false ->
            filelib:ensure_dir(DirName),
            file:make_dir(DirName)
    end.

copy_file(SourceFile, DestFile) ->
    filelib:ensure_dir(DestFile),
    file:copy(SourceFile, DestFile).

write_to_file(FileName, String) ->
    Cmd = io_lib:format("echo ~s > ~s", [String, FileName]),
    os:cmd(Cmd).

append_to_file(FileName, String) ->
    Cmd = io_lib:format("echo ~s >> ~s", [String, FileName]),
    os:cmd(Cmd).

is_file_differ(OldFileName, NewFileName) ->
    {ok, OldBinary} = file:read_file(OldFileName),
    {ok, NewBinary} = file:read_file(NewFileName),
    erlang:md5(OldBinary) =/= erlang:md5(NewBinary).

create_zip_file(WorkDir, ZipFileName, FileList) ->
    file:set_cwd(WorkDir),
    create_zip_file(ZipFileName, FileList).

create_zip_file(ZipFileName, FileList) ->
    AllFileList = [ZipFileName | FileList],
    ZipCmd = util:splice_strings("zip -r", AllFileList),
    os:cmd(ZipCmd).
