-module(afile_client).

-export([get_file/2, ls/1]).

ls(Server) ->
    Server ! {self(), list_dir},
    receive {Server, FileList} -> FileList end.

get_file(Server, File) ->
    Server ! {self(), {get_file, File}},
    receive {Server, Content} -> Content end.
