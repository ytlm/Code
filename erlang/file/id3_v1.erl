-module(id3_v1).
-import(lists, [filter/2, map/2, reverse/1]).
-export([test/0, dir/1, read_id3_tag/1]).

test() -> dir(".").

dir(Dir) ->
    Files = lib_files_find:files(Dir, "*.mp3", true),
    L1 = map(fun(I) ->
                     {I, (catch read_id3_tag(I))}
             end, Files),
    io:format("L1 ~p~n", [L1]),
    L2 = filter(fun({_, error}) -> false;
                   (_) -> true
                end, L1),
    io:format("L2 ~p~n", [L2]),
    lib_misc:dump("mp3data", L2).

read_id3_tag(File) ->
    io:format("file : ~p~n", [File]),
    case file:open(File, [read, binary, raw]) of
        {ok, S} ->
            Size = filelib:file_size(File),
            {ok, B2} = file:pread(S, Size-128, 128),
            Result = parse_v1_tag(B2),
            io:format("parse v1 tag result : ~p, ~p ~n", [Result, B2]),
            file:close(S),
            Result;
        Error ->
            {File, Error}
    end.

parse_v1_tag(<<$T, $A, $G,
               Title:30/binary, Artist:30/binary,
               Album:30/binary, _Year:4/binary,
               _Comment:28/binary, 0:8, Track:8, _Genre:8>>) ->
    {"ID3v1.1",
     [{track, Track}, {title, lib_misc:trim(Title)},
      {aritist, lib_misc:trim(Artist)}, {album, lib_misc:trim(Album)}]};

parse_v1_tag(<<$T, $A, $G,
               Title:30/binary, Artist:30/binary,
               Album:30/binary, _Year:4/binary,
               _Comment:30/binary, _Genre:8>>) ->
    {"ID3v1",
     [{title, lib_misc:trim(Title)},
      {aritist, lib_misc:trim(Artist)}, {album, lib_misc:trim(Album)}]};

parse_v1_tag(_) -> error.
