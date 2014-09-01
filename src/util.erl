-module(util).

-export([sleep/1,
         sleep_ms/1,
         countdown_exit/1,
         splice_strings/1,
         splice_strings/2,
         has_same_list_element/1,
         remove_same_list_element/1
        ]).


sleep(Second) ->
    receive
    after Second * 1000 ->
            ok
    end.

sleep_ms(MSecond) ->
    receive
    after MSecond ->
            ok
    end.

countdown_exit(Second) ->
    sleep(Second),
    init:stop().

splice_strings([]) -> 
    [];
splice_strings([FirstStr | RestStrs]) -> 
    splice_strings(FirstStr, RestStrs).

splice_strings(TargetStr, []) ->
    lists:flatten(TargetStr);
splice_strings(TmpStr, [Str | RestStrs]) ->
    NewTmpStr = io_lib:format("~s ~s", [TmpStr, Str]),
    splice_strings(NewTmpStr, RestStrs).

has_same_list_element(List) ->
    Set = sets:from_list(List),
    sets:size(Set) =/= length(List).

remove_same_list_element(List) ->
    Set = sets:from_list(List),
    sets:to_list(Set).
