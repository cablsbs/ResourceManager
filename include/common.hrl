-define(ERROR_MSG(Value), {assert_failed,
                           [{module, ?MODULE},
                            {line, ?LINE},
                            {expression, (??BoolExpr)},
                            {expected, true},
                            {value, case Value of
                                        false -> Value;
                                        _ -> {not_a_boolean, Value}
                                    end
                            }]}).

-define(ASSERT(BoolExpr), (fun() ->
                                   case (BoolExpr) of
                                       true -> void;
                                       Value -> erlang:error(?ERROR_MSG(Value))
                                   end
                           end())).

-define(ASSERT(BoolExpr, Msg), ((fun() ->
                                         case (BoolExpr) of
                                             true -> void;
                                             _Msg -> erlang:error(Msg)
                                         end
                                 end)())).

-define(TRACE(Str), io:format(Str)).
-define(TRACE(Str, Args), io:format(Str, Args)).

-define(TRACE_VAR(Arg), io:format("~n******~nModule: ~p, Line: ~p, ~nMy print's ~p is ~p~n******~n", [?MODULE, ?LINE, ??Arg, Arg])).

% unicode版
-define(TRACE_W(Str), io:format("~ts~n", [unicode:characters_to_binary(io_lib:format(Str, []))])).
-define(TRACE_W(Str, Args), io:format( "~ts", [unicode:characters_to_binary(io_lib:format(Str, Args))])).

