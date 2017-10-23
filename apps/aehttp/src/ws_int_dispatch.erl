-module(ws_int_dispatch).

-export([execute/3]).

-spec execute(Target :: atom(), Action :: atom(), Payload :: list()) ->
                      ok | {ok, list()} | {error, binary()}.
execute(Target, Action, Payload) ->
    case is_valid(Target, Action, Payload) of
        {false, Reason} ->
            {error, Reason};
        true ->
            case do_execute(Target, Action, Payload) of
                ok ->
                  ok;
                {ok, _} = OK ->
                    OK;
                {error, _} = Err ->
                    Err
            end
    end.

is_valid(target, invalid_message, _SomePayload) ->
    {false, <<"some reason">>};
is_valid(_, _, _) ->
    true.

do_execute(silence_dialyzer, _, _) ->
    {error, <<"oh, no">>};
do_execute(mining, stop, _) ->
    ok;
do_execute(mining, start, _) ->
    {ok, [{state, started}]};
do_execute(_, _, _) -> % a catch all for a prettier error when action is missing
    {error, <<"missing action">>}.
