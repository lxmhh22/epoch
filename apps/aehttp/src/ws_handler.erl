-module(ws_handler).

-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).
-export([broadcast/1]).

-define(GPROC_KEY, {p, l, {?MODULE, broadcast}}).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    gproc:reg(?GPROC_KEY),
    {ok, Req, undefined_state}.

websocket_handle({text, MsgBin}, Req, State) ->
    #{<<"target">> := Target0,
      <<"action">> := Action0,
      <<"payload">> := Payload} = jsx:decode(MsgBin, [return_maps]),
    Target = binary_to_existing_atom(Target0, utf8),
    Action = binary_to_existing_atom(Action0, utf8),
    Response0 = ws_int_dispatch:execute(Target, Action, Payload),
    Response =
        case Response0 of
            {error, ErrMsg} ->
                [{status, error}, {message, ErrMsg}];
            ok ->
                [{status, ok}];
            {ok, R} when is_list(R) ->
                [{status, ok}, {result, R}]
        end,
    {reply, {text, jsx:encode(Response)}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({send, Msg}, Req, State) ->
    {reply, {text, jsx:encode(Msg)}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  ok.

broadcast(Msg) ->
    gproc:send(?GPROC_KEY, {send, Msg}).

