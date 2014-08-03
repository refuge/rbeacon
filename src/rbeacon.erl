-module(rbeacon).
-behaviour(gen_server).

%% public api
-export([new/1,
         close/1,
         control/2,
         publish/2,
         subscribe/2,
         unsubscribe/1,
         silence/1,
         set_interval/2]).

%% gen server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock,
                port,
                addr,
                owner,
                filter,
                interval,
                transmit,
                tref=nil}).

%% default interval
-define(DEFAULT_INTERVAL, 1000).

%% reuse port option
-define(SOL_SOCKET, 16#ffff).
-define(SO_REUSEPORT, 16#0200).

new(Port) ->
    gen_server:start_link(?MODULE, [Port, self()], []).


%% @doc
%% Close a beacon.
- spec close(Ref::pid()) -> ok.
close(Ref) ->
    try
        gen_server:call(Ref, close, infinity)
    catch
        exit:{noproc,_} -> ok;
        exit:noproc -> ok;
        %% Handle the case where the monitor triggers
        exit:{normal, _} -> ok
    end.

control(Ref, Pid) ->
    gen_server:call(Ref, {control, Pid, self()}).


publish(Ref, Msg) ->
    gen_server:call(Ref, {publish, Msg}, infinity).

subscribe(Ref, Filter) when is_list(Filter) ->
    subscribe(Ref, list_to_binary(Filter));
subscribe(Ref, Filter0) when is_binary(Filter0) ->
    Filter = case Filter0 of
        <<"">> -> all;
        _ -> Filter0
    end,
    gen_server:call(Ref, {subscribe, Filter}).

unsubscribe(Ref) ->
    gen_server:call(Ref, unsubscribe).

silence(Ref) ->
    gen_server:call(Ref, silence).

set_interval(Ref, Interval) when is_integer(Interval) ->
    gen_server:call(Ref, {set_interval, Interval});
set_interval(_, _) ->
    {error, badarg}.


%% gen server functions

init([Port, Owner]) ->
    {ok, Sock} = open_udp_port(Port),
    {ok, #state{sock=Sock,
                port=Port,
                interval=?DEFAULT_INTERVAL,
                owner=Owner}}.

handle_call(close, _From, State) ->
    ok = cancel_timer(State),
    {stop, normal, ok, State#state{tref=nil}};

handle_call({control, To, From}, _From, #state{owner=From}=State) ->
    {reply, ok, State#state{owner=To}};
handle_call({control, _, _}, _From, State) ->
    {reply, {error, not_owner}, State};

handle_call({publish, Msg}, _From, State) ->
    %% broadcast the new message now
    ok = transmit_msg(Msg, State),
    %% reset timer with the new message
    ok = cancel_timer(State),
    {ok, TRef} = timer:send_interval(State#state.interval, self(),
                                     {transmit, Msg}),
    {reply, ok, State#state{transmit=Msg, tref=TRef}};

handle_call({set_interval, Interval}, _From, #state{transmit=Msg}=State) ->
    ok = cancel_timer(State),
    {ok, TRef} = timer:send_interval(Interval, self(), {transmit, Msg}),
    {reply, ok, State#state{interval=Interval, tref=TRef}};


handle_call(silence,  _From, State) ->
    ok = cancel_timer(State),
    {reply, ok, State#state{tref=nil}};

handle_call({subscribe, Filter}, _From, State) ->
    {reply, ok, State#state{filter=Filter}};

handle_call(unsubscribe, _From, State) ->
    {reply, ok, State#state{filter=nil}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({transmit, Msg}, State) ->
    %% broadcast the message now
    ok = transmit_msg(Msg, State),
    {noreply, State};

handle_info({udp, Sock, _IP, _, Msg}, #state{sock=Sock, transmit=Msg}=State) ->
    %% echo, ignore it
    {noreply, State};
handle_info({udp, _Sock, _IP, _, _Msg}, #state{filter=nil}=State) ->
    %% no subscribtion
    {noreply, State};
handle_info({udp, _Sock, IP, _, Msg}, #state{owner=Owner,
                                             filter=Filter}=State) ->
    case filter_match(Msg, Filter) of
        true ->
            Owner ! {rbeacon, self(), Msg, IP};
        false ->
            ok
    end,
    {noreply, State};

handle_info(_Msg, State) ->
    {noreply, State}.

%% close
terminate(normal, #state{owner=Owner}) ->
    Owner ! {rbeacon, self(), closed},
    ok;
%% exit
terminate(Reason, #state{owner=Owner}=State) ->
    Owner ! {'EXIT', rbeacon, self(), Reason},
    error_logger:info_msg("got terminate(~p, ~p)~n", [Reason, State]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



cancel_timer(#state{tref=Tref}) ->
    if Tref /= nil -> timer:cancel(Tref);
        true -> ok
    end,
    ok.

transmit_msg(Msg, #state{sock=Sock, port=Port}) ->
    case addresses() of
        [] ->
            error_logger:info_msg("no addresses to send");
        Addresses ->
            [begin
                    ok = gen_udp:send(Sock, Addr, Port, Msg)
            end || Addr <- Addresses]
    end,
    ok.

filter_match(_, all) -> true;
filter_match(Msg, Filter) when byte_size(Msg) >= byte_size(Filter) ->
    case binary:split(Msg, Filter, [trim]) of
        [Msg] -> false;
        _ -> true
    end;
filter_match(_, _) ->
    false.

open_udp_port(Port) ->
    %% defaullt options
    Options0 = [{active, true},
                {broadcast, true},
                {reuseaddr, true},
                inet,
                binary],
    %% reuse port ?
    Options = case reuseport() of
        false -> Options0;
        ReusePort -> Options0 ++ ReusePort
    end,
    gen_udp:open(Port, Options).

reuseport() ->
    case os:type() of
        {unix, OsName} ->
            case lists:member(OsName, [darwin, freebsd, openbsd, netbsd]) of
                true ->
                    [{raw, ?SOL_SOCKET, ?SO_REUSEPORT, <<1:32/native>>}];
                false ->
                    false
            end;
        _ ->
            false
    end.

addresses() -> addresses(get_env(broadcast_ip, undefined)).

addresses(Addr = {_, _, _, _}) ->
    [Addr];
addresses(_) ->
    [A || {ok, Is} <- [inet:getifaddrs()],
          {_, L} <- Is,
          {broadaddr, A = {_, _, _, _}} <- L,
          lists:member(up, proplists:get_value(flags, L, []))].

get_env(Key, Default) ->
    case application:get_env(?MODULE, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.
