%%% -*- erlang -*-
%%%
%%% This file is part of rbeacon released under the MIT license.
%%% See the NOTICE for more information.

%% @doc The rbeacon module implements a peer-to-peer discovery service for local
%% networks. A beacon can broadcast and/or capture service announcements using
%% UDP messages on the local area network. This implementation uses IPv4 UDP
%% broadcasts. You can define the format of your outgoing beacons, and set a
%% filter that validates incoming beacons. Beacons are sent and received
%% asynchronously in the background.
%%
-module(rbeacon).
-author('Benoît Chesneau <benoitc@e-engura.org>').
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

-type beacon() :: pid().
-export_type([beacon/0]).


%% @doc Create a new beacon on a certain UDP port.
- spec new(Port::integer()) -> {ok, beacon()} | {error, term()}.
new(Port) ->
    gen_server:start_link(?MODULE, [Port, self()], []).


%% @doc close a beacon
%% Close a beacon.
- spec close(Ref::beacon()) -> ok.
close(Ref) ->
    try
        gen_server:call(Ref, close, infinity)
    catch
        exit:{noproc,_} -> ok;
        exit:noproc -> ok;
        %% Handle the case where the monitor triggers
        exit:{normal, _} -> ok
    end.


%% @doc Assigns a new controlling process Pid to beacon
%% The controlling process is the process which receives messages from the
%% beacon. If called by any other process than the current controlling process,
%% `{error, not_owner}' is returned.
-spec control(beacon(), pid()) -> ok | {error, not_owner}.
control(Ref, Pid) ->
    gen_server:call(Ref, {control, Pid, self()}).

%% @doc Start broadcasting beacon to peers at the specified interval
-spec publish(beacon(), binary()) -> ok | {error, term()}.
publish(Ref, Msg) ->
    gen_server:call(Ref, {publish, to_binary(Msg)}, infinity).

%% @doc Start listening to other peers; zero-sized filter means get everything
%% All messages received by the peer will be then sent to the process owner.
%% Incoming messages are:
%%
%% - `{rbeacon, Beacon, Message, SenderIp}' : when a subscription is received
%%
%% Note: the filter allows you to filter a subscription by its prefix.
-spec subscribe(beacon(), binary() | string()) -> ok.
subscribe(Ref, Filter) when is_list(Filter) ->
    subscribe(Ref, list_to_binary(Filter));
subscribe(Ref, Filter0) when is_binary(Filter0) ->
    Filter = case Filter0 of
        <<"">> -> all;
        _ -> Filter0
    end,
    gen_server:call(Ref, {subscribe, Filter}).

%% @doc Stop listening to other peers
-spec unsubscribe(beacon()) -> ok.
unsubscribe(Ref) ->
    gen_server:call(Ref, unsubscribe).

%% @doc Stop broadcasting beacons
-spec silence(beacon()) -> ok.
silence(Ref) ->
    gen_server:call(Ref, silence).

%% @doc Set broadcast interval in milliseconds (default is 1000 msec)
-spec set_interval(beacon(), integer()) -> ok | {error, term()}.
set_interval(Ref, Interval) when is_integer(Interval) ->
    gen_server:call(Ref, {set_interval, Interval});
set_interval(_, _) ->
    {error, badarg}.


%% gen server functions
%% @private
init([Port, Owner]) ->
    {ok, Sock} = open_udp_port(Port),
    {ok, #state{sock=Sock,
                port=Port,
                interval=?DEFAULT_INTERVAL,
                owner=Owner}}.
%% @private
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

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
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
%% @private
terminate(normal, _State) ->
    ok;
%% exit
terminate(_Reason, _State) ->
    error_logger:info_msg("got terminate(~p, ~p)~n", [_Reason, _State]),
    ok.

%% @private
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


to_binary(V) when is_list(V) ->
    list_to_binary(V);
to_binary(V) when is_atom(V) ->
    atom_to_binary(V, latin1);
to_binary(V) when is_integer(V) ->
    list_to_binary(integer_to_list(V));
to_binary(V) when is_binary(V) ->
    V.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


loop_sub(_Beacon, Acc, 0) ->
    Acc;
loop_sub(Beacon, Acc, N) ->
    receive
        {rbeacon, Beacon, Msg, _} ->
            loop_sub(Beacon, [Msg | Acc], N-1)
    end.


rbeacon_test() ->
    {ok, Service} = rbeacon:new(9999),
    ?assert(is_pid(Service)),

    ok = rbeacon:set_interval(Service, 100),
    ok = rbeacon:publish(Service, <<"announcement">>),

    {ok, Client} = rbeacon:new(9999),
    ok = rbeacon:subscribe(Client, <<>>),
    receive
        {rbeacon, Client, <<"announcement">>, _Addr} ->
            ok
    end,

    ok = rbeacon:close(Service),
    ok = rbeacon:close(Client),

    {ok, Node1} = rbeacon:new(5670),
    {ok, Node2} = rbeacon:new(5670),
    {ok, Node3} = rbeacon:new(5670),

    rbeacon:publish(Node1, <<"Node/1">>),
    rbeacon:publish(Node2, <<"Node/2">>),
    rbeacon:publish(Node3, <<"GARBAGE">>),
    rbeacon:subscribe(Node1, <<"Node">>),

    Result = loop_sub(Node1, [], 1),

    ?assert(lists:member(<<"Node/2">>, Result)),

    rbeacon:close(Node1),
    rbeacon:close(Node2),
    rbeacon:close(Node3),

    ok.

-endif.
