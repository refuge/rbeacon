%%% -*- erlang -*-
%%%
%%% This file is part of rbeacon released under the Mozilla Public License
%%% Version 2.0. See the NOTICE for more information.

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
-export([new/1, new/2,
         close/1,
         control/2,
         publish/2,
         subscribe/2,
         unsubscribe/1,
         recv/1, recv/2,
         hostname/1,
         broadcast_ip/1,
         silence/1,
         noecho/1,
         set_interval/2,
         setopts/2]).

%% gen server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {sock,
                port,
                addr,
                broadcast_addr,
                active=false,
                owner,
                filter,
                interval,
                transmit,
                noecho=false,
                tref=nil,
                pending_recv=nil}).

%% default interval
-define(DEFAULT_INTERVAL, 1000).

%% reuse port option
-define(SOL_SOCKET, 16#ffff).
-define(SO_REUSEPORT, 16#0200).

-type beacon() :: pid().
-export_type([beacon/0]).

-type beacon_opts() :: [active |
                        {active, true | false | once | integer()} |
                        {interval, integer()} |
                        noecho |
                        {noecho, true | false}].
-export_type([beacon_opts/0]).


%% @doc Create a new beacon on a certain UDP port.
- spec new(Port::integer()) -> {ok, beacon()} | {error, term()}.
new(Port) ->
    new(Port, []).

%% @doc Create a new beacon on a certain UDP port with options.
-spec new(Port::integer(), Options::beacon_opts())
    -> {ok, beacon()} | {error, term()}.
new(Port, Options) ->
    ok = validate_options(Options),
    gen_server:start_link(?MODULE, [Port, self(), Options], []).


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
    gen_server:call(Ref, {control, Pid, self()}, infinity).

%% @doc Start broadcasting beacon to peers at the specified interval
-spec publish(beacon(), binary()) -> ok | {error, term()}.
publish(Ref, Msg) ->
    gen_server:call(Ref, {publish, to_binary(Msg)}, infinity).

%% @doc Start listening to other peers; zero-sized filter means get everything
%% All messages received by the peer will be then sent to the process owner.
%% Incoming messages are:
%%
%% - `{rbeacon, Beacon, Message, SenderIp}' : when a subscription is received
%% - `{rbeacon, Beacon, closed}' : when a beacon is closed
%% - <code>{'EXIT', rbeacon, Beacon, Reason}</code>: when a beacon exit anormaly

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
    gen_server:call(Ref, {subscribe, Filter}, infinity).

%% @doc Stop listening to other peers
-spec unsubscribe(beacon()) -> ok.
unsubscribe(Ref) ->
    gen_server:call(Ref, unsubscribe, infinity).

%% @doc wait to receive a beacon
-spec recv(beacon()) -> {ok, Addr::string(), Msg::binary()} | {error, term()}.
recv(Ref) ->
    recv(Ref, infinity).

%% @doc wait to receive a beacon with timeout. Timeout can be an integer in
%% milliseconds or infinity.
-spec recv(beacon(), integer() | infinity) ->
    {ok, Addr::string(), Msg::binary()}
    | {error, term()}.
recv(Ref, Timeout) when is_integer(Timeout) orelse Timeout =:= infinity ->
    gen_server:call(Ref, {recv, Timeout}, infinity).


%% @doc Return our own IP address as printable string
-spec hostname(beacon()) -> string().
hostname(Ref) ->
    gen_server:call(Ref, hostname, infinity).

%% @doc Return our own Broadcast IP address as printable string
-spec broadcast_ip(beacon()) -> string().
broadcast_ip(Ref) ->
    gen_server:call(Ref, broadcast_ip, infinity).


%% @doc Stop broadcasting beacons
-spec silence(beacon()) -> ok.
silence(Ref) ->
    gen_server:call(Ref, silence, infinity).

%% @doc  Filter out any beacon that looks exactly like ours
-spec noecho(beacon()) -> ok.
noecho(Ref) ->
    gen_server:call(Ref, noecho, infinity).


%% @doc Set broadcast interval in milliseconds (default is 1000 msec)
-spec set_interval(beacon(), integer()) -> ok | {error, term()}.
set_interval(Ref, Interval) when is_integer(Interval) ->
    gen_server:call(Ref, {set_interval, Interval});
set_interval(_, _) ->
    {error, badarg}.

%% @doc set beacon options. Valid options are:
%% <ul>
%% <li>`active', `{active, true | false | once| N}': like `inet:setopts/1'</li>
%% <li>`{interval, N}': Set
%% broadcast interval in milliseconds</li>
%% <li>`noecho | {noecho, true | false}': Filter out any beacon that
%% looks exactly like ours.</li>
%% </ul>
-spec setopts(beacon(), beacon_opts()) -> ok.
setopts(Ref, Options) ->
    ok = validate_options(Options),
    gen_server:call(Ref, {setopts, Options}, infinity).



%% gen server functions
%% @private
init([Port, Owner, Options]) ->
    Active = proplists:get_value(active, Options, false),
    Interval = proplists:get_value(interval, Options, ?DEFAULT_INTERVAL),
    NoEcho = proplists:get_value(noecho, Options, false),

    %% init the socket
    {ok, Sock} = open_udp_port(Port, Active),

    %% get broadcast address
    {Addr, BroadcastAddr} = broadcast_addr(),


    %% trap exit
    process_flag(trap_exit, true),

    {ok, #state{sock=Sock,
                port=Port,
                addr=Addr,
                broadcast_addr=BroadcastAddr,
                active=Active,
                interval=Interval,
                noecho=NoEcho,
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

handle_call({set_interval, Interval}, _From, #state{transmit=undefined}=State) ->
    {reply, ok, State#state{interval=Interval}};
handle_call({set_interval, Interval}, _From, #state{transmit=Msg}=State) ->
    ok = cancel_timer(State),
    {ok, TRef} = timer:send_interval(Interval, self(), {transmit, Msg}),
    {reply, ok, State#state{interval=Interval, tref=TRef}};

handle_call(hostname,  _From, #state{addr=Addr}=State) ->
    {reply, Addr, State#state{tref=nil}};

handle_call(broadcast_ip,  _From, #state{broadcast_addr=Addr}=State) ->
    {reply, Addr, State#state{tref=nil}};

handle_call(silence,  _From, State) ->
    ok = cancel_timer(State),
    {reply, ok, State#state{tref=nil}};

handle_call(noecho,  _From, State) ->
    {reply, ok, State#state{noecho=true}};

handle_call({setopts, Opts}, _From, State) ->
    {ok, NState} = do_setopts(Opts, State),
    {reply, ok, NState};

handle_call({subscribe, Filter}, _From, State) ->
    {reply, ok, State#state{filter=Filter}};

handle_call(unsubscribe, _From, State) ->
    {reply, ok, State#state{filter=nil}};

handle_call({recv, _}, _From, #state{active=Active}=State)
        when Active /= false ->
    {reply, {error, active}, State};

handle_call({recv, Timeout}, From, #state{pending_recv=nil}=State) ->
    {noreply, handle_recv(From, State, Timeout)};

handle_call({recv, _Timeout}, _From, State) ->
    {reply, {error, already_recv}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info({transmit, Msg}, State) ->
    %% broadcast the message now
    ok = transmit_msg(Msg, State),
    {noreply, State};

handle_info({udp, Sock, _IP, _, Msg}, #state{sock=Sock,
                                             transmit=Msg,
                                             noecho=true}=State) ->
    %% echo, ignore it
    {noreply, State};
handle_info({udp, _Sock, _IP, _, _Msg}, #state{filter=nil}=State) ->
    %% no subscribtion
    {noreply, State};
handle_info({udp, _Sock, IP, _, Msg}, #state{active=Active,
                                             owner=Owner,
                                             filter=Filter}=State) ->

    %% decrement Active if needed
    Active1 = if is_integer(Active) -> Active - 1;
        true -> Active
    end,

    case filter_match(Msg, Filter) of
        true ->
            Owner ! {rbeacon, self(), Msg, IP};
        false ->
            ok
    end,

    %% if active is once or null, then set it to false
    NState = case Active1 of
        once ->
            State#state{active=false};
        0 ->
            State#state{active=false};

        _ ->
            State
    end,
    {noreply, NState};

handle_info({'EXIT', Pid, _Reason}, #state{pending_recv=Pid}=State) ->
    {noreply,  State#state{pending_recv=nil}};
handle_info(_Msg, State) ->
    {noreply, State}.

%% close
%% @private
terminate(normal, #state{active=false}) ->
    ok;
terminate(normal, #state{owner=Owner}) ->
    Owner ! {rbeacon, self(), closed},
    ok;
%% exit
terminate(Reason, #state{active=false}=State) ->
    error_logger:info_msg("got terminate(~p, ~p)~n", [Reason, State]),
    ok;
terminate(Reason, #state{owner=Owner}=State) ->
    Owner ! {'EXIT', rbeacon, self(), Reason},
    error_logger:info_msg("got terminate(~p, ~p)~n", [Reason, State]),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



cancel_timer(#state{tref=Tref}) ->
    if Tref /= nil -> timer:cancel(Tref);
        true -> ok
    end,
    ok.


transmit_msg(Msg, #state{sock=Sock, port=Port, broadcast_addr=Addr}) ->
    gen_udp:send(Sock, Addr, Port, Msg).

filter_match(_, all) -> true;
filter_match(Msg, Filter) when byte_size(Msg) >= byte_size(Filter) ->
    case binary:split(Msg, Filter, [trim]) of
        [Msg] -> false;
        _ -> true
    end;
filter_match(_, _) ->
    false.


handle_recv(From, State, Timeout) ->
    Pid = spawn_link(fun() -> do_recv(From, State, Timeout) end),
    State#state{pending_recv=Pid}.


do_recv(From, _State, Timeout) when
        Timeout =< 0, timeout /= infinity ->
    gen_server:reply(From, {error, timeout});
do_recv(From, #state{sock=Sock, filter=Filter, transmit=Msg,
                     noecho=NoEcho}=State, Timeout) ->

    Start = os:timestamp(),
    case gen_udp:recv(Sock, 0, Timeout) of
        {ok, {Addr, _Port, Packet}} ->
            case filter_match(Packet, Filter) of
                true when Packet =:= Msg, NoEcho =:= true ->
                    do_recv(From, State, new_recv_timeout(Timeout, Start));
                true ->
                    gen_server:reply(From, {ok, Packet, Addr});
                false ->
                    do_recv(From, State, new_recv_timeout(Timeout, Start))
            end;
        Error ->
            gen_server:reply(From, Error)
    end.


new_recv_timeout(infinity, _) ->
    infinity;
new_recv_timeout(Timeout, From) ->
    TDiff = timer:now_diff(os:timestamp(), From),
    Timeout - TDiff.

open_udp_port(Port, Active) ->
    %% defaullt options
    Options0 = [{active, Active},
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


broadcast_addr() ->
    IfName = case os:getenv("RBEACON_INTERFACE") of
        false -> get_env("broadast_if", "");
        Name -> Name
    end,

    case IfName of
        "*" -> "255.255.255.255";
        _ -> broadcast_addr(IfName)
    end.


broadcast_addr(Name) ->
    {ok, Interfaces} = inet:getifaddrs(),
    broadcast_addr(Interfaces, Name, []).


broadcast_addr([], _Name, []) ->
    {any, "255.255.255.255"};
broadcast_addr([], _Name, [Addr | _]) ->
    Addr;
broadcast_addr([Interface | Rest], Name, Acc) ->
    {IfName, Props} = Interface,
    Flags = proplists:get_value(flags, Props, []),
    Addr = parse_addr(Props),

    Up = lists:member(up, Flags),
    Broadcast = lists:member(broadcast, Flags),
    LoopBack =  lists:member(loopback, Flags),
    P2P = lists:member(pointtopoint, Flags),

    if Up =:= true, Broadcast =:= true, LoopBack /= true andalso P2P /= true ->
            case proplists:get_value(broadaddr, Props) of
                A = {_, _, _, _} when IfName =:= Name ->
                    {Addr, A};
                A = {_, _, _, _} ->
                    broadcast_addr(Rest, Name, [{Addr, A} | Acc]);
                _ ->
                    broadcast_addr(Rest, Name, Acc)
            end;
        true ->
            broadcast_addr(Rest, Name, Acc)
    end.

parse_addr([]) ->
    any;
parse_addr([{addr, {_, _, _, _}=A} | _]) ->
    A;
parse_addr([_ | Rest]) ->
    parse_addr(Rest).


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


validate_options([]) ->
    ok;
validate_options([{active, N} | Rest]) when is_integer(N) ->
    validate_options(Rest);
validate_options([{active, once} | Rest]) ->
    validate_options(Rest);
validate_options([{active, true} | Rest]) ->
    validate_options(Rest);
validate_options([{active, false} | Rest]) ->
    validate_options(Rest);
validate_options([active | Rest]) ->
    validate_options(Rest);
validate_options([{interval, N} | Rest]) when is_integer(N) ->
    validate_options(Rest);
validate_options([{noecho, _NoEcho} | Rest]) ->
    validate_options(Rest);
validate_options([noecho | Rest]) ->
    validate_options(Rest);
validate_options(_) ->
    badarg.


do_setopts([], State) ->
    {ok, State};
do_setopts([{active, Active} | Rest], #state{sock=S}=State) ->
    ok = inet:setopts(S, [{active, Active}]),
    do_setopts(Rest, State#state{active=Active});
do_setopts([active | Rest], #state{sock=S}=State) ->
    ok = inet:setopts(S, [{active, true}]),
    do_setopts(Rest, State#state{active=true});
do_setopts([{interval, N} | Rest], #state{transmit=Msg}=State) ->
    State2 = case Msg of
        undefined -> State#state{interval=N};
        _ ->
            ok = cancel_timer(State),
            {ok, TRef} = timer:send_interval(N, self(), {transmit, Msg}),
            State#state{interval=N, tref=TRef}
    end,
    do_setopts(Rest, State2);
do_setopts([{noecho, NoEcho} | Rest], State) ->
    do_setopts(Rest, State#state{noecho=NoEcho});
do_setopts([noecho | Rest], State) ->
    do_setopts(Rest, State#state{noecho=true});
do_setopts(_, _State) ->
    badarg.



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

    {ok, Msg, _Addr} = rbeacon:recv(Client),
    ?assertEqual(Msg, <<"announcement">>),


    ok = rbeacon:close(Service),
    ok = rbeacon:close(Client),

    {ok, Node1} = rbeacon:new(5670),
    {ok, Node2} = rbeacon:new(5670),
    {ok, Node3} = rbeacon:new(5670),

    ok = rbeacon:noecho(Node1),

    rbeacon:publish(Node1, <<"Node/1">>),
    rbeacon:publish(Node2, <<"Node/2">>),
    rbeacon:publish(Node3, <<"GARBAGE">>),
    rbeacon:subscribe(Node1, <<"Node">>),

    {ok, Msg2, _Addr} = rbeacon:recv(Node1),
    ?assertEqual(Msg2, <<"Node/2">>),

    rbeacon:close(Node1),
    rbeacon:close(Node2),
    rbeacon:close(Node3),

    ok.

rbeacon_active_test() ->
    {ok, Service} = rbeacon:new(9999, [active, noecho]),
    ?assert(is_pid(Service)),

    ok = rbeacon:set_interval(Service, 100),
    ok = rbeacon:publish(Service, <<"announcement">>),

    {ok, Client} = rbeacon:new(9999, [active]),
    ok = rbeacon:subscribe(Client, <<>>),
    receive
        {rbeacon, Client, <<"announcement">>, _Addr} ->
            ok
    end,

    ok = rbeacon:close(Service),
    receive
        {rbeacon, Service, closed} -> ok
    end,

    ok = rbeacon:close(Client),
    receive
        {rbeacon, Client, closed} -> ok
    end,

    {ok, Node1} = rbeacon:new(5670, [active, noecho]),
    {ok, Node2} = rbeacon:new(5670, [active, noecho]),
    {ok, Node3} = rbeacon:new(5670, [active, noecho]),

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
