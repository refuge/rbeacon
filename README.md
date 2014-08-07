

# rbeacon -  LAN discovery and presence. #

Copyright (c) 2014 Benoît Chesneau.

__Version:__ 0.3.0

## Description

The rbeacon module implements a peer-to-peer discovery service for local
networks. A beacon can broadcast and/or capture service announcements using UDP
messages on the local area network. This implementation uses IPv4 UDP
broadcasts. You can define the format of your outgoing beacons, and set a filter
that validates incoming beacons. Beacons are sent and received asynchronously in
the background.

> Note: the current implementation is heavily inspired on
[zbeacon](http://czmq.zeromq.org/manual:zbeacon) from zeromq.

## Usage

Look at the [`rbeacon`](http://github.com/refuge/rbeacon/blob/fix/gh4/doc/rbeacon.md) module for the API usage.

> **Note:** You can set the interface on which you broadcast using the
application environment setting `broadast_if` or the the OS environment variable
`RBEACON_INTERFACE`.

## Example

From the rbeacon_test function:

```
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
rbeacon:close(Node3).
```

You can also receive from the beacon as message (from the rbeacon_active_test):

```
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
```

## Ownership and License
The contributors are listed in AUTHORS. This project uses the MPL v2 license,
see LICENSE.

rbeacon uses the
[C4.1 (Collective Code Construction Contract)](http://rfc.zeromq.org/spec:22)
process for contributions.

## Development

Under C4.1 process, you are more than welcome to help us by:

* join the discussion over anything from design to code style
* try out and [submit issue reports](http://rfc.zeromq.org/spec:22) or feature requests
* pick a task in [issues](https://github.com/refuge/rbeacon/issues) and get it done
* fork the repository and have your own fixes
* send us pull requests
* and even star this project ^_^

To  run the test suite:
$ rebar eunit


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/refuge/rbeacon/blob/fix/gh4/doc/rbeacon.md" class="module">rbeacon</a></td></tr></table>

