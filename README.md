

# rbeacon -  LAN discovery and presence. #

Copyright (c) 2014 Benoît Chesneau.

__Version:__ 0.1

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

Look at the [`rbeacon`](http://github.com/refuge/rbeacon/blob/master/doc/rbeacon.md) module for the API usage.

## Example

From the rbeacon_test function:

```
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
receive
    {rbeacon, Service, closed} -> ok
end,

ok = rbeacon:close(Client),
receive
    {rbeacon, Client, closed} -> ok
end,

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
```



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="http://github.com/refuge/rbeacon/blob/master/doc/rbeacon.md" class="module">rbeacon</a></td></tr></table>

