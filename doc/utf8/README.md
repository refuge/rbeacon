

# rbeacon -  LAN discovery and presence. #

Copyright (c) 2014 Benoît Chesneau.

__Version:__ 0.1

# Description

The rbeacon module implements a peer-to-peer discovery service for local
networks. A beacon can broadcast and/or capture service announcements using UDP
messages on the local area network. This implementation uses IPv4 UDP
broadcasts. You can define the format of your outgoing beacons, and set a filter
that validates incoming beacons. Beacons are sent and received asynchronously in
the background.
Note: the current implementation is heavily inspired on
[zbeacon](http://czmq.zeromq.org/manual:zbeacon) from zeromq.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="rbeacon.md" class="module">rbeacon</a></td></tr></table>

