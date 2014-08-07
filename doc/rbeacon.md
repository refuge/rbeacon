

# Module rbeacon #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


The rbeacon module implements a peer-to-peer discovery service for local
networks.
__Behaviours:__ [`gen_server`](gen_server.md).
<a name="description"></a>

## Description ##
A beacon can broadcast and/or capture service announcements using
UDP messages on the local area network. This implementation uses IPv4 UDP
broadcasts. You can define the format of your outgoing beacons, and set a
filter that validates incoming beacons. Beacons are sent and received
asynchronously in the background.

<a name="types"></a>

## Data Types ##




### <a name="type-beacon">beacon()</a> ###



<pre><code>
beacon() = pid()
</code></pre>





### <a name="type-beacon_opts">beacon_opts()</a> ###



<pre><code>
beacon_opts() = [active | {active, true | false | once | integer()} | {interval, integer()} | noecho | {noecho, true | false}]
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#broadcast_ip-1">broadcast_ip/1</a></td><td>Return our own Broadcast IP address as printable string.</td></tr><tr><td valign="top"><a href="#close-1">close/1</a></td><td>close a beacon
Close a beacon.</td></tr><tr><td valign="top"><a href="#control-2">control/2</a></td><td>Assigns a new controlling process Pid to beacon
The controlling process is the process which receives messages from the
beacon.</td></tr><tr><td valign="top"><a href="#hostname-1">hostname/1</a></td><td>Return our own IP address as printable string.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Create a new beacon on a certain UDP port.</td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td>Create a new beacon on a certain UDP port with options.</td></tr><tr><td valign="top"><a href="#noecho-1">noecho/1</a></td><td> Filter out any beacon that looks exactly like ours.</td></tr><tr><td valign="top"><a href="#publish-2">publish/2</a></td><td>Start broadcasting beacon to peers at the specified interval.</td></tr><tr><td valign="top"><a href="#recv-1">recv/1</a></td><td>wait to receive a beacon.</td></tr><tr><td valign="top"><a href="#recv-2">recv/2</a></td><td>wait to receive a beacon with timeout.</td></tr><tr><td valign="top"><a href="#set_interval-2">set_interval/2</a></td><td>Set broadcast interval in milliseconds (default is 1000 msec).</td></tr><tr><td valign="top"><a href="#setopts-2">setopts/2</a></td><td>set beacon options.</td></tr><tr><td valign="top"><a href="#silence-1">silence/1</a></td><td>Stop broadcasting beacons.</td></tr><tr><td valign="top"><a href="#subscribe-2">subscribe/2</a></td><td>Start listening to other peers; zero-sized filter means get everything
All messages received by the peer will be then sent to the process owner.</td></tr><tr><td valign="top"><a href="#unsubscribe-1">unsubscribe/1</a></td><td>Stop listening to other peers.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="broadcast_ip-1"></a>

### broadcast_ip/1 ###


<pre><code>
broadcast_ip(Ref::<a href="#type-beacon">beacon()</a>) -&gt; string()
</code></pre>

<br></br>


Return our own Broadcast IP address as printable string
<a name="close-1"></a>

### close/1 ###


<pre><code>
close(Ref::<a href="#type-beacon">beacon()</a>) -&gt; ok
</code></pre>

<br></br>


close a beacon
Close a beacon.
<a name="control-2"></a>

### control/2 ###


<pre><code>
control(Ref::<a href="#type-beacon">beacon()</a>, Pid::pid()) -&gt; ok | {error, not_owner}
</code></pre>

<br></br>


Assigns a new controlling process Pid to beacon
The controlling process is the process which receives messages from the
beacon. If called by any other process than the current controlling process,
`{error, not_owner}` is returned.
<a name="hostname-1"></a>

### hostname/1 ###


<pre><code>
hostname(Ref::<a href="#type-beacon">beacon()</a>) -&gt; string()
</code></pre>

<br></br>


Return our own IP address as printable string
<a name="new-1"></a>

### new/1 ###


<pre><code>
new(Port::integer()) -&gt; {ok, <a href="#type-beacon">beacon()</a>} | {error, term()}
</code></pre>

<br></br>


Create a new beacon on a certain UDP port.
<a name="new-2"></a>

### new/2 ###


<pre><code>
new(Port::integer(), Options::<a href="#type-beacon_opts">beacon_opts()</a>) -&gt; {ok, <a href="#type-beacon">beacon()</a>} | {error, term()}
</code></pre>

<br></br>


Create a new beacon on a certain UDP port with options.
<a name="noecho-1"></a>

### noecho/1 ###


<pre><code>
noecho(Ref::<a href="#type-beacon">beacon()</a>) -&gt; ok
</code></pre>

<br></br>


 Filter out any beacon that looks exactly like ours
<a name="publish-2"></a>

### publish/2 ###


<pre><code>
publish(Ref::<a href="#type-beacon">beacon()</a>, Msg::binary()) -&gt; ok | {error, term()}
</code></pre>

<br></br>


Start broadcasting beacon to peers at the specified interval
<a name="recv-1"></a>

### recv/1 ###


<pre><code>
recv(Ref::<a href="#type-beacon">beacon()</a>) -&gt; {ok, Addr::string(), Msg::binary()} | {error, term()}
</code></pre>

<br></br>


wait to receive a beacon
<a name="recv-2"></a>

### recv/2 ###


<pre><code>
recv(Ref::<a href="#type-beacon">beacon()</a>, Timeout::integer() | infinity) -&gt; {ok, Addr::string(), Msg::binary()} | {error, term()}
</code></pre>

<br></br>


wait to receive a beacon with timeout. Timeout can be an integer in
milliseconds or infinity.
<a name="set_interval-2"></a>

### set_interval/2 ###


<pre><code>
set_interval(Ref::<a href="#type-beacon">beacon()</a>, Interval::integer()) -&gt; ok | {error, term()}
</code></pre>

<br></br>


Set broadcast interval in milliseconds (default is 1000 msec)
<a name="setopts-2"></a>

### setopts/2 ###


<pre><code>
setopts(Ref::<a href="#type-beacon">beacon()</a>, Options::<a href="#type-beacon_opts">beacon_opts()</a>) -&gt; ok
</code></pre>

<br></br>


set beacon options. Valid options are:

* `active`, `{active, true | false | once| N}`: like `inet:setopts/1`

* `{interval, N}`: Set
broadcast interval in milliseconds

* `noecho | {noecho, true | false}`: Filter out any beacon that
looks exactly like ours.


<a name="silence-1"></a>

### silence/1 ###


<pre><code>
silence(Ref::<a href="#type-beacon">beacon()</a>) -&gt; ok
</code></pre>

<br></br>


Stop broadcasting beacons
<a name="subscribe-2"></a>

### subscribe/2 ###


<pre><code>
subscribe(Ref::<a href="#type-beacon">beacon()</a>, Filter::binary() | string()) -&gt; ok
</code></pre>

<br></br>



Start listening to other peers; zero-sized filter means get everything
All messages received by the peer will be then sent to the process owner.
Incoming messages are:


- `{rbeacon, Beacon, Message, SenderIp}` : when a subscription is received
- `{rbeacon, Beacon, closed}` : when a beacon is closed
- `{'EXIT', rbeacon, Beacon, Reason}`: when a beacon exit anormaly
<a name="unsubscribe-1"></a>

### unsubscribe/1 ###


<pre><code>
unsubscribe(Ref::<a href="#type-beacon">beacon()</a>) -&gt; ok
</code></pre>

<br></br>


Stop listening to other peers
