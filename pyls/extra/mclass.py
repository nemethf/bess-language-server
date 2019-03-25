# This file is auto-genereated by bess-gen-doc.
# See https://github.com/nemethf/bess-gen-doc
#
# It is based on bess/protobuf/module_msg.proto, which has the following copyright.

# Copyright (c) 2016-2017, Nefeli Networks, Inc.
# Copyright (c) 2017, The Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
# list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
# this list of conditions and the following disclaimer in the documentation
# and/or other materials provided with the distribution.
#
# * Neither the names of the copyright holders nor the names of their
# contributors may be used to endorse or promote products derived from this
# software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.


from pybess.module import Module
from pybess.bess import BESS

bess = BESS()

class ACL(Module):
  """
  ACL module from NetBricks

  The module ACL creates an access control module which by default blocks all traffic, unless it contains a rule which specifies otherwise.
  Examples of ACL can be found in [acl.bess](https://github.com/NetSys/bess/blob/master/bessctl/conf/samples/acl.bess)

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, rules=None):
    """
    The module ACL creates an access control module which by default blocks all traffic, unless it contains a rule which specifies otherwise.
    Examples of ACL can be found in [acl.bess](https://github.com/NetSys/bess/blob/master/bessctl/conf/samples/acl.bess)

    __Input Gates__: 1
    __Output Gates__: 1

    :param rules: A list of ACL rules.
    """
    pass

  def add(self, rules=None):
    """
    The module ACL creates an access control module which by default blocks all traffic, unless it contains a rule which specifies otherwise.
    Examples of ACL can be found in [acl.bess](https://github.com/NetSys/bess/blob/master/bessctl/conf/samples/acl.bess)

    __Input Gates__: 1
    __Output Gates__: 1

    :param rules: A list of ACL rules.
    """
    pass

  def clear(self):
    pass


class ArpResponder(Module):
  """
  Respond to ARP requests and learns new MAC's

  The ARP Responder module is responding to ARP requests
  TODO: Dynamic learn new MAC's-IP's mapping

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, ip=None, mac_addr=None):
    """
    The ARP Responder module is responding to ARP requests
    TODO: Dynamic learn new MAC's-IP's mapping

    __Input Gates__: 1
    __Output Gates__: 1

    :param ip: One ARP IP-MAC mapping

    The IP
    :param mac_addr: The MAC address
    """
    pass

  def add(self, ip=None, mac_addr=None):
    """
    The ARP Responder module is responding to ARP requests
    TODO: Dynamic learn new MAC's-IP's mapping

    __Input Gates__: 1
    __Output Gates__: 1

    :param ip: One ARP IP-MAC mapping

    The IP
    :param mac_addr: The MAC address
    """
    pass


class BPF(Module):
  """
  classifies packets with pcap-filter(7) syntax

  The BPF module is an access control module that sends packets out on a particular gate based on whether they match a BPF filter.

  __Input Gates__: 1
  __Output Gates__: many (configurable)
  """

  def __init__(self, filters=None):
    """
    The BPF module is an access control module that sends packets out on a particular gate based on whether they match a BPF filter.

    __Input Gates__: 1
    __Output Gates__: many (configurable)

    :param filters: The BPF initialized function takes a list of BPF filters.
    """
    pass

  def add(self, filters=None):
    """
    The BPF module is an access control module that sends packets out on a particular gate based on whether they match a BPF filter.

    __Input Gates__: 1
    __Output Gates__: many (configurable)

    :param filters: The BPF initialized function takes a list of BPF filters.
    """
    pass

  def clear(self):
    """
    The BPF module has a command `clear()` that takes no parameters.
    This command removes all filters from the module.
    """
    pass


class Buffer(Module):
  """
  buffers packets into larger batches

  The Buffer module takes no parameters to initialize (ie, `Buffer()` is sufficient to create one).
  Buffer accepts packets and stores them; it may forward them to the next module only after it has
  received enough packets to fill an entire PacketBatch.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self):
    """
    The Buffer module takes no parameters to initialize (ie, `Buffer()` is sufficient to create one).
    Buffer accepts packets and stores them; it may forward them to the next module only after it has
    received enough packets to fill an entire PacketBatch.

    __Input Gates__: 1
    __Output Gates__: 1
    """
    pass


class Bypass(Module):
  """
  bypasses packets without any processing

  The Bypass module forwards packets by emulating pre-defined packet processing overhead.
  It burns cpu cycles per_batch, per_packet, and per-bytes.
  Bypass is useful primarily for testing and performance evaluation.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, cycles_per_batch=None, cycles_per_packet=None, cycles_per_byte=None):
    """
    The Bypass module forwards packets by emulating pre-defined packet processing overhead.
    It burns cpu cycles per_batch, per_packet, and per-bytes.
    Bypass is useful primarily for testing and performance evaluation.

    __Input Gates__: 1
    __Output Gates__: 1
    """
    pass


class DRR(Module):
  """
  Deficit Round Robin

  The Module DRR provides fair scheduling of flows based on a quantum which is
  number of bytes allocated to each flow on each round of going through all flows.
  Examples can be found [./bessctl/conf/samples/drr.bess]

  __Input_Gates__: 1
  __Output_Gates__:  1
  """

  def __init__(self, num_flows=None, quantum=None, max_flow_queue_size=None):
    """
    The Module DRR provides fair scheduling of flows based on a quantum which is
    number of bytes allocated to each flow on each round of going through all flows.
    Examples can be found [./bessctl/conf/samples/drr.bess]

    __Input_Gates__: 1
    __Output_Gates__:  1

    :param num_flows: Number of flows to handle in module
    :param quantum: the number of bytes to allocate to each on every round
    :param max_flow_queue_size: the max size that any Flows queue can get
    """
    pass

  def set_quantum_size(self, quantum=None):
    """
    the SetQuantumSize function sets a new quantum for DRR module to operate on.

    :param quantum: the number of bytes to allocate to each on every round
    """
    pass

  def set_max_flow_queue_size(self, max_queue_size=None):
    """
    The SetMaxQueueSize function sets a new maximum flow queue size for DRR module.
    If the flow's queue gets to this size, the module starts dropping packets to
    that flow until the queue is below this size.

    :param max_queue_size: the max size that any Flows queue can get
    """
    pass


class Dump(Module):
  """
  Dump packet data and metadata attributes

  The Dump module blindly forwards packets without modifying them. It periodically samples a packet and prints out out to the BESS log (by default stored in `/tmp/bessd.INFO`).

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, interval=None):
    """
    The Dump module blindly forwards packets without modifying them. It periodically samples a packet and prints out out to the BESS log (by default stored in `/tmp/bessd.INFO`).

    __Input Gates__: 1
    __Output Gates__: 1

    :param interval: How frequently to sample and print a packet, in seconds.
    """
    pass

  def set_interval(self, interval=None):
    """
    The Dump module blindly forwards packets without modifying them. It periodically samples a packet and prints out out to the BESS log (by default stored in `/tmp/bessd.INFO`).

    __Input Gates__: 1
    __Output Gates__: 1

    :param interval: How frequently to sample and print a packet, in seconds.
    """
    pass


class EtherEncap(Module):
  """
  encapsulates packets with an Ethernet header

  The EtherEncap module wraps packets in an Ethernet header, but it takes no parameters. Instead, Ethernet source, destination, and type are pulled from a packet's metadata attributes.
  For example: `SetMetadata('dst_mac', 11:22:33:44:55) -> EtherEncap()`
  This is useful when upstream modules wish to assign a MAC address to a packet, e.g., due to an ARP request.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self):
    """
    The EtherEncap module wraps packets in an Ethernet header, but it takes no parameters. Instead, Ethernet source, destination, and type are pulled from a packet's metadata attributes.
    For example: `SetMetadata('dst_mac', 11:22:33:44:55) -> EtherEncap()`
    This is useful when upstream modules wish to assign a MAC address to a packet, e.g., due to an ARP request.

    __Input Gates__: 1
    __Output Gates__: 1
    """
    pass


class ExactMatch(Module):
  """
  Multi-field classifier with an exact match table

  The ExactMatch module splits packets along output gates according to exact match values in arbitrary packet fields.
  To instantiate an ExactMatch module, you must specify which fields in the packet to match over. You can add rules using the function `ExactMatch.add(...)`
  Fields may be stored either in the packet data or its metadata attributes.
  An example script using the ExactMatch code is found
  in [`bess/bessctl/conf/samples/exactmatch.bess`](https://github.com/NetSys/bess/blob/master/bessctl/conf/samples/exactmatch.bess).

  __Input Gates__: 1
  __Output Gates__: many (configurable)
  """

  def __init__(self, fields=None, masks=None):
    """
    The ExactMatch module splits packets along output gates according to exact match values in arbitrary packet fields.
    To instantiate an ExactMatch module, you must specify which fields in the packet to match over. You can add rules using the function `ExactMatch.add(...)`
    Fields may be stored either in the packet data or its metadata attributes.
    An example script using the ExactMatch code is found
    in [`bess/bessctl/conf/samples/exactmatch.bess`](https://github.com/NetSys/bess/blob/master/bessctl/conf/samples/exactmatch.bess).

    __Input Gates__: 1
    __Output Gates__: many (configurable)

    :param fields: A list of ExactMatch Fields
    :param masks: mask(i) corresponds to the mask for field(i)
    """
    pass

  def get_initial_arg(self):
    pass

  def get_runtime_config(self):
    pass

  def set_runtime_config(self, default_gate=None, rules=None):
    """
    ExactMatchConfig represents the current runtime configuration
    of an ExactMatch module, as returned by get_runtime_config and
    set by set_runtime_config.

    :return: ExactMatchConfig represents the current runtime configuration
    of an ExactMatch module, as returned by get_runtime_config and
    set by set_runtime_config.
    """
    pass

  def add(self, gate=None, fields=None):
    """
    The ExactMatch module has a command `add(...)` that takes two parameters.
    The ExactMatch initializer specifies what fields in a packet to inspect; add() specifies
    which values to check for over these fields.
    add() inserts a new rule into the ExactMatch module such that traffic matching
    that bytestring will be forwarded
    out a specified gate.
    Example use: `add(fields=[aton('12.3.4.5'), aton('5.4.3.2')], gate=2)`

    :param gate: The gate to forward out packets that mach this rule.
    :param fields: The exact match values to check for
    """
    pass

  def delete(self, fields=None):
    """
    The ExactMatch module has a command `delete(...)` which deletes an existing rule.
    Example use: `delete(fields=[aton('12.3.4.5'), aton('5.4.3.2')])`

    :param fields: The field values for the rule to be deleted.
    """
    pass

  def clear(self):
    """
    The ExactMatch module has a command `clear()` which takes no parameters.
    This command removes all rules from the ExactMatch module.
    """
    pass

  def set_default_gate(self, gate=None):
    """
    The ExactMatch module has a command `set_default_gate(...)` which takes one parameter.
    This command routes all traffic which does _not_ match a rule to a specified gate.
    Example use in bessctl: `setDefaultGate(gate=2)`

    :param gate: The gate number to send the default traffic out.
    """
    pass


class FlowGen(Module):
  """
  generates packets on a flow basis

  The FlowGen module generates simulated TCP flows of packets with correct SYN/FIN flags and sequence numbers.
  This module is useful for testing, e.g., a NAT module or other flow-aware code.
  Packets are generated off a base, "template" packet by modifying the IP src/dst and TCP src/dst. By default, only the ports are changed and will be modified by incrementing the template ports by up to 20000 more than the template values.

  __Input Gates__: 0
  __Output Gates__: 1
  """

  def __init__(self, template=None, pps=None, flow_rate=None, flow_duration=None, arrival=None, duration=None, quick_rampup=None, ip_src_range=None, ip_dst_range=None, port_src_range=None, port_dst_range=None):
    """
    The FlowGen module generates simulated TCP flows of packets with correct SYN/FIN flags and sequence numbers.
    This module is useful for testing, e.g., a NAT module or other flow-aware code.
    Packets are generated off a base, "template" packet by modifying the IP src/dst and TCP src/dst. By default, only the ports are changed and will be modified by incrementing the template ports by up to 20000 more than the template values.

    __Input Gates__: 0
    __Output Gates__: 1

    :param template: The packet "template". All data packets are derived from this template and contain the same payload.
    :param pps: The total number of packets per second to generate.
    :param flow_rate: The number of new flows to create every second. flow_rate must be <= pps.
    :param flow_duration: The lifetime of a flow in seconds.
    :param arrival: The packet arrival distribution -- must be either "uniform" or "exponential"
    :param duration: The flow duration distribution -- must be either "uniform" or "pareto"
    :param quick_rampup: Whether or not to populate the flowgenerator with initial flows (start generating full pps rate immediately) or to wait for new flows to be generated naturally (all flows have a SYN packet).
    :param ip_src_range: When generating new flows, FlowGen modifies the template packet by changing the IP src, incrementing it by at most ip_src_range (e.g., if the base packet is 10.0.0.1 and range is 5, it will generate packets with IPs 10.0.0.1-10.0.0.6).
    :param ip_dst_range: When generating new flows, FlowGen modifies the template packet by changing the IP dst, incrementing it by at most ip_dst_range.
    :param port_src_range: When generating new flows, FlowGen modifies the template packet by changing the TCP port, incrementing it by at most port_src_range.
    :param port_dst_range: When generating new flows, FlowGen modifies the template packet by changing the TCP dst port, incrementing it by at most port_dst_range.
    """
    pass

  def update(self, template=None, pps=None, flow_rate=None, flow_duration=None, arrival=None, duration=None, quick_rampup=None, ip_src_range=None, ip_dst_range=None, port_src_range=None, port_dst_range=None):
    """
    The FlowGen module generates simulated TCP flows of packets with correct SYN/FIN flags and sequence numbers.
    This module is useful for testing, e.g., a NAT module or other flow-aware code.
    Packets are generated off a base, "template" packet by modifying the IP src/dst and TCP src/dst. By default, only the ports are changed and will be modified by incrementing the template ports by up to 20000 more than the template values.

    __Input Gates__: 0
    __Output Gates__: 1

    :param template: The packet "template". All data packets are derived from this template and contain the same payload.
    :param pps: The total number of packets per second to generate.
    :param flow_rate: The number of new flows to create every second. flow_rate must be <= pps.
    :param flow_duration: The lifetime of a flow in seconds.
    :param arrival: The packet arrival distribution -- must be either "uniform" or "exponential"
    :param duration: The flow duration distribution -- must be either "uniform" or "pareto"
    :param quick_rampup: Whether or not to populate the flowgenerator with initial flows (start generating full pps rate immediately) or to wait for new flows to be generated naturally (all flows have a SYN packet).
    :param ip_src_range: When generating new flows, FlowGen modifies the template packet by changing the IP src, incrementing it by at most ip_src_range (e.g., if the base packet is 10.0.0.1 and range is 5, it will generate packets with IPs 10.0.0.1-10.0.0.6).
    :param ip_dst_range: When generating new flows, FlowGen modifies the template packet by changing the IP dst, incrementing it by at most ip_dst_range.
    :param port_src_range: When generating new flows, FlowGen modifies the template packet by changing the TCP port, incrementing it by at most port_src_range.
    :param port_dst_range: When generating new flows, FlowGen modifies the template packet by changing the TCP dst port, incrementing it by at most port_dst_range.
    """
    pass

  def set_burst(self, burst=None):
    """
    The FlowGen module has a command `set_burst(...)` that allows you to specify
    the maximum number of packets to be stored in a single PacketBatch released
    by the module.
    """
    pass


class GenericDecap(Module):
  """
  remove specified bytes from the beginning of packets

  The GenericDecap module strips off the first few bytes of data from a packet.

  __Input Gates__: 1
  __Ouptut Gates__: 1
  """

  def __init__(self, bytes=None):
    """
    The GenericDecap module strips off the first few bytes of data from a packet.

    __Input Gates__: 1
    __Ouptut Gates__: 1

    :param bytes: The number of bytes to strip off.
    """
    pass


class GenericEncap(Module):
  """
  encapsulates packets with constant values and metadata attributes

  The GenericEncap module adds a header to packets passing through it.
  Takes a list of fields. Each field is either:

   1. {'size': X, 'value': Y}          (for constant values)
   2. {'size': X, 'attribute': Y}      (for metadata attributes)

  e.g.: `GenericEncap([{'size': 4, 'value': 0xdeadbeef},
                       {'size': 2, 'attribute': 'foo'},
                       {'size': 2, 'value': 0x1234}])`
  will prepend a 8-byte header:
     `de ad be ef <xx> <xx> 12 34`
  where the 2-byte `<xx> <xx>` comes from the value of metadata attribute `'foo'`
  for each packet.
  An example script using GenericEncap is in [`bess/bessctl/conf/samples/generic_encap.bess`](https://github.com/NetSys/bess/blob/master/bessctl/conf/samples/generic_encap.bess).

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, fields=None):
    """
    The GenericEncap module adds a header to packets passing through it.
    Takes a list of fields. Each field is either:

     1. {'size': X, 'value': Y}          (for constant values)
     2. {'size': X, 'attribute': Y}      (for metadata attributes)

    e.g.: `GenericEncap([{'size': 4, 'value': 0xdeadbeef},
                         {'size': 2, 'attribute': 'foo'},
                         {'size': 2, 'value': 0x1234}])`
    will prepend a 8-byte header:
       `de ad be ef <xx> <xx> 12 34`
    where the 2-byte `<xx> <xx>` comes from the value of metadata attribute `'foo'`
    for each packet.
    An example script using GenericEncap is in [`bess/bessctl/conf/samples/generic_encap.bess`](https://github.com/NetSys/bess/blob/master/bessctl/conf/samples/generic_encap.bess).

    __Input Gates__: 1
    __Output Gates__: 1
    """
    pass


class HashLB(Module):
  """
  splits packets on a flow basis with L2/L3/L4 header fields

  The HashLB module partitions packets between output gates according to either
  a hash over their MAC src/dst (`mode='l2'`), their IP src/dst (`mode='l3'`), the full
  IP/TCP 5-tuple (`mode='l4'`), or the N-tuple defined by `fields`.

  __Input Gates__: 1
  __Output Gates__: many (configurable)
  """

  def __init__(self, gates=None, mode=None, fields=None):
    """
    The HashLB module partitions packets between output gates according to either
    a hash over their MAC src/dst (`mode='l2'`), their IP src/dst (`mode='l3'`), the full
    IP/TCP 5-tuple (`mode='l4'`), or the N-tuple defined by `fields`.

    __Input Gates__: 1
    __Output Gates__: many (configurable)

    :param gates: A list of gate numbers over which to partition packets
    :param mode: The mode (`'l2'`, `'l3'`, or `'l4'`) for the hash function.
    :param fields: A list of fields that define a custom tuple.
    """
    pass

  def set_mode(self, mode=None, fields=None):
    """
    The HashLB module has a command `set_mode(...)` which takes two parameters.
    The `mode` parameter specifies whether the load balancer will hash over the
    src/dest ethernet header (`'l2'`), over the src/dest IP addresses (`'l3'`), or over
    the flow 5-tuple (`'l4'`).  Alternatively, if the `fields` parameter is set, the
    load balancer will hash over the N-tuple with the specified offsets and
    sizes.
    Example use in bessctl: `lb.set_mode('l2')`

    :param mode: What fields to hash over, `'l2'`, `'l3'`, and `'l4'` are only valid values.
    :param fields: A list of fields that define a custom tuple.
    """
    pass

  def set_gates(self, gates=None):
    """
    The HashLB module has a command `set_gates(...)` which takes one parameter.
    This function takes in a list of gate numbers to send hashed traffic out over.
    Example use in bessctl: `lb.setGates(gates=[0,1,2,3])`

    :param gates: A list of gate numbers to load balance traffic over
    """
    pass


class IPChecksum(Module):
  """
  recomputes the IPv4 checksum
  """


class IPEncap(Module):
  """
  encapsulates packets with an IPv4 header

  Encapsulates a packet with an IP header, where IP src, dst, and proto are filled in
  by metadata values carried with the packet. Metadata attributes must include:
  ip_src, ip_dst, ip_proto, ip_nexthop, and ether_type.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self):
    """
    Encapsulates a packet with an IP header, where IP src, dst, and proto are filled in
    by metadata values carried with the packet. Metadata attributes must include:
    ip_src, ip_dst, ip_proto, ip_nexthop, and ether_type.

    __Input Gates__: 1
    __Output Gates__: 1
    """
    pass


class IPLookup(Module):
  """
  performs Longest Prefix Match on IPv4 packets

  An IPLookup module perfroms LPM lookups over a packet destination.
  IPLookup takes no parameters to instantiate.
  To add rules to the IPLookup table, use `IPLookup.add()`

  __Input Gates__: 1
  __Output Gates__: many (configurable, depending on rule values)
  """

  def __init__(self, max_rules=None, max_tbl8s=None):
    """
    An IPLookup module perfroms LPM lookups over a packet destination.
    IPLookup takes no parameters to instantiate.
    To add rules to the IPLookup table, use `IPLookup.add()`

    __Input Gates__: 1
    __Output Gates__: many (configurable, depending on rule values)

    :param max_rules: Maximum number of rules (default: 1024)
    :param max_tbl8s: Maximum number of IP prefixes with smaller than /24 (default: 128)
    """
    pass

  def add(self, prefix=None, prefix_len=None, gate=None):
    """
    The IPLookup module has a command `add(...)` which takes three paramters.
    This function accepts the routing rules -- CIDR prefix, CIDR prefix length,
    and what gate to forward matching traffic out on.
    Example use in bessctl: `table.add(prefix='10.0.0.0', prefix_len=8, gate=2)`

    :param prefix: The CIDR IP part of the prefix to match
    :param prefix_len: The prefix length
    :param gate: The number of the gate to forward matching traffic on.
    """
    pass

  def delete(self, prefix=None, prefix_len=None):
    """
    The IPLookup module has a command `delete(...)` which takes two paramters.
    This function accepts the routing rules -- CIDR prefix, CIDR prefix length,
    Example use in bessctl: `table.delete(prefix='10.0.0.0', prefix_len=8)`

    :param prefix: The CIDR IP part of the prefix to match
    :param prefix_len: The prefix length
    """
    pass

  def clear(self):
    """
    The IPLookup module has a command `clear()` which takes no parameters.
    This function removes all rules in the IPLookup table.
    Example use in bessctl: `myiplookuptable.clear()`
    """
    pass


class IPSwap(Module):
  """
  swaps source/destination IP addresses and L4 ports
  """


class L2Forward(Module):
  """
  classifies packets with destination MAC address

  An L2Forward module forwards packets to an output gate according to exact-match rules over
  an Ethernet destination.
  Note that this is _not_ a learning switch -- forwards according to fixed
  routes specified by `add(..)`.

  __Input Gates__: 1
  __Ouput Gates__: many (configurable, depending on rules)
  """

  def __init__(self, size=None, bucket=None):
    """
    An L2Forward module forwards packets to an output gate according to exact-match rules over
    an Ethernet destination.
    Note that this is _not_ a learning switch -- forwards according to fixed
    routes specified by `add(..)`.

    __Input Gates__: 1
    __Ouput Gates__: many (configurable, depending on rules)

    :param size: Configures the forwarding hash table -- total number of hash table entries.
    :param bucket: Configures the forwarding hash table -- total number of slots per hash value.
    """
    pass

  def add(self, entries=None):
    """
    The L2Forward module forwards traffic via exact match over the Ethernet
    destination address. The command `add(...)`  allows you to specifiy a
    MAC address and which gate the L2Forward module should direct it out of.

    :param entries: A list of L2Forward entries.
    """
    pass

  def delete(self, addrs=None):
    """
    The L2Forward module has a function `delete(...)` to remove a rule
    from the MAC forwarding table.

    :param addrs: The address to remove from the forwarding table
    """
    pass

  def set_default_gate(self, gate=None):
    """
    For traffic reaching the L2Forward module which does not match a MAC rule,
    the function `set_default_gate(...)` allows you to specify a default gate
    to direct unmatched traffic to.

    :param gate: The default gate to forward traffic which matches no entry to.
    """
    pass

  def lookup(self, addrs=None):
    """
    The L2Forward module has a function `lookup(...)` to query what output gate
    a given MAC address will be forwared to; it returns the gate ID number.

    :param addrs: The MAC address to query for

    :return: This message type provides the reponse to the L2Forward function `lookup(..)`.
    It returns the gate that a requested MAC address is currently assigned to.
    """
    pass

  def populate(self, base=None, count=None, gate_count=None):
    """
    The L2Forward module has a command `populate(...)` which allows for fast creation
    of the forwarding table given a range of MAC addresses. The function takes in a
    'base' MAC address, a count (number of MAC addresses), and a gate_id. The module
    will route all MAC addresses starting from the base address, up to base+count address
    round-robin over gate_count total gates.
    For example, `populate(base='11:22:33:44:00', count = 10, gate_count = 2) would
    route addresses 11:22:33:44::(00, 02, 04, 06, 08) out a gate 0 and the odd-suffixed
    addresses out gate 1.

    :param base: The base MAC address
    :param count: How many addresses beyond base to populate into the routing table
    :param gate_count: How many gates to create in the L2Forward module.
    """
    pass


class L4Checksum(Module):
  """
  recomputes the TCP/Ipv4 and UDP/IPv4 checksum
  """


class MACSwap(Module):
  """
  swaps source/destination MAC addresses

  The MACSwap module takes no arguments. It swaps the src/destination MAC addresses
  within a packet.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self):
    """
    The MACSwap module takes no arguments. It swaps the src/destination MAC addresses
    within a packet.

    __Input Gates__: 1
    __Output Gates__: 1
    """
    pass


class MPLSPop(Module):
  """
  Pop MPLS label

  The MPLS pop module removes MPLS labels

  __Input Gates__: 1
  __Output Gates__: 2
  """

  def __init__(self, remove_eth_header=None, next_eth_type=None):
    """
    The MPLS pop module removes MPLS labels

    __Input Gates__: 1
    __Output Gates__: 2

    :param remove_eth_header: Remove ETH header with the pop
    :param next_eth_type: The next ETH type to set
    """
    pass

  def set(self, remove_eth_header=None, next_eth_type=None):
    """
    The MPLS pop module removes MPLS labels

    __Input Gates__: 1
    __Output Gates__: 2

    :param remove_eth_header: Remove ETH header with the pop
    :param next_eth_type: The next ETH type to set
    """
    pass


class Measure(Module):
  """
  measures packet latency (paired with Timestamp module)

  The measure module tracks latencies, packets per second, and other statistics.
  It should be paired with a Timestamp module, which attaches a timestamp to packets.
  The measure module will log how long (in nanoseconds) it has been for each packet it received since it was timestamped.
  This module is somewhat experimental and undergoing various changes.
  There is a test for the the Measure module in [`bessctl/module_tests/timestamp.py`](https://github.com/NetSys/bess/blob/master/bessctl/module_tests/timestamp.py).

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, offset=None, jitter_sample_prob=None, latency_ns_max=None, latency_ns_resolution=None):
    """
    The measure module tracks latencies, packets per second, and other statistics.
    It should be paired with a Timestamp module, which attaches a timestamp to packets.
    The measure module will log how long (in nanoseconds) it has been for each packet it received since it was timestamped.
    This module is somewhat experimental and undergoing various changes.
    There is a test for the the Measure module in [`bessctl/module_tests/timestamp.py`](https://github.com/NetSys/bess/blob/master/bessctl/module_tests/timestamp.py).

    __Input Gates__: 1
    __Output Gates__: 1

    :param offset: int64 warmup = 1; /// removed: instead of warmup delay, user should Clear()

    / Where to store the current time within the packet, offset in bytes.
    :param jitter_sample_prob: How often the module should sample packets for inter-packet arrival measurements (to measure jitter).
    :param latency_ns_max: maximum latency expected, in ns (default 0.1 s)
    :param latency_ns_resolution: resolution, in ns (default 100)
    """
    pass

  def get_summary(self, clear=None, latency_percentiles=None, jitter_percentiles=None):
    """
    The Measure module measures and collects latency/jitter data for packets
    annotated by a Timestamp module. Note that Timestamp and Measure module must reside
    on the server for accurate measurement (as a result, the most typical use case is
    measuring roundtrip time).
    Optionally, you can also retrieve percentile values by specifying points in
    "percentiles". For example, "percentiles" of [50.0, 99.0] will return
    [median, 99'th %-ile tail latency] in "percentile_values_ns" in the response.

    :param clear: if true, the data will be all cleared after read
    :param latency_percentiles: ascending list of real numbers in [0.0, 100.0]
    :param jitter_percentiles: ascending list of real numbers in [0.0, 100.0]

    :return: The Measure module function `get_summary()` returns the following values.
    Note that the resolution value tells you how grainy the samples are,
    e.g., 100 means that anything from 0-99 ns counts as "0",
    anything from 100-199 counts as "100", and so on.  The average
    is of samples using this graininess, but (being a result of division)
    may not be a multiple of the resolution.
    """
    pass

  def clear(self):
    pass


class Merge(Module):
  """
  All input gates go out of a single output gate

  The merge module takes no parameters. It has multiple input gates,
  and passes out all packets from a single output gate.

  __Input Gates__: many (configurable)
  __Output Gates__: 1
  """

  def __init__(self):
    """
    The merge module takes no parameters. It has multiple input gates,
    and passes out all packets from a single output gate.

    __Input Gates__: many (configurable)
    __Output Gates__: 1
    """
    pass


class MetadataTest(Module):
  """
  Dynamic metadata test module

  The MetadataTest module is used for internal testing purposes.
  """

  def __init__(self, read=None, write=None, update=None):
    """
    The MetadataTest module is used for internal testing purposes.
    """
    pass


class NAT(Module):
  """
  Dynamic Network address/port translator

  The NAT module implements Dynamic IPv4 address/port translation,
  rewriting packet source addresses with external addresses as specified,
  and destination addresses for packets on the reverse direction.
  L3/L4 checksums are updated correspondingly.
  To see an example of NAT in use, see:
  [`bess/bessctl/conf/samples/nat.bess`](https://github.com/NetSys/bess/blob/master/bessctl/conf/samples/nat.bess)

  Currently only supports TCP/UDP/ICMP.
  Note that address/port in packet payload (e.g., FTP) are NOT translated.

  __Input Gates__: 2 (0 for internal->external, and 1 for external->internal direction)
  __Output Gates__: 2 (same as the input gate)
  """

  def __init__(self, ext_addrs=None):
    """
    The NAT module implements Dynamic IPv4 address/port translation,
    rewriting packet source addresses with external addresses as specified,
    and destination addresses for packets on the reverse direction.
    L3/L4 checksums are updated correspondingly.
    To see an example of NAT in use, see:
    [`bess/bessctl/conf/samples/nat.bess`](https://github.com/NetSys/bess/blob/master/bessctl/conf/samples/nat.bess)

    Currently only supports TCP/UDP/ICMP.
    Note that address/port in packet payload (e.g., FTP) are NOT translated.

    __Input Gates__: 2 (0 for internal->external, and 1 for external->internal direction)
    __Output Gates__: 2 (same as the input gate)

    :param ext_addrs: list of external IP addresses
    """
    pass

  def get_initial_arg(self):
    pass

  def get_runtime_config(self):
    pass

  def set_runtime_config(self):
    pass


class NoOP(Module):
  """
  creates a task that does nothing

  This module is used for testing purposes.
  """

  def __init__(self):
    """
    This module is used for testing purposes.
    """
    pass


class PortInc(Module):
  """
  receives packets from a port

  The PortInc module connects a physical or virtual port and releases
  packets from it. PortInc does not support multiqueueing.
  For details on how to configure PortInc using DPDK, virtual ports,
  or libpcap, see the sidebar in the wiki.

  __Input Gates__: 0
  __Output Gates__: 1
  """

  def __init__(self, port=None, prefetch=None):
    """
    The PortInc module connects a physical or virtual port and releases
    packets from it. PortInc does not support multiqueueing.
    For details on how to configure PortInc using DPDK, virtual ports,
    or libpcap, see the sidebar in the wiki.

    __Input Gates__: 0
    __Output Gates__: 1

    :param port: The portname to connect to.
    :param prefetch: Whether or not to prefetch packets from the port.
    """
    pass

  def set_burst(self, burst=None):
    """
    The module PortInc has a function `set_burst(...)` that allows you to specify the
    maximum number of packets to be stored in a single PacketBatch released by
    the module.

    :param burst: The maximum "burst" of packets (ie, the maximum batch size)
    """
    pass


class PortOut(Module):
  """
  sends pakets to a port

  The PortOut module connects to a physical or virtual port and pushes
  packets to it. For details on how to configure PortOut with DPDK,
  virtual ports, libpcap, etc, see the sidebar in the wiki.

  __Input Gates__: 1
  __Output Gates__: 0
  """

  def __init__(self, port=None):
    """
    The PortOut module connects to a physical or virtual port and pushes
    packets to it. For details on how to configure PortOut with DPDK,
    virtual ports, libpcap, etc, see the sidebar in the wiki.

    __Input Gates__: 1
    __Output Gates__: 0

    :param port: The portname to connect to.
    """
    pass


class Queue(Module):
  """
  terminates current task and enqueue packets for new task

  The Queue module implements a simple packet queue.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, size=None, prefetch=None, backpressure=None):
    """
    The Queue module implements a simple packet queue.

    __Input Gates__: 1
    __Output Gates__: 1

    :param size: The maximum number of packets to store in the queue.
    :param prefetch: When prefetch is enabled, the module will perform CPU prefetch on the first 64B of each packet onto CPU L1 cache. Default value is false.
    :param backpressure: When backpressure is enabled, the module will notify upstream if it is overloaded.
    """
    pass

  def set_burst(self, burst=None):
    """
    The module Queue has a function `set_burst(...)` that allows you to specify
    the maximum number of packets to be stored in a single PacketBatch released
    by the module.

    :param burst: The maximum "burst" of packets (ie, the maximum batch size)
    """
    pass

  def set_size(self, size=None):
    """
    The module Queue has a function `set_size(...)` that allows specifying the
    size of the queue in total number of packets.

    :param size: The maximum number of packets to store in the queue.
    """
    pass

  def get_status(self):
    """
    Modules that are queues or contain queues may contain functions
    `get_status()` that return QueueCommandGetStatusResponse.

    :return: Modules that are queues or contain queues may contain functions
    `get_status()` that take no parameters and returns the queue occupancy and
    size.
    """
    pass


class QueueInc(Module):
  """
  receives packets from a port via a specific queue

  The module QueueInc produces input packets from a physical or virtual port.
  Unlike PortInc, it supports multiqueue ports.
  For details on how to configure QueueInc with DPDK, virtualports,
  libpcap, etc, see the sidebar in the wiki.

  __Input Gates__: 0
  __Output Gates__: 1
  """

  def __init__(self, port=None, qid=None, prefetch=None):
    """
    The module QueueInc produces input packets from a physical or virtual port.
    Unlike PortInc, it supports multiqueue ports.
    For details on how to configure QueueInc with DPDK, virtualports,
    libpcap, etc, see the sidebar in the wiki.

    __Input Gates__: 0
    __Output Gates__: 1

    :param port: The portname to connect to (read from).
    :param qid: The queue on that port to read from. qid starts from 0.
    :param prefetch: When prefetch is enabled, the module will perform CPU prefetch on the first 64B of each packet onto CPU L1 cache. Default value is false.
    """
    pass

  def set_burst(self, burst=None):
    """
    The module QueueInc has a function `set_burst(...)` that allows you to specify
    the maximum number of packets to be stored in a single PacketBatch released
    by the module.

    :param burst: The maximum "burst" of packets (ie, the maximum batch size)
    """
    pass


class QueueOut(Module):
  """
  sends packets to a port via a specific queue

  The QueueOut module releases packets to a physical or virtual port.
  Unlike PortOut, it supports multiqueue ports.
  For details on how to configure QueueOut with DPDK, virtualports,
  libpcap, etc, see the sidebar in the wiki.

  __Input Gates__: 1
  __Output Gates__: 0
  """

  def __init__(self, port=None, qid=None):
    """
    The QueueOut module releases packets to a physical or virtual port.
    Unlike PortOut, it supports multiqueue ports.
    For details on how to configure QueueOut with DPDK, virtualports,
    libpcap, etc, see the sidebar in the wiki.

    __Input Gates__: 1
    __Output Gates__: 0

    :param port: The portname to connect to.
    :param qid: The queue on that port to write out to.
    """
    pass


class RandomSplit(Module):
  """
  randomly splits/drops packets

  The RandomSplit module randomly split/drop packets

  __InputGates__: 1
  __Output Gates__: many (configurable)
  """

  def __init__(self, drop_rate=None, gates=None):
    """
    The RandomSplit module randomly split/drop packets

    __InputGates__: 1
    __Output Gates__: many (configurable)

    :param drop_rate: Probability of dropping packet.
    :param gates: A list of gate numbers to split the traffic.
    """
    pass

  def set_droprate(self, drop_rate=None):
    """
    The RandomSplit module has a function `set_droprate(...)` which specifies
    the probability of dropping packets

    :param drop_rate: Probability of dropping packet.
    """
    pass

  def set_gates(self, gates=None):
    """
    The RandomSplit module has a function `set_gates(...)` which changes
    the total number of output gates in the module.

    :param gates: A list of gate numbers to split the traffic.
    """
    pass


class RandomUpdate(Module):
  """
  updates packet data with random values

  The RandomUpdate module rewrites a specified field (`offset` and `size`) in a packet
  with a random value between a specified min and max values.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, fields=None):
    """
    The RandomUpdate module rewrites a specified field (`offset` and `size`) in a packet
    with a random value between a specified min and max values.

    __Input Gates__: 1
    __Output Gates__: 1

    :param fields: A list of Random Update Fields.
    """
    pass

  def add(self, fields=None):
    """
    The RandomUpdate module rewrites a specified field (`offset` and `size`) in a packet
    with a random value between a specified min and max values.

    __Input Gates__: 1
    __Output Gates__: 1

    :param fields: A list of Random Update Fields.
    """
    pass

  def clear(self):
    """
    The function `clear()` for RandomUpdate takes no parameters and clears all
    state in the module.
    """
    pass


class Replicate(Module):
  """
  makes a copy of a packet and sends it out over n gates

  The Replicate module makes copies of a packet sending one copy out over each
  of n output gates.

  __Input Gates__: 1
  __Output Gates__: many (configurable)
  """

  def __init__(self, gates=None):
    """
    The Replicate module makes copies of a packet sending one copy out over each
    of n output gates.

    __Input Gates__: 1
    __Output Gates__: many (configurable)

    :param gates: A list of gate numbers to send packet copies to.
    """
    pass

  def set_gates(self, gates=None):
    """
    The Replicate module has a function `set_gates(...)` which changes
    the total number of output gates in the module.

    :param gates: A list of gate numbers to replicate the traffic over.
    """
    pass


class Rewrite(Module):
  """
  replaces entire packet data

  The Rewrite module replaces an entire packet body with a packet "template"
  converting all packets that pass through to copies of the of one of
  the templates.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, templates=None):
    """
    The Rewrite module replaces an entire packet body with a packet "template"
    converting all packets that pass through to copies of the of one of
    the templates.

    __Input Gates__: 1
    __Output Gates__: 1

    :param templates: A list of bytestrings representing packet templates.
    """
    pass

  def add(self, templates=None):
    """
    The Rewrite module replaces an entire packet body with a packet "template"
    converting all packets that pass through to copies of the of one of
    the templates.

    __Input Gates__: 1
    __Output Gates__: 1

    :param templates: A list of bytestrings representing packet templates.
    """
    pass

  def clear(self):
    """
    The function `clear()` for Rewrite takes no parameters and clears all state
    in the module.
    """
    pass


class RoundRobin(Module):
  """
  splits packets evenly with round robin

  The RoundRobin module splits packets from one input gate across multiple output
  gates.

  __Input Gates__: 1
  __Output Gates__: many (configurable)
  """

  def __init__(self, gates=None, mode=None):
    """
    The RoundRobin module splits packets from one input gate across multiple output
    gates.

    __Input Gates__: 1
    __Output Gates__: many (configurable)

    :param gates: A list of gate numbers to split packets across.
    :param mode: Whether to split across gate with every `'packet'` or every `'batch'`.
    """
    pass

  def set_mode(self, mode=None):
    """
    The RoundRobin module has a function `set_mode(...)` which specifies whether
    to balance traffic across gates per-packet or per-batch.

    :param mode: whether to perform `'packet'` or `'batch'` round robin partitioning.
    """
    pass

  def set_gates(self, gates=None):
    """
    The RoundRobin module has a function `set_gates(...)` which changes
    the total number of output gates in the module.

    :param gates: A list of gate numbers to round-robin the traffic over.
    """
    pass


class SetMetadata(Module):
  """
  Set metadata attributes to packets

  The SetMetadata module adds metadata attributes to packets, which are not stored
  or sent out with packet data. For examples of SetMetadata use, see
  [`bess/bessctl/conf/attr_match.bess`](https://github.com/NetSys/bess/blob/master/bessctl/conf/metadata/attr_match.bess)

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, attrs=None):
    """
    The SetMetadata module adds metadata attributes to packets, which are not stored
    or sent out with packet data. For examples of SetMetadata use, see
    [`bess/bessctl/conf/attr_match.bess`](https://github.com/NetSys/bess/blob/master/bessctl/conf/metadata/attr_match.bess)

    __Input Gates__: 1
    __Output Gates__: 1

    :param attrs: A list of attributes to attach to the packet.
    """
    pass


class Sink(Module):
  """
  discards all packets

  The sink module drops all packets that are sent to it.

  __Input Gates__: 1
  __Output Gates__: 0
  """

  def __init__(self):
    """
    The sink module drops all packets that are sent to it.

    __Input Gates__: 1
    __Output Gates__: 0
    """
    pass


class Source(Module):
  """
  infinitely generates packets with uninitialized data

  The Source module generates packets with no payload contents.

  __Input Gates__: 0
  __Output Gates__: 1
  """

  def __init__(self, pkt_size=None):
    """
    The Source module generates packets with no payload contents.

    __Input Gates__: 0
    __Output Gates__: 1

    :param pkt_size: The size (in bytes) of packet data to produce.
    """
    pass

  def set_pkt_size(self, pkt_size=None):
    """
    The Source module has a function `set_pkt_size(...)` which specifies the size
    of packets to be produced by the Source module.

    :param pkt_size: The size (in bytes) of the packets for Source to create.
    """
    pass

  def set_burst(self, burst=None):
    """
    The Source module has a function `set_burst(...)` which
    specifies the maximum number of packets to release in a single packetbatch
    from the module.

    :param burst: The maximum number of packets to release in a packetbatch from the module.
    """
    pass


class Split(Module):
  """
  split packets depending on packet data or metadata attributes

  The Split module is a basic classifier which directs packets out a gate
  based on data in the packet (e.g., if the read in value is 3, the packet
  is directed out output gate 3).

  __Input Gates__: 1
  __Output Gates__: many (up to 2^(size * 8))
  """

  def __init__(self, size=None, attribute=None, offset=None):
    """
    The Split module is a basic classifier which directs packets out a gate
    based on data in the packet (e.g., if the read in value is 3, the packet
    is directed out output gate 3).

    __Input Gates__: 1
    __Output Gates__: many (up to 2^(size * 8))

    :param size: The size of the value to read in bytes
    :param attribute: The name of the metadata field to read.
    :param offset: The offset (in bytes) of the data field to read.
    """
    pass


class StaticNAT(Module):
  """
  Static network address translator

  Static NAT module implements one-to-one translation of source/destination
  IPv4 addresses. No port number is translated.
  L3/L4 checksums are updated correspondingly.
  To see an example of NAT in use, see:
  [`bess/bessctl/conf/samples/nat.bess`](https://github.com/NetSys/bess/blob/master/bessctl/conf/samples/nat.bess)

  Forward direction (from input gate 0 to output gate 0):
   - Source IP address is updated, from internal to external address.
  Reverse direction (from input gate 1 to output gate 1):
   - Destination IP address is updated, from external to internal address.
  If the original address is outside any of the ranges, packets are forwarded
  without NAT.

  Note that address in packet payload (e.g., FTP) are NOT translated.

  __Input Gates__: 2 (0 for internal->external, and 1 for external->internal direction)
  __Output Gates__: 2 (same as the input gate)
  """

  def __init__(self, pairs=None):
    """
    Static NAT module implements one-to-one translation of source/destination
    IPv4 addresses. No port number is translated.
    L3/L4 checksums are updated correspondingly.
    To see an example of NAT in use, see:
    [`bess/bessctl/conf/samples/nat.bess`](https://github.com/NetSys/bess/blob/master/bessctl/conf/samples/nat.bess)

    Forward direction (from input gate 0 to output gate 0):
     - Source IP address is updated, from internal to external address.
    Reverse direction (from input gate 1 to output gate 1):
     - Destination IP address is updated, from external to internal address.
    If the original address is outside any of the ranges, packets are forwarded
    without NAT.

    Note that address in packet payload (e.g., FTP) are NOT translated.

    __Input Gates__: 2 (0 for internal->external, and 1 for external->internal direction)
    __Output Gates__: 2 (same as the input gate)
    """
    pass

  def get_initial_arg(self):
    pass

  def get_runtime_config(self):
    pass

  def set_runtime_config(self):
    pass


class Timestamp(Module):
  """
  marks current time to packets (paired with Measure module)

  The timestamp module takes an offset parameter. It inserts the current
  time in nanoseconds into the packet, to be used for latency measurements
  alongside the Measure module.  The default offset is after an IPv4 UDP
  header.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, offset=None):
    """
    The timestamp module takes an offset parameter. It inserts the current
    time in nanoseconds into the packet, to be used for latency measurements
    alongside the Measure module.  The default offset is after an IPv4 UDP
    header.

    __Input Gates__: 1
    __Output Gates__: 1
    """
    pass


class Update(Module):
  """
  updates packet data with specified values

  The Update module rewrites a field in a packet's data with a specific value.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, fields=None):
    """
    The Update module rewrites a field in a packet's data with a specific value.

    __Input Gates__: 1
    __Output Gates__: 1

    :param fields: A list of Update Fields.
    """
    pass

  def add(self, fields=None):
    """
    The Update module rewrites a field in a packet's data with a specific value.

    __Input Gates__: 1
    __Output Gates__: 1

    :param fields: A list of Update Fields.
    """
    pass

  def clear(self):
    """
    The function `clear()` for Update takes no parameters and clears all state in
    the module.
    """
    pass


class UpdateTTL(Module):
  """
  decreases the IP TTL field by 1
  """


class UrlFilter(Module):
  """
  Filter HTTP connection

  The URLFilter performs TCP reconstruction over a flow and blocks
  connections which mention a banned URL.

  __Input Gates__: 2
  __Output Gates__: 2

  Note that the add() command takes this same argument, and the
  clear() command takes an empty argument.
  """

  def __init__(self, blacklist=None):
    """
    The URLFilter performs TCP reconstruction over a flow and blocks
    connections which mention a banned URL.

    __Input Gates__: 2
    __Output Gates__: 2

    Note that the add() command takes this same argument, and the
    clear() command takes an empty argument.

    :param blacklist: A list of Urls to block.
    """
    pass

  def get_initial_arg(self):
    pass

  def get_runtime_config(self):
    pass

  def set_runtime_config(self, blacklist=None):
    """
    The runtime configuration of a URLFilter is the current
    blacklist.  This means that getting the Arg gets an *empty*
    list: we assume anyone using get_initial_arg is also using
    get_runtime_config.

    :return: The runtime configuration of a URLFilter is the current
    blacklist.  This means that getting the Arg gets an *empty*
    list: we assume anyone using get_initial_arg is also using
    get_runtime_config.
    """
    pass

  def add(self, blacklist=None):
    """
    The URLFilter performs TCP reconstruction over a flow and blocks
    connections which mention a banned URL.

    __Input Gates__: 2
    __Output Gates__: 2

    Note that the add() command takes this same argument, and the
    clear() command takes an empty argument.

    :param blacklist: A list of Urls to block.
    """
    pass

  def clear(self):
    pass


class VLANPop(Module):
  """
  removes 802.1Q/802.11ad VLAN tag

  VLANPop removes the VLAN tag.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self):
    """
    VLANPop removes the VLAN tag.

    __Input Gates__: 1
    __Output Gates__: 1
    """
    pass


class VLANPush(Module):
  """
  adds 802.1Q/802.11ad VLAN tag

  VLANPush appends a VLAN tag with a specified TCI value.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, tci=None):
    """
    VLANPush appends a VLAN tag with a specified TCI value.

    __Input Gates__: 1
    __Output Gates__: 1

    :param tci: The TCI value to insert in the VLAN tag.
    """
    pass

  def set_tci(self, tci=None):
    """
    VLANPush appends a VLAN tag with a specified TCI value.

    __Input Gates__: 1
    __Output Gates__: 1

    :param tci: The TCI value to insert in the VLAN tag.
    """
    pass


class VLANSplit(Module):
  """
  split packets depending on their VID

  Splits packets across output gates according to VLAN id (e.g., id 3 goes out gate 3).

  __Input Gates__: 1
  __Output Gates__: many
  """

  def __init__(self):
    """
    Splits packets across output gates according to VLAN id (e.g., id 3 goes out gate 3).

    __Input Gates__: 1
    __Output Gates__: many
    """
    pass


class VXLANDecap(Module):
  """
  decapsulates the outer Ethetnet/IP/UDP/VXLAN headers

  VXLANDecap module decapsulates a VXLAN header on a packet.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self):
    """
    VXLANDecap module decapsulates a VXLAN header on a packet.

    __Input Gates__: 1
    __Output Gates__: 1
    """
    pass


class VXLANEncap(Module):
  """
  encapsulates packets with UDP/VXLAN headers

  VXLANEncap module wraps a packet in a VXLAN header with a specified destination port.

  __Input Gates__: 1
  __Output Gates__: 1
  """

  def __init__(self, dstport=None):
    """
    VXLANEncap module wraps a packet in a VXLAN header with a specified destination port.

    __Input Gates__: 1
    __Output Gates__: 1

    :param dstport: The destination UDP port
    """
    pass


class WildcardMatch(Module):
  """
  Multi-field classifier with a wildcard match table

  The WildcardMatch module matches over multiple fields in a packet and
  pushes packets that do match out a specified gate, and those that don't out a default
  gate. WildcardMatch is initialized with the fields it should inspect over,
  rules are added via the `add(...)` function.
  An example of WildcardMatch is in [`bess/bessctl/conf/samples/wildcardmatch.bess`](https://github.com/NetSys/bess/blob/master/bessctl/conf/samples/wildcardmatch.bess)

  __Input Gates__: 1
  __Output Gates__: many (configurable)
  """

  def __init__(self, fields=None):
    """
    The WildcardMatch module matches over multiple fields in a packet and
    pushes packets that do match out a specified gate, and those that don't out a default
    gate. WildcardMatch is initialized with the fields it should inspect over,
    rules are added via the `add(...)` function.
    An example of WildcardMatch is in [`bess/bessctl/conf/samples/wildcardmatch.bess`](https://github.com/NetSys/bess/blob/master/bessctl/conf/samples/wildcardmatch.bess)

    __Input Gates__: 1
    __Output Gates__: many (configurable)

    :param fields: A list of WildcardMatch fields.
    """
    pass

  def get_initial_arg(self):
    pass

  def get_runtime_config(self):
    pass

  def set_runtime_config(self, default_gate=None, rules=None):
    """
    WildcardMatchConfig represents the current runtime configuration
    of a WildcardMatch module, as returned by get_runtime_config and
    set by set_runtime_config.

    :return: WildcardMatchConfig represents the current runtime configuration
    of a WildcardMatch module, as returned by get_runtime_config and
    set by set_runtime_config.
    """
    pass

  def add(self, gate=None, priority=None, values=None, masks=None):
    """
    The module WildcardMatch has a command `add(...)` which inserts a new rule
    into the WildcardMatch module. For an example of code using WilcardMatch see
    `bess/bessctl/conf/samples/wildcardmatch.bess`.

    :param gate: Traffic matching this new rule will be sent to this gate.
    :param priority: If a packet matches multiple rules, the rule with higher priority will be applied. If priorities are equal behavior is undefined.
    :param values: The values to check for in each field.
    :param masks: The bitmask for each field -- set `0x0` to ignore the field altogether.
    """
    pass

  def delete(self, values=None, masks=None):
    """
    The module WildcardMatch has a command `delete(...)` which removes a rule -- simply specify the values and masks from the previously inserted rule to remove them.

    :param values: The values being checked for in the rule
    :param masks: The bitmask from the rule.
    """
    pass

  def clear(self):
    """
    The function `clear()` for WildcardMatch takes no parameters, it clears
    all state in the WildcardMatch module (is equivalent to calling delete for all rules)
    """
    pass

  def set_default_gate(self, gate=None):
    """
    For traffic which does not match any rule in the WildcardMatch module,
    the `set_default_gate(...)` function specifies which gate to send this extra traffic to.
    """
    pass


class WorkerSplit(Module):
  """
  send packets to output gate X, the id of current worker

  WorkerSplit splits packets based on the worker calling ProcessBatch(). It has
  two modes.
  1) Packets from worker `x` are mapped to output gate `x`. This is the default
     mode.
  2) When the `worker_gates` field is set, packets from a worker `x` are mapped
     to `worker_gates[x]`.  In this mode, packet batches from workers not
     mapped to an output gate will be dropped.

  Calling the `reset` command with an empty `worker_gates` field will revert
  WorkerSplit to the default mode.

  __Input Gates__: 1
  __Output Gates__: many
  """

  def __init__(self, worker_gates=None):
    """
    WorkerSplit splits packets based on the worker calling ProcessBatch(). It has
    two modes.
    1) Packets from worker `x` are mapped to output gate `x`. This is the default
       mode.
    2) When the `worker_gates` field is set, packets from a worker `x` are mapped
       to `worker_gates[x]`.  In this mode, packet batches from workers not
       mapped to an output gate will be dropped.

    Calling the `reset` command with an empty `worker_gates` field will revert
    WorkerSplit to the default mode.

    __Input Gates__: 1
    __Output Gates__: many

    :param worker_gates: ogate -> worker mask
    """
    pass

  def reset(self, worker_gates=None):
    """
    WorkerSplit splits packets based on the worker calling ProcessBatch(). It has
    two modes.
    1) Packets from worker `x` are mapped to output gate `x`. This is the default
       mode.
    2) When the `worker_gates` field is set, packets from a worker `x` are mapped
       to `worker_gates[x]`.  In this mode, packet batches from workers not
       mapped to an output gate will be dropped.

    Calling the `reset` command with an empty `worker_gates` field will revert
    WorkerSplit to the default mode.

    __Input Gates__: 1
    __Output Gates__: many

    :param worker_gates: ogate -> worker mask
    """
    pass


