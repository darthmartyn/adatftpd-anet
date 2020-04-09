with Anet.Receivers.Datagram;
with Anet.Sockets.Inet;

package Unix_UDP_Receiver is new Anet.Receivers.Datagram
  (Socket_Type  => Anet.Sockets.Inet.UDPv4_Socket_Type,
   Address_Type => Anet.Sockets.Inet.IPv4_Sockaddr_Type,
   Receive      => Anet.Sockets.Inet.Receive);
