separate (adatftpd)
procedure Send_TFTP_Error
  (From_Server : Anet.Sockets.Inet.UDPv4_Socket_Type;
   To_Client   : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
   Error_Data  : Ada.Streams.Stream_Element_Array)
is

   use type Ada.Streams.Stream_Element_Array;

   Null_Terminator : constant Ada.Streams.Stream_Element_Array := (1 => 0);

   Datagram : constant Ada.Streams.Stream_Element_Array :=
     (From_U16_To_Bytes (16#0005#) & Error_Data & Null_Terminator);

   Last_Sent : Ada.Streams.Stream_Element_Offset;
   pragma Unreferenced (Last_Sent);

begin

   null;

end Send_TFTP_Error;
