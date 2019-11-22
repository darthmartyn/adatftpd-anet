separate (adatftpd)
procedure Send_TFTP_Error
  (From_Server : Anet.Sockets.Inet.UDPv4_Socket_Type;
   To_Client   : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
   Error       : TFTP_Error_Type)
is

   use type Ada.Streams.Stream_Element_Array;

   Null_Terminator : constant Ada.Streams.Stream_Element_Array := (1 => 0);

   Datagram : constant Ada.Streams.Stream_Element_Array :=
     ((case Error is
       when FILE_NOT_FOUND      => From_U16_To_Bytes (TFTP_FILE_NOT_FOUND),
       when ILLEGAL_OPERATION   => From_U16_To_Bytes (TFTP_ILLEGAL_OP),
       when UNKNOWN_TRANSFER_ID => From_U16_To_Bytes (TFTP_UNKNOWN_XFER_ID))
      & Null_Terminator);

begin

   From_Server.Send
     (Item     => Datagram,
      Dst_Addr => To_Client.Addr,
      Dst_Port => To_Client.Port);

end Send_TFTP_Error;
