with Ada.Text_IO;

separate (adatftpd)
procedure Receive_Datagram
  (Item : Ada.Streams.Stream_Element_Array;
   Src  : Anet.Sockets.Inet.IPv4_Sockaddr_Type)
is

   --  This subprogram is called-back by Anet.Receivers.Datagram.Listen

   OpCode : Interfaces.Unsigned_16 :=
     From_Bytes_To_U16 (From => Item (1 .. 2));
   --  The first two bytes of a packet determine it's type which here is
   --  called OpCode.

begin

   case OpCode is

      when TFTP_RRQ =>

         Process_RRQ
           (To_Server   => Server,
            From_Client => Src,
            Data        => Item (3 .. Item'Last));

      when TFTP_ACK =>

         Process_ACK
           (To_Server   => Server,
            From_Client => Src,
            Data        => Item (3 .. Item'Last));

      when TFTP_ERROR =>

         Process_Client_Error
           (From_Client => Src);

      when TFTP_WRQ =>
      --   Transferring files from the the client to the server
      --   is not supported.

         Send_TFTP_Error
           (From_Server => Server,
            To_Client   => Src,
            Error       => ILLEGAL_OPERATION);

      when others =>

         Send_TFTP_Error
           (From_Server => Server,
            To_Client   => Src,
            Error       => ILLEGAL_OPERATION);

   end case;

end Receive_Datagram;
