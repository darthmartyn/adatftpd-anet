--  This subprogram is called-back by Anet.Receivers.Datagram.Listen
separate (adatftpd)
procedure Receive_Datagram
  (Item : Ada.Streams.Stream_Element_Array;
   Src  : Anet.Sockets.Inet.IPv4_Sockaddr_Type)
is
begin

   if Item'Length > 1 then
   --   Satisfy pre-condition of calling Process_Datagram

      Process_Datagram
        (Item => Item,
         Src  => Src);

   else

      Send_TFTP_Error
        (From_Server => Server,
         To_Client   => Src,
         Error       => ILLEGAL_OPERATION);

   end if;

end Receive_Datagram;
