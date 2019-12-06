separate (adatftpd)
procedure Process_Datagram
  (Item : Ada.Streams.Stream_Element_Array;
   Src  : Anet.Sockets.Inet.IPv4_Sockaddr_Type)
is

   use type Ada.Streams.Stream_Element_Offset;

   OpCode : constant Interfaces.Unsigned_16 :=
     From_Bytes_To_U16 (From => Item (Item'First .. Item'First + 1));
   --  The first two bytes of a packet determine it's type which here is
   --  called OpCode.  Pre-condition ensures Item'First+1 is safe.

begin

   if (OpCode = TFTP_ACK or else OpCode = TFTP_RRQ) and then
       Item'Length > 3 and then
       (Item'First <= Item'Last - 2)
   then

      if OpCode = TFTP_ACK then

         Process_ACK
           (To_Server   => Server,
            From_Client => Src,
            Data        => Item (Item'First + 2 .. Item'Last));

      else

         Process_RRQ
           (To_Server   => Server,
            From_Client => Src,
            Data        => Item (Item'First + 2 .. Item'Last));

      end if;

   elsif OpCode = TFTP_ERROR then

      Process_Client_Error
        (From_Client => Src);

   else

      Send_TFTP_Error
        (From_Server => Server,
         To_Client   => Src,
         Error       => ILLEGAL_OPERATION);

   end if;

end Process_Datagram;
