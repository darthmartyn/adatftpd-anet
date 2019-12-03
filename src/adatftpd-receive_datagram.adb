with Ada.Text_IO;

--  This subprogram is called-back by Anet.Receivers.Datagram.Listen
separate (adatftpd)
procedure Receive_Datagram
  (Item : Ada.Streams.Stream_Element_Array;
   Src  : Anet.Sockets.Inet.IPv4_Sockaddr_Type)
is

   procedure Process_Datagram
     (Item : Ada.Streams.Stream_Element_Array;
      Src  : Anet.Sockets.Inet.IPv4_Sockaddr_Type);

   procedure Process_Datagram
     (Item : Ada.Streams.Stream_Element_Array;
      Src  : Anet.Sockets.Inet.IPv4_Sockaddr_Type)
   is

      First_Byte : constant Ada.Streams.Stream_Element_Offset := Item'First;

      Second_Byte : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Offset'Succ (First_Byte);

      Third_Byte : constant Ada.Streams.Stream_Element_Offset :=
        Ada.Streams.Stream_Element_Offset'Succ (Second_Byte);

      OpCode : constant Interfaces.Unsigned_16 :=
        From_Bytes_To_U16 (From => Item (First_Byte .. Second_Byte));
      --  The first two bytes of a packet determine it's type which here is
      --  called OpCode.

   begin

      case OpCode is

         when TFTP_RRQ =>

            if Item'Length > 3 then

               Process_RRQ
                 (To_Server   => Server,
                  From_Client => Src,
                  Data        => Item (Third_Byte .. Item'Last));

            else

               Send_TFTP_Error
                (From_Server => Server,
                 To_Client   => Src,
                 Error       => ILLEGAL_OPERATION);

            end if;

         when TFTP_ACK =>

            if Item'Length > 3 then

               Process_ACK
                 (To_Server   => Server,
                  From_Client => Src,
                  Data        => Item (Third_Byte .. Item'Last));

            else

               Send_TFTP_Error
                 (From_Server => Server,
                  To_Client   => Src,
                  Error       => ILLEGAL_OPERATION);

            end if;

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

   end Process_Datagram;

begin

   if Item'Length > 1 then

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
