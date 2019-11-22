with Ada.Strings.Unbounded;

separate (adatftpd)
procedure Process_ACK
  (To_Server   : Anet.Sockets.Inet.UDPv4_Socket_Type;
   From_Client : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
   Data        : Ada.Streams.Stream_Element_Array)
is

   use Interfaces;
   use Ada.Streams;

   Existing_Session : Connection_Type :=
     (Client                => From_Client,
      Bytes_Sent            => 0,
      Expected_Block_Number => 0,
      Filename              => Ada.Strings.Unbounded.To_Unbounded_String (""));

   Existing_Session_Cursor : constant Cursor :=
      Find (Container => Connections, Item => Existing_Session);

   Existing_Session_Found : constant Boolean :=
     (Existing_Session_Cursor /= No_Element);

   ACK_Block_Number : constant Interfaces.Unsigned_16 :=
     From_Bytes_To_U16 (From => Data (Data'First .. Data'First + 1));

begin

   if Existing_Session_Found then

      Existing_Session :=
        Element (Position => Existing_Session_Cursor);

      if ACK_Block_Number = Existing_Session.Expected_Block_Number then

         Send_TFTP_Data_Block
           (From_Server  => To_Server, To_Client => From_Client,
            Filename     =>
              Ada.Strings.Unbounded.To_String (Existing_Session.Filename),
            Block_Number => Existing_Session.Expected_Block_Number,
            Bytes_Sent   => Existing_Session.Bytes_Sent);

         Replace_Element
           (Container => Connections, Position => Existing_Session_Cursor,
            New_Item  => Existing_Session);

      else

         Send_TFTP_Error
           (From_Server => Server,
            To_Client   => From_Client,
            Error       => UNKNOWN_TRANSFER_ID);

      end if;

   end if;
   --  Ignore datagrams from any client who hasn't already sent
   --  an RRQ request.  Eventually,  they will timeout.

end Process_ACK;
