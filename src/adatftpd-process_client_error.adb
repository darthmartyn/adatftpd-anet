separate (adatftpd)
procedure Process_Client_Error
  (From_Client : Anet.Sockets.Inet.IPv4_Sockaddr_Type)
is

   Existing_Session : Connection_Type :=
     (Client                => From_Client,
      Bytes_Sent            => 0,
      File_Size             => 0,
      Expected_Block_Number => 0,
      Filename              => Ada.Strings.Unbounded.To_Unbounded_String (""));

   Existing_Session_Cursor : Cursor :=
     Find (Container => Connections, Item => Existing_Session);

   Existing_Session_Found : constant Boolean :=
     (Existing_Session_Cursor /= No_Element);

begin

   if Existing_Session_Found then
   --  The client reporting the error was mid-transfer.
   --  Remove the client from the Connections store.
   --  No response is made to the client.  Let them time-out on further
   --  transmissions.

      Delete (Container => Connections, Position => Existing_Session_Cursor);

   end if;

end Process_Client_Error;
