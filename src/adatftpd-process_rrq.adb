with Ada.Directories;

separate (adatftpd)
procedure Process_RRQ
  (To_Server   : Anet.Sockets.Inet.UDPv4_Socket_Type;
   From_Client : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
   Data        : Ada.Streams.Stream_Element_Array)
is

   use type Ada.Streams.Stream_Element_Offset;
   use type Ada.Directories.File_Size;

   Filename_String : constant String := Find_String_In_Bytes (Data => Data);

   Start_Of_Mode_String : constant Ada.Streams.Stream_Element_Offset :=
     (3 + Filename_String'Length + 1);

   Mode_String : constant String :=
     Find_String_In_Bytes (Data (Start_Of_Mode_String .. Data'Last));

   File_Exists : constant Boolean := Ada.Directories.Exists (Filename_String);

begin

   if File_Exists and then Mode_String = "octet"
      --  only support binary file transfers

     and then Ada.Directories.Size (Name => Filename_String) > 0
   then

      declare

         Block_Number : Interfaces.Unsigned_16 := 0;
         Bytes_Sent   : Byte_IO.Count          := 0;

      begin

         Send_TFTP_Data_Block
           (From_Server => Server, To_Client => From_Client,
            Filename    => Filename_String, Block_Number => Block_Number,
            Bytes_Sent  => Bytes_Sent);

         declare

            New_Session : constant Connection_Type :=
              (Client                => From_Client,
               Bytes_Sent            => Bytes_Sent,
               Expected_Block_Number => Block_Number,
               Filename              =>
                 Ada.Strings.Unbounded.To_Unbounded_String (Filename_String));

            Previous_Session : constant Cursor :=
              Connections.Find (Item => New_Session);

            Previous_Session_Found : constant Boolean :=
              (Previous_Session /= No_Element);

         begin

            if Previous_Session_Found then

               Connections.Replace_Element
                 (Position  => Previous_Session,
                  New_Item  => New_Session);

            else

               Connections.Append
                 (New_Item  => New_Session);

            end if;

         end;

      end;

   else

      Send_TFTP_Error
        (From_Server => Server,
         To_Client   => From_Client,
         Error_Data  =>
           (if not File_Exists then From_U16_To_Bytes (16#0001#)
            else From_U16_To_Bytes (16#0004#)));

   end if;

end Process_RRQ;
