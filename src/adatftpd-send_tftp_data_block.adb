separate (adatftpd)
procedure Send_TFTP_Data_Block
  (From_Server  : Anet.Sockets.Inet.UDPv4_Socket_Type;
   To_Client    : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
   Filename     : String;
   Block_Number : in out Interfaces.Unsigned_16;
   Bytes_Sent   : in out Byte_IO.Count)
is

   use type Byte_IO.Count;
   use type Ada.Streams.Stream_Element_Array;
   use type Interfaces.Unsigned_16;

   File_Handle : Byte_IO.File_Type;

begin

   Byte_IO.Open
     (File => File_Handle, Mode => Byte_IO.In_File, Name => Filename);

   Block_Number :=
     (if Block_Number = Interfaces.Unsigned_16'Last then 1
      else Interfaces.Unsigned_16'Succ (Block_Number));

   declare

      use type Byte_IO.Positive_Count;

      Data_Part : constant Ada.Streams.Stream_Element_Array :=
        Read_TFTP_File_Block
          (From_File => File_Handle, At_Index => (Bytes_Sent + 1));

      Datagram : constant Ada.Streams.Stream_Element_Array :=
        From_U16_To_Bytes (TFTP_DATA) &
        From_U16_To_Bytes (From => Block_Number) & Data_Part;

      Last_Sent : Ada.Streams.Stream_Element_Offset;
      pragma Unreferenced(Last_Sent);

   begin

      From_Server.Send
        (Item     => Datagram,
         Dst_Addr => To_Client.Addr,
         Dst_Port => To_Client.Port);

      Bytes_Sent := Bytes_Sent + Data_Part'Length;

   end;

   Byte_IO.Close (File => File_Handle);

end Send_TFTP_Data_Block;
