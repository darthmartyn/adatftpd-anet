package body adatftpd is

   function Find_String_In_Bytes
     (Data : Ada.Streams.Stream_Element_Array) return String is separate;

   function Convert_Bytes_To_String
     (Bytes : Ada.Streams.Stream_Element_Array) return String is separate;

   function From_Bytes_To_U16
     (From : Ada.Streams.Stream_Element_Array) return Interfaces.Unsigned_16
      is separate;

   function From_U16_To_Bytes
     (From : Interfaces.Unsigned_16) return Ada.Streams.Stream_Element_Array
      is separate;

   function Read_TFTP_File_Block
     (From_File : Byte_IO.File_Type;
      At_Index  : Byte_IO.Positive_Count)
      return Ada.Streams.Stream_Element_Array is separate;

   procedure Send_TFTP_Data_Block
     (From_Server  : Anet.Sockets.Inet.UDPv4_Socket_Type;
      To_Client    : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
      Filename     : String;
      Block_Number : in out Interfaces.Unsigned_16;
      Bytes_Sent   : in out Byte_IO.Count) is separate;

   procedure Send_TFTP_Error
     (From_Server : Anet.Sockets.Inet.UDPv4_Socket_Type;
      To_Client   : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
      Error_Data  : Ada.Streams.Stream_Element_Array) is separate;

   procedure Process_ACK
     (To_Server   : Anet.Sockets.Inet.UDPv4_Socket_Type;
      From_Client : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
      Data        : Ada.Streams.Stream_Element_Array) is separate;

   procedure Process_Client_Error
     (From_Client : Anet.Sockets.Inet.IPv4_Sockaddr_Type) is separate;

   procedure Process_RRQ
     (To_Server   : Anet.Sockets.Inet.UDPv4_Socket_Type;
      From_Client : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
      Data        : Ada.Streams.Stream_Element_Array) is separate;

   procedure Receive_Datagram
     (Item : Ada.Streams.Stream_Element_Array;
      Src  : Anet.Sockets.Inet.IPv4_Sockaddr_Type) is separate;

   procedure Run is
   begin

      Server.Init;

      Server.Bind
        (Port       => 69,
         Reuse_Addr => True);

      Receiver.Listen
        (Callback => Receive_Datagram'Access);

   end Run;

end adatftpd;
