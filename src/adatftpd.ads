with Ada.Containers.Doubly_Linked_Lists;
with Ada.Direct_IO;
with Ada.Directories;
with Ada.Streams;
with Ada.Strings.Unbounded;
with Anet;
with Anet.Receivers.Datagram;
with Anet.Sockets.Inet;
with Byte_IO;
with Interfaces;
with Unix_UDP_Receiver;

package adatftpd is

   procedure Run;

private

   type Connection_Type is record
      Client                : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
      Bytes_Sent            : Byte_IO.Count;
      File_Size             : Byte_IO.Count;
      Expected_Block_Number : Interfaces.Unsigned_16;
      Filename              : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function "=" (L, R : Connection_Type) return Boolean is
      (Anet.Sockets.Inet."=" (L.Client, R.Client));

   package Connection_Store is new
     Ada.Containers.Doubly_Linked_Lists (Connection_Type, "=");
   --  It is useful to keep track of current transfers because
   --  datagrams can arrive from different clients.  Elements are added
   --  to this list upon receipt of a valid RRQ datagram and removed
   --- upon successful file transmission,  or an error is detected.

   TFTP_RRQ   : constant Interfaces.Unsigned_16 := 16#0001#;
   TFTP_WRQ   : constant Interfaces.Unsigned_16 := 16#0002#;
   TFTP_DATA  : constant Interfaces.Unsigned_16 := 16#0003#;
   TFTP_ACK   : constant Interfaces.Unsigned_16 := 16#0004#;
   TFTP_ERROR : constant Interfaces.Unsigned_16 := 16#0005#;

   TFTP_FILE_NOT_FOUND  : constant Interfaces.Unsigned_16 := 16#0001#;
   TFTP_ILLEGAL_OP      : constant Interfaces.Unsigned_16 := 16#0004#;
   TFTP_UNKNOWN_XFER_ID : constant Interfaces.Unsigned_16 := 16#0004#;
   --  The above constant values comes from the TFTP protocol (revision 2)
   --  https://www.ietf.org/rfc/rfc1350.txt

   type TFTP_Error_Type is
     (FILE_NOT_FOUND, ILLEGAL_OPERATION, UNKNOWN_TRANSFER_ID);

   --  Private use clauses

   use Byte_IO;
   use Interfaces;
   use Connection_Store;
   use Ada.Streams;

   --  Private Varaibles

   Server   : aliased Anet.Sockets.Inet.UDPv4_Socket_Type;

   Receiver : Unix_UDP_Receiver.Receiver_Type (S => Server'Access);

   Connections : Connection_Store.List;

   --  Private Subprograms

   function Find_String_In_Bytes
     (Data : Ada.Streams.Stream_Element_Array) return String;

   function Convert_Bytes_To_String
     (Bytes : Ada.Streams.Stream_Element_Array) return String with
      Post => Convert_Bytes_To_String'Result'Length = Bytes'Length;

   function From_Bytes_To_U16
     (From : Ada.Streams.Stream_Element_Array) return
      Interfaces.Unsigned_16 with
      Pre => From'Length = 2;

   function From_U16_To_Bytes
     (From : Interfaces.Unsigned_16) return
      Ada.Streams.Stream_Element_Array with
      Post => From_U16_To_Bytes'Result'Length = 2;

   function Read_TFTP_File_Block
     (From_File : Byte_IO.File_Type;
      At_Index  : Byte_IO.Positive_Count)
      return Ada.Streams.Stream_Element_Array
      with Post => Read_TFTP_File_Block'Result'Length <= 512;

   procedure Send_TFTP_Data_Block
     (From_Server  : Anet.Sockets.Inet.UDPv4_Socket_Type;
      To_Client    : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
      Filename     : String;

      Block_Number : in out Interfaces.Unsigned_16;
      --  Either incremented by 1, or if 65535 then set to 1

      Bytes_Sent   : in out Byte_IO.Count
      --  Incremenets by either 512 or the amount of the last
      --  block that is less than 512 bytes in size
      ) with Post => (((Block_Number'Old = Interfaces.Unsigned_16'Last
                       and then
                       Block_Number = 1)
                       or else
                       Block_Number =
                         Interfaces.Unsigned_16'Succ (Block_Number'Old))
                       and then
                       (Bytes_Sent in Bytes_Sent'Old .. Bytes_Sent'Old + 512));

   procedure Send_TFTP_Error
     (From_Server : Anet.Sockets.Inet.UDPv4_Socket_Type;
      To_Client   : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
      Error       : TFTP_Error_Type);

   procedure Process_ACK
     (To_Server   : Anet.Sockets.Inet.UDPv4_Socket_Type;
      From_Client : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
      Data        : Ada.Streams.Stream_Element_Array);

   procedure Process_Client_Error
     (From_Client : Anet.Sockets.Inet.IPv4_Sockaddr_Type);

   procedure Process_RRQ
     (To_Server   : Anet.Sockets.Inet.UDPv4_Socket_Type;
      From_Client : Anet.Sockets.Inet.IPv4_Sockaddr_Type;
      Data        : Ada.Streams.Stream_Element_Array);

   procedure Process_Datagram
     (Item : Ada.Streams.Stream_Element_Array;
      Src  : Anet.Sockets.Inet.IPv4_Sockaddr_Type) with
        Pre => Item'Length > 1;

   procedure Receive_Datagram
     (Item : Ada.Streams.Stream_Element_Array;
      Src  : Anet.Sockets.Inet.IPv4_Sockaddr_Type);

end adatftpd;
