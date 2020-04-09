with Ada.Direct_IO;
with Ada.Streams;

package Byte_IO is new Ada.Direct_IO
  (Element_Type => Ada.Streams.Stream_Element);
