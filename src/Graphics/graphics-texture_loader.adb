with Ada.Text_IO;   use Ada.Text_IO;

with QOI; use QOI;
with GNAT.OS_Lib;

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;

package body Graphics.Texture_Loader is

  -- from the example in the QOI package
  -- https://github.com/Fabien-Chouteau/qoi-spark/blob/main/tests/src/tests.adb
  function Load_QOI (Filename : String) return QOI_Image_Data is
    use GNAT.OS_Lib;

    FD  : File_Descriptor;
    Ret : Integer;

    Result : QOI_Image_Data;
  begin

   Put_Line("Loading QOI file: " & Filename);

    FD := GNAT.OS_Lib.Open_Read (Filename, Binary);

    if FD = Invalid_FD then
      Ada.Text_IO.Put_Line (Standard_Error, GNAT.OS_Lib.Errno_Message);
      GNAT.OS_Lib.OS_Exit (1);
    end if;

    declare
      Len     : constant Storage_Count := Storage_Count (File_Length (FD));
      In_Data : constant Storage_Array_Access := new Storage_Array (1 .. Len);
    begin
      Ret := Read (FD, In_Data.all'Address, In_Data.all'Length);

      if Ret /= In_Data'Length then
        Ada.Text_IO.Put_Line (GNAT.OS_Lib.Errno_Message);
        GNAT.OS_Lib.OS_Exit (1);
      end if;

      Close (FD);

      QOI.Get_Desc (In_Data.all, Result.Desc);

      declare
        Out_Len     : constant Storage_Count        :=
         Result.Desc.Width * Result.Desc.Height * Result.Desc.Channels;
        Out_Data    : constant Storage_Array_Access :=
         new Storage_Array (1 .. Out_Len);
        Output_Size : Storage_Count;
      begin
        QOI.Decode
         (Data => In_Data.all, Desc => Result.Desc, Output => Out_Data.all,
          Output_Size => Output_Size);

        Result.Data := Out_Data;

        return Result;

      end;

    end;

  end Load_QOI;


end Graphics.Texture_Loader;