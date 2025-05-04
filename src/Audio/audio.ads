with Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;
with Win32;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;
with System;
with Ada.Strings.Fixed;

package Audio is
   package IC renames Interfaces.C; use IC;
   pragma Linker_Options ("-lwinmm");

   function mciSendStringA (
      lpszCommand : Win32.LPCSTR;
      lpszReturnString : Win32.LPSTR;
      uReturnLength : IC.int;
      hwndCallback : Win32.HANDLE
   ) return IC.int;

   pragma Import (Stdcall, mciSendStringA, "mciSendStringA");

   procedure Play_Audio(File_Path : String);
   procedure Cleanup_Sounds;
   procedure Stop_Audio;
   function Is_Playing return Boolean;

private
   use Ada.Strings.Unbounded;

   package Unbounded_Strings renames Ada.Strings.Unbounded;
   package Alias_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Unbounded_Strings.Unbounded_String);

   Tracked_Aliases   : Alias_Vectors.Vector;
   Audio_ID          : Natural := 0;
   Is_Audio_Playing  : Boolean := False; -- Track if audio is playing
end Audio;
