with System;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Win32; use Win32;
package Window is
   pragma Elaborate_Body;
   type Fixed_String is array (1 .. 100) of Character;
   
   type Window_T is tagged private;
   
   type Window_Access is access all Window_T'Class;

   Current_Width : Interfaces.C.int;
   Current_Height : Interfaces.C.int;

   procedure Draw_Buffer(W_Instance: in out Window_T; Buffer : System.Address);
   function New_Window(Width : Interfaces.C.int; Height : Interfaces.C.int; Title : Unbounded_String) return Window_Access;


private
   type Window_T is tagged record
      Height : Interfaces.C.int;
      Width  : Interfaces.C.int; 
      Title  : Unbounded_String;
      Handle : HWND := System.Null_Address;
   end record;
end Window;