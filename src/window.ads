with Ada.Unchecked_Deallocation;
with System;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Win32; use Win32;
package Window is
   pragma Elaborate_Body;
   
    type Window_T is tagged record
      Height : Interfaces.C.int;
      Width  : Interfaces.C.int; 
      Current_Width : Interfaces.C.int;
      Current_Height : Interfaces.C.int;
      Title  : Unbounded_String;
      Handle : HWND := System.Null_Address;
   end record;
   
   type Window_Access is access all Window_T'Class;



   procedure Draw_Buffer(Buffer : System.Address);
   function New_Window(Width : Interfaces.C.int; Height : Interfaces.C.int; Title : Unbounded_String) return Window_Access;
   procedure Free_Window is new Ada.Unchecked_Deallocation(Window_T'Class, Window_Access);
   
   
end Window;