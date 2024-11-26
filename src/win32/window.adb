with System;
with Interfaces.C;
with Interfaces.C.Strings;
with Ada.Unchecked_Conversion;
with Ada.Text_IO; use Ada.Text_IO;
with ECS.Event_Manager; use ECS.Event_Manager;
with ECS.Event;

with Win32; use Win32;

package body Window is

   package IC renames Interfaces.C; use IC;
   package ICS renames IC.Strings;

   -- Declare the event manager
   Manager : aliased ECS.Event_Manager.Platform_Event_Handler;

   procedure Draw_Buffer (H_Wnd : HWND) is
      PS        : aliased PAINTSTRUCT;
      H_Dc      : HDC;
      Mem_DC    : HDC;
      H_Bitmap  : HBITMAP;
      Old_Bitmap: HBITMAP;
      
      Bmi_Reset : Byte_Array (0 .. BITMAPINFO'Size / 8 - 1) := (others => 0);
      Bmi       : aliased BITMAPINFO with Address => Bmi_Reset'Address;

      Bits      : aliased PVOID;
      Buffer    : Byte_Array := (255, 0, 255, 0,    -- BGRA format
                                 0,   0, 255, 0,
                                 255, 0, 255, 255,
                                 0,   0, 255, 255);
      Success   : Boolean;
      Result    : IC.int;
      Last_Err  : DWORD;
   begin
      H_Dc := Begin_Paint (H_Wnd, PS'Access);
      Mem_DC := Create_Compatible_DC (H_Dc);
      Bmi.bmiHeader.biSize := BITMAPINFOHEADER'Size / 8;
      Bmi.bmiHeader.biWidth := 2;
      Bmi.bmiHeader.biHeight := -2;  -- Top-down
      Bmi.bmiHeader.biPlanes := 1;
      Bmi.bmiHeader.biBitCount := 32;
      Bmi.bmiHeader.biCompression := BI_RGB;
      H_Bitmap := Create_DIB_Section (Mem_DC, Bmi'Access, DIB_RGB_COLORS, Bits'Access, System.Null_Address, 0);
      Last_Err := Get_Last_Error;
      Old_Bitmap := Select_Object (Mem_DC, H_Bitmap);
      Result := Set_DI_Bits (H_Dc, H_Bitmap, 0, 2, Buffer'Address, Bmi'Access, DIB_RGB_COLORS);
      Success := Bit_Blt (H_Dc, 0, 0, 2, 2, Mem_DC, 0, 0, SRCCOPY);
      H_Bitmap := Select_Object (Mem_DC, Old_Bitmap);
      Success := Delete_Object (H_Bitmap);
      Success := Delete_DC (Mem_DC);
      Success := End_Paint (H_Wnd, PS'Access);
   end Draw_Buffer;

   procedure Fill_Black (H_Wnd : HWND) is
      PS : aliased PAINTSTRUCT;
      H_Dc : HDC := Begin_Paint (H_Wnd, PS'Access);
      Res_Fill : IC.int;
      Res_Bool : Boolean;
   begin
      Res_Fill := Fill_Rect (H_Dc, PS.Rc_Paint'Address, HBRUSH (Get_Stock_Object (BLACK_BRUSH)));
      Res_Bool := End_Paint (H_Wnd, PS'Access);
   end;

   -- EDITED 11/26/24 to add WM_KEYDOWN and WM_LBUTTONDOWN
   function Wnd_Proc (H_Wnd   : HWND; 
                     Msg     : IC.unsigned; 
                     W_Param : WPARAM; 
                     L_Param : LPARAM) return LRESULT is
   begin
      case Msg is
         when WM_DESTROY =>
            Post_Quit_Message (0);

         when WM_PAINT =>
            Draw_Buffer (H_Wnd);

         when WM_KEYDOWN =>
         declare
            KeyCode : ECS.Event.Byte := ECS.Event.Byte(W_Param);
            Event   : ECS.Event.Event_T :=
            (Source    => 0,
               EventType => ECS.Event.KeyPress,
               Data      => (KeyCode    => KeyCode,
                           MouseX     => 0,
                           MouseY     => 0,
                           Additional => (others => 0)));
         begin
            Emit_Event(Manager, Event);
         end;

         when WM_LBUTTONDOWN =>
         declare
            L_Param_U : Interfaces.C.unsigned_long := Interfaces.C.unsigned_long(L_Param);
            MouseX    : Integer := Integer(L_Param_U and 16#FFFF#);
            MouseY    : Integer := Integer((L_Param_U / 16#10000#) and 16#FFFF#);
            MouseEvent : ECS.Event.Event_T :=
              (Source    => 0,
               EventType => ECS.Event.MouseClick,
               Data      => (KeyCode    => 0,
                             MouseX     => MouseX,
                             MouseY     => MouseY,
                             Additional => (others => 0)));
         begin
            Emit_Event(Manager, MouseEvent);
         end;


         when others =>
            return Def_Window_Proc(H_Wnd, Msg, W_Param, L_Param);
      end case;
      return 0;
   end Wnd_Proc;



   procedure Window is
    WC             : aliased WNDCLASS;
    H_Instance     : HINSTANCE := Get_H_Instance;
    Res_Atom       : ATOM;
    H_Wnd          : HWND := System.Null_Address;
    Res_Bool       : Boolean;
    use IC;
   begin
        WC.Lp_fn_Wnd_Proc  := Wnd_Proc'Access;
        WC.H_Instance      := H_Instance;
        WC.H_br_Background := HBRUSH (Get_Stock_Object (COLOR_BACKGROUND));
        Res_Atom := Register_Class (WC'Access);
        if Check (Res_Atom) then
            H_Wnd := Create_Window (0,
                                  Lp_Class_Name,
                                  Lp_Window_Name,
                                  WS_OVERLAPPEDWINDOW or WS_VISIBLE,
                                  0, 0, 640, 480,
                                  System.Null_Address,
                                  System.Null_Address,
                                  H_Instance,
                                  System.Null_Address);
        end if;
        Res_Bool := Update_Window (H_Wnd);
        declare
            Message        : MSG_Access := new MSG;
            Has_Msg        : Boolean := True;
            Lp_Result      : LRESULT;
        begin
            while Has_Msg loop
               Lp_Result := Dispatch_Message (Message);
               Has_Msg := Get_Message (Message, System.Null_Address, 0, 0);

               --  -- Process emitted events here
               --  Process_Events(Manager.all);
            end loop;
        end;
   end Window;

   task Background_Task is
   end Background_Task;

   task body Background_Task is
   begin
      Window;
   end Background_Task;
end Window;
