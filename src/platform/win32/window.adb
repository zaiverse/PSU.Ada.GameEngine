with Interfaces.C;
with Interfaces.C.Strings;
with System;

with Win32; use Win32;
with Ada.Text_IO;

package body Window is

   -- Win32 API types and constants
   package IC renames Interfaces.C;

   function Wnd_Proc
     (H_Wnd   : HWND;
      Msg     : IC.Unsigned;
      W_Param : WPARAM;
      L_Param : LPARAM) return LRESULT;

   pragma Convention (C, Wnd_Proc);

   procedure Initialize
     (Window : in out Win32_Window_Type;
      Width  : Positive;
      Height : Positive) is
      WC          : aliased WNDCLASS;
      H_Instance  : HINSTANCE := Get_H_Instance;
      Window_Name : constant String := "AdaWin32Window";
   begin
      -- Register the window class
      WC.Style := 0;
      WC.LpfnWndProc := Wnd_Proc'Access;
      WC.CbClsExtra := 0;
      WC.CbWndExtra := 0;
      WC.HInstance := H_Instance;
      WC.HIcon := Get_Stock_Object (IDI_APPLICATION);
      WC.HCursor := Load_Cursor (null, IDC_ARROW);
      WC.HbrBackground := Get_Stock_Object (WHITE_BRUSH);
      WC.LpszMenuName := null;
      WC.LpszClassName := To_C (Window_Name);

      if Register_Class (WC'Access) = 0 then
         raise Program_Error with "Failed to register window class";
      end if;

      -- Create the window
      Window.Handle := Create_Window
        (WS_OVERLAPPEDWINDOW,
         To_C (Window_Name),
         To_C ("Ada Win32 Window"),
         WS_OVERLAPPEDWINDOW or WS_VISIBLE,
         0,
         0,
         IC.Int (Width),
         IC.Int (Height),
         null,
         null,
         H_Instance,
         null);

      if Window.Handle = null then
         raise Program_Error with "Failed to create window";
      end if;

      Window.Width := Width;
      Window.Height := Height;
   end Initialize;

   procedure Present_Buffer
     (Window : in out Win32_Window_Type;
      Buffer : Bitmap.Bitmap_Buffer) is
      PS         : aliased PAINTSTRUCT;
      Hdc        : HDC;
      MemDC      : HDC;
      BitmapInfo : aliased BITMAPINFO;
      DIBSection : HBITMAP;
      Bits       : System.Address;
   begin
      -- Begin painting
      Hdc := Begin_Paint (Window.Handle, PS'Access);
      MemDC := Create_Compatible_DC (Hdc);

      -- Configure BITMAPINFO for the buffer
      BitmapInfo.bmiHeader.biSize := BITMAPINFOHEADER'Size / 8;
      BitmapInfo.bmiHeader.biWidth := IC.Long (Buffer'Range (1)'Length);
      BitmapInfo.bmiHeader.biHeight := -IC.Long (Buffer'Range (2)'Length); -- Top-down bitmap
      BitmapInfo.bmiHeader.biPlanes := 1;
      BitmapInfo.bmiHeader.biBitCount := 32;
      BitmapInfo.bmiHeader.biCompression := BI_RGB;

      -- Create a DIB section
      DIBSection := Create_DIB_Section
        (MemDC,
         BitmapInfo'Access,
         DIB_RGB_COLORS,
         Bits'Access,
         null,
         0);

      -- Copy bitmap data to DIB section
      for X in Buffer'Range (1) loop
         for Y in Buffer'Range (2) loop
            declare
               Index : constant Integer := (Y - Buffer'First (2)) * Buffer'Length (1) + (X - Buffer'First (1));
               Color : constant Bitmap.Color := Buffer (X, Y);
            begin
               System.Address_To_Access_Values (Bits, Index) := Color;
            end;
         end loop;
      end loop;

      -- Select and blit the DIB section
      Select_Object (MemDC, DIBSection);
      Bit_Blt (Hdc, 0, 0, Window.Width, Window.Height, MemDC, 0, 0, SRCCOPY);

      -- Clean up
      Delete_Object (DIBSection);
      Delete_DC (MemDC);
      End_Paint (Window.Handle, PS'Access);
   end Present_Buffer;

   procedure Poll_Events
     (Window : in out Win32_Window_Type) is
      Msg : aliased MSG;
   begin
      while Peek_Message (Msg'Access, null, 0, 0, PM_REMOVE) /= 0 loop
         Translate_Message (Msg'Access);
         Dispatch_Message (Msg'Access);
      end loop;
   end Poll_Events;

   function Wnd_Proc
     (H_Wnd   : HWND;
      Msg     : IC.Unsigned;
      W_Param : WPARAM;
      L_Param : LPARAM) return LRESULT is
   begin
      case Msg is
         when WM_DESTROY =>
            Post_Quit_Message (0);
            return 0;
         when others =>
            return Def_Window_Proc (H_Wnd, Msg, W_Param, L_Param);
      end case;
   end Wnd_Proc;

end Window;
