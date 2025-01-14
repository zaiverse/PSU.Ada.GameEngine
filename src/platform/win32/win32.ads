with Window;
with Bitmap;

package Win32 is
   type Win32_Window_Type is new Window.Window_Type with private;

   -- Initialize the window
   procedure Initialize
     (Window : in out Win32_Window_Type;
      Width  : Positive;
      Height : Positive);

   -- Present the bitmap buffer to the window
   procedure Present_Buffer
     (Window : in out Win32_Window_Type;
      Buffer : Bitmap.Bitmap_Buffer);

   -- Poll for and handle window events
   procedure Poll_Events
     (Window : in out Win32_Window_Type);

private
   type HWND is new Interfaces.C.Ptr; -- Win32 window handle

   type Win32_Window_Type is new Window.Window_Type with record
      Handle : HWND := Interfaces.C.Ptr_Null; -- Store the window handle
      Width  : Positive;
      Height : Positive;
   end record;
end Win32;
