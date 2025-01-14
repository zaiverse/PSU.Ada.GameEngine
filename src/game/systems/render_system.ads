with Window;
with Bitmap;

package Render_System is
   type Renderer_Impl is limited record
      Screen_Buffer : Bitmap.Bitmap_Buffer_Access;
      Width         : Positive;
      Height        : Positive;
   end record;

   -- Initializes the renderer with a window, width, and height.
   procedure Initialize
     (Renderer   : in out Renderer_Impl;
      Window_Obj : in out Window.Window_Type'Class;
      Width      : Positive;
      Height     : Positive);

   -- Renders entities
   procedure Render_Entities (Renderer : in out Renderer_Impl);

   -- Render frame to window
   procedure Render_Frame
     (Renderer   : in out Renderer_Impl;
      Window_Obj : in out Window.Window_Type'Class;
      Delta_Time : Float);

end Render_System;
