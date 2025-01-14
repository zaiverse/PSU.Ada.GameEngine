with Window;
with Bitmap;
with Shapes;
with Shape_Component;
with Entities;
with Ada.Text_IO; use Ada.Text_IO;

package body Render_System is

   procedure Initialize
     (Renderer   : in out Renderer_Impl;
      Window_Obj : in out Window.Window_Type'Class;
      Width      : Positive;
      Height     : Positive) is
   begin
      Renderer.Width := Width;
      Renderer.Height := Height;

      -- Allocate and initialize the bitmap buffer
      Renderer.Screen_Buffer := new Bitmap.Bitmap_Buffer;
      Bitmap.Initialize (Renderer.Screen_Buffer.all, Width, Height);

      -- Optional: Perform any window-specific initialization
      Window.Initialize (Window_Obj, Width, Height);
   end Initialize;

   procedure Render_Entities (Renderer : in out Renderer_Impl) is
   begin
      -- Iterate over all active entities
      for Entity of Entities.Active_Entities loop
         -- Check if the entity has a Shape_Component
         declare
            Shape_Comp : constant Shape_Component.Shape_Component_Access :=
              Shape_Component.Get_Component(Entity);
         begin
            if Shape_Comp /= null and then Shape_Comp.Shape /= null then
               -- Delegate drawing to the shape's Draw procedure
               Shapes.Draw (Shape_Comp.Shape.all, Renderer.Screen_Buffer.all);
            else
               Put_Line ("Entity has no shape to render.");
            end if;
         end;
      end loop;
   end Render_Entities;

   procedure Render_Frame
     (Renderer   : in out Renderer_Impl;
      Window_Obj : in out Window.Window_Type'Class;
      Delta_Time : Float) is
   begin
      -- Clear the screen buffer before rendering
      Bitmap.Clear (Renderer.Screen_Buffer.all, 16#000000#); -- Black

      -- Render all entities
      Render_Entities (Renderer);

      -- Present the screen buffer to the window
      Window.Present_Buffer (Window_Obj, Renderer.Screen_Buffer.all);
   end Render_Frame;

end Render_System;






--  with Window;
--  with Bitmap;
--  with Math;

--  package body Render_System is

--     procedure Initialize
--       (Renderer   : in out Renderer_Impl;
--        Window_Obj : in out Window.Window_Type'Class;
--        Width      : Positive;
--        Height     : Positive) is
--     begin
--        Renderer.Width := Width;
--        Renderer.Height := Height;

--        -- Allocate and initialize the bitmap buffer
--        Renderer.Screen_Buffer := new Bitmap.Bitmap_Buffer;
--        Bitmap.Initialize (Renderer.Screen_Buffer.all, Width, Height);

--        -- Optional: Perform any window-specific initialization
--        Window.Initialize (Window_Obj, Width, Height);
--     end Initialize;

--     procedure Render_Shape (Renderer : in out Renderer_Impl) is
--        Points : Bitmap.Point2D_Array(1 .. 3) := (
--           (X => 50.0, Y => 50.0),
--           (X => 100.0, Y => 50.0),
--           (X => 75.0, Y => 100.0)
--        );
--     begin
--        -- Call existing shape rendering logic
--        Bitmap.Draw_Polygon (
--           Buffer        => Renderer.Screen_Buffer.all,
--           Points        => Points,
--           Polygon_Color => 16#FFFFFF# -- White
--        );
--     end Render_Shape;

--     procedure Render_Sprite
--       (Renderer   : in out Renderer_Impl;
--        Delta_Time : Float) is
--     begin
--        -- Add your sprite rendering logic
--        null;
--     end Render_Sprite;

--     procedure Render_Frame
--       (Renderer   : in out Renderer_Impl;
--        Window_Obj : in out Window.Window_Type'Class;
--        Delta_Time : Float) is
--     begin
--        -- Clear the screen buffer before rendering
--        Bitmap.Clear (Renderer.Screen_Buffer.all, 16#000000#); -- Black

--        -- Perform any rendering pipeline logic
--        Render_Shape (Renderer);
--        Render_Sprite (Renderer, Delta_Time);

--        -- Present the screen buffer to the window
--        Window.Present_Buffer (Window_Obj, Renderer.Screen_Buffer.all);
--     end Render_Frame;

--  end Render_System;


