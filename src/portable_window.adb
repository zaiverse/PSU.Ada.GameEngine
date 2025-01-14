with Ada.Text_IO; use Ada.Text_IO;
with Window;
with Render_System;

procedure Portable_Window is
--     My_Window   : Window;
--     My_Renderer : Render_System.Renderer_Impl;

--     Frame_Count : Integer := 0;
--     Max_Frames  : constant Integer := 300; -- ~5 seconds at 60 FPS
--  begin
--     while Frame_Count < Max_Frames loop
--        -- Poll for window events
--        Window.Poll_Events (My_Window);

--        -- Render a frame
--        Render_System.Render_Frame (Renderer => My_Renderer, Window => My_Window);

--        Frame_Count := Frame_Count + 1;
begin
      Put_Line ("Exiting application.");
--     end loop;
end Portable_Window;



--  procedure Portable_Window is
--     -- Instantiate a platform-specific window and renderer
--     My_Window   : Window;
--     My_Renderer : Render_System.Renderer_Impl;

--     -- Define the screen dimensions
--     Screen_Width  : constant Positive := 800;
--     Screen_Height : constant Positive := 600;

--     Is_Running : Boolean := True;
--  begin

--     -- Initialize the renderer
--     Render_System.Initialize (Renderer => My_Renderer, Window => My_Window, Width => Screen_Width, Height => Screen_Height);

--     -- Main rendering loop
--     while Is_Running loop
--        -- Poll for window events
--        Window.Poll_Events (My_Window);

--        -- Render a frame
--        Render_System.Render_Frame (Renderer => My_Renderer, Window => My_Window);

--        -- For testing, limit the loop to a fixed number of frames
--        declare
--           Frame_Count : Integer := 0;
--           Max_Frames  : constant Integer := 300; -- ~5 seconds at 60 FPS
--        begin
--           Frame_Count := Frame_Count + 1;
--           if Frame_Count >= Max_Frames then
--              Is_Running := False;
--           end if;
--        end;
--     end loop;

--     Put_Line ("Exiting application.");
--  end Portable_Window;





--  with Ada.Text_IO; use Ada.Text_IO;
--  with Window;
--  with Render_System;

--  procedure Portable_Window is
--     -- Instantiate a platform-agnostic window and renderer
--     My_Window   : Window.Window_Type'Class;
--     My_Renderer : Render_System.Renderer_Impl;

--     -- Define the screen dimensions
--     Screen_Width  : constant Positive := 800;
--     Screen_Height : constant Positive := 600;

--     Is_Running : Boolean := True;
--  begin
--     -- Initialize the platform-agnostic window
--     Window.Initialize (My_Window, Width => Screen_Width, Height => Screen_Height);

--     -- Initialize the renderer
--     Render_System.Initialize (My_Renderer, My_Window, Width => Screen_Width, Height => Screen_Height);

--     -- Main rendering loop
--     while Is_Running loop
--        -- Poll for window events
--        Window.Poll_Events (My_Window);

--        -- Render a frame
--        Render_System.Render_Frame (My_Renderer, My_Window);

--        -- For testing, limit the loop to a fixed number of frames
--        declare
--           Frame_Count : Integer := 0;
--           Max_Frames  : constant Integer := 300; -- ~5 seconds at 60 FPS
--        begin
--           Frame_Count := Frame_Count + 1;
--           if Frame_Count >= Max_Frames then
--              Is_Running := False;
--           end if;
--        end;
--     end loop;

--     Put_Line ("Exiting application.");
--  end Portable_Window;
