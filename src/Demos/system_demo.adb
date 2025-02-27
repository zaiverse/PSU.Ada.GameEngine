-- Ada Libraries
with Ada.Real_Time;           use Ada.Real_Time;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Interfaces.C;
with System;
-- Game Engine ECS modules
with ECS.Component;           use ECS.Component;
with ECS.System.Enemy_Spawner;use ECS.System.Enemy_Spawner;
with ECS.Entity;              use ECS.Entity;
with ECS.Entity_Manager;      use ECS.Entity_Manager;
with ECS.Event;               use ECS.Event;
with ECS.Event_Manager;       use ECS.Event_Manager;
with ECS.System;              use ECS.System;
with ECS.System.Collision;    use ECS.System.Collision;
with ECS.System.Movement;     use ECS.System.Movement;
with ECS.System.Render;       use ECS.System.Render;
with ECS.System.User_Input;   use ECS.System.User_Input;
with ECS.Vec2;                use ECS.Vec2;
-- Game Engine Graphics modules
with Graphics.Color;          use Graphics.Color;
with Graphics.Renderer;       use Graphics.Renderer;
with Graphics.Texture_Loader; use Graphics.Texture_Loader;
-- win32 interface
with Win32;                 use Win32;
with Window;                use Window;
-- User defined modules
with Input_Callbacks; use Input_Callbacks;

procedure System_Demo is
   package IC renames Interfaces.C;
   use IC;
   Width                 : Integer                 := 640;
   Height                : Integer                 := 360;
   Title                 : Unbounded_String        := To_Unbounded_String ("Game Window");
   GameWindow            : Window_Access;
   Buffer                : Win32.Byte_Array_Access := new Win32.Byte_Array (0 .. Width * Height * 4);
   Start_Time, Stop_Time : Time;
   Elapsed_Time          : Duration;
   -- Entity Manager and Entities
   Manager               : Manager_Access           := new Entity_Manager_T' (Entity_List.Empty_Vector,Entity_List.Empty_Vector);
   Event_Mgr             : Platform_Event_Handler_Access := new Platform_Event_Handler;
   Player                : Entity_Access            := Manager.all.AddEntity ("Playr");
   -- Systems
   Mover              : Mover_T          := (Width, Height);
   Collision          : Collision_T      := (Width, Height);
   Render             : Render_T         := (Width, Height, Buffer);
   UserInput          : User_Input_T     := (Player, Event_Mgr, False, True);
   EnemySpawner       : Enemy_Spawn_T;
   -- Player components
   Transform_P : Component_Access := new Transform_T'(Position => (X => 50.0, Y => 150.0), Velocity => (X => 0.0, Y => 0.0), Rotation => 0.0);
   T_P : Transform_T renames Transform_T(Transform_P.all);
   Rigidbody_P : Component_Access := new Rigidbody_T'(Mass => 1.0);
   AABB_P      : Component_Access := new AABB_T'(
      Left => T_P.Position.X, 
      Bottom => T_P.Position.Y, 
      Right => T_P.Position.X, 
      Top => T_P.Position.Y);
   Collision_Params_P : Component_Access := new Collision_Params_T'(
      Collision_Enabled => True,
      Collision_Occurred => False,
      Destroy_On_Collision => True,
      Wall_Collision => False
   );
   C_P         : Collision_Params_T renames Collision_Params_T(Collision_Params_P.all);
   Shape_P     : Component_Access := new Quad_T'(
      Width => 36.0,
      Height => 54.0,
      C => (R=> 255, G => 255, B => 0, A => 255)
   );

   Texture_P : Component_Access;
   bkgrd  : constant String := "C:\ProgramData\Ada\PSU.Ada.GameEngine.Fork\Data\terrace_360.qoi";
   player_texture : constant String := "C:\ProgramData\Ada\PSU.Ada.GameEngine.Fork\Data\char.qoi";
begin
   -- Define input keys
   Register_Input_Callback (16#20#, Space_Key'Access); -- Todo: Add all Key constants to win32.ads file
   Register_Input_Callback (16#57#, W_Key'Access);
   Register_Input_Callback (16#41#, A_Key'Access);
   Register_Input_Callback (16#53#, S_Key'Access);
   Register_Input_Callback (16#44#, D_Key'Access);
   -- Add entity components
   Player.all.Add_Component (Transform_P);
   Player.all.Add_Component (Rigidbody_P);
   Player.all.Add_Component (AABB_P);
   Player.all.Add_Component (Collision_Params_P);
   Player.all.Add_Component (Shape_P);
   -- Used to calculate the frame time
   Start_Time := Clock;
   Stop_Time  := Clock;
   GameWindow := New_Window (IC.int (Width), IC.int (Height), Title);
   declare
      Message           : MSG_Access := new MSG;
      Has_Msg           : Boolean    := True;
      Lp_Result         : LRESULT;
      Texture_Image     : QOI_Image_Data;
      Background_Image  : QOI_Image_Data;
   begin
      -- Load textures
      Background_Image    := Load_QOI (bkgrd);
      Texture_Image       := Load_QOI(player_texture);
      Texture_P           := new Texture_T'(Integer(Texture_Image.Desc.Width),Integer(Texture_Image.Desc.Height),Texture_Image.Data);
      Player.all.Add_Component (Texture_P);
      -- Windows message loop (game loop)
      while Has_Msg loop
         Stop_Time    := Clock;
         Elapsed_Time := To_Duration(Stop_Time - Start_Time);
         Start_Time   := Stop_Time;
         Lp_Result    := Dispatch_Message (Message);
         Has_Msg      := Get_Message (Message, System.Null_Address, 0, 0);
         Manager.all.Update;
         -- Game system calls
         if not Started then
            Draw_Image_To_Buffer (Buffer.all, Background_Image.Data, 0, 0, Integer(Background_Image.Desc.Width), Integer(Background_Image.Desc.Height), Width, Height);
            Draw_String(Buffer.all,255,166,0,0,"PRESS ANY KEY",(255,255,255,255),Width,Height);
            Draw_Buffer (Buffer.all'Address);
            UserInput.Execute (Elapsed_Time, Manager);
         elsif GameOver then
            Draw_Image_To_Buffer (Buffer.all, Background_Image.Data, 0, 0, Integer(Background_Image.Desc.Width), Integer(Background_Image.Desc.Height), Width, Height);
            Draw_String(Buffer.all,280,166,0,0,"GAMEOVER",(255,255,255,255),Width,Height);
            Draw_Buffer (Buffer.all'Address);
         else
            UserInput.Execute (Elapsed_Time, Manager);
            EnemySpawner.Execute (Elapsed_Time, Manager);
            Collision.Execute (Elapsed_Time, Manager);
            Mover.Execute (Elapsed_Time, Manager);
            Draw_Image_To_Buffer (Buffer.all, Background_Image.Data, 0, 0, Integer(Background_Image.Desc.Width), Integer(Background_Image.Desc.Height), Width, Height);
            Draw_String(Buffer.all,1,7,0,0,"SCORE:" & Integer'Image(Score),(255,255,255,255),Width,Height);
            Render.Execute (Elapsed_Time, Manager);
            Draw_Buffer (Buffer.all'Address);
         end if;
      end loop;
  end;
end System_Demo;