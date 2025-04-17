-- Ada Libraries
with Ada.Tags;                use Ada.Tags;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Real_Time;           use Ada.Real_Time;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with GNAT.Source_Info;        use GNAT.Source_Info;
with Interfaces.C;
with System;
-- Game Engine ECS modules
with ECS.Component;           use ECS.Component;
--with ECS.System.Enemy_Spawner;use ECS.System.Enemy_Spawner;
with ECS.Entity;              use ECS.Entity;
with ECS.Entity_Manager;      use ECS.Entity_Manager;
with ECS.Event;               use ECS.Event;
with ECS.Event_Manager;       use ECS.Event_Manager;
with ECS.System;              use ECS.System;
with ECS.System.Collision;    use ECS.System.Collision;
with ECS.System.Movement;     use ECS.System.Movement;
with ECS.System.Render;       use ECS.System.Render;
with ECS.System.User_Input;   use ECS.System.User_Input;
with GameMath;                use GameMath;
-- Game Engine Graphics modules
with Graphics.Color;          use Graphics.Color;
with Graphics.Renderer;       use Graphics.Renderer;
with Graphics.Texture_Loader; use Graphics.Texture_Loader;
-- win32 interface
with Win32;                 use Win32;
with Window;                use Window;
-- User defined modules
with Input_Callbacks; use Input_Callbacks;
with Test_Logger;           use Test_Logger;




procedure Unit_Tests is
   T : constant Character := ASCII.HT;
   Bright_Blue : constant String := ASCII.ESC & "[94m"; 
   Reset : constant String := ASCII.ESC & "[0m"; 
   T_Tab : constant String := T&T&T&T&T; 
   -- Begin Window instantiation test
   procedure Create_Window is
      W : Window_Access := null;
   begin
      W := Window.New_Window(800, 600, To_Unbounded_String("Test"));
      Log_Test (Source_Location & ": Window Instantiation"&T_Tab, W /= null, "Window successfully instantiated.", "Window is null.");
      Free_Window(W);
      Log_Test (Source_Location & ": Window Memory Freed"&T_Tab, W = null, "True", Boolean'Image(W=null));
   end Create_Window;
   -- End Window instantiation test

-- Begin entity management tests
   procedure Test_AddEntity is
      Manager : Manager_Access := new Entity_Manager_T'(
         Entities  => Entity_List.Empty_Vector,
         ToBeAdded => Entity_List.Empty_Vector
      );
      Player : Entity_Access := Manager.all.AddEntity("Playr");
      Length : Natural;
   begin
      Length := Natural(Manager.ToBeAdded.Length);
      Log_Test(Source_Location & ": ToBeAdded Length"&T_Tab, Length = 1, "1", Natural'Image(Length));
      Log_Test(Source_Location & ": AddEntity Player ID"&T_Tab, Player.Id = "Playr", "Playr", Player.Id);
      Free_Manager(Manager);
      Free_Entity(Player);
      Log_Test(Source_Location & ": Manager Memory Freed"&T_Tab, Manager = null, "True", Boolean'Image(Manager = null));
      Log_Test(Source_Location & ": Entity Memory Freed"&T_Tab, Player = null, "True", Boolean'Image(Player = null));
   end Test_AddEntity;

   procedure Test_Update_AddEntities is
      Manager : Manager_Access := new Entity_Manager_T'(
         Entities  => Entity_List.Empty_Vector,
         ToBeAdded => Entity_List.Empty_Vector
      );
      Player : Entity_Access := Manager.all.AddEntity("Playr");
      E1 : Entity_Access := Manager.all.AddEntity("E0001");
      
      
   begin
      declare
         Length_ToBeAdded : Natural := Natural(Manager.ToBeAdded.Length);
      begin
         Log_Test(Source_Location & ": Pre-Update ToBeAdded"&T_Tab, Length_ToBeAdded = 2, "2", Natural'Image(Length_ToBeAdded));
      end;
      Manager.Update;
      declare
      Length_Entities  : Natural := Natural(Manager.Entities.Length);
      Length_ToBeAdded : Natural := Natural(Manager.ToBeAdded.Length);
      begin
         Log_Test(Source_Location & ": Update Entities"&T_Tab, Length_Entities = 2, "2", Natural'Image(Length_Entities));
         Log_Test(Source_Location & ": Update ToBeAdded"&T_Tab, Length_ToBeAdded = 0, "0", Natural'Image(Natural(Length_ToBeAdded)));
      end;
   end Test_Update_AddEntities;

   procedure Test_Update_RemoveDestroyed is
      Manager : Manager_Access := new Entity_Manager_T'(
         Entities  => Entity_List.Empty_Vector,
         ToBeAdded => Entity_List.Empty_Vector
      );
      Player : Entity_Access := Manager.all.AddEntity("Playr");
      E1 : Entity_Access := Manager.all.AddEntity("E0001");
      E2 : Entity_Access := Manager.all.AddEntity("E0002");

      Length_Before_Remove : Natural;
      Length_After_Remove  : Natural;
   begin
      Manager.Update; -- Ensure all entities are added to `Entities`

      -- Flag one entity as destroyed
      E2.all.Destroyed := True;

      Length_Before_Remove := Natural(Manager.Entities.Length);
      Log_Test(Source_Location & ": Pre-Update Entities"&T_Tab, Length_Before_Remove = 3, "3", Natural'Image(Length_Before_Remove));

      Manager.Update; -- Run update to remove destroyed entities

      Length_After_Remove := Natural(Manager.Entities.Length);
      Log_Test(Source_Location & ": Updated Entity List"&T_Tab, Length_After_Remove = 2, "2", Natural'Image(Length_After_Remove));
      Log_Test(Source_Location & ": Entity E2 Not Found"&T_Tab, not Manager.Entities.Contains(E2), "False", "True");
   end Test_Update_RemoveDestroyed;

   -- End Entity Management Tests

   -- Begin Component Tests 
   procedure Test_Add_Component is
      Entity_Count : constant Positive := 1;
      Entity_Type  : constant Id_T := "E0002";
      Entity : Entity_Access := new Entity_T'(
         Count      => Entity_Count,
         Id         => Entity_Type,
         Destroyed  => False,
         Components => Component_List.Empty_Vector
      );
      Transform_P : Component_Access := new Transform_T'(
         Position => (X => 1.0, Y => 2.0),
         Velocity => (X => 2.0, Y => 0.0),
         Rotation => 0.0
      );
   begin
      Add_Component(Entity.all, Transform_P);
      declare
         Length : Natural := Natural(Entity.Components.Length);
         
      begin
         Log_Test(Source_Location & ": Component Length"&T_Tab, Length = 1, "1", Natural'Image(Length));
         Log_Test(Source_Location & ": Add Component Tag"&T_Tab, Entity.Components(0).all'Tag = Transform_P'Tag, Transform_P'Tag'Image, Entity.Components(0).all'Tag'Image);
         Entity.Free_Components;
         Free_Entity (Entity);
      end;
   end Test_Add_Component;

   procedure Test_Get_Component is
      Entity_Count : constant Positive := 1;
      Entity_Type  : constant Id_T := "E0003";
      Entity : Entity_Access := new Entity_T'(
         Count      => Entity_Count,
         Id         => Entity_Type,
         Destroyed  => False,
         Components => Component_List.Empty_Vector
      );
      Transform_P : Component_Access := new Transform_T'(
         Position => (X => 1.0, Y => 2.0),
         Velocity => (X => 2.0, Y => 0.0),
         Rotation => 0.0
      );
      Rigidbody_P : Component_Access := new Rigidbody_T'(Mass => 10.0);
      Trans : Component_Access;
   begin
      Add_Component(Entity.all, Transform_P);
      Add_Component(Entity.all, Rigidbody_P);

      -- Retrieve the Transform component by its tag
      Trans := Entity.all.Get_Component(Transform_T'Tag);

      Log_Test(Source_Location & ": Get Component Found"&T_Tab, Trans /= null, "Not null", "null");

      if Trans /= null then
         Log_Test(Source_Location & ": Get Component Tag"&T_Tab, Trans'Tag = Transform_P'Tag, 
                  Transform_P'Tag'Image, Trans'Tag'Image);
      end if;
   end Test_Get_Component;

   -- End Component Tests

   -- Begin Vec2 Tests
   procedure Test_Initialization is
      V : Vec2 := New_Vec2(1.0, 2.0);
   begin
      Log_Test(Source_Location & ": Initialization X"&T_Tab, V.X = 1.0, "1.0", Float'Image(V.X));
      Log_Test(Source_Location & ": Initialization Y"&T_Tab, V.Y = 2.0, "2.0", Float'Image(V.Y));
   end Test_Initialization;

   procedure Test_Equality is
      V1, V2 : Vec2 := New_Vec2(1.0, 2.0);
      V3 : Vec2 := New_Vec2(3.0, 4.0);
   begin
      Log_Test(Source_Location & ": Equality Test 1"&T_Tab, V1 = V2, "True", Boolean'Image(V1 = V2));
      Log_Test(Source_Location & ": Equality Test 2"&T_Tab, not(V1 = V3), "False", Boolean'Image(V1 = V3));
   end Test_Equality;

   procedure Test_Arithmetic is
      V1 : Vec2 := New_Vec2(1.0, 2.0);
      V3 : Vec2 := New_Vec2(3.0, 4.0);
      Sum : Vec2 := V1 + V3;
      Diff : Vec2 := V3 - V1;
      Scaled : Vec2 := V1 * 2.0;
      Divided : Vec2 := V3 / 2.0;
   begin
      Log_Test(Source_Location & ": Addition Test"&T_Tab, Sum = New_Vec2(4.0, 6.0), "(4.0, 6.0)", "(" & Float'Image(Sum.X) & ", " & Float'Image(Sum.Y) & ")");
      Log_Test(Source_Location & ": Subtraction Test"&T_Tab, Diff = New_Vec2(2.0, 2.0), "(2.0, 2.0)", "(" & Float'Image(Diff.X) & ", " & Float'Image(Diff.Y) & ")");
      Log_Test(Source_Location & ": Scaling Test"&T_Tab, Scaled = New_Vec2(2.0, 4.0), "(2.0, 4.0)", "(" & Float'Image(Scaled.X) & ", " & Float'Image(Scaled.Y) & ")");
      Log_Test(Source_Location & ": Division Test"&T_Tab, Divided = New_Vec2(1.5, 2.0), "(1.5, 2.0)", "(" & Float'Image(Divided.X) & ", " & Float'Image(Divided.Y) & ")");
   end Test_Arithmetic;

   procedure Test_Procedures is
      V : Vec2 := New_Vec2(1.0, 2.0);
      Addend : Vec2 := New_Vec2(2.0, 3.0);
      Scale_Factor : Float := 2.0;
   begin
      Add(V, Addend);
      Log_Test(Source_Location & ": Add Procedure"&T_Tab, V = New_Vec2(3.0, 5.0), "(3.0, 5.0)", "(" & Float'Image(V.X) & ", " & Float'Image(V.Y) & ")");
      
      Scale(V, Scale_Factor);
      Log_Test(Source_Location & ": Scale Procedure"&T_Tab, V = New_Vec2(6.0, 10.0), "(6.0, 10.0)", "(" & Float'Image(V.X) & ", " & Float'Image(V.Y) & ")");
   end Test_Procedures;

   procedure Test_Normalize is
      V : Vec2 := New_Vec2(3.0, 4.0);
   begin
      Normalize(V);
      Log_Test(Source_Location & ": Normalize Procedure"&T_Tab, V.X = 3.0 / 5.0 and V.Y = 4.0 / 5.0, 
               "(0.6, 0.8)", 
               "(" & Float'Image(V.X) & ", " & Float'Image(V.Y) & ")");
   end Test_Normalize;

   procedure Test_Rotate is
      V : Vec2 := New_Vec2(1.0, 0.0);
   begin
      Rotate(V, 90.0);
      Log_Test(Source_Location & ": Rotate Procedure"&T_Tab, Nearly_Equals(V.X, 0.0) and Nearly_Equals(V.Y,1.0), 
               "(0.0, 1.0)", 
               "(" & Float'Image(V.X) & ", " & Float'Image(V.Y) & ")");
   end Test_Rotate;

   procedure Test_Dot is
      V1 : Vec2 := New_Vec2(1.0, 2.0);
      V2 : Vec2 := New_Vec2(3.0, 4.0);
      Result : Float := Dot(V1, V2);
   begin
      Log_Test(Source_Location & ": Dot Product Test"&T_Tab, Result = 11.0, "11.0", Float'Image(Result));
   end Test_Dot;

   procedure Test_Dist_Squared is
      V1 : Vec2 := New_Vec2(1.0, 2.0);
      V2 : Vec2 := New_Vec2(4.0, 6.0);
      Result : Float := Dist_Squared(V1, V2);
   begin
      Log_Test(Source_Location & ": Dist_Squared Test"&T_Tab, Result = 25.0, "25.0", Float'Image(Result));
   end Test_Dist_Squared;

   -- End Vec2 Tests

   -- Begin Graphics Rendering Tests
   procedure Test_Set_Pixel_Color is
      Buffer : Win32.Byte_Array_Access := new Win32.Byte_Array (0 .. 25 * 25 * 4);
      C: Graphics.Color.Color := (128,128,128,128);
      Expected_C : Graphics.Color.Color;
   begin
      set_pixel_color (Buffer.all, 5, 5, C, 25, 25);
      Expected_C := Get_Pixel_Color(Buffer.all, 5,5,25,25);
      Log_Test(Source_Location & ": Set Pixel Color"&T_Tab, C = Expected_C, "True", "False");
   end;

   procedure Test_Line is
      Buffer : Win32.Byte_Array_Access := new Win32.Byte_Array (0 .. 25 * 25 * 4);
      C : Graphics.Color.Color := (128, 128, 128, 128);
      Expected_C : Graphics.Color.Color;
      X0, Y0, X1, Y1 : Integer; 
   begin
      X0 := 5;
      Y0 := 5;
      X1 := 10;
      Y1 := 10;
      line(X0, Y0, X1, Y1, C, Buffer.all, 25, 25);
      
      -- Verify that expected pixels are set to the correct color
      for I in 0 .. 5 loop
         Expected_C := Get_Pixel_Color(Buffer.all, 5 + I, 5 + I, 25, 25);
         Log_Test(Source_Location & ": Line Pixel" & Integer'Image(5 + I) & "," & Integer'Image(5 + I) & T_Tab,
                  C = Expected_C, "True", "False");
      end loop;
   end Test_Line;

   procedure Test_Regular_Polygon is
      Buffer : Win32.Byte_Array_Access := new Win32.Byte_Array (0 .. 25 * 25 * 4);
      C: Graphics.Color.Color := (128,128,128,128);
      Expected_C : Graphics.Color.Color;
   begin
      Draw_Regular_Polygon (Buffer.all, 3, 5, 10.0, 10.0, C, 25,25);
      Expected_C := Get_Pixel_Color(Buffer.all, 15,10,25,25);
      Log_Test(Source_Location & ": RP Vertex 1 Color"&T_Tab, C = Expected_C, "True", "False");
      Expected_C := Get_Pixel_Color(Buffer.all, 8,14,25,25);
      Log_Test(Source_Location & ": RP Vertex 2 Color"&T_Tab, C = Expected_C, "True", "False");
      Expected_C := Get_Pixel_Color(Buffer.all, 8,6,25,25);
      Log_Test(Source_Location & ": RP Vertex 3 Color"&T_Tab, C = Expected_C, "True", "False");
   end Test_Regular_Polygon;

   procedure Test_Draw_Quad is
      Buffer : Win32.Byte_Array_Access := new Win32.Byte_Array (0 .. 25 * 25 * 4);
      C: Graphics.Color.Color := (128,128,128,128);
      Expected_C : Graphics.Color.Color;
   begin
      Draw_Filled_Quad (Buffer.all,10.0,10.0,5.0,5.0, C, 25,25);
      Expected_C := Get_Pixel_Color(Buffer.all, 10,10,25,25);
      Log_Test(Source_Location & ": DQ Vertex 1 Color"&T_Tab, C = Expected_C, "True", "False");
      Expected_C := Get_Pixel_Color(Buffer.all, 10,15,25,25);
      Log_Test(Source_Location & ": DQ Vertex 1 Color"&T_Tab, C = Expected_C, "True", "False");
      Expected_C := Get_Pixel_Color(Buffer.all, 15,10,25,25);
      Log_Test(Source_Location & ": DQ Vertex 1 Color"&T_Tab, C = Expected_C, "True", "False");
      Expected_C := Get_Pixel_Color(Buffer.all, 15,15,25,25);
      Log_Test(Source_Location & ": DQ Vertex 1 Color"&T_Tab, C = Expected_C, "True", "False");
      Expected_C := Get_Pixel_Color(Buffer.all, 12,13,25,25);
      Log_Test(Source_Location & ": DQ Fill Color"&T_Tab, C = Expected_C, "True", "False");

   end Test_Draw_Quad;

   procedure Test_Draw_Texture is
      Buffer : Win32.Byte_Array_Access := new Win32.Byte_Array (0 .. 25 * 25 * 4);
      Texture_File : constant String := "Data\testimage.qoi";
      Texture_Image : QOI_Image_Data := Load_QOI(Texture_File);
      Expected_C : Graphics.Color.Color;
      Actual_C : Graphics.Color.Color;
   begin
      Draw_Image_To_Buffer(Buffer.all, Texture_Image.Data, 0, 0, 25, 25, 0,0, 25, 25,Natural(Texture_Image.Desc.Width));
      Expected_C := (0,0,0,255);
      Actual_C := Get_Pixel_Color(Buffer.all, 0,0,25,25);
      Log_Test(Source_Location & ": DT TL Pixel Color"&T_Tab, Actual_C = Expected_C, "True", "False");
      Actual_C := Get_Pixel_Color(Buffer.all, 25,0,25,25);
      Log_Test(Source_Location & ": DT TR Pixel Color"&T_Tab, Actual_C = Expected_C, "True", "False");
      Actual_C := Get_Pixel_Color(Buffer.all, 0,25,25,25);
      Log_Test(Source_Location & ": DT BL Pixel Color"&T_Tab, Actual_C = Expected_C, "True", "False");
      Actual_C := Get_Pixel_Color(Buffer.all, 25,25,25,25);
      Log_Test(Source_Location & ": DT BR Pixel Color"&T_Tab, Actual_C = Expected_C, "True", "False");
   end Test_Draw_Texture;

   -- End Graphics Rendering Tests

   -- Begin Game Systems Tests
   
   -- End Game Systems Tests
begin
   Put_Line(Bright_Blue & Center_Text ("TEST WINDOW INSTANTIATION", 100) & Reset);
   Create_Window;

   Put_Line(Bright_Blue & Center_Text ("TEST ADD ENTITY AND UPDATE", 100) & Reset);
   Test_AddEntity;
   Test_Update_AddEntities;

   Put_Line(Bright_Blue & Center_Text ("TEST REMOVE ENTITY AND UPDATE", 100) & Reset);
   Test_Update_RemoveDestroyed;

   Put_Line(Bright_Blue & Center_Text("TEST ADD COMPONENTS", 100) & Reset);
   Test_Add_Component;

   Put_Line(Bright_Blue & Center_Text ("TEST GET COMPONENT", 100) & Reset);
   Test_Get_Component;

   Put_Line(Bright_Blue & Center_Text("TEST VEC2", 100) & Reset);
   Test_Initialization;
   Test_Equality;
   Test_Arithmetic;
   Test_Procedures;
   Test_Normalize;
   Test_Rotate;
   Test_Dot;
   Test_Dist_Squared;

   Put_Line(Bright_Blue & Center_Text ("TEST GRAPHICS RENDERING", 100) & Reset);
   Test_Set_Pixel_Color;
   Test_Line;
   Test_Regular_Polygon;
   Test_Draw_Quad;
   Test_Draw_Texture;
end Unit_Tests;