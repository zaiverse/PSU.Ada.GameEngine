with Ada.Text_IO; use Ada.Text_IO;
with Entities;
with Components;
with Position_Component;
with Shape_Component;
with Systems;
with Render_System;
with Bitmap;
with Math;
with Window;
with Shapes;

procedure ECS_Test is
   -- Entity Manager
   Entity_Manager_Instance : Entities.Entity_Manager;

   -- Component Manager
   Component_Manager_Instance : Components.Component_Manager;

   -- System Manager
   System_Manager_Instance : Systems.System_Manager;

   -- Render System
   Render_System_Instance : aliased Render_System.Renderer_Impl;

   -- Window (Assuming a Window package is available)
   Window_Instance : aliased Window.Window_Type'Class;

   -- Entity
   Entity_Instance : Entities.Entity_Id;

   --  -- Position Component
   --  Position_Instance : aliased Position_Component.Position_Component;

   --  -- Shape Component
   --  Shape_Instance : aliased Shape_Component.Shape_Component;

   -- Position Component
   Position_Instance : Position_Component.Position_Component_Access  := new Position_Component.Position_Component;

   -- Shape Component
   Shape_Instance : Position_Component.Position_Component_Access  := new Shape_Component.Shape_Component;

   -- Polygon Shape
   Polygon_Shape_Instance : aliased Shapes.Polygon;

   -- Screen dimensions
   Screen_Width  : constant Positive := 800;
   Screen_Height : constant Positive := 600;

begin
   -- Initialize Managers
   Entities.Initialize(Entity_Manager_Instance);
   Components.Initialize(Component_Manager_Instance);
   Systems.Initialize(System_Manager_Instance);

   -- Create an Entity
   Entity_Instance := Entities.Create_Entity(Entity_Manager_Instance);

   -- Initialize Position Component
   Position_Instance.X := 100.0;
   Position_Instance.Y := 150.0;

   -- Initialize Polygon Shape
   Polygon_Shape_Instance.Points := (1 => (X => 0.0, Y => 0.0),
                            2 => (X => 50.0, Y => 0.0),
                            3 => (X => 25.0, Y => 50.0));
   Polygon_Shape_Instance.Color := 16#FF00FF00#; -- Green color

   -- Assign Shape to Shape Component
   Shape_Instance.Shape := Polygon_Shape_Instance'Access;

   -- Add Components to Entity
   Components.Add_Component(Component_Manager_Instance, Entity_Instance, Position_Instance'Access);
   Components.Add_Component(Component_Manager_Instance, Entity_Instance, Shape_Instance'Access);

   -- Initialize Render System
   Render_System.Initialize(Render_System_Instance, Window_Instance, Screen_Width, Screen_Height);

   -- Register Render System
   Systems.Register_System(System_Manager_Instance, Render_System_Instance'Access);

   -- Invoke the Render System (Assuming a delta time of 0.016 for ~60 FPS)
   Systems.Update_System(System_Manager_Instance, 0.016);

   -- Output a message indicating the test has completed
   Put_Line("ECS test completed successfully.");

exception
   when others =>
      Put_Line("An error occurred during the ECS test.");
end ECS_Test;
