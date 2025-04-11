with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ECS.Entity; use ECS.Entity;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Component; use ECS.Component;
with ECS.Vec2; use ECS.Vec2;
with Graphics.Color; use Graphics.Color;
with ECS.System.Movement;     use ECS.System.Movement;
with ECS.System.Collision;    use ECS.System.Collision;

package ECS.Config_Loader is

   procedure Load_Config(
      Manager      : in out Entity_Manager_T;
      File_Name    : String;
      Window_Width : out Integer;
      Window_Height: out Integer;
      Window_Color : out Graphics.Color.Color
   );

   procedure Parse_Section(
      Section_Name : String;
      Section      : out Unbounded_String;
      Entity       : out Entity_Access;
      Manager      : in out Entity_Manager_T
   );

   procedure Parse_Key_Value(
      Section       : Unbounded_String;
      Key           : String;
      Value         : String;
      Entity        : Entity_Access;
      Window_Width  : in out Integer;
      Window_Height: in out Integer;
      Window_Color  : in out Graphics.Color.Color
   );

   procedure Handle_Window_Properties(
      Key          : String;
      Value        : String;
      Window_Width : in out Integer;
      Window_Height: in out Integer;
      Window_Color : in out Graphics.Color.Color
   );

   procedure Handle_Transform_Properties(
      Entity       : Entity_Access;
      Property_Name: String;
      Value        : String
   );

   procedure Handle_RigidBody_Properties(
      Entity       : Entity_Access;
      Property_Name: String;
      Value        : String
   );

   procedure Handle_Quad_Properties(
      Entity       : Entity_Access;
      Property_Name: String;
      Value        : String
   );

   procedure Handle_Collision_Params_Properties(
      Entity       : Entity_Access;
      Property_Name: String;
      Value        : String
   );

   procedure Initialize_AABB(Entity : Entity_Access);

end ECS.Config_Loader;
