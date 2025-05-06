with System.Storage_Elements; use System.Storage_Elements;
with System; use System;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ECS.Entity; use ECS.Entity;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Component; use ECS.Component;
with GameMath; use GameMath;
with Graphics.Color; use Graphics.Color;
with ECS.System.Movement; use ECS.System.Movement;
with ECS.System.Collision; use ECS.System.Collision;
with ECS.System.Animation; use ECS.System.Animation;
with Graphics.Texture_Loader; use Graphics.Texture_Loader;
with ECS.System.User_Input; use ECS.System.User_Input;
with ECS.Event_Manager; use ECS.Event_Manager;
with Window; use Window;
with Win32; use Win32;
with Graphics.Renderer; use Graphics.Renderer;
with ecs.System.Render; use ecs.System.Render;
with Ada.Real_Time; use Ada.Real_Time;
with ECS.Event; use ECS.Event;

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with ECS.Scene_Manager; use ECS.Scene_Manager;
with Ada.Containers.Vectors;

with Audio;


package ECS.Config_Loader is

   procedure Load_Config(
      Manager          : in out Entity_Manager_T;
      File_Name        : String;
      Window_Width     : out Integer;
      Window_Height    : out Integer;
      Window_Color     : out Graphics.Color.Color;
      User_Input_System: out User_Input_T
   );

   procedure Initialize_Systems(
      Manager          : in out Entity_Manager_T;
      Window_Width     : Integer;
      Window_Height    : Integer;
      Window_Color     : Graphics.Color.Color;
      User_Input_System: in out User_Input_T
   );
   
private
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
      Window_Height : in out Integer;
      Window_Color  : in out Graphics.Color.Color;
      User_Input_System : in out User_Input_T
   );

   procedure Handle_Window_Properties(
      Key          : String;
      Value        : String;
      Window_Width : in out Integer;
      Window_Height: in out Integer;
      Window_Color : in out Graphics.Color.Color
   );

   --  procedure Handle_Audio_Properties(
   --     Key : String;
   --     Value : String);
   procedure Handle_Scene_Properties(
      Section_Name : String;
      Key          : String;
      Value        : String
   );
   function Parse_Entity_List(Entity_Names : String) return String_Vectors.Vector;

   procedure Handle_Entity_Texture_Properties(
      Entity        : Entity_Access;
      Property_Name : String;
      Value         : String
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

   -- Movmement related maps and procedures
   package Entity_Action_Map is new Ada.Containers.Indefinite_Hashed_Maps
      (Key_Type        => String,
      Element_Type    => Entity_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   Entity_Actions : Entity_Action_Map.Map;
   procedure Initialize_AABB(Entity : Entity_Access);

   type Control_Action is (Move_Up, Move_Left, Move_Down, Move_Right);
   type Control_Velocity_Map is array (Control_Action) of GameMath.Vec2;

   Control_Velocities : Control_Velocity_Map := (others => (X => 0.0, Y => 0.0));
   procedure Move_Up_Callback(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
   procedure Move_Left_Callback(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
   procedure Move_Down_Callback(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
   procedure Move_Right_Callback(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean);
   
   procedure Register_Keybinding_Config(
      Entity : Entity_Access; 
      Action : String; 
      Keycode : Integer; 
      User_Input_System : in out User_Input_T);

end ECS.Config_Loader;
