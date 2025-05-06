with Ada.Containers.Vectors;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Audio;
with ECS.Entity; use ECS.Entity;
with Ada.Text_IO; use Ada.Text_IO;
with GameMath; use GameMath;
with ECS.Component; use ECS.Component;

package ECS.Scene_Manager is
   -- Instantiate the generic vector package for Unbounded_String
   package String_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Ada.Strings.Unbounded.Unbounded_String);

   type Scene_Info is record
      Menu_Song        : Ada.Strings.Unbounded.Unbounded_String;
      Background_Music : Ada.Strings.Unbounded.Unbounded_String;
      Battle_Song      : Ada.Strings.Unbounded.Unbounded_String;
      Gameover_Song    : Ada.Strings.Unbounded.Unbounded_String;
      Entities         : String_Vectors.Vector;
   end record;

   type Original_Position_Record is record
      Entity_Id : Id_T;
      Position  : GameMath.Vec2;
   end record;

   package Original_Position_Vectors is new Ada.Containers.Vectors
    (Index_Type => Natural, Element_Type => Original_Position_Record);

   Original_Positions : Original_Position_Vectors.Vector;

   type Scene_Type is (Main_Menu, Gameplay, Battle, End_Screen);

   Scene_Infos : array (Scene_Type) of Scene_Info;

   procedure Initialize(Manager : in out Entity_Manager_T);
   procedure Set_Scene(Manager : in out Entity_Manager_T; New_Scene : Scene_Type);
   procedure Update(Manager : in out Entity_Manager_T);
   procedure Activate_Scene_Entities(Manager : in out Entity_Manager_T; Scene : Scene_Type);
   procedure Deactivate_Scene_Entities(Manager : in out Entity_Manager_T; Scene : Scene_Type);
   procedure Play_Scene_Audio;
   procedure Move_Entity_Out_Of_View(Entity : Entity_Access);
   procedure Restore_Entity_Position(Entity : Entity_Access);
   function Find_Original_Position(Entity_Id : Id_T) return GameMath.Vec2;
   procedure Update_Game_Logic(Manager : in out Entity_Manager_T);
   function Generate_Random_Off_Screen_Position return GameMath.Vec2;

   Current_Scene : Scene_Type;
   GameOver : Boolean;

end ECS.Scene_Manager;