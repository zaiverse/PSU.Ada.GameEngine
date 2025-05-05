with Ada.Containers.Vectors;
with ECS.Entity_Manager; use ECS.Entity_Manager;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Audio;
with ECS.Entity; use ECS.Entity;
with Ada.Text_IO; use Ada.Text_IO;

package ECS.Scene_Manager is
   -- Instantiate the generic vector package for Unbounded_String
   package String_Vectors is new Ada.Containers.Vectors
      (Index_Type   => Natural,
       Element_Type => Ada.Strings.Unbounded.Unbounded_String);

   type Scene_Info is record
      Background_Music : Ada.Strings.Unbounded.Unbounded_String;
      Battle_Song      : Ada.Strings.Unbounded.Unbounded_String;
      Entities         : String_Vectors.Vector;
   end record;

   type Scene_Type is (Main_Menu, Gameplay, Battle, End_Screen);

   Scene_Infos : array (Scene_Type) of Scene_Info;

   procedure Initialize(Manager : in out Entity_Manager_T);
   procedure Set_Scene(Manager : in out Entity_Manager_T; New_Scene : Scene_Type);
   procedure Update(Manager : in out Entity_Manager_T);
   procedure Activate_Scene_Entities(Manager : in out Entity_Manager_T; Scene : Scene_Type);
   procedure Deactivate_Scene_Entities(Manager : in out Entity_Manager_T; Scene : Scene_Type);
   procedure Play_Scene_Audio;
   procedure Update_Game_Logic(Manager : in out Entity_Manager_T);

private
   Current_Scene : Scene_Type;
   GameOver : Boolean;

end ECS.Scene_Manager;
