with Audio;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package ECS.Scene_Manager is
    type Scene_Type is (Main_Menu, Gameplay, Battle, End_Screen);

   procedure Initialize(Battle_Song_Path : String; Background_Music_Path : String);
   procedure Set_Scene(New_Scene : Scene_Type);
   procedure Update;

private
   Current_Scene : Scene_Type;
   Battle_Song : Unbounded_String;
   Background_Music : Unbounded_String;
end ECS.Scene_Manager ;

