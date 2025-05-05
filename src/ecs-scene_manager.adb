package body ECS.Scene_Manager is

   procedure Initialize(Manager : in out Entity_Manager_T) is
   begin
      Current_Scene := Main_Menu; -- Default scene
      Activate_Scene_Entities(Manager, Current_Scene);
      GameOver := False;
   end Initialize;

   procedure Set_Scene(Manager : in out Entity_Manager_T; New_Scene : Scene_Type) is
   begin
      Audio.Stop_Audio;
      Deactivate_Scene_Entities(Manager, Current_Scene);
      Current_Scene := New_Scene;
      Activate_Scene_Entities(Manager, Current_Scene);
      Play_Scene_Audio;
   end Set_Scene;

   procedure Activate_Scene_Entities(Manager : in out Entity_Manager_T; Scene : Scene_Type) is
   begin
      for Entity_Name of Scene_Infos(Scene).Entities loop
         Put_Line("Activate Scene: This is entity: " & To_String(Entity_Name));
         declare
            Entity : constant Entity_Access := Manager.GetEntity(To_String(Entity_Name));
         begin
            if Entity /= null then
               Entity.Active := True;
            end if;
         end;
      end loop;
   end Activate_Scene_Entities;

   procedure Deactivate_Scene_Entities(Manager : in out Entity_Manager_T; Scene : Scene_Type) is
   begin
      for Entity_Name of Scene_Infos(Scene).Entities loop
         declare
            Entity : constant Entity_Access := Manager.GetEntity(To_String(Entity_Name));
         begin
            if Entity /= null then
               Entity.Active := False;
            end if;
         end;
      end loop;
   end Deactivate_Scene_Entities;

   procedure Play_Scene_Audio is
   begin
      case Current_Scene is
         when Battle =>
            Audio.Play_Audio(To_String(Scene_Infos(Current_Scene).Battle_Song));
         when Gameplay =>
            Audio.Play_Audio(To_String(Scene_Infos(Current_Scene).Background_Music));
         when others => null;
      end case;
   end Play_Scene_Audio;

   procedure Update(Manager : in out Entity_Manager_T) is
   begin
      if not Audio.Is_Playing then
         Play_Scene_Audio;
      end if;

      Update_Game_Logic(Manager);

      if GameOver then
         Set_Scene(Manager, End_Screen);
      end if;
   end Update;

   procedure Update_Game_Logic(Manager : in out Entity_Manager_T) is
   begin
      null; -- Implement game logic updates here
   end Update_Game_Logic;

end ECS.Scene_Manager;