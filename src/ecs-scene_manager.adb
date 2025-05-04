package body ECS.Scene_Manager is

   procedure Initialize(Battle_Song_Path : String; Background_Music_Path : String) is
   begin
      Battle_Song := To_Unbounded_String(Battle_Song_Path);
      Background_Music := To_Unbounded_String(Background_Music_Path);
      Current_Scene := Main_Menu; -- Default scene
   end Initialize;

   procedure Set_Scene(New_Scene : Scene_Type) is
   begin
      -- Stop any currently playing audio
      Audio.Stop_Audio;

      Current_Scene := New_Scene;
      case Current_Scene is
         when Battle =>
            Audio.Play_Audio(To_String(Battle_Song));
         when Gameplay =>
            Audio.Play_Audio(To_String(Background_Music));
         when others =>
            null; -- No audio for other scenes
      end case;
   end Set_Scene;

   procedure Update is
   begin
      -- Check if the current audio is playing and replay if necessary
      if Current_Scene = Battle or Current_Scene = Gameplay then
         if not Audio.Is_Playing then
            case Current_Scene is
               when Battle =>
                  Audio.Play_Audio(To_String(Battle_Song));
               when Gameplay =>
                  Audio.Play_Audio(To_String(Background_Music));
               when others =>
                  null;
            end case;
         end if;
      end if;
   end Update;

end ECS.Scene_Manager;