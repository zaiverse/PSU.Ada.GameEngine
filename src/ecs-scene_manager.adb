with Ada.Numerics.Discrete_Random;
package body ECS.Scene_Manager is

   -- Instantiate the random number generator for Natural numbers
   package Random_Natural is new Ada.Numerics.Discrete_Random(Natural);

   -- Create a random number generator
   RNG : Random_Natural.Generator;

   -- Function to generate a random off-screen position
   function Generate_Random_Off_Screen_Position return GameMath.Vec2 is
      Random_Offset : constant Integer := Random_Natural.Random(RNG) mod 1000;
   begin
      return (X => Float(-1000.0) - Float(Random_Offset), Y => Float(-1000.0) - Float(Random_Offset));
   end Generate_Random_Off_Screen_Position;

   procedure Initialize(Manager : in out Entity_Manager_T) is
   begin
      Random_Natural.Reset(RNG); -- Initialize the random number generator
      Current_Scene := Battle; -- Default scene
      Set_Scene(Manager, Current_Scene);
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
         declare
            Entity : constant Entity_Access := Manager.GetEntity(To_String(Entity_Name));
         begin
            if Entity /= null then
               Entity.Active := True;
               Restore_Entity_Position(Entity);
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
               Move_Entity_Out_Of_View(Entity);
               Put_Line("");
               -- Debug output to verify the position is stored
               Put_Line("Deactivating and storing position for entity " & Entity.Id);
            end if;
         end;
      end loop;
   end Deactivate_Scene_Entities;


   procedure Play_Scene_Audio is
   begin
      case Current_Scene is
         when Main_Menu =>
            Audio.Play_Audio(To_String(Scene_Infos(Current_Scene).Menu_Song));
         when Battle =>
            Audio.Play_Audio(To_String(Scene_Infos(Current_Scene).Battle_Song));
         when Gameplay =>
            Audio.Play_Audio(To_String(Scene_Infos(Current_Scene).Background_Music));
         when End_Screen =>
            Audio.Play_Audio(To_String(Scene_Infos(Current_Scene).Gameover_Song));
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

   procedure Move_Entity_Out_Of_View(Entity : Entity_Access) is
      Transform_Comp : constant Component_Access := Entity.Get_Component(Transform_T'Tag);
      Position_Record : Original_Position_Record;
      Random_Position : GameMath.Vec2;
   begin
      if Transform_Comp /= null then
         -- Store the original position
         Position_Record := (Entity_Id => Entity.Id, Position => Transform_T(Transform_Comp.all).Position);
         Original_Positions.Append(Position_Record);
         Put_Line("Storing original position for entity " & Entity.Id & ": ("
                 & GameMath.Vec2'Image(Transform_T(Transform_Comp.all).Position) & ")");
       -- Generate a random off-screen position for the entity
         Random_Position := Generate_Random_Off_Screen_Position;
         Transform_T(Transform_Comp.all).Position := Random_Position;
         Put_Line("Moved entity " & Entity.Id & " to random off-screen position: ("
                  & GameMath.Vec2'Image(Random_Position) & ")");
      end if;
   end Move_Entity_Out_Of_View;

   function Find_Original_Position(Entity_Id : Id_T) return GameMath.Vec2 is
   begin
      for Position_Record of Original_Positions loop
         if Position_Record.Entity_Id = Entity_Id then
            -- Debug output
            Put_Line("Found original position for entity " & Entity_Id & ": ("
                     & GameMath.Vec2'Image(Position_Record.Position) & ")");
               return Position_Record.Position;
         end if;
      end loop;
      -- Debug output for not found
      Put_Line("Original position not found for entity " & Entity_Id & ". Returning default position (0.0, 0.0).");
      -- Return a default position if not found
      return (X => 0.0, Y => 0.0);
   end Find_Original_Position;

   procedure Restore_Entity_Position(Entity : Entity_Access) is
      Transform_Comp : constant Component_Access := Entity.Get_Component(Transform_T'Tag);
      Original_Pos   : GameMath.Vec2;
   begin
      if Transform_Comp /= null then
         -- Retrieve the original position
         Original_Pos := Find_Original_Position(Entity.Id);
         -- Restore the original position
         Transform_T(Transform_Comp.all).Position := Original_Pos;

         -- Debug output
         Put_Line("Restored position for entity " & Entity.Id & ": ("
                  & GameMath.Vec2'Image(Original_Pos) & ")");
         Put_Line("");
      end if;
   end Restore_Entity_Position;

   procedure Update_Game_Logic(Manager : in out Entity_Manager_T) is
   begin
      null; -- Implement game logic updates here
   end Update_Game_Logic;

end ECS.Scene_Manager;