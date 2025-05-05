package body ECS.Config_Loader is

   function Integer_Hash(Key : Integer) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type(Key);
   end Integer_Hash;

   procedure Load_Config(
      Manager          : in out Entity_Manager_T;
      File_Name        : String;
      Window_Width     : out Integer;
      Window_Height    : out Integer;
      Window_Color     : out Graphics.Color.Color;
      User_Input_System: out User_Input_T
   ) is
      Input  : File_Type;
      Line   : String(1 .. 256);
      Last   : Natural;
      Section: Unbounded_String;
      Entity : Entity_Access;
   begin
      Open(File => Input, Mode => In_File, Name => File_Name);
      while not End_Of_File(Input) loop
         Get_Line(File => Input, Item => Line, Last => Last);
         declare
            Trimmed_Line : constant String := Ada.Strings.Fixed.Trim(Line(Line'First .. Last), Ada.Strings.Both);
         begin
            if Trimmed_Line'Length = 0 or else Trimmed_Line(Trimmed_Line'First) = '#' then
               null; -- Skip empty lines and comments
            elsif Trimmed_Line(Trimmed_Line'First) = '[' then
               Parse_Section(Trimmed_Line(Trimmed_Line'First + 1 .. Trimmed_Line'Last - 1), Section, Entity, Manager);
            else
               declare
                  Key_Index : constant Natural := Ada.Strings.Fixed.Index(Trimmed_Line, "=");
                  Key       : constant String := Ada.Strings.Fixed.Trim(Trimmed_Line(Trimmed_Line'First .. Key_Index - 1), Ada.Strings.Both);
                  Value     : constant String := Ada.Strings.Fixed.Trim(Trimmed_Line(Key_Index + 1 .. Trimmed_Line'Last), Ada.Strings.Both);
               begin
                  Parse_Key_Value(Section, Key, Value, Entity, Window_Width, Window_Height, Window_Color, User_Input_System);
               end;
            end if;
         end;

         -- Initialize AABB based on Transform and Quad components, but skip for Window section
         if Section /= "Window" and then Ada.Strings.Fixed.Index(To_String(Section), "Scene.") = 0 then
            Initialize_AABB(Entity);
         end if;
      end loop;
      Close(Input);
   end Load_Config;

   procedure Initialize_Systems(
      Manager          : in out Entity_Manager_T;
      Window_Width     : Integer;
      Window_Height    : Integer;
      Window_Color     : Graphics.Color.Color;
      User_Input_System: in out User_Input_T
   ) is
      Mover      : Mover_T := (Window_Width, Window_Height);
      Collider   : Collision_T := (Window_Width, Window_Height);
      Animator   : Animation_T;
      GameWindow : Window_Access;
      Buffer     : Win32.Byte_Array_Access := new Win32.Byte_Array(0 .. Window_Width * Window_Height * 4);
      Render     : Render_T := (Window_Width, Window_Height, Buffer);
      Start_Time, Stop_Time : Time;
      Elapsed_Time : Time_Span;
      Has_Msg : Boolean := True;
      Message : MSG_Access := new MSG;
      Lp_Result : LRESULT;
      Event_Mgr : Platform_Event_Handler_Access := new Platform_Event_Handler;
      Player_Entity : Entity_Access;
   begin

      Scene_Manager.Set_Scene(Manager, Scene_Manager.Battle);
      -- Ensure all entities and components are initialized
      --User_Input_System := (Manager.GetEntity("Playe"), Event_Mgr, True, False);

      GameWindow := New_Window(IC.int(Window_Width), IC.int(Window_Height), To_Unbounded_String("Game Window"));
      Put_Line("Start Engine");

      -- Initialize Start_Time before the loop
      Start_Time := Clock;

      -- Ensure all entities and components are initialized
      for Entity of Manager.Entities loop
         -- Retrieve entity that has player tag and initialize user input system
         if Entity.Get_Component(Player_Tag_Component_T'Tag) /= null then
            Player_Entity := Entity;
            User_Input_System := (Player_Entity, Event_Mgr, True, False);
         end if;

         declare
            Transform : constant Component_Access := Entity.Get_Component(Transform_T'Tag);
            Quad      : constant Component_Access := Entity.Get_Component(Quad_T'Tag);
            Texture   : constant Component_Access := Entity.Get_Component(Texture_T'Tag);
            Animation : constant Component_Access := Entity.Get_Component(Animation_Component_T'Tag);
         begin
            Put_Line("Entity ID:" & Entity.Id);

            if Transform /= null then
               Put_Line("  Transform: Position("
                         & Float'Image(Transform_T(Transform.all).Position.X) & ", "
                         & Float'Image(Transform_T(Transform.all).Position.Y) & "), Velocity("
                         & Float'Image(Transform_T(Transform.all).Velocity.X) & ", "
                         & Float'Image(Transform_T(Transform.all).Velocity.Y) & "), Rotation("
                         & Float'Image(Transform_T(Transform.all).Rotation) & ")");
            else
               Put_Line("  Transform: Not Initialized");
            end if;

            if Quad /= null then
               Put_Line("  Quad: Width(" & Float'Image(Quad_T(Quad.all).Width) & "), Height("
                         & Float'Image(Quad_T(Quad.all).Height) & "), Color("
                         & Graphics.Color.Color_Int'Image(Quad_T(Quad.all).C.R) & ", "
                         & Graphics.Color.Color_Int'Image(Quad_T(Quad.all).C.G) & ", "
                         & Graphics.Color.Color_Int'Image(Quad_T(Quad.all).C.B) & ", "
                         & Graphics.Color.Color_Int'Image(Quad_T(Quad.all).C.A) & ")");
            else
               Put_Line("  Quad: Not Initialized");
            end if;

            if Texture /= null then
               Put_Line("  Texture: Width(" & Integer'Image(Texture_T(Texture.all).Width) & "), Height("
                         & Integer'Image(Texture_T(Texture.all).Height) & ")");
            else
               Put_Line("  Texture: Not Initialized");
            end if;

            if Animation /= null then
               Put_Line("  Animation: Current State(" & Entity_State'Image(Animation_Component_T(Animation.all).Current) & ")");
            else
               Put_Line("  Animation: Not Initialized");
            end if;
         end;
      end loop;

      -- Main game loop
      while Has_Msg loop
         Stop_Time := Clock;
         Elapsed_Time := Stop_Time - Start_Time;
         Start_Time := Stop_Time;
         Lp_Result := Dispatch_Message(Message);
         Has_Msg := Get_Message(Message, Null_Address, 0, 0);

         -- Update the User Input system
         User_Input_System.Execute(To_Duration(Elapsed_Time), Manager'Access);

         -- Update the Collision system
         Collider.Execute(To_Duration(Elapsed_Time), Manager'Access);

         -- Update the Movement system
         Mover.Execute(To_Duration(Elapsed_Time), Manager'Access);

         Manager.Update;
         Clear_Screen(Buffer.all, Window_Color, Window_Width, Window_Height);

         -- Render the scene
         for Entity of Manager.Entities loop
            if Entity.Active then
               -- Process and render the entity
               Render.Execute(To_Duration(Elapsed_Time), Manager'Access);
            end if;
         end loop;

         -- Update the Animation system
         Animator.Execute(To_Duration(Elapsed_Time), Manager'Access);
         Draw_Buffer(Buffer.all'Address);

         -- Update Scene Manager
         Scene_Manager.Update(Manager);
      end loop;
   end Initialize_Systems;

   procedure Parse_Section(
      Section_Name : String;
      Section      : out Unbounded_String;
      Entity       : out Entity_Access;
      Manager      : in out Entity_Manager_T
   ) is
      Max_Entity_Name_Length : constant := 5; -- Define the maximum length for entity names
      Trimmed_Section_Name   : String(1 .. Max_Entity_Name_Length); -- Ensure it's exactly 5 characters
      Temp_Section_Name      : constant String := Ada.Strings.Fixed.Trim(Section_Name, Ada.Strings.Both);
   begin
      -- Trim and adjust the section name
      if Temp_Section_Name = "Window" or Temp_Section_Name = "Controls" then
         Section := To_Unbounded_String(Temp_Section_Name);
         Entity := null; -- Skip creating an entity for the Window section
         return;
      elsif Ada.Strings.Fixed.Index(Temp_Section_Name, "Scene.") = 1 then
         Section := To_Unbounded_String(Temp_Section_Name);
         Entity := null; -- Skip creating an entity for the Scene section
         return;
      end if;

      if Temp_Section_Name'Length >= Max_Entity_Name_Length then
         Trimmed_Section_Name := Temp_Section_Name(Temp_Section_Name'First .. Temp_Section_Name'First + Max_Entity_Name_Length - 1);
      else
         Trimmed_Section_Name(1 .. Temp_Section_Name'Length) := Temp_Section_Name;
         Trimmed_Section_Name(Temp_Section_Name'Length + 1 .. Max_Entity_Name_Length) := (others => ' ');
      end if;

      Section := To_Unbounded_String(Trimmed_Section_Name);
      Entity := Manager.AddEntity(Trimmed_Section_Name);
      Entity.Active := False;
   end Parse_Section;

   procedure Parse_Key_Value(
      Section        : Unbounded_String;
      Key            : String;
      Value          : String;
      Entity         : Entity_Access;
      Window_Width   : in out Integer;
      Window_Height  : in out Integer;
      Window_Color   : in out Graphics.Color.Color;
      User_Input_System : in out User_Input_T
   ) is
      Dot_Index : constant Natural := Ada.Strings.Fixed.Index(Key, ".");
   begin
      if Section = "Window" then
         Handle_Window_Properties(Key, Value, Window_Width, Window_Height, Window_Color);
      elsif Ada.Strings.Fixed.Index(To_String(Section), "Scene.") = 1 then
         Handle_Scene_Properties(To_String(Section), Key, Value);
      elsif Dot_Index /= 0 then
         declare
            Component_Name : constant String := Ada.Strings.Fixed.Head(Key, Dot_Index - 1);
            Property_Name  : constant String := Ada.Strings.Fixed.Tail(Key, Key'Length - Dot_Index);
            Action : Control_Action;
         begin
            case Component_Name is
               when "Transform" => Handle_Transform_Properties(Entity, Property_Name, Value);
               when "RigidBody" => Handle_RigidBody_Properties(Entity, Property_Name, Value);
               when "Quad" => Handle_Quad_Properties(Entity, Property_Name, Value);
               when "Collision_Params" => Handle_Collision_Params_Properties(Entity, Property_Name, Value);
               when "Texture" => Handle_Entity_Texture_Properties(Entity, Property_Name, Value);
               when "Control" =>
               case Property_Name is
                  when "Move_Up.Velocity.Y" =>
                     Control_Velocities(Move_Up).Y := Float'Value(Value);
                  when "Move_Left.Velocity.X" =>
                     Control_Velocities(Move_Left).X := Float'Value(Value);
                  when "Move_Down.Velocity.Y" =>
                     Control_Velocities(Move_Down).Y := Float'Value(Value);
                  when "Move_Right.Velocity.X" =>
                     Control_Velocities(Move_Right).X := Float'Value(Value);
                  when "Move_Up" | "Move_Left" | "Move_Down" | "Move_Right" =>
                     Register_Keybinding_Config(Entity, Property_Name, Integer'Value(Value), User_Input_System);
                  when others =>
                     null;
               end case;
            when others =>
               null;
         end case;
      end;
   end if;
   end Parse_Key_Value;

   procedure Handle_Window_Properties(
      Key          : String;
      Value        : String;
      Window_Width : in out Integer;
      Window_Height: in out Integer;
      Window_Color : in out Graphics.Color.Color
   ) is
   begin
      case Key is
         when "Width" => Window_Width := Integer'Value(Value);
         when "Height" => Window_Height := Integer'Value(Value);
         when "Color.R" => Window_Color.R := Color_Int'Value(Value);
         when "Color.G" => Window_Color.G := Color_Int'Value(Value);
         when "Color.B" => Window_Color.B := Color_Int'Value(Value);
         when "Color.A" => Window_Color.A := Color_Int'Value(Value);
         when others => null;
      end case;
   end Handle_Window_Properties;

   procedure Handle_Scene_Properties(
      Section_Name : String;
      Key          : String;
      Value        : String
   ) is
      Scene_Name : constant String := Ada.Strings.Fixed.Tail(Section_Name, Section_Name'Length - 6);
      Scene_Type_Value : constant Scene_Type := Scene_Type'Value(Scene_Name);
      Prefixed_Value : constant String := "../" & Value;
   begin
      case Key is
         when "Background_Music" =>
            Scene_Infos(Scene_Type_Value).Background_Music := To_Unbounded_String(Prefixed_Value);
         when "Battle_Song" =>
            Scene_Infos(Scene_Type_Value).Battle_Song := To_Unbounded_String(Prefixed_Value);
         when "Entities" =>
            Scene_Infos(Scene_Type_Value).Entities := Parse_Entity_List(Value);
         when others => null;
      end case;
   end Handle_Scene_Properties;

   function Parse_Entity_List(Entity_Names : String) return String_Vectors.Vector is
      Entity_List  : String_Vectors.Vector;
      Start_Idx    : Natural := Entity_Names'First;
      Comma_Idx    : Natural;
   begin
      loop
         Comma_Idx := Ada.Strings.Fixed.Index(Entity_Names, ",", Start_Idx);
         exit when Comma_Idx = 0;
         Entity_List.Append(To_Unbounded_String(Entity_Names(Start_Idx .. Comma_Idx - 1)));
         Start_Idx := Comma_Idx + 1;
      end loop;
      Entity_List.Append(To_Unbounded_String(Entity_Names(Start_Idx .. Entity_Names'Last)));
      return Entity_List;
   end Parse_Entity_List;

   procedure Move_Up_Callback(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean) is
      Action : constant String := "Move_Up";
      Entity : constant Entity_Access := Entity_Actions.Element(Action);
      Trans : Component_Access;
      Animation : Component_Access;
   begin
      if Entity = null then
         Put_Line("Player entity not found");
         return;
      end if;

      Trans := Entity.Get_Component(Transform_T'Tag);
      Animation := Entity.Get_Component(Animation_Component_T'Tag);

      if Trans /= null and Animation /= null then
         declare
            T renames Transform_T(Trans.all);
            A renames Animation_Component_T(Animation.all);
         begin
            if KeyDown then
               T.Velocity.Y := Control_Velocities(Move_Up).Y;
               A.Current := Walk;
            else
               T.Velocity.Y := 0.0;
               A.Current := Idle;
            end if;
         end;
      end if;
   end Move_Up_Callback;

   procedure Move_Left_Callback(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean) is
      Action : constant String := "Move_Left";
      Entity : constant Entity_Access := Entity_Actions.Element(Action);
      Trans : Component_Access;
      Animation : Component_Access;
   begin
      if Entity = null then
         Put_Line("Player entity not found");
         return;
      end if;

      Trans := Entity.Get_Component(Transform_T'Tag);
      Animation := Entity.Get_Component(Animation_Component_T'Tag);

      if Trans /= null and Animation /= null then
         declare
            T renames Transform_T(Trans.all);
            A renames Animation_Component_T(Animation.all);
         begin
            if KeyDown then
               T.Velocity.X := Control_Velocities(Move_Left).X;
               A.Current := Walk;
            else
               T.Velocity.X := 0.0;
               A.Current := Idle;
            end if;
         end;
      end if;
   end Move_Left_Callback;

   procedure Move_Down_Callback(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean) is
      Action : constant String := "Move_Down";
      Entity : constant Entity_Access := Entity_Actions.Element(Action);
      Trans : Component_Access;
      Animation : Component_Access;
   begin
      if Entity = null then
         Put_Line("Player entity not found");
         return;
      end if;

      Trans := Entity.Get_Component(Transform_T'Tag);
      Animation := Entity.Get_Component(Animation_Component_T'Tag);

      if Trans /= null and Animation /= null then
         declare
            T renames Transform_T(Trans.all);
            A renames Animation_Component_T(Animation.all);
         begin
            if KeyDown then
               T.Velocity.Y := Control_Velocities(Move_Down).Y;
               A.Current := Walk;
            else
               T.Velocity.Y := 0.0;
               A.Current := Idle;
            end if;
         end;
      end if;
   end Move_Down_Callback;

   procedure Move_Right_Callback(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown : Boolean) is
      Action : constant String := "Move_Right";
      Entity : constant Entity_Access := Entity_Actions.Element(Action);
      Trans : Component_Access;
      Animation : Component_Access;
   begin
      if Entity = null then
         Put_Line("Player entity not found");
         return;
      end if;

      Trans := Entity.Get_Component(Transform_T'Tag);
      Animation := Entity.Get_Component(Animation_Component_T'Tag);

      if Trans /= null and Animation /= null then
         declare
            T renames Transform_T(Trans.all);
            A renames Animation_Component_T(Animation.all);
         begin
            if KeyDown then
               T.Velocity.X := Control_Velocities(Move_Right).X;
               A.Current := Walk;
            else
               T.Velocity.X := 0.0;
               A.Current := Idle;
            end if;
         end;
      end if;
   end Move_Right_Callback;

   procedure Register_Keybinding_Config(
      Entity            : Entity_Access;
      Action            : String;
      Keycode           : Integer;
      User_Input_System : in out User_Input_T
   ) is
      Callback_Access : Input_Callback;
      Velocity_Player      : GameMath.Vec2;
      Player_Tag_Comp : Component_Access;
   begin
      -- Check if the entity has the Player_Tag_Component_T
      Player_Tag_Comp := Entity.Get_Component(Player_Tag_Component_T'Tag);
      if Player_Tag_Comp = null then
         -- Add the Player_Tag_Component_T if it doesn't exist
         Player_Tag_Comp := new Player_Tag_Component_T;
         Entity.Add_Component(Player_Tag_Comp);
      end if;

      -- Store the entity reference in the map
      Entity_Actions.Include(Action, Entity);

      -- Register the appropriate callback based on the action
      case Action is
         when "Move_Up" =>
             Velocity_Player := Control_Velocities(Move_Up);
            Callback_Access := Move_Up_Callback'Access;
         when "Move_Left" =>
             Velocity_Player := Control_Velocities(Move_Left);
            Callback_Access := Move_Left_Callback'Access;
         when "Move_Down" =>
             Velocity_Player := Control_Velocities(Move_Down);
            Callback_Access := Move_Down_Callback'Access;
         when "Move_Right" =>
             Velocity_Player := Control_Velocities(Move_Right);
            Callback_Access := Move_Right_Callback'Access;
         when others =>
            return; -- Ignore unknown actions
      end case;

      -- Register the key callback for the specific keycode
      Register_Key_Callback(Keycode, Callback_Access);
   end Register_Keybinding_Config;


   procedure Handle_Entity_Texture_Properties(
      Entity        : Entity_Access;
      Property_Name : String;
      Value         : String
   ) is
      Animation_Comp : Component_Access := Entity.Get_Component(Animation_Component_T'Tag);
      New_Animation_Comp : Component_Access;
      Texture_Image      : QOI_Image_Data;
      Texture_Access_Var : Texture_Access;
      State             : Entity_State;
      Sub_Property_Index : Natural;
      Sub_Property_Name  : String(1 .. Property_Name'Length);
      Full_Path         : Unbounded_String;
      Is_Background     : Boolean := False;
   begin
      Put_Line("Property_Name: " & Property_Name & ", Value: " & Value);
      -- Determine if this is a background texture
      if Property_Name = "Background" then
         Is_Background := True;
      else
         -- Determine the state from the Property_Name
         Sub_Property_Index := Ada.Strings.Fixed.Index(Property_Name, ".");
         if Sub_Property_Index /= 0 then
            Sub_Property_Name(1 .. Sub_Property_Index - 1) := Property_Name(Property_Name'First .. Sub_Property_Index - 1);
            case Sub_Property_Name(1 .. Sub_Property_Index - 1) is
               when "Walk" => State := Walk;
               when "Idle" => State := Idle;
               when others => return; -- Ignore unknown states
            end case;

            Sub_Property_Name(1 .. Property_Name'Length - Sub_Property_Index) := Property_Name(Sub_Property_Index + 1 .. Property_Name'Length);
         end if;
      end if;

      if Is_Background then
         -- Construct the full path
         Full_Path := To_Unbounded_String("..\" & Value);
         Put_Line("Loading background QOI file: " & To_String(Full_Path));

         -- Load the texture
         Texture_Image := Graphics.Texture_Loader.Load_QOI(To_String(Full_Path));
         Texture_Access_Var := new Texture_T'(
            Width => Integer(Texture_Image.Desc.Width),
            Height => Integer(Texture_Image.Desc.Height),
            Data => Texture_Image.Data
         );

         Entity.Add_Component(Component_Access(Texture_Access_Var));

      elsif Sub_Property_Index /= 0 then
         if Animation_Comp = null then
            -- Initialize the Animation_Component_T if it doesn't exist
            New_Animation_Comp := new Animation_Component_T'(
               Animations => (others => null),
               Textures => (others => null),
               Current => Idle
            );
            Entity.Add_Component(New_Animation_Comp);
         else
            New_Animation_Comp := Animation_Comp;
         end if;

         -- Initialize the animation for the state if not already done
         if Animation_Component_T(New_Animation_Comp.all).Animations(State) = null then
            Animation_Component_T(New_Animation_Comp.all).Animations(State) := new Single_Animation_T;
         end if;

         -- Set properties based on Sub_Property_Name
         case Sub_Property_Name(1 .. Property_Name'Length - Sub_Property_Index) is
            when "Texture" =>
               -- Construct the full path
               Full_Path := To_Unbounded_String("..\" & Value);
               Put_Line("Loading QOI file: " & To_String(Full_Path));

               -- Load the texture
               Texture_Image := Graphics.Texture_Loader.Load_QOI(To_String(Full_Path));
               Texture_Access_Var := new Texture_T'(
                  Width => Integer(Texture_Image.Desc.Width),
                  Height => Integer(Texture_Image.Desc.Height),
                  Data => Texture_Image.Data
               );

               Entity.Add_Component(Component_Access(Texture_Access_Var));

               Animation_Component_T(New_Animation_Comp.all).Textures(State) := Texture_Access_Var;

            when "OffsetX" =>
               Animation_Component_T(New_Animation_Comp.all).Animations(State).OffsetX := Integer'Value(Value);

            when "OffsetY" =>
               Animation_Component_T(New_Animation_Comp.all).Animations(State).OffsetY := Integer'Value(Value);

            when "Time" =>
               Animation_Component_T(New_Animation_Comp.all).Animations(State).Time := Duration'Value(Value);

            when "TotalTime" =>
               Animation_Component_T(New_Animation_Comp.all).Animations(State).Total_Time := Duration'Value(Value);

            when "InitialX" =>
               Animation_Component_T(New_Animation_Comp.all).Animations(State).InitialX := Integer'Value(Value);

            when "InitialY" =>
               Animation_Component_T(New_Animation_Comp.all).Animations(State).InitialY := Integer'Value(Value);

            when "CurX" =>
               Animation_Component_T(New_Animation_Comp.all).Animations(State).CurX := Integer'Value(Value);

            when "CurY" =>
               Animation_Component_T(New_Animation_Comp.all).Animations(State).CurY := Integer'Value(Value);

            when "CurFrame" =>
               Animation_Component_T(New_Animation_Comp.all).Animations(State).CurFrame := Integer'Value(Value);

            when "TotalFrames" =>
               Animation_Component_T(New_Animation_Comp.all).Animations(State).TotFrame := Integer'Value(Value);

            when others =>
               null;
         end case;
      end if;
   end Handle_Entity_Texture_Properties;

   procedure Handle_Transform_Properties(
      Entity        : Entity_Access;
      Property_Name : String;
      Value         : String
   ) is
      Transform_Comp : constant Component_Access := Entity.Get_Component(Transform_T'Tag);
      New_Transform_Comp : Component_Access;
   begin
      if Transform_Comp = null then
         New_Transform_Comp := new Transform_T;
         Entity.Add_Component(New_Transform_Comp);
      else
         New_Transform_Comp := Transform_Comp;
      end if;

      case Property_Name is
         when "Position.X" => Transform_T(New_Transform_Comp.all).Position.X := Float'Value(Value);
         when "Position.Y" => Transform_T(New_Transform_Comp.all).Position.Y := Float'Value(Value);
         when "Velocity.X" => Transform_T(New_Transform_Comp.all).Velocity.X := Float'Value(Value);
         when "Velocity.Y" => Transform_T(New_Transform_Comp.all).Velocity.Y := Float'Value(Value);
         when "Rotation" => Transform_T(New_Transform_Comp.all).Rotation := Float'Value(Value);
         when others => null;
      end case;
   end Handle_Transform_Properties;

   procedure Handle_RigidBody_Properties(
      Entity        : Entity_Access;
      Property_Name : String;
      Value         : String
   ) is
      RigidBody_Comp : constant Component_Access := Entity.Get_Component(Rigidbody_T'Tag);
      New_RigidBody_Comp : Component_Access;
   begin
      if RigidBody_Comp = null then
         New_RigidBody_Comp := new Rigidbody_T;
         Entity.Add_Component(New_RigidBody_Comp);
      else
         New_RigidBody_Comp := RigidBody_Comp;
      end if;

      case Property_Name is
         when "Mass" => Rigidbody_T(New_RigidBody_Comp.all).Mass := Float'Value(Value);
         when others => null;
      end case;
   end Handle_RigidBody_Properties;

   procedure Handle_Quad_Properties(
      Entity        : Entity_Access;
      Property_Name : String;
      Value         : String
   ) is
      Quad_Comp : constant Component_Access := Entity.Get_Component(Quad_T'Tag);
      New_Quad_Comp : Component_Access;
   begin
      if Quad_Comp = null then
         New_Quad_Comp := new Quad_T;
         Entity.Add_Component(New_Quad_Comp);
      else
         New_Quad_Comp := Quad_Comp;
      end if;

      case Property_Name is
         when "Width" => Quad_T(New_Quad_Comp.all).Width := Float'Value(Value);
         when "Height" => Quad_T(New_Quad_Comp.all).Height := Float'Value(Value);
         when "C.R" => Quad_T(New_Quad_Comp.all).C.R := Color_Int'Value(Value);
         when "C.G" => Quad_T(New_Quad_Comp.all).C.G := Color_Int'Value(Value);
         when "C.B" => Quad_T(New_Quad_Comp.all).C.B := Color_Int'Value(Value);
         when "C.A" => Quad_T(New_Quad_Comp.all).C.A := Color_Int'Value(Value);
         when others => null;
      end case;
   end Handle_Quad_Properties;

   procedure Handle_Collision_Params_Properties(
      Entity        : Entity_Access;
      Property_Name : String;
      Value         : String
   ) is
      Collision_Params_Comp : constant Component_Access := Entity.Get_Component(Collision_Params_T'Tag);
      New_Collision_Params_Comp : Component_Access;
   begin
      if Collision_Params_Comp = null then
         New_Collision_Params_Comp := new Collision_Params_T;
         Entity.Add_Component(New_Collision_Params_Comp);
      else
         New_Collision_Params_Comp := Collision_Params_Comp;
      end if;

      case Property_Name is
         when "Collision_Enabled" => Collision_Params_T(New_Collision_Params_Comp.all).Collision_Enabled := Boolean'Value(Value);
         when "Destroy_On_Collision" => Collision_Params_T(New_Collision_Params_Comp.all).Destroy_On_Collision := Boolean'Value(Value);
         when "Collision_Occurred" => Collision_Params_T(New_Collision_Params_Comp.all).Collision_Occurred := Boolean'Value(Value);
         when "Wall_Collision" => Collision_Params_T(New_Collision_Params_Comp.all).Wall_Collision := Boolean'Value(Value);
         when others => null;
      end case;
   end Handle_Collision_Params_Properties;

   procedure Initialize_AABB(Entity : Entity_Access) is
      Transform_Comp : constant Component_Access := Entity.Get_Component(Transform_T'Tag);
      Quad_Comp      : constant Component_Access := Entity.Get_Component(Quad_T'Tag);
   begin
      if Transform_Comp /= null and Quad_Comp /= null then
         declare
            Transform : Transform_T renames Transform_T(Transform_Comp.all);
            Quad      : Quad_T renames Quad_T(Quad_Comp.all);
            AABB_Comp : constant Component_Access := Entity.Get_Component(AABB_T'Tag);
            New_AABB_Comp : Component_Access;
         begin
            if AABB_Comp = null then
               New_AABB_Comp := new AABB_T;
               Entity.Add_Component(New_AABB_Comp);
            else
               New_AABB_Comp := AABB_Comp;
            end if;

            AABB_T(New_AABB_Comp.all).Left   := Transform.Position.X;
            AABB_T(New_AABB_Comp.all).Right  := Transform.Position.X + Quad.Width;
            AABB_T(New_AABB_Comp.all).Bottom := Transform.Position.Y;
            AABB_T(New_AABB_Comp.all).Top    := Transform.Position.Y + Quad.Height;
         end;
      end if;
   end Initialize_AABB;

end ECS.Config_Loader;
