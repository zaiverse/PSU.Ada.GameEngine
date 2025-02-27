package body ECS.Config_Loader is

   procedure Load_Config(Manager : in out Entity_Manager_T; File_Name : String) is
      Input  : File_Type;
      Line   : String(1 .. 256);
      Last   : Natural;
      Section: Unbounded_String;
      Entity : Entity_Access;
   begin
      Put_Line("Opening file: " & File_Name);
      Open(File => Input, Mode => In_File, Name => File_Name);
      while not End_Of_File(Input) loop
         Get_Line(File => Input, Item => Line, Last => Last);
         declare
            Trimmed_Line : constant String := Ada.Strings.Fixed.Trim(Line(Line'First .. Last), Ada.Strings.Both);
         begin
            Put_Line("Read line: """ & Trimmed_Line & """");
            if Trimmed_Line'Length = 0 or else Trimmed_Line(Trimmed_Line'First) = '#' then
               -- Skip empty lines and comments
               Put_Line("Skipping empty or comment line");
               null;
            elsif Trimmed_Line(Trimmed_Line'First) = '[' then
               -- New section
               declare
                  Section_Name : constant String := Ada.Strings.Fixed.Trim(Trimmed_Line(Trimmed_Line'First + 1 .. Trimmed_Line'Last - 1), Ada.Strings.Both);
                  Adjusted_Section_Name : String(1 .. 5); -- Ensure it's exactly 5 characters
               begin
                  Put_Line("Found section: " & Trimmed_Line);
                  Put_Line("Trimmed_Line'Length: " & Integer'Image(Trimmed_Line'Length));
                  Put_Line("Section Start Index: 2");
                  Put_Line("Section End Index: " & Natural'Image(Trimmed_Line'Last - 1));
                  Put_Line("Extracted Section Name: " & Section_Name);

                  if Section_Name'Length >= 5 then
                     Adjusted_Section_Name := Section_Name(Section_Name'First .. Section_Name'First + 4);
                  else
                     Adjusted_Section_Name := Section_Name & (1 .. 5 - Section_Name'Length => ' ');
                  end if;

                  Section := To_Unbounded_String(Adjusted_Section_Name);
                  Entity := Manager.AddEntity(To_String(Section));
                  Put_Line("Created new entity with ID: " & Entity.Id);
               end;
            else
               -- Parse key-value pair
               declare
                  Key_Value : constant String := Trimmed_Line;
                  Key_Index : constant Natural := Ada.Strings.Fixed.Index(Key_Value, "=");
                  Key       : constant String := Ada.Strings.Fixed.Trim(Key_Value(Key_Value'First .. Key_Index - 1), Ada.Strings.Both);
                  Value     : constant String := Ada.Strings.Fixed.Trim(Key_Value(Key_Index + 1 .. Key_Value'Last), Ada.Strings.Both);
                  Dot_Index : constant Natural := Ada.Strings.Fixed.Index(Key, ".");
               begin
                  Put_Line("Parsing Key: " & Key);
                  Put_Line("Parsing Value: " & Value);

                  if Key'Length > 0 and Dot_Index /= 0 then
                     declare
                        Component_Name : constant String := Ada.Strings.Fixed.Head(Key, Dot_Index - 1);
                        Property_Name  : constant String := Ada.Strings.Fixed.Tail(Key, Key'Length - Dot_Index);
                     begin
                        Put_Line("Component Name: " & Component_Name);
                        Put_Line("Property Name: " & Property_Name);

                        if Component_Name = "Transform" then
                           declare
                              Transform_Comp : constant Component_Access := Entity.Get_Component(Transform_T'Tag);
                              New_Transform_Comp : Component_Access;
                           begin
                              if Transform_Comp = null then
                                 New_Transform_Comp := new Transform_T;
                                 Entity.Add_Component(New_Transform_Comp);
                                 Put_Line("Created new Transform component");
                              else
                                 New_Transform_Comp := Transform_Comp;
                                 Put_Line("Using existing Transform component");
                              end if;

                              case Property_Name is
                                 when "Position.X" =>
                                    Transform_T(New_Transform_Comp.all).Position.X := Float'Value(Value);
                                    Put_Line("Set Transform Position.X to " & Float'Image(Transform_T(New_Transform_Comp.all).Position.X));
                                 when "Position.Y" =>
                                    Transform_T(New_Transform_Comp.all).Position.Y := Float'Value(Value);
                                    Put_Line("Set Transform Position.Y to " & Float'Image(Transform_T(New_Transform_Comp.all).Position.Y));
                                 when "Velocity.X" =>
                                    Transform_T(New_Transform_Comp.all).Velocity.X := Float'Value(Value);
                                    Put_Line("Set Transform Velocity.X to " & Float'Image(Transform_T(New_Transform_Comp.all).Velocity.X));
                                 when "Velocity.Y" =>
                                    Transform_T(New_Transform_Comp.all).Velocity.Y := Float'Value(Value);
                                    Put_Line("Set Transform Velocity.Y to " & Float'Image(Transform_T(New_Transform_Comp.all).Velocity.Y));
                                 when "Rotation" =>
                                    Transform_T(New_Transform_Comp.all).Rotation := Float'Value(Value);
                                    Put_Line("Set Transform Rotation to " & Float'Image(Transform_T(New_Transform_Comp.all).Rotation));
                                 when others =>
                                    Put_Line("Unknown property for Transform component: " & Property_Name);
                              end case;
                           end;
                        elsif Component_Name = "RigidBody" then
                           declare
                              RigidBody_Comp : constant Component_Access := Entity.Get_Component(Rigidbody_T'Tag);
                              New_RigidBody_Comp : Component_Access;
                           begin
                              if RigidBody_Comp = null then
                                 New_RigidBody_Comp := new Rigidbody_T;
                                 Entity.Add_Component(New_RigidBody_Comp);
                                 Put_Line("Created new RigidBody component");
                              else
                                 New_RigidBody_Comp := RigidBody_Comp;
                                 Put_Line("Using existing RigidBody component");
                              end if;

                              case Property_Name is
                                 when "Mass" =>
                                    Rigidbody_T(New_RigidBody_Comp.all).Mass := Float'Value(Value);
                                    Put_Line("Set RigidBody Mass to " & Float'Image(Rigidbody_T(New_RigidBody_Comp.all).Mass));
                                 when others =>
                                    Put_Line("Unknown property for RigidBody component: " & Property_Name);
                              end case;
                           end;
                        elsif Component_Name = "AABB" then
                           declare
                              AABB_Comp : constant Component_Access := Entity.Get_Component(AABB_T'Tag);
                              New_AABB_Comp : Component_Access;
                           begin
                              if AABB_Comp = null then
                                 New_AABB_Comp := new AABB_T;
                                 Entity.Add_Component(New_AABB_Comp);
                                 Put_Line("Created new AABB component");
                              else
                                 New_AABB_Comp := AABB_Comp;
                                 Put_Line("Using existing AABB component");
                              end if;

                              case Property_Name is
                                 when "Left" =>
                                    AABB_T(New_AABB_Comp.all).Left := Float'Value(Value);
                                    Put_Line("Set AABB Left to " & Float'Image(AABB_T(New_AABB_Comp.all).Left));
                                 when "Bottom" =>
                                    AABB_T(New_AABB_Comp.all).Bottom := Float'Value(Value);
                                    Put_Line("Set AABB Bottom to " & Float'Image(AABB_T(New_AABB_Comp.all).Bottom));
                                 when "Right" =>
                                    AABB_T(New_AABB_Comp.all).Right := Float'Value(Value);
                                    Put_Line("Set AABB Right to " & Float'Image(AABB_T(New_AABB_Comp.all).Right));
                                 when "Top" =>
                                    AABB_T(New_AABB_Comp.all).Top := Float'Value(Value);
                                    Put_Line("Set AABB Top to " & Float'Image(AABB_T(New_AABB_Comp.all).Top));
                                 when others =>
                                    Put_Line("Unknown property for AABB component: " & Property_Name);
                              end case;
                           end;
                        elsif Component_Name = "Collision_Params" then
                           declare
                              Collision_Params_Comp : constant Component_Access := Entity.Get_Component(Collision_Params_T'Tag);
                              New_Collision_Params_Comp : Component_Access;
                           begin
                              if Collision_Params_Comp = null then
                                 New_Collision_Params_Comp := new Collision_Params_T;
                                 Entity.Add_Component(New_Collision_Params_Comp);
                                 Put_Line("Created new Collision_Params component");
                              else
                                 New_Collision_Params_Comp := Collision_Params_Comp;
                                 Put_Line("Using existing Collision_Params component");
                              end if;

                              case Property_Name is
                                 when "Collision_Enabled" =>
                                    Collision_Params_T(New_Collision_Params_Comp.all).Collision_Enabled := Boolean'Value(Value);
                                    Put_Line("Set Collision_Params Collision_Enabled to " & Boolean'Image(Collision_Params_T(New_Collision_Params_Comp.all).Collision_Enabled));
                                 when "Destroy_On_Collision" =>
                                    Collision_Params_T(New_Collision_Params_Comp.all).Destroy_On_Collision := Boolean'Value(Value);
                                    Put_Line("Set Collision_Params Destroy_On_Collision to " & Boolean'Image(Collision_Params_T(New_Collision_Params_Comp.all).Destroy_On_Collision));
                                 when others =>
                                    Put_Line("Unknown property for Collision_Params component: " & Property_Name);
                              end case;
                           end;
                        else
                           Put_Line("Unknown component: " & Component_Name);
                        end if;
                     end;
                  else
                     Put_Line("Invalid key format: " & Key);
                  end if;
               end;
            end if;
         end;
      end loop;
      Put_Line("Closing file");
      Close(Input);
   end Load_Config;

end ECS.Config_Loader;









--  package body ECS.Config_Loader is

--     procedure Load_Config(Manager : in out Entity_Manager_T; File_Name : String) is
--        Input  : File_Type;
--        Line   : String(1 .. 256);
--        Last   : Natural;
--        Section: Unbounded_String;
--        Entity : Entity_Access;
--     begin
--        Open(File => Input, Mode => In_File, Name => File_Name);
--        while not End_Of_File(Input) loop
--           Get_Line(File => Input, Item => Line, Last => Last);
--           declare
--              Trimmed_Line : constant String := Trim(Line(1 .. Last), Ada.Strings.Both);
--           begin
--              if Trimmed_Line'Length = 0 or else Trimmed_Line(Trimmed_Line'First) = '#' then
--                 -- Skip empty lines and comments
--                 null;
--              elsif Trimmed_Line(Trimmed_Line'First) = '[' then
--                 -- New section
--                 Put_Line("Found section: " & Trimmed_Line);
--                 Put_Line("Trimmed_Line'Length: " & Integer'Image(Trimmed_Line'Length));

--                 -- Debugging the extraction of the section name
--                 declare
--                    Start_Index : constant Natural := 2;
--                    End_Index   : constant Natural := Trimmed_Line'Last - 1;
--                    Section_Name : constant String := Trim(Trimmed_Line(Start_Index .. End_Index), Ada.Strings.Both);
--                    Adjusted_Section_Name : String(1 .. 5); -- Ensure it's exactly 5 characters
--                 begin
--                    Put_Line("Section Start Index: " & Natural'Image(Start_Index));
--                    Put_Line("Section End Index: " & Natural'Image(End_Index));
--                    Put_Line("Extracted Section Name: " & Section_Name);

--                    -- Adjust the section name to fit the Id_T constraints (change later)
--                    if Section_Name'Length >= 5 then
--                       Adjusted_Section_Name := Section_Name(Section_Name'First .. Section_Name'First + 4);
--                    else
--                       Adjusted_Section_Name := Section_Name & (1 .. 5 - Section_Name'Length => ' ');
--                    end if;

--                    Section := To_Unbounded_String(Adjusted_Section_Name);
--                    Entity := Manager.AddEntity(To_String(Section));
--                 end;
--              else
--                 -- Parse key-value pair
--                 declare
--                    Key_Value : constant String := Trimmed_Line;
--                    Key_Index : constant Natural := Index(Key_Value, "=");
--                    Key       : constant String := Trim(Key_Value(Key_Value'First .. Key_Index - 1), Ada.Strings.Both);
--                    Value     : constant String := Trim(Key_Value(Key_Index + 1 .. Key_Value'Last), Ada.Strings.Both);
--                 begin
--                    Put_Line("Parsing Key: " & Key);
--                    Put_Line("Parsing Value: " & Value);

--                    if Key = "Transform" then
--                       declare
--                          Pos_X_Start : constant Natural := Index(Value, "Position(") + 14;
--                          Pos_X_End   : constant Natural := Index(Value, ",");
--                          Pos_Y_Start : constant Natural := Pos_X_End + 2;
--                          Pos_Y_End   : constant Natural := Index(Value, ")", Pos_Y_Start);

--                          Vel_X_Start : constant Natural := Index(Value, "Velocity:(") + 14;
--                          Vel_X_End   : constant Natural := Index(Value, ",");
--                          Vel_Y_Start : constant Natural := Vel_X_End + 2;
--                          Vel_Y_End   : constant Natural := Index(Value, ")", Vel_Y_Start);

--                          Rot_Start : constant Natural := Index(Value, "Rotation: ") + 10;
--                          Rot_End   : constant Natural := Index(Value, " ", Rot_Start) - 1;

--                          Transform_Comp : Component_Access := new Transform_T;
--                       begin
--                          Put_Line("Position X Substring: " & Value(Pos_X_Start .. Pos_X_End - 1));
--                          Put_Line("Position Y Substring: " & Value(Pos_Y_Start .. Pos_Y_End - 1));
--                          Put_Line("Velocity X Substring: " & Value(Vel_X_Start .. Vel_X_End - 1));
--                          Put_Line("Velocity Y Substring: " & Value(Vel_Y_Start .. Vel_Y_End - 1));
--                          Put_Line("Rotation Substring: " & Value(Rot_Start .. Rot_End));

--                          Transform_T(Transform_Comp.all).Position.X := Float'Value(Trim(Value(Pos_X_Start .. Pos_X_End - 1), Ada.Strings.Both));
--                          Transform_T(Transform_Comp.all).Position.Y := Float'Value(Trim(Value(Pos_Y_Start .. Pos_Y_End - 1), Ada.Strings.Both));
--                          Transform_T(Transform_Comp.all).Velocity.X := Float'Value(Trim(Value(Vel_X_Start .. Vel_X_End - 1), Ada.Strings.Both));
--                          Transform_T(Transform_Comp.all).Velocity.Y := Float'Value(Trim(Value(Vel_Y_Start .. Vel_Y_End - 1), Ada.Strings.Both));
--                          Transform_T(Transform_Comp.all).Rotation := Float'Value(Trim(Value(Rot_Start .. Rot_End), Ada.Strings.Both));
--                          Entity.Add_Component(Transform_Comp);
--                       end;
--                    elsif Key = "RigidBody" then
--                       declare
--                          Mass_Start : constant Natural := Index(Value, "Mass: ") + 6;
--                          Mass_End   : constant Natural := Index(Value, " ", Mass_Start) - 1;
--                          RigidBody_Comp : Component_Access := new Rigidbody_T;
--                       begin
--                          Put_Line("Mass Substring: " & Value(Mass_Start .. Mass_End));

--                          Rigidbody_T(RigidBody_Comp.all).Mass := Float'Value(Trim(Value(Mass_Start .. Mass_End), Ada.Strings.Both));
--                          Entity.Add_Component(RigidBody_Comp);
--                       end;
--                    elsif Key = "AABB" then
--                       declare
--                          Left_Start  : constant Natural := Index(Value, "Left: ") + 6;
--                          Left_End    : constant Natural := Index(Value, ",");
--                          Bottom_Start: constant Natural := Index(Value, "Bottom: ") + 8;
--                          Bottom_End  : constant Natural := Index(Value, ",");
--                          Right_Start : constant Natural := Index(Value, "Right: ") + 7;
--                          Right_End   : constant Natural := Index(Value, ",");
--                          Top_Start   : constant Natural := Index(Value, "Top: ") + 5;
--                          Top_End     : constant Natural := Index(Value, " ");
--                          AABB_Comp : Component_Access := new AABB_T;
--                       begin
--                          Put_Line("AABB Left Substring: " & Value(Left_Start .. Left_End - 1));
--                          Put_Line("AABB Bottom Substring: " & Value(Bottom_Start .. Bottom_End - 1));
--                          Put_Line("AABB Right Substring: " & Value(Right_Start .. Right_End - 1));
--                          Put_Line("AABB Top Substring: " & Value(Top_Start .. Top_End - 1));

--                          AABB_T(AABB_Comp.all).Left   := Float'Value(Trim(Value(Left_Start .. Left_End - 1), Ada.Strings.Both));
--                          AABB_T(AABB_Comp.all).Bottom := Float'Value(Trim(Value(Bottom_Start .. Bottom_End - 1), Ada.Strings.Both));
--                          AABB_T(AABB_Comp.all).Right  := Float'Value(Trim(Value(Right_Start .. Right_End - 1), Ada.Strings.Both));
--                          AABB_T(AABB_Comp.all).Top    := Float'Value(Trim(Value(Top_Start .. Top_End - 1), Ada.Strings.Both));
--                          Entity.Add_Component(AABB_Comp);
--                       end;
--                    elsif Key = "Collision_Params" then
--                       declare
--                          Collision_Enabled_Start : constant Natural := Index(Value, "Collision_Enabled: ") + 18;
--                          Collision_Enabled_End   : constant Natural := Index(Value, ",");
--                          Destroy_On_Collision_Start : constant Natural := Index(Value, "Destroy_On_Collision: ") + 21;
--                          Destroy_On_Collision_End   : constant Natural := Index(Value, " ");
--                          Collision_Params_Comp : Component_Access := new Collision_Params_T;
--                       begin
--                          Put_Line("Collision Enabled Substring: " & Value(Collision_Enabled_Start .. Collision_Enabled_End - 1));
--                          Put_Line("Destroy On Collision Substring: " & Value(Destroy_On_Collision_Start .. Destroy_On_Collision_End - 1));

--                          Collision_Params_T(Collision_Params_Comp.all).Collision_Enabled := Boolean'Value(Trim(Value(Collision_Enabled_Start .. Collision_Enabled_End - 1), Ada.Strings.Both));
--                          Collision_Params_T(Collision_Params_Comp.all).Destroy_On_Collision := Boolean'Value(Trim(Value(Destroy_On_Collision_Start .. Destroy_On_Collision_End - 1), Ada.Strings.Both));
--                          Entity.Add_Component(Collision_Params_Comp);
--                       end;
--                    end if;
--                 end;
--              end if;
--           end;
--        end loop;
--        Close(Input);
--     end Load_Config;

--  end ECS.Config_Loader;
