package body ECS.Config_Loader is

   procedure Load_Config(Manager : in out Entity_Manager_T; File_Name : String; Window_Width : out Integer; Window_Height : out Integer; Window_Color : out Graphics.Color.Color) is
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
                  Parse_Key_Value(Section, Key, Value, Entity, Window_Width, Window_Height, Window_Color);
               end;
            end if;
         end;

         -- Initialize AABB based on Transform and Quad components, but skip for Window section
         if Section /= "Window" then
            Initialize_AABB(Entity);
         end if;
      end loop;
      Close(Input);
   end Load_Config;

   procedure Parse_Section(Section_Name : String; Section : out Unbounded_String; Entity : out Entity_Access; Manager : in out Entity_Manager_T) is
      Max_Entity_Name_Length : constant := 5; -- Define the maximum length for entity names
      Trimmed_Section_Name   : String(1 .. Max_Entity_Name_Length); -- Ensure it's exactly 5 characters
      Temp_Section_Name      : constant String := Ada.Strings.Fixed.Trim(Section_Name, Ada.Strings.Both);
   begin
      -- Trim and adjust the section name
      if Temp_Section_Name = "Window" then
         Section := To_Unbounded_String(Temp_Section_Name);
         Entity := null; -- Skip creating an entity for the Window section
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
   end Parse_Section;

   procedure Parse_Key_Value(Section : Unbounded_String; Key : String; Value : String; Entity : Entity_Access; Window_Width : in out Integer; Window_Height : in out Integer; Window_Color : in out Graphics.Color.Color) is
      Dot_Index : constant Natural := Ada.Strings.Fixed.Index(Key, ".");
   begin
      if Section = "Window" then
         Handle_Window_Properties(Key, Value, Window_Width, Window_Height, Window_Color);
      elsif Dot_Index /= 0 then
         declare
            Component_Name : constant String := Ada.Strings.Fixed.Head(Key, Dot_Index - 1);
            Property_Name  : constant String := Ada.Strings.Fixed.Tail(Key, Key'Length - Dot_Index);
         begin
            case Component_Name is
               when "Transform" => Handle_Transform_Properties(Entity, Property_Name, Value);
               when "RigidBody" => Handle_RigidBody_Properties(Entity, Property_Name, Value);
               when "Quad" => Handle_Quad_Properties(Entity, Property_Name, Value);
               when "Collision_Params" => Handle_Collision_Params_Properties(Entity, Property_Name, Value);
               when others => null;
            end case;
         end;
      end if;
   end Parse_Key_Value;

   procedure Handle_Window_Properties(Key : String; Value : String; Window_Width : in out Integer; Window_Height : in out Integer; Window_Color : in out Graphics.Color.Color) is
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

   procedure Handle_Transform_Properties(Entity : Entity_Access; Property_Name : String; Value : String) is
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

   procedure Handle_RigidBody_Properties(Entity : Entity_Access; Property_Name : String; Value : String) is
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

   procedure Handle_Quad_Properties(Entity : Entity_Access; Property_Name : String; Value : String) is
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

   procedure Handle_Collision_Params_Properties(Entity : Entity_Access; Property_Name : String; Value : String) is
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
