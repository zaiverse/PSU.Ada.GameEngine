with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
package body ECS.System.Render is 


   -- Wrapper procedures to contain data initialization before calling the internal drawing procedures
   procedure Draw_Circle(Self : in out Render_T; Transform, Circle : Component_Access) is   
      T renames Transform_T(Transform.all);
      C renames Circle_T(Circle.all);
   begin
      Draw_Regular_Polygon(Self.Buffer.all, C.Sides, C.Radius, T.Position.X,T.Position.Y, C.C, Self.Width, Self.Height);
   end Draw_Circle;
-------------------------------------------------------------------------------------------------------------------------------------------------------------
   procedure Draw_Animated_Texture(Self : in out Render_T; Transform, Animation, Quad : Component_Access) is
      T renames Transform_T(Transform.all);
      Q renames Quad_T(Quad.all);
      AC renames Animation_Component_T(Animation.all);
      A : Single_Animation_Access := AC.Animations(AC.Current);
      Tx : Texture_Access := AC.Textures(AC.Current);
   begin
      Draw_Image_To_Buffer(Self.Buffer.all, Tx.Data, Integer(T.Position.X), Integer(T.Position.Y), Integer(Q.Width), Integer(Q.Height), A.CurX,A.CurY, Self.Width, Self.Height,Natural(Tx.Width));
   end Draw_Animated_Texture;
-------------------------------------------------------------------------------------------------------------------------------------------------------------
   procedure Draw_Static_Texture(Self : in out Render_T; Transform, Texture, Quad : Component_Access) is
      T renames Transform_T(Transform.all);
      Q renames Quad_T(Quad.all);
      Tx renames Texture_T(Texture.all);
   begin
      Draw_Image_To_Buffer(Self.Buffer.all, Tx.Data, Integer(T.Position.X), Integer(T.Position.Y), Tx.Width, Tx.Height, Self.Width, Self.Height);
   end Draw_Static_Texture;
-------------------------------------------------------------------------------------------------------------------------------------------------------------
   procedure Draw_Rectangle(Self : in out Render_T; Transform, Quad : Component_Access) is
      T renames Transform_T(Transform.all);
      Q renames Quad_T(Quad.all);
   begin
      Draw_Filled_Quad (Self.Buffer.all,T.Position.X, T.Position.Y, Q.Width, Q.Height, Q.C,Self.Width, Self.Height);
   end Draw_Rectangle;
-------------------------------------------------------------------------------------------------------------------------------------------------------------
   procedure Draw_Text(Self : in out Render_T; Transform, Text : Component_Access) is
      T renames Transform_T(Transform.all);
      Txt renames Text_T(Text.all);
   begin
      Draw_String (Self.Buffer.all, Integer(T.Position.X), Integer(T.Position.Y), 0, 0, To_String(Txt.Text), Txt.C, Self.Width,Self.Height);
   end Draw_Text;
-------------------------------------------------------------------------------------------------------------------------------------------------------------

   -- Calls the drawing function for each entity depending on the attached components
   procedure Execute (Self       : in out Render_T;
                      Dt         : Duration;
                      Manager    : access Entity_Manager_T'Class ) is
   begin
      for Entity of Manager.all.Entities loop
      declare
         Transform   : Component_Access   :=    Entity.all.Get_Component(Transform_T'Tag);
         Circle      : Component_Access   :=    Entity.all.Get_Component(Circle_T'Tag);
         Quad        : Component_Access   :=    Entity.all.Get_Component(Quad_T'Tag);
         Text        : Component_Access   :=    Entity.all.Get_Component(Text_T'Tag);
         Texture     : Component_Access   :=    Entity.all.Get_Component(Texture_T'Tag);
         Animation   : Component_Access   :=    Entity.all.Get_Component(Animation_Component_T'Tag);
      begin
         if Transform /= null then
            -- Draw components if they exist
            if Circle /= null then
               Self.Draw_Circle (Transform, Circle);
            end if;
         if Quad /= null then
            if Animation /= null  then
               Self.Draw_Animated_Texture (Transform, Animation, Quad);
            elsif Texture /= null then
               Self.Draw_Static_Texture(Transform,Texture,Quad);
            else
               Self.Draw_Rectangle(Transform, Quad);
            end if;
         end if;
            if Text /= null then
               Self.Draw_Text (Transform, Text);
            end if;
         else
            Put_Line ("Entity:"& Entity.all.Id'Image & " missing transform component.");
         end if;
      end;
      end loop; 
   end Execute;
end ECS.System.Render;