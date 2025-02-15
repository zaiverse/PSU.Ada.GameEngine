with Ada.Text_IO; use Ada.Text_IO;
with ECS.Entity; use ECS.Entity;
with ECS.Component; use ECS.Component;
package body Input_Callbacks is 

   Player_Entity : Entity_Access := null;
   Last_Fired : Duration := 0.0;

   procedure Spawn_Bullet(Manager : access Entity_Manager_T'Class) is
      Trans    : Component_Access;
      Rect     : Component_Access;
   begin
      if Player_Entity = null then
         Player_Entity := Manager.GetEntity("Playr");
      end if;
      Trans := Player_Entity.all.Get_Component(Transform_T'Tag);
      Rect :=  Player_Entity.all.Get_Component(Quad_T'Tag);
      declare
         T renames Transform_T(Trans.all);
         R renames Quad_T(Rect.all);
         StartPosX   : Float;
         B_Trans     : Component_Access;
         B_RBody     : Component_Access;
         B_AABB      : Component_Access;
         B_ColPar    : Component_Access;
         B_Shape     : Component_Access;
      begin
         Bullet : Entity_Access := Manager.all.AddEntity ("B0001");
         StartPosX := (T.Position.X + (R.Width / 2.0)) ;
         B_Trans := new Transform_T'((StartPosX,T.Position.Y - 2.0),(0.0,-200.0),0.0);
         B_RBody := new Rigidbody_T'(Mass => 0.0);
         B_Shape := new Quad_T'(1.0,1.0,(255,255,255,255));
         B_ColPar := new Collision_Params_T'(True,False,True,True);
         declare
            BT renames Transform_T(B_Trans.all);
            BS renames Quad_T(B_Shape.all);
         begin
            B_AABB  := new AABB_T'(BT.Position.X, BT.Position.Y + BS.Height, BT.Position.X + BS.Width, BT.Position.Y);
         end;
         Bullet.all.Add_Component (B_Trans);
         Bullet.all.Add_Component (B_RBody);
         Bullet.all.Add_Component (B_AABB);
         Bullet.all.Add_Component (B_ColPar);
         Bullet.all.Add_Component (B_Shape);
      end;
   end Spawn_Bullet;



   procedure Space_Key (Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is

   Cooldown   : constant Duration := 0.01;
   begin
      if KeyDown then
         Last_Fired := Last_Fired + Dt;
         if Last_Fired >= Cooldown then
            Spawn_Bullet(Manager);
            Last_Fired := Last_Fired - Cooldown;
         end if;
      else
         Last_Fired := 0.0;
      end if;
   end Space_Key;

   procedure W_Key(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
   Trans : Component_Access;
   begin
      if Player_Entity = null then
         Player_Entity := Manager.GetEntity("Playr");
      end if;
      Trans := Player_Entity.all.Get_Component (Transform_T'Tag);
      declare
         T renames Transform_T(Trans.all);
      begin
         if KeyDown then
            T.Velocity.Y := -200.0;
         else
            T.Velocity.Y := 0.0;
         end if;
      end; 
   end W_Key;
   procedure A_Key(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
      Trans : Component_Access;
   begin
      if Player_Entity = null then
         Player_Entity := Manager.GetEntity("Playr");
      end if;
      Trans := Player_Entity.all.Get_Component (Transform_T'Tag);
      declare
         T renames Transform_T(Trans.all);
      begin
         if KeyDown then
            T.Velocity.X := -200.0;
         else
            T.Velocity.X := 0.0;
         end if;
      end; 
   end A_Key;
   procedure S_Key(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
      Trans : Component_Access;
   begin
      if Player_Entity = null then
         Player_Entity := Manager.GetEntity("Playr");
      end if;
      Trans := Player_Entity.all.Get_Component (Transform_T'Tag);
      declare
         T renames Transform_T(Trans.all);
      begin
         if KeyDown then
            T.Velocity.Y := 200.0;
         else
            T.Velocity.Y := 0.0;
         end if;
      end; 
   end S_Key;   
   procedure D_Key(Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean) is
      Trans : Component_Access;
   begin
      if Player_Entity = null then
         Player_Entity := Manager.GetEntity("Playr");
      end if;
      Trans := Player_Entity.all.Get_Component (Transform_T'Tag);
      declare
         T renames Transform_T(Trans.all);
      begin
         if KeyDown then
            T.Velocity.X := 200.0;
         else
            T.Velocity.X := 0.0;
         end if;
      end; 
   end D_Key;

end Input_Callbacks;