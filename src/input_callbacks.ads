with ECS.Entity_Manager; use ECS.Entity_Manager;
with ECS.Entity; use ECS.Entity;

package Input_Callbacks is



procedure Space_Key(Entity: Entity_Access; Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean);
procedure W_Key(Entity: Entity_Access; Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean);
procedure A_Key(Entity: Entity_Access; Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean);
procedure S_Key(Entity: Entity_Access; Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean);
procedure D_Key(Entity: Entity_Access; Manager : access Entity_Manager_T'Class; Dt : Duration; KeyDown: Boolean);
end Input_Callbacks;