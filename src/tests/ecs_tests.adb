with Ada.Text_IO; use Ada.Text_IO;
with ecs.entity; use ecs.entity;
with ecs.component; use ecs.component;
with ecs.entity_manager; use ecs.entity_manager;
with ecs.system; use ecs.system;

  procedure ECS_Tests is

  Manager : Entity_Manager_T;
  Player : Entity_Access := Manager.AddEntity("Playr");
  Enemy : Entity_Access := Manager.AddEntity("Enemy");
  Mover : Mover_T;

  Transform : Component_Access := new Transform_T'(X => 1.0, Y => 2.0, Rotation => 0.0);
  Rigidbody : Component_Access := new Rigidbody_T'(Mass => 1.0);

  begin
  Put_Line("Running Tests");

  Player.all.Add_Component(Transform);
  Player.all.Add_Component(Rigidbody);

  Manager.Update;  
    -- 1/60 of a second after program launch
  Execute ( Mover, 1.0/60.0, Player);

    -- 2/60 of a second after the last frame
  Execute ( Mover, 2.0/60.0, Player);


  for E of Manager.Entities loop
    Put_Line(E.all.Id);
  end loop;

  Enemy.all.Destroyed := True;
  Manager.Update;
  for E of Manager.Entities loop
    Put_Line(E.all.Id);
  end loop;

  end ECS_Tests;

  