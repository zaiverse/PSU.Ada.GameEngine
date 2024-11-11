with Ada.Text_IO; use Ada.Text_IO;
with ecs.entity; use ecs.entity;
with ecs.component; use ecs.component;
with ecs.system; use ecs.system;

procedure GameEngine is

Player : aliased Entity_T := (1,
                Id => "Playr",
                Components => (0 => new Transform_T'(X => 0.0, Y => 0.0, Rotation => 0.0),
                               1 => new Rigidbody_T'(Mass => 1.0))
                );
    Mover : Mover_T;

begin
    Put_Line ("Start Engine");

    -- 1/60 of a second after program launch
    Execute ( Mover, 1.0/60.0, Player );

    -- 2/60 of a second after the last frame
    Execute ( Mover, 2.0/60.0, Player );

end GameEngine;