with Ada.Text_IO; use Ada.Text_IO;
with ecs; use ecs;

procedure GameEngine is

Player : aliased Entity_T := (1,
                Id => "Playr",
                Components => (0 => new Transform_T'(X => 0.0, Y => 0.0, Rotation => 0.0),
                               1 => new Rigidbody_T'(Mass => 1.0))
                );
    Mover : Mover_T;

begin
    Put_Line ("Start Engine");

    Execute ( Mover, 1.0/60.0, Player );

end GameEngine;