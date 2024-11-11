with ecs.component; use ecs.component;
with ecs.entity; use ecs.entity;
with Ada.Tags; use Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;

package body ecs.system is

    procedure Execute (Self : Mover_T;
                       Dt   : Duration; 
                       E    : in out Entity_T'Class;  
                       ES   : Entities_T := Entities_T'(1 .. 0 => null)) is
        Trans  : Components_T :=       E.Get_Components (Transform_T'Tag);
        Rigidbodies : Components_T :=  E.Get_Components (Rigidbody_T'Tag);

        DeltaY : Float := Float(Dt) * (-9.8);
    begin

        if Trans'Length = 0 then
            Put_Line ("No Transform");
            return;
        end if;
        if Rigidbodies'Length = 0 then
            Put_Line ("No Rigidbodies");
            return;
        end if;

        declare
            T renames Transform_T (Trans(0).all);
            R renames Rigidbody_T (Rigidbodies(0).all);           

        begin
        Put_Line ("Mover: " & T.X'Image & ", " & T.Y'Image & ", " & T.Rotation'Image);
        T.Y := T.Y + DeltaY;
        Put_Line ("Mover: " & T.X'Image & ", " & T.Y'Image & ", " & T.Rotation'Image);
        end;
    end Execute;

end ecs.system;