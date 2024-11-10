with Ada.Text_IO; use Ada.Text_IO;

package body ecs is

    function Get_Components (E: Entity_T'Class; Tag: Ada.Tags.Tag) return Components_T is
    Count : Natural := 0;
    begin
        for C of E.Components loop
            Count := (if C'Tag = Tag then Count + 1 else count);
        end loop;
        declare
            Result : Components_T (0 .. Count-1);
        begin
            Count := 0;
            for C of E.Components loop
                if C'Tag = Tag then
                    Result (Count) := C;
                    Count := Count + 1;
                end if;
            end loop;
            return Result;
        end;
    end;

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


end ecs;