with ecs.component; use ecs.component;
with Ada.Tags; use Ada.Tags;

package body ecs.entity is
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
end ecs.entity;