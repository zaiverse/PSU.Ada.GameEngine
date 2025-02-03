with Graphics.Text; use Graphics.Text;
with Ada.Text_IO; use Ada.Text_IO;


procedure ECS_Test_Text is
   C : Text_Array := Get_Character('C');
begin
   for I in 1 .. C'Length - 1 loop
      declare
         Bits : Text := C(I);
      begin
         for J in reverse 0 .. 7 loop
            -- Print the most significant bit
            declare
               Bit : Integer := Integer((Bits / (2**J)) and 1);
            begin
               if Bit = 1 then
                  Put("XX");
               else
                  Put("..");
               end if;
            end;
         end loop;
         Ada.Text_IO.New_Line;
      end;
   end loop;
end ECS_Test_Text;