with Ada.Unchecked_Conversion;
with Graphics.Text; use Graphics.Text;
with Ada.Text_IO; use Ada.Text_IO;


procedure ECS_Test_Text is

   C : Text_Array := Get_Character('A');
begin
   for I in 0 .. C'Length - 1 loop
      declare
         Bits : Graphics.Text.Text := C(I);
      begin
         for J in reverse 0 .. 7 loop
            -- Print the most significant bit
            declare
               Bit : Integer := Integer((Bits / (2**J)) and 1);
            begin
               Put(Bit'Image);
            end;
         end loop;
         New_Line;
      end;
   end loop;
   
end ECS_Test_Text;