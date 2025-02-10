with Ada.Unchecked_Conversion;
package Graphics.Text is
   type Text is mod 2**8 with size => 8; 
   type Text_Array is array (0 .. 13) of Text;           
   function Get_Character(C : Character) return Text_Array;
end Graphics.Text;