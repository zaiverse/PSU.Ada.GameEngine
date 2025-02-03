package Graphics.Text is
  type Text is mod 2**8 with size => 8; 
  type Text_Array is array (0 .. 12) of Text;             -- Representive character value + 12 lines per character
  type Text_Array_2D is array (0 .. 37) of Text_Array;    -- Digits 0-9, Chars ' ', '!', 'A' - 'Z' 
  type Text_Array_Access_2D is access Text_Array_2D;

  function Get_Character(C : Character) return Text_Array;
end Graphics.Text;