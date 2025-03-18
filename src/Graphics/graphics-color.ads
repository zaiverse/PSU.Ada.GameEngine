package Graphics.Color is
   type Color_Int is mod 2 ** 8;

   type Color is record
      R: Color_Int;
      G: Color_Int;
      B: Color_Int;
      A: Color_Int;
   end record;

   -- static colors
   Red    : constant Color := (R => 255, G => 0,   B => 0,   A => 255);
   Green  : constant Color := (R => 0,   G => 255, B => 0,   A => 255);
   Blue   : constant Color := (R => 0,   G => 0,   B => 255, A => 255);
   White  : constant Color := (R => 255, G => 255, B => 255, A => 255);
   Black  : constant Color := (R => 0,   G => 0,   B => 0,   A => 255);
   Invisible_Black : constant Color := (0,0,0,0);
end Graphics.Color;