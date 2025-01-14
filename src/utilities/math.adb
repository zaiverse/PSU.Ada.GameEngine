package body Math is

   function Add (A, B: Vector2D) return Vector2D is
   begin
      return (X => A.X + B.X, Y => A.Y + B.Y);
   end Add;

   function Subtract (A, B: Vector2D) return Vector2D is
   begin
      return (X => A.X - B.X, Y => A.Y - B.Y);
   end Subtract;

   function Scale (V: Vector2D; Factor: Float) return Vector2D is
   begin
      return (X => V.X * Factor, Y => V.Y * Factor);
   end Scale;

   function Magnitude (V: Vector2D) return Float is
   begin
      return Float*Sqrt(V.X**2 + V.Y**2);
   end Magnitude;

   function Normalize (V: Vector2D) return Vector2D is
      Mag: Float := Magnitude (V);
   begin
      if Mag = 0.0 then
         return V;
      else
         return Scale(V, 1.0/Mag);
      end if;
   end Normalize;

   function Clamp (Value, Min, Max: Float) return Float is
   begin
      if Value < Min then
         return Min;
      elsif Value > Max then
         return Max;
      else
         return Value;
      end if;
   end Clamp;

end Math;