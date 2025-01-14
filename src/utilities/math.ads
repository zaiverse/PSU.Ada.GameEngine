package Math is

   -- 2D vector
   type Vector2D is record
      X: Float;
      Y: Float;
   end record;

   -- 3D vector (Future use)
   type Vector3D is record
      X: Float;
      Y: Float;
      Z: Float;
   end record;

   -- Operations
   function Add (A, B: Vector2D) return Vector2D;
   function Subtract (A, B: Vector2D) return Vector2D;
   function Scale (V: Vector2D; Factor: Float) return Vector2D;
   function Magniture (V: Vector2D) return Float;
   function Normalize (V: Vector2D) return Vector2D;

   -- Miscellaneous functions
   function Clamp (Value, Min, Max: Float) return Float;

end Math;