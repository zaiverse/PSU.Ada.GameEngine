with Bitmap;
with Math;

package Shapes is

   -- Abstract base type for all shapes
   type Shape_Type is abstract tagged null record;

   -- Access type for Shape_Type
   type Shape_Access is access all Shape_Type'Class;

   -- Abstract procedure for drawing shapes
   procedure Draw (Shape : in Shape_Type; Buffer : in out Bitmap.Bitmap_Buffer);

   -- Polygon shape type
   type Polygon is new Shape_Type with record
      Points : Bitmap.Point2D_Array;
      Color  : Bitmap.Color;
   end record;

end Shapes;
