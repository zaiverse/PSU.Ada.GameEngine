with Bitmap;
with Math;
with Ada.Text_IO; use Ada.Text_IO;

package body Shapes is

   -- Implementation of Draw for Polygon
   procedure Draw (Shape : in Polygon; Buffer : in out Bitmap.Bitmap_Buffer) is
   begin
      -- Convert Points to Bitmap.Point2D_Array if necessary
      declare
         Bitmap_Points : Bitmap.Point2D_Array(1 .. Shape.Points'Length);
      begin
         for I in Shape.Points'Range loop
            Bitmap_Points(I).X := Shape.Points(I).X;
            Bitmap_Points(I).Y := Shape.Points(I).Y;
         end loop;

         -- Draw the polygon on the buffer
         Bitmap.Draw_Polygon (
            Buffer        => Buffer,
            Points        => Bitmap_Points,
            Polygon_Color => Shape.Color
         );
      end;
   end Draw;

end Shapes;
