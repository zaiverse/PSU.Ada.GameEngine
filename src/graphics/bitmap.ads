with Math;
with Ada.Containers.Vectors;

package Bitmap is
   type Color is mod 2**32;

   type Point2D_Array is array (Positive range <>) of Math.Vector2D;

   -- Instantiate the vector package for Color
   package Color_Vectors is new Ada.Containers.Vectors (
      Index_Type   => Positive,
      Element_Type => Color
   );

   -- Define Bitmap_Buffer using the instantiated vector
   type Bitmap_Buffer is record
      Data   : Color_Vectors.Vector;
      Width  : Positive;
      Height : Positive;
   end record;

   -- Declare the access type for Bitmap_Buffer
   type Bitmap_Buffer_Access is access all Bitmap_Buffer;


   --  procedure Draw_Sprite (
   --     Buffer      : in out Bitmap_Buffer;
   --     Sprite_Name : in String;
   --     Position    : in Math.Vector2D;
   --     Color       : in Color := 16#FFFFFFFF#  -- Default to white (no tint)
   --  );

   procedure Initialize (Buffer: in out Bitmap_Buffer; Width, Height: Positive);
   procedure Clear (Buffer: in out Bitmap_Buffer; Fill_Color: Color);
   procedure Set_Pixel (Buffer: in out Bitmap_Buffer; X, Y: Integer; Pixel_Color: Color);
   function Get_Pixel (Buffer: Bitmap_Buffer; X, Y: Integer) return Color;

   procedure Draw_Line (Buffer : in out Bitmap_Buffer; X1, Y1, X2, Y2 : Integer; Line_Color : Color);

   -- For polygons
   procedure Draw_Polygon (Buffer: in out Bitmap_Buffer; Points: Point2D_Array; Polygon_Color: Color);
   procedure Fill_Polygon (Buffer: in out Bitmap_Buffer; Points: Point2D_Array; Fill_Color: Color);

end Bitmap;