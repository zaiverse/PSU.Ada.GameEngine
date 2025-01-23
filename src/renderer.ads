with Interfaces;
with Interfaces.C;
with Win32; use Win32;
with ECS.Color; use ECS.Color;
with ECS.Vec2; use ECS.Vec2;
package Renderer is

   type bool is new boolean;
   for bool'size use 8;

   procedure Clear_Screen
      (img : in out Byte_Array; c : color; Width : Natural; Height : Natural);

   procedure set_pixel_color
     (img : in out Byte_Array; x : Integer; y : Integer; c : color; Width : Natural; Height : Natural);

   procedure line
     (x0  : in out Integer;
      y0  : in out Integer;
      x1  : in out Integer;
      y1  : in out Integer;
      c   : color;
      img : in out Byte_Array;
      Width : Natural;
      Height : Natural);

   procedure Draw_Regular_Polygon
     (img      : in out Byte_Array;
      Sides    : Positive;
      Radius   : Positive;
      Center_X : Float;
      Center_Y : Float;
      c        : Color;
      Width    : Natural;
      Height   : Natural);

   --procedure Draw_Image_To_Window (img : Image);
   procedure Draw_Filled_Triangle(img : in out Byte_Array; V1, V2, V3 : in out Vec2; C : Color; Width, Height : Natural);

   generic
      type t is private;
   procedure generic_swap (x, y : in out t);

end Renderer;