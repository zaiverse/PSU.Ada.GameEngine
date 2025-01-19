with Interfaces;
with Interfaces.C;
with Win32; use Win32;
with ECS.Color; use ECS.Color;
package Renderer is

   type bool is new boolean;
   for bool'size use 8;

   procedure Clear_Screen
      (img : in out Byte_Array; c : color; Width : Natural; Height : Natural);

   procedure set_pixel_color
     (img : in out Byte_Array; x : natural; y : natural; c : color; Width : Natural);

   procedure line
     (x0  : in out natural;
      y0  : in out natural;
      x1  : in out natural;
      y1  : in out natural;
      c   : color;
      img : in out Byte_Array;
      Width : Natural);

   procedure Draw_Regular_Polygon
     (img      : in out Byte_Array;
      Sides    : Positive;
      Radius   : Positive;
      Center_X : Float;
      Center_Y : Float;
      c        : Color;
      Width    : Natural);

   --procedure Draw_Image_To_Window (img : Image);

   generic
      type t is private;
   procedure generic_swap (x, y : in out t);

end Renderer;