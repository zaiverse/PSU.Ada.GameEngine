with ecs.entity;    use ecs.entity;
with ecs.component; use ecs.component;
with Ada.Tags;      use Ada.Tags;
with Ada.Text_IO;   use Ada.Text_IO;
-- with Window;        use Window;
with Ada.Numerics.Elementary_Functions;

package body renderer is
   package IC renames interfaces.C;

   type Point is record
      X : Integer;
      Y : Integer;
   end record;

   type Polygon is array (Natural range <>) of Point;

   procedure generic_swap (x, y : in out t) is
      tmp : constant t := x;
   begin
      x := y;
      y := tmp;
   end generic_swap;
   
   procedure set_pixel_color
     (img : in out Byte_Array; x : Integer; y : Integer; c : color; Screen_Width : Natural; Screen_Height : Natural) is
     Index : Natural := ((y mod Screen_Height) * Screen_Width + (x mod Screen_Width) ) * 4;
   begin
      Img(Index)     := Byte(C.B);
      Img(Index + 1) := Byte(C.G);
      Img(Index + 2) := Byte(C.R);
      Img(Index + 3) := Byte(C.A);
   end set_pixel_color;

   procedure Clear_Screen ( img : in out Byte_Array; c : color; Screen_Width : Natural; Screen_Height : Natural) is
   begin
      for Y in 0 .. Screen_Height - 1 loop
         for X in 0 .. Screen_Width - 1 loop
            set_pixel_color(img, X, Y, c, Screen_Width, Screen_Height);
         end loop;
      end loop;
   end Clear_Screen;

   procedure line
     (x0  : in out Integer;
      y0  : in out Integer;
      x1  : in out Integer;
      y1  : in out Integer;
      c   : color;
      img : in out Byte_Array;
      Screen_Width : Natural;
      Screen_Height : Natural)

   is
      procedure swap is new generic_swap (T => Integer);
      steep              : Boolean := False;
      dx                 : Integer := x0 - x1;
      dy                 : Integer := y0 - y1;
      derror2, error2, y : integer := 0;

   begin
      if abs dx < abs dy then
         swap (x0, y0);
         swap (x1, y1);
         steep := true;
      end if;
      if x0 > x1 then
         swap (x0, x1);
         swap (y0, y1);
      end if;

      dx := x1 - x0;
      dy := y1 - y0;
      derror2 := (abs dy) * 2;
      y := y0;

      for x in x0 .. x1 loop
         if steep then
            set_pixel_color (img, y, x, c, Screen_Width, Screen_Height);
         else
            set_pixel_color (img, x, y, c, Screen_Width, Screen_Height);
         end if;
         error2 := error2 + derror2;
         if error2 > dx then
            y := y + (if y1 > y0 then 1 else -1);
            error2 := error2 - dx * 2;
         end if;
      end loop;

   end line;

   function Generate_Polygon_Vertices
     (Sides : Positive; Radius : Natural; Center_X, Center_Y : Float; Screen_Width : Natural; Screen_Height : Natural)
      return Polygon
   is
      Vertices      : Polygon (0 .. Sides - 1);
   begin
      for I in Vertices'Range loop
         declare
            Angle : constant Float := 2.0 * Ada.Numerics.Pi / Float (Sides) * Float (I);
            X_Pos : Float := Float(Center_X) + Float(Radius) * Ada.Numerics.Elementary_Functions.Cos(Angle);
            Y_Pos : Float := Float(Center_Y) + Float(Radius) * Ada.Numerics.Elementary_Functions.Sin(Angle); 
         begin
            Vertices(I).X := Integer(X_Pos);
            Vertices(I).Y := Integer(Y_Pos);
         end;
      end loop;
      return Vertices;
   end Generate_Polygon_Vertices;

   procedure Draw_Regular_Polygon
     (img      : in out Byte_Array;
      Sides    : Positive;
      Radius   : Positive;
      Center_X : float;
      Center_Y : float;
      c        : Color;
      Screen_Width    : Natural;
      Screen_Height   : Natural)
   is
      Vertices : Polygon :=
        Generate_Polygon_Vertices (Sides, Radius, Center_X, Center_Y, Screen_Width, Screen_Height);
   begin
      for I in Vertices'Range loop
         declare
            Next_I     : constant Natural := ((I + 1) mod Vertices'Length);
            Vertex     : Point := Vertices (I);
            New_Vertex : Point := Vertices (Next_I);
         begin
            line (Vertex.X, Vertex.Y, New_Vertex.X, New_Vertex.Y, c, img, Screen_Width, Screen_Height);
         end;
      end loop;
   end Draw_Regular_Polygon;
   -- Helper for Draw_Filled_Triangle
   procedure DrawHorizontalLine(img : in out Byte_Array; X1,X2: in out Float; Y : Integer; C : Color; Screen_Width,Screen_Height : Natural) is
      procedure swap is new generic_swap(T => Float);
   begin
      -- make sure X1 is less than X2
      if X1 > X2 then 
         swap(X1, X2);
      end if;
      for X in Integer(Float'Floor(X1)) .. Integer(Float'Floor(X2)) loop
         set_pixel_color (img, X, Integer(Y), C, Screen_Width, Screen_Height);
      end loop; 
      
   end DrawHorizontalLine;
   -- Helper for Draw_Filled Triangle
   procedure Fill_Top_Triangle(img : in out Byte_Array; V1, V2, V3 : in out Vec2; C : Color; Screen_Width, Screen_Height : Natural) is
   begin
      -- Calculate slopes for the left and right edges
      LeftSlope : Float := (V3.X - V1.X) / (V3.Y - V1.Y);
      RightSlope : Float := (V3.X - V2.X) / (V3.Y - V2.Y);
      -- Set x-coord for the edges
      CurXLeft : Float := V3.X;
      CurXRight : Float := V3.X;
      -- Iterate through the scanlines
      Y : Integer := Integer(Float'Floor(V3.Y));
      while Y >= Integer(Float'Floor(V1.Y)) loop
         DrawHorizontalLine(img, CurXLeft, CurXRight, Y, C, Screen_Width,Screen_Height);
         CurXLeft := CurXLeft - LeftSlope;
         CurXRight := CurXRight - RightSlope;
         Y := Y - 1;
      end loop;
   end Fill_Top_Triangle;
   -- Helper for Draw_Filled_Triangle
   procedure Fill_Bottom_Triangle(img : in out Byte_Array; V1, V2, V3 : in out Vec2; C : Color; Screen_Width, Screen_Height : Natural) is
   begin
      -- Calculate slopes for the left and right edges
      LeftSlope : Float := (V2.X - V1.X) / (V2.Y - V1.Y);
      RightSlope : Float := (V3.X - V1.X) / (V3.Y - V1.Y);
      -- Set x-coord for the edges
      CurXLeft : Float := V1.X;
      CurXRight : Float := V1.X;
      -- Iterate through the scanlines
      for Y in Integer(V1.Y) .. Integer(V2.Y) loop
         DrawHorizontalLine(img, CurXLeft, CurXRight, Y, C, Screen_Width,Screen_Height);
         CurXLeft := CurXLeft + LeftSlope;
         CurXRight := CurXRight + RightSlope;
      end loop;
   end Fill_Bottom_Triangle;
   -- Scanline rasterization for filled triangle
   procedure Draw_Filled_Triangle(img : in out Byte_Array; V1, V2, V3 : in out Vec2; C : Color; Screen_Width, Screen_Height : Natural) is
      procedure swap is new generic_swap (T => Float);
   begin
      -- Sort vertices by Y
      if V1.Y > V2.Y then 
         swap(V1.X, V2.X);
         swap(V1.Y, V2.Y);
      end if;
      if V2.Y > V3.Y then
         swap(V2.X, V3.X);
         swap(V2.Y, V3.Y);
      end if;
      -- Recheck the first pair to ensure order
      if V1.Y > V2.Y then
         swap(V1.X, V2.X);
         swap(V1.Y, V2.Y);
      end if;
      -- Split triangle into top-flat and bottom-flat
      if V2.Y = V3.Y then
         Fill_Bottom_Triangle(img, V1, V2, V3, C, Screen_Width,Screen_Height);
      elsif V1.Y = V2.Y then
         Fill_Top_Triangle(img, V1, V2, V3, C, Screen_Width,Screen_Height);
      else
         -- Split Vertex
         SplitX : Float := V1.X + (V2.Y - V1.Y) / (V3.Y - V1.Y) * (V3.X - V1.X);
         SplitVertex : Vec2 := (SplitX, V2.Y);
         Fill_Bottom_Triangle(img, V1, V2, SplitVertex, C, Screen_Width, Screen_Height);
         Fill_Top_Triangle(img, V2, SplitVertex, V3, C, Screen_Width, Screen_Height);
      end if;
   end Draw_Filled_Triangle;

   procedure Draw_Filled_Quad(img : in out Byte_Array; X,Y,Width,Height : Float; C : Color; Screen_Width, Screen_Height : Natural) is
      V1, V2, V3, V4, V5, V6 : Vec2;
   begin
      V1 := (X,Y);
      V2 := (X, Y + Height);
      V3 := (X + Width, Y);
      V4 := (X, Y + Height);
      V5 := (X + Width, Y + Height);
      V6 := (X + Width, Y);
      Draw_Filled_Triangle(img, V1, V2, V3,C,Screen_Width,Screen_Height);
      Draw_Filled_Triangle(img, V4, V5, V6,C,Screen_Width,Screen_Height);
   end Draw_Filled_Quad;

   --  procedure Draw_Image_To_Window (img : Image) is
   --     use IC;

   --     x0, y0 : IC.int := 0;

   --  begin
   --     for i in img.r'range(1) loop
   --        for j in img.r'range(2) loop
   --           declare
   --              c       : color :=
   --                (img.r (i, j), img.g (i, j), img.b (i, j), img.a (i, j));
   --              c_color : IC.unsigned_long;
   --              for c_color'address use c'address;
   --           begin
   --              if c_color > 0 then
   --                 Window.draw_pixel (IC.int (i), IC.int (j), c_color);
   --              end if;
   --           end;
   --        end loop;
   --     end loop;
   --  end Draw_Image_To_Window;

end Renderer;