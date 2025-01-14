package body Bitmap is

   procedure Initialize
     (Buffer : in out Bitmap_Buffer;
      Width  : Positive;
      Height : Positive) is
   begin
      Buffer.Width := Width;
      Buffer.Height := Height;
      -- Resize the vector to hold Width * Height elements
      Color_Vectors.Resize(Buffer.Data, Width * Height);
      -- Initialize the vector elements as needed
   end Initialize;

   procedure Clear (Buffer: in out Bitmap_Buffer; Fill_Color: Color) is
   begin
      for Y in 1 .. Buffer.Height loop
         for X in 1 .. Buffer.Width loop
            Color_Vectors.Replace_Element (
               Container => Buffer.Data,
               Index     => Index(Buffer, X, Y),
               New_Item  => Fill_Color
            );
         end loop;
      end loop;
   end Clear;

   --  procedure Clear (Buffer: in out Bitmap_Buffer; Fill_Color: Color) is
   --  begin
   --     for X in Buffer'Range(1) loop
   --        for Y in Buffer'Range(2) loop
   --           Buffer(X, Y) := Fill_Color;
   --        end loop;
   --     end loop;
   --  end Clear;

   -- Calculate the index for (X, Y) position
   function Index(Buffer : Bitmap_Buffer; X, Y : Positive) return Ada.Containers.Count_Type is
   begin
      return (Y - 1) * Buffer.Width + (X - 1) + 1;
   end Index;

   procedure Set_Pixel
     (Buffer      : in out Bitmap_Buffer;
      X, Y        : Integer;
      Pixel_Color : Color) is
   begin
      if X in 1 .. Buffer.Width and Y in 1 .. Buffer.Height then
         Color_Vectors.Replace_Element(Buffer.Data, Index(Buffer, X, Y), Pixel_Color);
      end if;
   end Set_Pixel;

   function Get_Pixel
     (Buffer : Bitmap_Buffer;
      X, Y   : Integer) return Color is
   begin
      if X in 1 .. Buffer.Width and Y in 1 .. Buffer.Height then
         return Color_Vectors.Element(Buffer.Data, Index(Buffer, X, Y));
      else
         return 0; -- Default color if out of bounds
      end if;
   end Get_Pixel;

   procedure Draw_Line (
      Buffer     : in out Bitmap_Buffer;
      X1, Y1     : Integer;
      X2, Y2     : Integer;
      Line_Color : Color
   ) is
      -- Calculate differences
      Dx : Integer := abs(X2 - X1);
      Dy : Integer := abs(Y2 - Y1);
      -- Determine the direction of the increment
      Sx : Integer := (if X1 < X2 then 1 else -1);
      Sy : Integer := (if Y1 < Y2 then 1 else -1);
      Err : Integer := (if Dx > Dy then Dx else -Dy) / 2;
      E2  : Integer;
   begin
      loop
         -- Set the pixel at the current position
         Set_Pixel(Buffer, X1, Y1, Line_Color);
         -- Check if the end point has been reached
         exit when X1 = X2 and Y1 = Y2;
         E2 := Err;
         if E2 > -Dx then
            Err := Err - Dy;
            X1  := X1 + Sx;
         end if;
         if E2 < Dy then
            Err := Err + Dx;
            Y1  := Y1 + Sy;
         end if;
      end loop;
   end Draw_Line;


   procedure Draw_Polygon (
      Buffer        : in out Bitmap_Buffer;
      Points        : Point2D_Array;
      Polygon_Color : Color
   ) is
   begin
      for I in Points'First .. Points'Last - 1 loop
         Draw_Line(
            Buffer,
            Integer(Points(I).X),
            Integer(Points(I).Y),
            Integer(Points(I + 1).X),
            Integer(Points(I + 1).Y),
            Polygon_Color
         );
      end loop;
      -- Connect the last point to the first
      Draw_Line(
         Buffer,
         Integer(Points(Points'Last).X),
         Integer(Points(Points'Last).Y),
         Integer(Points(Points'First).X),
         Integer(Points(Points'First).Y),
         Polygon_Color
      );
   end Draw_Polygon;


   procedure Fill_Polygon (
      Buffer     : in out Bitmap_Buffer;
      Points     : Point2D_Array;
      Fill_Color : Color
   ) is
      type Intersections_Array is array (Integer range <>) of Integer;
      -- Determine the bounding box of the polygon
      Min_Y : Integer := Integer(Points(Points'First).Y);
      Max_Y : Integer := Integer(Points(Points'First).Y);
   begin
      -- Find the minimum and maximum Y coordinates
      for P of Points loop
         if Integer(P.Y) < Min_Y then
            Min_Y := Integer(P.Y);
         elsif Integer(P.Y) > Max_Y then
            Max_Y := Integer(P.Y);
         end if;
      end loop;

      -- Scan each line in the bounding box
      for Y in Min_Y .. Max_Y loop
         declare
            Intersections : Intersections_Array(1 .. Points'Length);
            Count         : Integer := 0;
         begin
            -- Find intersections with the polygon edges
            for I in Points'First .. Points'Last loop
               declare
                  P1 : Math.Vector2D := Points(I);
                  P2 : Math.Vector2D := Points((if I = Points'Last then Points'First else I + 1));
               begin
                  if (P1.Y <= Float(Y) and P2.Y > Float(Y)) or
                     (P2.Y <= Float(Y) and P1.Y > Float(Y))
                  then
                     -- Compute the X coordinate of the intersection
                     Intersections(Count + 1) :=
                     Integer(P1.X + ((Float(Y) - P1.Y) / (P2.Y - P1.Y)) * (P2.X - P1.X));
                     Count := Count + 1;
                  end if;
               end;
            end loop;

            -- Sort the intersections
            for I in 1 .. Count - 1 loop
               for J in I + 1 .. Count loop
                  if Intersections(I) > Intersections(J) then
                     declare
                        Temp : Integer := Intersections(I);
                     begin
                        Intersections(I) := Intersections(J);
                        Intersections(J) := Temp;
                     end;
                  end if;
               end loop;
            end loop;

            -- Fill between pairs of intersections
            for I in 1 .. Count - 1 by 2 loop
               for X in Intersections(I) .. Intersections(I + 1) loop
                  Set_Pixel(Buffer, X, Y, Fill_Color);
               end loop;
            end loop;
         end;
      end loop;
   end Fill_Polygon;


   --  procedure Set_Pixel (Buffer: in out Bitmap_Buffer; X, Y: Integer; Pixel_Color: Color) is
   --  begin
   --     if X in Buffer'Range(1) and Y in Buffer'Range(2) then
   --        Buffer(X, Y) := Pixel_Color;
   --     end if;
   --  end Set_Pixel;

   --  function Get_Pixel (Buffer: Bitmap_Buffer; X, Y: Integer) return Color is
   --  begin
   --     if X in Buffer'Range(1) and Y in Buffer'Range(2) then
   --        return Buffer(X, Y);
   --     else
   --        return 0;
   --     end if;
   --  end Get_Pixel;

   --  procedure Draw_Line (Buffer: in out Bitmap_Buffer; X1, Y1, X2, Y2: Integer; Line_Color: Color) is
   --     Start_Point : Math.Vector2D := (X => Float(X1), Y => Float(Y1));
   --     End_Point   : Math.Vector2D := (X => Float(X2), Y => Float(Y2));
   --     Direction   : Math.Vector2D := Math.Subtract(End_Point, Start_Point);
   --     Steps       : Float := Math.Magnitude(Direction);
   --     Unit_Dir    : Math.Vector2D := Math.Normalize(Direction);
   --     Current     : Math.Vector2D := Start_Point;
   --  begin
   --     for Step in 0 .. Integer(Steps) loop
   --        Set_Pixel(Buffer, Integer(Current.X), Integer(Current.Y), Line_Color);
   --        Current := Math.Add(Current, Unit_Dir);
   --     end loop;
   --  end Draw_Line;

   --  procedure Draw_Polygon (Buffer: in out Bitmap_Buffer; Points: Point2D_Array; Polygon_Color: Color) is
   --  begin
   --     for I in Points'First .. Points'Last - 1 loop
   --        Draw_Line(
   --           Buffer,
   --           Integer(Points(I).X),
   --           Integer(Points(I).Y),
   --           Integer(Points(I + 1).X),
   --           Integer(Points(I + 1).Y),
   --           Polygon_Color
   --        );
   --     end loop;
   --     -- Connect last point to the first
   --     Draw_Line(
   --        Buffer,
   --        Integer(Points(Points'Last).X),
   --        Integer(Points(Points'Last).Y),
   --        Integer(Points(Points'First).X),
   --        Integer(Points(Points'First).Y),
   --        Polygon_Color
   --     );
   --  end Draw_Polygon;

   --  procedure Fill_Polygon (Buffer: in out Bitmap_Buffer; Points: Point2D_Array; Fill_Color: Color) is
   --     Min_Y, Max_Y : Integer := Integer(Points(Points'First).Y);
   --     Intersections : array (Positive range <>) of Integer;
   --  begin
   --     -- Find the bounding box
   --     for P of Points loop
   --        if Integer(P.Y) < Min_Y then
   --           Min_Y := Integer(P.Y);
   --        elsif Integer(P.Y) > Max_Y then
   --           Max_Y := Integer(P.Y);
   --        end if;
   --     end loop;

   --     -- Scan each line in the bounding box
   --     for Y in Min_Y .. Max_Y loop
   --        declare
   --           Current_Intersections : Positive := 0;
   --        begin
   --           for I in Points'First .. Points'Last loop
   --              declare
   --                 P1, P2 : Math.Vector2D :=
   --                   (Points(I), Points((if I = Points'Last then Points'First else I + 1)));
   --              begin
   --                 if (P1.Y <= Float(Y) and P2.Y > Float(Y)) or (P2.Y <= Float(Y) and P1.Y > Float(Y)) then
   --                    -- Compute X intersection
   --                    Intersections(Current_Intersections) := Integer(P1.X + ((Float(Y) - P1.Y) / (P2.Y - P1.Y)) * (P2.X - P1.X));
   --                    Current_Intersections := Current_Intersections + 1;
   --                 end if;
   --              end;
   --           end loop;

   --           -- Sort intersections
   --           Math.Sort(Intersections);

   --           -- Fill between pairs of intersections
   --           for J in Intersections'First .. Intersections'Last by 2 loop
   --              for X in Intersections(J) .. Intersections(J + 1) loop
   --                 Set_Pixel(Buffer, X, Y, Fill_Color);
   --              end loop;
   --           end loop;
   --        end;
   --     end loop;
   --  end Fill_Polygon;

end Bitmap;
