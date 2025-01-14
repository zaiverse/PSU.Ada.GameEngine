package Time is

   type Time_Point is private;

   procedure intialize_Timer;
   function Get_Time_Point return Time_Point;
   function Delta_Time (Start, End_Point : Time_Point) return Float;
   procedure Sleep (Milliseconds: Integer);

   private
      type Time_Point is record
         Seconds: Float;
      end record;

end Time;