with Ada.Real_Time; use Ada.Real_Time;
with Ada.Calendar; use Ada.Calendar;

package body Time is
   procedure intialize_Timer is
   begin
      null; -- No explicit init necessary to initialize Ada.Real_Time
   end intialize_Timer;

   function Get_Time_Point return Time_Point is
      Now: Ada.Real_Time.Time := Clock;
   begin
      return (Seconds => Ada.Real_Time.To_Duration(Now));
   end Get_Time_Point;

   function Delta_Time (Start : Time_Point; End_Point : Time_Point) return Float is
   begin
      return End_Point.Seconds - Start.Seconds;
   end Delta_Time;

   procedure Sleep (Milliseconds : Integer) is
      Duration_To_Sleep : Duration := Float(Milliseconds) / 1000.0;
   begin
      delay Duration_To_Sleep;
   end Sleep;

end Time;