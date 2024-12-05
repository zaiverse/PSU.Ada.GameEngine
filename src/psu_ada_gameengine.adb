
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Window; use Window;
with Win32; use Win32;
with System;
with Renderer; use Renderer;
with Ada.Real_Time; use Ada.Real_Time;
procedure Psu_Ada_Gameengine is
Title : Unbounded_String := To_Unbounded_String("Game Window");
GameWindow : Window_Access;
Buffer : Win32.Byte_Array(0 .. 800 * 600 * 4) := (others => 0);
Red  : Color := (R => Max, G => 0.0, B => 0.0, A => Max);
SkyBlue : Color := (R => 0.2, G => 0.3, B => 0.36, A => Max);

Start_Time, Stop_Time : Time;
Elapsed_Time          : Time_Span;
Counter : Float := 1.0;              
begin
   Start_Time := Clock;

   GameWindow := New_Window(800,600,Title);
   Put_Line ("Start Engine");

   declare
      Message        : MSG_Access := new MSG;
      Has_Msg        : Boolean := True;
      Lp_Result      : LRESULT;
   begin
      while Has_Msg loop
      Stop_Time := Clock;
      Elapsed_Time := Stop_Time - Start_Time;
      Lp_Result := Dispatch_Message (Message);
      Has_Msg := Get_Message (Message, System.Null_Address, 0, 0);

               --  -- Process emitted events here
               --  Process_Events(Manager.all);
      if To_Duration(Elapsed_Time) > 1.0/60.0 then
         Clear_Screen(Buffer, SkyBlue, 800, 600);
         Draw_Regular_Polygon(Buffer, 20, 50, 50.0 + Counter, 50.0 + Counter, Red, 800);
         GameWindow.Draw_Buffer(Buffer'Address);
         Start_Time := Clock;
         Counter := Counter + 0.0;
      end if;
      end loop;
   end;
end Psu_Ada_Gameengine;
