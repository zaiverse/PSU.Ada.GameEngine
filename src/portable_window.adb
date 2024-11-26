with Window;
with ECS.System; use ECS.System;
with ECS.Event_Manager; use ECS.Event_Manager;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

procedure Portable_Window is
   Manager : aliased ECS.Event_Manager.Platform_Event_Handler;
begin
   -- Start the window
   Put_Line("Starting Portable Window...");

   -- Event processing loop
   loop
      -- Process events from the Event_Manager
      Process_Events(Manager);

      -- Timing logic to avoid busy-waiting
      delay 0.1;
   end loop;
end Portable_Window;
