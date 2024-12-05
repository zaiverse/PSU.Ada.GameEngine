with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;

package body ECS.Event_Manager is
   -- Event Queue Type
   package Event_Vectors is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => ECS.Event.Event_T);
   Event_Queue : Event_Vectors.Vector;

   -- Procedure to emit an event
   overriding procedure Emit_Event (Handler : in out Platform_Event_Handler; Event : ECS.Event.Event_T) is
   begin
      Event_Queue.Append (Event);
   end Emit_Event;

   -- Procedure to process all queued events
   overriding procedure Process_Events (Handler : in out Platform_Event_Handler) is
      Current_Event : ECS.Event.Event_T;
   begin
      while not Event_Queue.Is_Empty loop
         Current_Event := Event_Queue.First_Element;
         Event_Queue.Delete_First;

         -- Process the event
         Put_Line ("Processing Event: " & Current_Event'Image);
      end loop;
   end Process_Events;
end ECS.Event_Manager;