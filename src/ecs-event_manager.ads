with ECS.Event; use ECS.Event;

package ECS.Event_Manager is
   -- Abstract Event Handler for the Event Manager
   type Platform_Event_Handler is new ECS.Event.Event_Handler with null record;
   type Platform_Event_Handler_Access is access all Platform_Event_Handler;

   -- Procedure to emit an event
   procedure Emit_Event (Handler : in out Platform_Event_Handler; Event : ECS.Event.Event_T);

   -- Procedure to process all queued events
   procedure Process_Events (Handler : in out Platform_Event_Handler);

   -- Function to get the next event in the queue
   function Get_Next_Event(Handler : in out Platform_Event_Handler) return Event_T;
end ECS.Event_Manager;