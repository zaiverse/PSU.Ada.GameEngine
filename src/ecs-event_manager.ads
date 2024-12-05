with ECS.Event; use ECS.Event;

package ECS.Event_Manager is
   -- Abstract Event Handler for the Event Manager
   type Platform_Event_Handler is new ECS.Event.Event_Handler with null record;

   -- Procedure to emit an event
   procedure Emit_Event (Handler : in out Platform_Event_Handler; Event : ECS.Event.Event_T);

   -- Procedure to process all queued events
   procedure Process_Events (Handler : in out Platform_Event_Handler);
end ECS.Event_Manager;