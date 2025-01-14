package body Systems is

   procedure Initialize (Manager: in out System_Manager) is
   begin
      Manager.Systems.Clear;
   end Initialize;

   procedure Register_System (Manager: in out System_Manager; System: System_Access) is
   begin
      Manager.Systems.Append (System);
   end Register_System;

   procedure Update_System (Manager : in out System_Manager; Delta_Time : Float) is
      Cursor : System_Vectors.Cursor;
   begin
      Cursor := Manager.Systems.First;
      while Manager.Systems.Has_Element(Cursor) loop
         Manager.Systems.Element(Cursor).Process(Delta_Time);
         Cursor := Manager.Systems.Next(Cursor);
      end loop;
   end Update_System;

end Systems;