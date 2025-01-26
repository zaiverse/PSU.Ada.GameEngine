with ECS.System; use ECS.System;

package ECS.System.Render is 

   type Render_T is new System_T with record
      Width : Natural;
      Height : Natural;
      Buffer : Byte_Array_Access;
   end record;

   overriding
   procedure Execute (Self : in out Render_T;
                      Dt   : Duration;
                      Manager : access Entity_Manager_T'Class );

end ECS.System.Render;