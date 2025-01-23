with ECS.System; use ECS.System;

package ECS.System.Movement is 

   type Mover_T is new System_T with record
      Width : Natural;
      Height : Natural;
   end record;

   overriding
   procedure Execute ( Self : in out Mover_T;
                       Dt   : Duration; 
                       Manager    : access Entity_Manager_T'Class );

end ECS.System.Movement;