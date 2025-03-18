with ECS.System; use ECS.System;
  
package ECS.System.Animation is
   type Animation_T is new System_T with null record;
   overriding
   procedure Execute ( Self : in out Animation_T;
                       Dt   : Duration;
                       Manager    : access Entity_Manager_T'Class );
end ECS.System.Animation;