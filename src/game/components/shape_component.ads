with Components;
with Shapes;

package Shape_Component is
   type Shape_Component is new Components.Component_Type with record
      Shape : Shapes.Shape_Access;
   end record;

   -- Access type for Shape_Component
   type Shape_Component_Access is access all Shape_Component;
end Shape_Component;