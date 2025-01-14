package Renderable is
   type Renderable_Type is abstract tagged null record;
   type Renderable_Access is access all Renderable_Type'Class;
end Renderable;