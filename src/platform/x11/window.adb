with Interfaces.C.Strings;
with Interfaces;
with System;

with X11; use X11;

package body Window is

   package ICS renames Interfaces.C.Strings;
   package IC renames interfaces.C;

   procedure Window is
      Event_Ptr  : X_Event_Access := new X_Event;
      Display    : X_Display_Access := X_Open_Display (ICS.Null_Ptr);
      Screen_num : IC.Int := X_Default_Screen (Display);
      Win        : X_Window :=
        X_Create_Simple_Window
          (Display,
           X_Default_Root_Window (Display),
           50,
           50,
           250,
           250,
           1,
           X_Black_Pixel (Display, Screen_num),
           X_White_Pixel (Display, Screen_num));
   begin
      X_Map_Window (Display, Win);
      X_Select_Input (Display, Win, Exposure_Mask);
      loop
         X_Next_Event (Display, Event_Ptr);
         case Event_Ptr.all.Event_Type is
            when Expose =>
               X_Draw_String
                 (Display,
                  X_Drawable (Win),
                  X_Default_Graphic_Context (Display, 0),
                  100,
                  100,
                  IC.To_C ("Thanks for Watching!"),
                  20);
            when others =>
               null;
         end case;
      end loop;
   end Window;


   task Background_Task is

   end Background_Task;

   task body Background_Task is
   begin
      Window;
   end Background_Task;
end Window;
