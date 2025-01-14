with Bitmap;

package Window is
   --  pragma Elaborate_Body;

   type Window_Type is abstract tagged limited null record;

   procedure Initialize (Window : in out Window_Type; Width, Height : Positive) is abstract;
   procedure Present_Buffer (Window : in out Window_Type; Buffer : Bitmap.Bitmap_Buffer) is abstract;
   procedure Poll_Events (Window : in out Window_Type) is abstract;
end Window;