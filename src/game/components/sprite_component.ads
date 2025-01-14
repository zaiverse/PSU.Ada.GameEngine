package Sprite_Component is
   type Sprite_ID is new Natural;
   type Sprite_Data is record
      Width      : Positive;
      Height     : Positive;
      Pixel_Data : array (1 .. Width * Height) of Bitmap.Color;
      -- Additional properties can be added here
   end record;

   type Sprite_Access is access all Sprite_Data;

   procedure Load_Sprite (ID : in Sprite_ID; Filepath : in String);
   function Get_Sprite (ID : in Sprite_ID) return Sprite_Access;
   -- Other sprite management procedures...
end Sprite_Component;
