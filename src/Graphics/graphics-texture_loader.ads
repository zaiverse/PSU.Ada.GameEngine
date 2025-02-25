with QOI; use QOI;
with GNAT.OS_Lib;

with Graphics.Renderer; use Graphics.Renderer;

package Graphics.Texture_Loader is

   type QOI_Image_Data is
      record
         Desc : QOI.QOI_Desc;
         Data : Storage_Array_Access;
      end record;

   function Load_QOI (Filename : String) return QOI_Image_Data;

end Graphics.Texture_Loader;