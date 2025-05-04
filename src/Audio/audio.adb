package body Audio is
   use Ada.Text_IO;
   use Interfaces.C;
   use Interfaces.C.Strings;
   use Win32;

   procedure Play_Audio(File_Path : String) is
      Alias       : constant String := "sfx_" & Ada.Strings.Fixed.Trim(Natural'Image(Audio_ID), Ada.Strings.Left);
      Alias_UB    : constant Unbounded_String := Ada.Strings.Unbounded.To_Unbounded_String(Alias);

      Open_String : constant String := "open """ & File_Path & """ alias " & Alias;
      Play_String : constant String := "play " & Alias & " repeat"; -- Add 'repeat' to loop the audio

      Open_Cmd    : chars_ptr := New_String(Open_String);
      Play_Cmd    : chars_ptr := New_String(Play_String);
      Result      : int;

   begin
      Audio_ID := Audio_ID + 1;
      Ada.Text_IO.Put_Line(Play_String);

      declare
         Result_Open : int := mciSendStringA(TO_LPCSTR(Open_Cmd), LPSTR(Null_Ptr), 0, System.Null_Address);
         Result_Play : int := mciSendStringA(TO_LPCSTR(Play_Cmd), LPSTR(Null_Ptr), 0, System.Null_Address);
      begin
         Free(Open_Cmd); -- Free allocated memory
         Free(Play_Cmd); -- Free allocated memory

         if Result_Open /= 0 then
            Ada.Text_IO.Put_Line("Failed to open sound: " & File_Path);
         elsif Result_Play /= 0 then
            Ada.Text_IO.Put_Line("Failed to play sound: " & File_Path);
         else
            Tracked_Aliases.Append(Alias_UB);
            Is_Audio_Playing := True; -- Set audio as playing
         end if;
      end;

   end Play_Audio;

   procedure Cleanup_Sounds is
   begin
      for Alias_UB of Tracked_Aliases loop
         declare
            Alias          : constant String := To_String(Alias_UB);
            Close_Cmd      : chars_ptr := New_String("close " & Alias);
            Result_Close   : int := mciSendStringA(TO_LPCSTR(Close_Cmd), LPSTR(Null_Ptr), 0, System.Null_Address);
         begin
            Free(Close_Cmd); -- Free allocated memory

            if Result_Close /= 0 then
               Ada.Text_IO.Put_Line("Failed to close alias: " & Alias);
            end if;
         end;
      end loop;

      Tracked_Aliases.Clear;
      Audio_ID := 0;
      Is_Audio_Playing := False; -- Reset audio playing status
   end Cleanup_Sounds;

   procedure Stop_Audio is
   begin
      for Alias_UB of Tracked_Aliases loop
         declare
            Alias     : constant String := To_String(Alias_UB);
            Stop_Cmd  : chars_ptr := New_String("stop " & Alias);
            Result    : int := mciSendStringA(TO_LPCSTR(Stop_Cmd), LPSTR(Null_Ptr), 0, System.Null_Address);
         begin
            Free(Stop_Cmd); -- Free allocated memory

            if Result = 0 then
               Is_Audio_Playing := False; -- Set audio as not playing
            end if;
         end;
      end loop;
   end Stop_Audio;

   function Is_Playing return Boolean is
   begin
      return Is_Audio_Playing;
   end Is_Playing;

end Audio;
