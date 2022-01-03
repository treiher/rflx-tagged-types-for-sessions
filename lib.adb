pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with Ada.Text_IO;
with RFLX.RFLX_Types;

package body Lib with
  SPARK_Mode
is

   function Image (Chan : Session.Channel) return String is
     ((case Chan is
          when Session.C_Channel =>
             "Channel"));

   procedure Print (Prefix : String; Buffer : RFLX.RFLX_Types.Bytes) is
   begin
      Ada.Text_IO.Put (Prefix & ":");
      for B of Buffer loop
         Ada.Text_IO.Put (B'Image);
      end loop;
      Ada.Text_IO.New_Line;
   end Print;

   procedure Read (Chan : Session.Channel) with
     Pre =>
       Session.Initialized
       and then Session.Has_Data (Chan),
     Post =>
       Session.Initialized
   is
      use type RFLX.RFLX_Types.Index;
      use type RFLX.RFLX_Types.Length;
      Buffer : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095) := (others => 0);
   begin
      if Buffer'Length >= Session.Read_Buffer_Size (Chan) then
         Session.Read (Chan, Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (Session.Read_Buffer_Size (Chan) + 1)));
         Print ("Read " & Image (Chan), Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (Session.Read_Buffer_Size (Chan) + 1)));
      else
         Ada.Text_IO.Put_Line ("Read " & Chan'Image & ": buffer too small");
      end if;
   end Read;

   type Number_Per_Channel is array (Session.Channel) of Natural;

   Written_Messages : Number_Per_Channel := (others => 0);

   function Next_Message (Chan : Session.Channel) return RFLX.RFLX_Types.Bytes is
      None : constant RFLX.RFLX_Types.Bytes (1 .. 0) := (others => 0);
      Message : constant RFLX.RFLX_Types.Bytes := (if Written_Messages (Chan) = 0 then (1, 0, 3, 0, 1, 2) else None);
   begin
      return Message;
   end Next_Message;

   procedure Write (Chan : Session.Channel) with
     Pre =>
       Session.Initialized
       and then Session.Needs_Data (Chan),
     Post =>
       Session.Initialized
   is
      use type RFLX.RFLX_Types.Length;
      Message : constant RFLX.RFLX_Types.Bytes := Next_Message (Chan);
   begin
      if
         Message'Length > 0
         and Message'Length <= Session.Write_Buffer_Size (Chan)
      then
         Print ("Write " & Image (Chan), Message);
         Session.Write (Chan, Message);
         if Written_Messages (Chan) < Natural'Last then
            Written_Messages (Chan) := Written_Messages (Chan) + 1;
         end if;
      end if;
   end Write;

   procedure Run is
   begin
      Session.Initialize;
      while Session.Active loop
         pragma Loop_Invariant (Session.Initialized);
         for C in Session.Channel'Range loop
            pragma Loop_Invariant (Session.Initialized);
            if Session.Has_Data (C) then
               Read (C);
            end if;
            if Session.Needs_Data (C) then
               Write (C);
            end if;
         end loop;
         Session.Run;
      end loop;
      Session.Finalize;
   end Run;

end Lib;
