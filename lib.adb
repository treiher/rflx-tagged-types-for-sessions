pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with Ada.Text_IO;
with RFLX.RFLX_Types;
with RFLX.Test.Session;
with Session;

package body Lib with
  SPARK_Mode
is

   function Image (Chan : RFLX.Test.Session.Channel) return String is
     ((case Chan is
          when RFLX.Test.Session.C_Channel =>
             "Channel"));

   procedure Print (Prefix : String; Buffer : RFLX.RFLX_Types.Bytes) is
   begin
      Ada.Text_IO.Put (Prefix & ":");
      for B of Buffer loop
         Ada.Text_IO.Put (B'Image);
      end loop;
      Ada.Text_IO.New_Line;
   end Print;

   procedure Read (Ctx : Session.Context; Chan : RFLX.Test.Session.Channel) with
     Pre =>
       RFLX.Test.Session.Initialized (Ctx)
       and then RFLX.Test.Session.Has_Data (Ctx, Chan),
     Post =>
       RFLX.Test.Session.Initialized (Ctx)
   is
      use type RFLX.RFLX_Types.Index;
      use type RFLX.RFLX_Types.Length;
      Buffer : RFLX.RFLX_Types.Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095) := (others => 0);
      Size : constant RFLX.RFLX_Types.Length := RFLX.Test.Session.Read_Buffer_Size (Ctx, Chan);
   begin
      if Size = 0 then
         Ada.Text_IO.Put_Line ("Read " & Chan'Image & ": read buffer size is 0");
         return;
      end if;
      if Buffer'Length < Size then
         Ada.Text_IO.Put_Line ("Read " & Chan'Image & ": buffer too small");
         return;
      end if;
      RFLX.Test.Session.Read (Ctx, Chan, Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (Size + 1)));
      Print ("Read " & Image (Chan), Buffer (Buffer'First .. Buffer'First - 2 + RFLX.RFLX_Types.Index (Size + 1)));
   end Read;

   type Number_Per_Channel is array (RFLX.Test.Session.Channel) of Natural;

   Written_Messages : Number_Per_Channel := (others => 0);

   function Next_Message (Chan : RFLX.Test.Session.Channel) return RFLX.RFLX_Types.Bytes is
      None : constant RFLX.RFLX_Types.Bytes (1 .. 0) := (others => 0);
      Message : constant RFLX.RFLX_Types.Bytes := (if Written_Messages (Chan) = 0 then (1, 0, 3, 0, 1, 2) else None);
   begin
      return Message;
   end Next_Message;

   procedure Write (Ctx : in out Session.Context; Chan : RFLX.Test.Session.Channel) with
     Pre =>
       RFLX.Test.Session.Initialized (Ctx)
       and then RFLX.Test.Session.Needs_Data (Ctx, Chan),
     Post =>
       RFLX.Test.Session.Initialized (Ctx)
   is
      use type RFLX.RFLX_Types.Length;
      Message : constant RFLX.RFLX_Types.Bytes := Next_Message (Chan);
   begin
      if
         Message'Length > 0
         and Message'Length <= RFLX.Test.Session.Write_Buffer_Size (Ctx, Chan)
      then
         Print ("Write " & Image (Chan), Message);
         RFLX.Test.Session.Write (Ctx, Chan, Message);
         if Written_Messages (Chan) < Natural'Last then
            Written_Messages (Chan) := Written_Messages (Chan) + 1;
         end if;
      end if;
   end Write;

   procedure Run is
      Ctx : Session.Context;
   begin
      RFLX.Test.Session.Initialize (Ctx);
      while RFLX.Test.Session.Active (Ctx) loop
         pragma Loop_Invariant (RFLX.Test.Session.Initialized (Ctx));
         for C in RFLX.Test.Session.Channel'Range loop
            pragma Loop_Invariant (RFLX.Test.Session.Initialized (Ctx));
            if RFLX.Test.Session.Has_Data (Ctx, C) then
               Read (Ctx, C);
            end if;
            if RFLX.Test.Session.Needs_Data (Ctx, C) then
               Write (Ctx, C);
            end if;
         end loop;
         RFLX.Test.Session.Run (Ctx);
      end loop;
      pragma Warnings (Off, "statement has no effect");
      pragma Warnings (Off, """Ctx"" is set by ""Finalize"" but not used after the call");
      RFLX.Test.Session.Finalize (Ctx);
      pragma Warnings (On, """Ctx"" is set by ""Finalize"" but not used after the call");
      pragma Warnings (On, "statement has no effect");
   end Run;

end Lib;
