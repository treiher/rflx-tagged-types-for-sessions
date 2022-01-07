pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Universal;
use type RFLX.Universal.Message_Type;
use type RFLX.Universal.Length;
with RFLX.RFLX_Types;
use type RFLX.RFLX_Types.Bit_Length;

package body RFLX.Test.Session with
  SPARK_Mode
is

   procedure Start (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      Universal.Message.Verify_Message (Ctx.State.Message_Ctx);
      if
         (Universal.Message.Structural_Valid_Message (Ctx.State.Message_Ctx)
          and then Universal.Message.Get_Message_Type (Ctx.State.Message_Ctx) = Universal.MT_Data)
         and then Universal.Message.Get_Length (Ctx.State.Message_Ctx) = 3
      then
         Ctx.State.P_Next_State := S_Process;
      else
         Ctx.State.P_Next_State := S_Terminated;
      end if;
   end Start;

   procedure Process (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
      Valid : Boolean;
      Message_Type : Universal.Option_Type;
   begin
      Get_Message_Type (Ctx, Message_Type);
      Valid_Message (Ctx, Valid, Message_Type, True);
      if Universal.Message.Structural_Valid (Ctx.State.Message_Ctx, Universal.Message.F_Data) then
         declare
            Fixed_Size_Message : Fixed_Size.Simple_Message.Structure;
         begin
            Create_Message (Ctx, Fixed_Size_Message, Message_Type, Universal.Message.Get_Data (Ctx.State.Message_Ctx));
            Fixed_Size.Simple_Message.To_Context (Fixed_Size_Message, Ctx.State.Fixed_Size_Message_Ctx);
         end;
      else
         Ctx.State.P_Next_State := S_Terminated;
         return;
      end if;
      if not Valid then
         Ctx.State.P_Next_State := S_Terminated;
      else
         Ctx.State.P_Next_State := S_Reply;
      end if;
   end Process;

   procedure Reply (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      Ctx.State.P_Next_State := S_Terminated;
   end Reply;

   procedure Initialize (Ctx : in out Context'Class) is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
      Fixed_Size_Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      Ctx.State.Memory.Slot_1 := (others => 0);
      Ctx.State.Memory.Slot_2 := (others => 0);
      Test.Session_Allocator.Initialize (Ctx.State.Slots, Ctx.State.Memory);
      Message_Buffer := Ctx.State.Slots.Slot_Ptr_1;
      pragma Warnings (Off, "unused assignment");
      Ctx.State.Slots.Slot_Ptr_1 := null;
      pragma Warnings (On, "unused assignment");
      Universal.Message.Initialize (Ctx.State.Message_Ctx, Message_Buffer);
      Fixed_Size_Message_Buffer := Ctx.State.Slots.Slot_Ptr_2;
      pragma Warnings (Off, "unused assignment");
      Ctx.State.Slots.Slot_Ptr_2 := null;
      pragma Warnings (On, "unused assignment");
      Fixed_Size.Simple_Message.Initialize (Ctx.State.Fixed_Size_Message_Ctx, Fixed_Size_Message_Buffer);
      Ctx.State.P_Next_State := S_Start;
   end Initialize;

   procedure Finalize (Ctx : in out Context'Class) is
      Message_Buffer : RFLX_Types.Bytes_Ptr;
      Fixed_Size_Message_Buffer : RFLX_Types.Bytes_Ptr;
   begin
      pragma Warnings (Off, "unused assignment to ""Message_Ctx""");
      pragma Warnings (Off, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Universal.Message.Take_Buffer (Ctx.State.Message_Ctx, Message_Buffer);
      pragma Warnings (On, """Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Message_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Ctx.State.Slots.Slot_Ptr_1 := Message_Buffer;
      pragma Warnings (On, "unused assignment");
      pragma Warnings (Off, "unused assignment to ""Fixed_Size_Message_Ctx""");
      pragma Warnings (Off, """Fixed_Size_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      Fixed_Size.Simple_Message.Take_Buffer (Ctx.State.Fixed_Size_Message_Ctx, Fixed_Size_Message_Buffer);
      pragma Warnings (On, """Fixed_Size_Message_Ctx"" is set by ""Take_Buffer"" but not used after the call");
      pragma Warnings (On, "unused assignment to ""Fixed_Size_Message_Ctx""");
      pragma Warnings (Off, "unused assignment");
      Ctx.State.Slots.Slot_Ptr_2 := Fixed_Size_Message_Buffer;
      pragma Warnings (On, "unused assignment");
      Test.Session_Allocator.Finalize (Ctx.State.Slots);
      Ctx.State.P_Next_State := S_Terminated;
   end Finalize;

   procedure Reset_Messages_Before_Write (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx)
   is
   begin
      case Ctx.State.P_Next_State is
         when S_Start =>
            Universal.Message.Reset (Ctx.State.Message_Ctx, Ctx.State.Message_Ctx.First, Ctx.State.Message_Ctx.First - 1);
         when S_Process | S_Reply | S_Terminated =>
            null;
      end case;
   end Reset_Messages_Before_Write;

   procedure Tick (Ctx : in out Context'Class) is
   begin
      case Ctx.State.P_Next_State is
         when S_Start =>
            Start (Ctx);
         when S_Process =>
            Process (Ctx);
         when S_Reply =>
            Reply (Ctx);
         when S_Terminated =>
            null;
      end case;
      Reset_Messages_Before_Write (Ctx);
   end Tick;

   function In_IO_State (Ctx : Context'Class) return Boolean is
     (Ctx.State.P_Next_State in S_Start | S_Reply);

   procedure Run (Ctx : in out Context'Class) is
   begin
      Tick (Ctx);
      while
         Active (Ctx)
         and not In_IO_State (Ctx)
      loop
         pragma Loop_Invariant (Initialized (Ctx));
         Tick (Ctx);
      end loop;
   end Run;

   function Has_Data (Ctx : Context'Class; Chan : Channel) return Boolean is
     ((case Chan is
          when C_Channel =>
             (case Ctx.State.P_Next_State is
                 when S_Reply =>
                    Fixed_Size.Simple_Message.Structural_Valid_Message (Ctx.State.Fixed_Size_Message_Ctx)
                    and Fixed_Size.Simple_Message.Byte_Size (Ctx.State.Fixed_Size_Message_Ctx) > 0,
                 when others =>
                    False)));

   function Read_Buffer_Size (Ctx : Context'Class; Chan : Channel) return RFLX_Types.Length is
     ((case Chan is
          when C_Channel =>
             (case Ctx.State.P_Next_State is
                 when S_Reply =>
                    Fixed_Size.Simple_Message.Byte_Size (Ctx.State.Fixed_Size_Message_Ctx),
                 when others =>
                    raise Program_Error)));

   procedure Read (Ctx : Context'Class; Chan : Channel; Buffer : out RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) is
      function Read_Pre (Message_Buffer : RFLX_Types.Bytes) return Boolean is
        (Buffer'Length > 0
         and then Offset < Message_Buffer'Length);
      procedure Read (Message_Buffer : RFLX_Types.Bytes) with
        Pre =>
          Read_Pre (Message_Buffer)
      is
         Length : constant RFLX_Types.Index := RFLX_Types.Index (RFLX_Types.Length'Min (Buffer'Length, Message_Buffer'Length - Offset));
         Buffer_Last : constant RFLX_Types.Index := Buffer'First - 1 + Length;
      begin
         Buffer (Buffer'First .. RFLX_Types.Index (Buffer_Last)) := Message_Buffer (RFLX_Types.Index (RFLX_Types.Length (Message_Buffer'First) + Offset) .. Message_Buffer'First - 2 + RFLX_Types.Index (Offset + 1) + Length);
      end Read;
      procedure Fixed_Size_Simple_Message_Read is new Fixed_Size.Simple_Message.Generic_Read (Read, Read_Pre);
   begin
      Buffer := (others => 0);
      case Chan is
         when C_Channel =>
            case Ctx.State.P_Next_State is
               when S_Reply =>
                  Fixed_Size_Simple_Message_Read (Ctx.State.Fixed_Size_Message_Ctx);
               when others =>
                  raise Program_Error;
            end case;
      end case;
   end Read;

   function Needs_Data (Ctx : Context'Class; Chan : Channel) return Boolean is
     ((case Chan is
          when C_Channel =>
             (case Ctx.State.P_Next_State is
                 when S_Start =>
                    True,
                 when others =>
                    False)));

   function Write_Buffer_Size (Unused_Ctx : Context'Class; Chan : Channel) return RFLX_Types.Length is
     ((case Chan is
          when C_Channel =>
             4096));

   procedure Write (Ctx : in out Context'Class; Chan : Channel; Buffer : RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) is
      Write_Buffer_Length : constant RFLX_Types.Length := Write_Buffer_Size (Ctx, Chan);
      function Write_Pre (Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length) return Boolean is
        (Buffer'Length > 0
         and then Context_Buffer_Length = Write_Buffer_Length
         and then Offset <= RFLX_Types.Length'Last - Buffer'Length
         and then Buffer'Length + Offset <= Write_Buffer_Length);
      procedure Write (Message_Buffer : out RFLX_Types.Bytes; Length : out RFLX_Types.Length; Context_Buffer_Length : RFLX_Types.Length; Offset : RFLX_Types.Length) with
        Pre =>
          Write_Pre (Context_Buffer_Length, Offset)
          and then Offset <= RFLX_Types.Length'Last - Message_Buffer'Length
          and then Message_Buffer'Length + Offset = Write_Buffer_Length,
        Post =>
          Length <= Message_Buffer'Length
          and Message_Buffer'Initialized,
        Relaxed_Initialization =>
          Message_Buffer
      is
      begin
         Length := Buffer'Length;
         Message_Buffer (Message_Buffer'First .. RFLX_Types.Index (RFLX_Types.Length (Message_Buffer'First) - 1 + Length)) := Buffer;
         Message_Buffer (RFLX_Types.Index (RFLX_Types.Length (Message_Buffer'First) + Length) .. Message_Buffer'Last) := (others => 0);
      end Write;
      procedure Universal_Message_Write is new Universal.Message.Generic_Write (Write, Write_Pre);
   begin
      case Chan is
         when C_Channel =>
            case Ctx.State.P_Next_State is
               when S_Start =>
                  Universal_Message_Write (Ctx.State.Message_Ctx, Offset);
               when others =>
                  raise Program_Error;
            end case;
      end case;
   end Write;

end RFLX.Test.Session;
