pragma Restrictions (No_Streams);
pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Test.Session_Allocator;
with RFLX.RFLX_Types;
with RFLX.Universal;
with RFLX.Universal.Message;
with RFLX.Fixed_Size;
with RFLX.Fixed_Size.Simple_Message;

package RFLX.Test.Session with
  SPARK_Mode
is

   use type RFLX.RFLX_Types.Index;

   use type RFLX.RFLX_Types.Length;

   type Channel is (C_Channel);

   type State is (S_Start, S_Process, S_Reply, S_Terminated);

   type Context_State is private;

   type Context is abstract tagged
      record
         State : Context_State;
      end record;

   procedure Get_Message_Type (Ctx : in out Context; Get_Message_Type : out RFLX.Universal.Option_Type) is abstract with
     Pre'Class =>
       Initialized (Ctx)
       and not Get_Message_Type'Constrained,
     Post'Class =>
       Initialized (Ctx);

   procedure Create_Message (Ctx : in out Context; Create_Message : out RFLX.Fixed_Size.Simple_Message.Structure; Message_Type : RFLX.Universal.Option_Type; Data : RFLX_Types.Bytes) is abstract with
     Pre'Class =>
       Initialized (Ctx),
     Post'Class =>
       Initialized (Ctx);

   procedure Valid_Message (Ctx : in out Context; Valid_Message : out Boolean; Message_Type : RFLX.Universal.Option_Type; Strict : Boolean) is abstract with
     Pre'Class =>
       Initialized (Ctx),
     Post'Class =>
       Initialized (Ctx);

   function Uninitialized (Ctx : Context'Class) return Boolean;

   function Initialized (Ctx : Context'Class) return Boolean;

   function Active (Ctx : Context'Class) return Boolean;

   procedure Initialize (Ctx : in out Context'Class) with
     Pre =>
       Uninitialized (Ctx),
     Post =>
       Initialized (Ctx)
       and Active (Ctx);

   procedure Finalize (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Uninitialized (Ctx)
       and not Active (Ctx);

   pragma Warnings (Off, "subprogram ""Tick"" has no effect");

   procedure Tick (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx);

   pragma Warnings (On, "subprogram ""Tick"" has no effect");

   pragma Warnings (Off, "subprogram ""Run"" has no effect");

   procedure Run (Ctx : in out Context'Class) with
     Pre =>
       Initialized (Ctx),
     Post =>
       Initialized (Ctx);

   pragma Warnings (On, "subprogram ""Run"" has no effect");

   function Next_State (Ctx : Context'Class) return State;

   function Has_Data (Ctx : Context'Class; Chan : Channel) return Boolean with
     Pre =>
       Initialized (Ctx);

   function Read_Buffer_Size (Ctx : Context'Class; Chan : Channel) return RFLX_Types.Length with
     Pre =>
       Initialized (Ctx)
       and then Has_Data (Ctx, Chan);

   procedure Read (Ctx : Context'Class; Chan : Channel; Buffer : out RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) with
     Pre =>
       Initialized (Ctx)
       and then Has_Data (Ctx, Chan)
       and then Buffer'Length > 0
       and then Offset <= RFLX_Types.Length'Last - Buffer'Length
       and then Buffer'Length + Offset <= Read_Buffer_Size (Ctx, Chan),
     Post =>
       Initialized (Ctx);

   function Needs_Data (Ctx : Context'Class; Chan : Channel) return Boolean with
     Pre =>
       Initialized (Ctx);

   function Write_Buffer_Size (Unused_Ctx : Context'Class; Chan : Channel) return RFLX_Types.Length;

   procedure Write (Ctx : in out Context'Class; Chan : Channel; Buffer : RFLX_Types.Bytes; Offset : RFLX_Types.Length := 0) with
     Pre =>
       Initialized (Ctx)
       and then Needs_Data (Ctx, Chan)
       and then Buffer'Length > 0
       and then Offset <= RFLX_Types.Length'Last - Buffer'Length
       and then Buffer'Length + Offset <= Write_Buffer_Size (Ctx, Chan),
     Post =>
       Initialized (Ctx);

private

   type Context_State is
      record
         P_Next_State : State := S_Start;
         Message_Ctx : Universal.Message.Context;
         Fixed_Size_Message_Ctx : Fixed_Size.Simple_Message.Context;
         Slots : Test.Session_Allocator.Slots;
         Memory : Test.Session_Allocator.Memory;
      end record;

   function Uninitialized (Ctx : Context'Class) return Boolean is
     (not Universal.Message.Has_Buffer (Ctx.State.Message_Ctx)
      and not Fixed_Size.Simple_Message.Has_Buffer (Ctx.State.Fixed_Size_Message_Ctx)
      and Test.Session_Allocator.Uninitialized (Ctx.State.Slots));

   function Initialized (Ctx : Context'Class) return Boolean is
     (Universal.Message.Has_Buffer (Ctx.State.Message_Ctx)
      and then Ctx.State.Message_Ctx.Buffer_First = RFLX_Types.Index'First
      and then Ctx.State.Message_Ctx.Buffer_Last = RFLX_Types.Index'First + 4095
      and then Fixed_Size.Simple_Message.Has_Buffer (Ctx.State.Fixed_Size_Message_Ctx)
      and then Ctx.State.Fixed_Size_Message_Ctx.Buffer_First = RFLX_Types.Index'First
      and then Ctx.State.Fixed_Size_Message_Ctx.Buffer_Last = RFLX_Types.Index'First + 4095
      and then Test.Session_Allocator.Global_Allocated (Ctx.State.Slots));

   function Active (Ctx : Context'Class) return Boolean is
     (Ctx.State.P_Next_State /= S_Terminated);

   function Next_State (Ctx : Context'Class) return State is
     (Ctx.State.P_Next_State);

end RFLX.Test.Session;
