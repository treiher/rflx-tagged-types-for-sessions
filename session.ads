pragma Style_Checks ("N3aAbcdefhiIklnOprStux");

with RFLX.RFLX_Types;
with RFLX.Universal;
with RFLX.Fixed_Size.Simple_Message;
with RFLX.Test.Session;

package Session with
   SPARK_Mode,
   Elaborate_Body
is

   type Context is new RFLX.Test.Session.Context with null record;

   pragma Warnings (Off, """Ctx"" is not modified, could be IN");
   pragma Warnings (Off, "unused variable ""Ctx""");

   overriding
   procedure Get_Message_Type
      (Ctx    : in out Context;
       Result :    out RFLX.Universal.Option_Type);

   overriding
   procedure Create_Message
      (Ctx          : in out Context;
       Result       :    out RFLX.Fixed_Size.Simple_Message.Structure;
       Message_Type :        RFLX.Universal.Option_Type;
       Data         :        RFLX.RFLX_Types.Bytes);

   overriding
   procedure Valid_Message
      (Ctx           : in out Context;
       Valid_Message :    out Boolean;
       Message_Type  :        RFLX.Universal.Option_Type;
       Strict        :        Boolean);

   pragma Warnings (On, "unused variable ""Ctx""");
   pragma Warnings (On, """Ctx"" is not modified, could be IN");

end Session;
