pragma Style_Checks ("N3aAbcdefhiIklnOprStux");
pragma Warnings (Off, "redundant conversion");
with RFLX.Test.Session;
with Func;

package Lib with
  SPARK_Mode,
  Initial_Condition =>
    Session.Uninitialized
is

   package Session is new RFLX.Test.Session (Func.Get_Message_Type, Func.Create_Message, Func.Valid_Message);

   procedure Run with
     Pre =>
       Session.Uninitialized,
     Post =>
       Session.Uninitialized;

end Lib;
