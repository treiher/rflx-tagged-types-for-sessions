with Lib;
pragma Elaborate (Lib);

procedure Main with
   SPARK_Mode,
   Pre => Lib.Session.Uninitialized
is
begin
   Lib.Run;
end Main;
