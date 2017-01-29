package body My_World is
   function Create_Point (X : Natural;
                          Y : Natural)
                          return Point
   is
      Object : Point;
   begin
      Object.X := X;
      Object.Y := Y;

      return Object;
   end Create_Point;

   procedure Move_Up (Object : in out Point) is
   begin
      Object.Y := Object.Y - 1;
   end Move_Up;

   procedure Move_Down (Object : in out Point) is
   begin
      Object.Y := Object.Y + 1;
   end Move_Down;

   procedure Move_Left (Object : in out Point) is
   begin
      Object.X := Object.X - 1;
   end Move_Left;

   procedure Move_Right (Object : in out Point) is
   begin
      Object.X := Object.X + 1;
   end Move_Right;

end My_World;
