package body My_World is
   function ">=" (Object : Point; Item : Point) return Boolean is
   begin
      if Object.X >= Item.X and Object.Y >= Item.Y then
         return True;
      else
         return False;
      end if;
   end ">=";

   function "<=" (Object : Point; Item : Point) return Boolean is
   begin
       if Object.X <= Item.X and Object.Y <= Item.Y then
           return True;
       else
           return False;
       end if;
   end "<=";

   function ">" (Object : Point; Item : Point) return Boolean is
   begin
       if Object.X > Item.X and Object.Y > Item.Y then
           return True;
       else
           return False;
       end if;
   end ">";

   function "<" (Object : Point; Item : Point) return Boolean is
   begin
       if Object.X < Item.X and Object.Y < Item.Y then
           return True;
       else
           return False;
       end if;
   end "<";

   function Create_Point (X : Natural;
                          Y : Natural)
                          return Point is
      Object : Point;
   begin
      if X <= -1 or Y <= -1 then
         raise Value_Error with "Create_Point: Can not create Point with negative position";
      end if;

      Object.X := X;
      Object.Y := Y;

      return Object;
   end Create_Point;

   function Create_Point (From : Point;
                          To   : Point)
                          return Point is
      Object : Point;
   begin
      if To.X > From.X or To.Y > From.Y then
         raise Value_Error with "Create_Point: Second point must small than First Point";
      end if;

      Object.X := From.X - To.X;
      Object.Y := From.Y - To.Y;

      return Object;
   end Create_Point;
   function Distance (From : Point; To : Point) return Natural is
   begin
      return ((From.X - To.X) ** 2 + (From.Y - To.Y) ** 2) ** (1/2);
   end Distance;

   procedure Move_Up (Object : in out Point) is
   begin
      if Object.Y <= 0 then
         raise Value_Error with "Position Y Must big then 0";
      end if;

      Object.Y := Object.Y - 1;
   end Move_Up;

   procedure Move_Down (Object : in out Point) is
   begin
      Object.Y := Object.Y + 1;
   end Move_Down;

   procedure Move_Left (Object : in out Point) is
   begin
      if Object.X <= 0 then
         raise Value_Error with "Position X Must big then 0";
      end if;

      Object.X := Object.X - 1;
   end Move_Left;

   procedure Move_Right (Object : in out Point) is
   begin
      Object.X := Object.X + 1;
   end Move_Right;

   function Create_Rectangle (First_Point : Point;
                              Last_Point : Point)
                              return Rectangle is
      Object : Rectangle;
   begin
      Object.First_Point := First_Point;
      Object.Last_Point  := Last_Point;

      return Object;
   end Create_Rectangle;

   function Is_In_Rectangle (Object : Rectangle;
                             Item   : Point) return Boolean is
   begin
      if Item >= Object.First_Point and Item < Object.Last_Point then
         return True;
      else
         return False;
      end if;
   end Is_In_Rectangle;
end My_World;
