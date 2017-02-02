package body My_World is
   function ">=" (Left, Right : Point) return Boolean is
   begin
      if Left.X >= Right.X and Left.Y >= Right.Y then
         return True;
      else
         return False;
      end if;
   end ">=";

   function "<=" (Left, Right : Point) return Boolean is
   begin
      if Left.X <= Right.X and Left.Y <= Right.Y then
         return True;
      else
         return False;
      end if;
   end "<=";

   function ">" (Left, Right : Point) return Boolean is
   begin
      if Left.X > Right.X and Left.Y > Right.Y then
         return True;
      else
         return False;
      end if;
   end ">";

   function "<" (Left, Right : Point) return Boolean is
   begin
      if Left.X < Right.X and Left.Y < Right.Y then
         return True;
      else
         return False;
      end if;
   end "<";

   function Create_Point (X : Natural; Y : Natural) return Point is
      New_Point : Point;
   begin
      New_Point.X := X;
      New_Point.Y := Y;

      return New_Point;
   end Create_Point;

   function Create_Point (From : Point; To : Point) return Point is
      New_Point : Point;
   begin
      New_Point.X := From.X - To.X;
      New_Point.Y := From.Y - To.Y;

      return New_Point;
   end Create_Point;

   function Distance (From : Point; To : Point) return Natural is
   begin
      return ((From.X - To.X)**2 + (From.Y - To.Y)**2)**(1 / 2);
   end Distance;

   procedure Move_Up (Current_Point : in out Point) is
   begin
      Current_Point.Y := Current_Point.Y - 1;
   end Move_Up;

   procedure Move_Down (Current_Point : in out Point) is
   begin
      Current_Point.Y := Current_Point.Y + 1;
   end Move_Down;

   procedure Move_Left (Current_Point : in out Point) is
   begin
      Current_Point.X := Current_Point.X - 1;
   end Move_Left;

   procedure Move_Right (Current_Point : in out Point) is
   begin
      Current_Point.X := Current_Point.X + 1;
   end Move_Right;

   function Get_X (Current_Point : in Point) return Natural is
   begin
      return Current_Point.X;
   end Get_X;

   function Get_Y (Current_Point : in Point) return Natural is
   begin
      return Current_Point.Y;
   end Get_Y;

   procedure Set_X (Current_Point : in out Point; X : Natural) is
   begin
      Current_Point.X := X;
   end Set_X;

   procedure Set_Y (Current_Point : in out Point; Y : Natural) is
   begin
      Current_Point.Y := Y;
   end Set_Y;

   function Position_X_Greater_Then_Zero
     (Current_Point : Point) return Boolean
   is
   begin
      if Current_Point.X <= 0 then
         raise Value_Error with "Position X Must big then 0";
      end if;

      return True;
   end Position_X_Greater_Then_Zero;

   function Position_Y_Greater_Then_Zero
     (Current_Point : Point) return Boolean
   is
   begin
      if Current_Point.Y <= 0 then
         raise Value_Error with "Position Y Must big then 0";
      end if;

      return True;
   end Position_Y_Greater_Then_Zero;

   procedure Raise_Value_Error (Result_Value : Boolean; Message : String) is
      Discard_Result : Truth;
   begin
      Discard_Result := Raise_Value_Error (Result_Value, Message);
   end Raise_Value_Error;

   function Raise_Value_Error
     (Result_Value : Boolean;
      Message      : String) return Truth
   is
   begin
      case Result_Value is
         when False =>
            raise Value_Error with Message;
         when others =>
            return Result_Value;
      end case;
   end Raise_Value_Error;

   function Create_Rectangle
     (First_Point : Point;
      Last_Point  : Point) return Rectangle
   is
      New_Rectangle : Rectangle;
   begin
      New_Rectangle.First_Point := First_Point;
      New_Rectangle.Last_Point  := Last_Point;

      return New_Rectangle;
   end Create_Rectangle;

   function Get_First_Point (Current_Rectangle : Rectangle) return Point is
   begin
      return Current_Rectangle.First_Point;
   end Get_First_Point;

   function Is_In_Rectangle
     (Current_Rectangle : Rectangle;
      Target_Point      : Point) return Boolean
   is
   begin
      if Target_Point >= Current_Rectangle.First_Point and
        Target_Point <= Current_Rectangle.Last_Point
      then
         return True;
      else
         return False;
      end if;
   end Is_In_Rectangle;
end My_World;
