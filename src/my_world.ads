package My_World is
   pragma Assertion_Policy (Check);

   Value_Error : exception;

   subtype Truth is Boolean range True .. True;
   type Point is private;

   function ">" (Left, Right : Point) return Boolean;

   function Create_Point (X : Natural; Y : Natural) return Point with
     Pre => Raise_Value_Error
       (X >= 0,
        "Create Point: Can not create Point with negative position") and
     Raise_Value_Error
       (Y >= 0,
        "Create Point: Can not create Point with negative position");

   function Create_Point (From : Point; To : Point) return Point with
     Pre => Raise_Value_Error
       (From > To,
        "Create_Point: Second point must small than First Point");

   function Distance (From : Point; To : Point) return Natural;

   procedure Move_Up (Current_Point : in out Point) with
     Pre  => Position_Y_Greater_Then_Zero (Current_Point),
     Post => (Get_Y (Current_Point) >= 0);
   procedure Move_Down (Current_Point : in out Point);
   procedure Move_Left (Current_Point : in out Point) with
     Pre  => Position_X_Greater_Then_Zero (Current_Point),
     Post => (Get_X (Current_Point) >= 0);

   procedure Move_Right (Current_Point : in out Point);

   function Get_X (Current_Point : in Point) return Natural;
   function Get_Y (Current_Point : in Point) return Natural;
   procedure Set_X (Current_Point : in out Point; X : Natural);
   procedure Set_Y (Current_Point : in out Point; Y : Natural);

   function Position_X_Greater_Then_Zero
     (Current_Point : Point) return Boolean;
   function Position_Y_Greater_Then_Zero
     (Current_Point : Point) return Boolean;

   procedure Raise_Value_Error (Result_Value : Boolean; Message : String);
   function Raise_Value_Error
     (Result_Value : Boolean;
      Message      : String) return Truth;

   type Rectangle is private;

   function Create_Rectangle
     (First_Point : Point;
      Last_Point  : Point) return Rectangle;

   function Get_First_Point (Current_Rectangle : Rectangle) return Point;

   function Is_In_Rectangle
     (Current_Rectangle : Rectangle;
      Target_Point      : Point) return Boolean;
private
   type Point is record
      X : Natural;
      Y : Natural;
   end record;

   type Rectangle is record
      First_Point : Point;
      Last_Point  : Point;
   end record;
end My_World;
