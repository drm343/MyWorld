package My_World is
   Value_Error : exception;

   type Point is private;

   function Create_Point (X : Natural;
                          Y : Natural)
                          return Point;

   function Create_Point (From : Point;
                          To   : Point)
                          return Point;

   function Distance (From : Point; To : Point) return Natural;

   procedure Move_Up    (Object : in out Point);
   procedure Move_Down  (Object : in out Point);
   procedure Move_Left  (Object : in out Point);
   procedure Move_Right (Object : in out Point);

   type Rectangle is private;

   function Create_Rectangle (First_Point : Point;
                              Last_Point : Point)
                              return Rectangle;

   function Is_In_Rectangle (Object : Rectangle;
                             Item   : Point) return Boolean;
private
   type Point is
      record
         X : Natural;
         Y : Natural;
      end record;

   type Rectangle is
      record
         First_Point : Point;
         Last_Point  : Point;
      end record;
end My_World;
