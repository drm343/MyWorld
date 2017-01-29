package My_World is
   type Point is tagged private;

   function Create_Point (X : Natural;
                          Y : Natural)
                          return Point;

   procedure Move_Up    (Object : in out Point);
   procedure Move_Down  (Object : in out Point);
   procedure Move_Left  (Object : in out Point);
   procedure Move_Right (Object : in out Point);
private
   type Point is tagged
      record
         X : Integer;
         Y : Integer;
      end record;
end My_World;
