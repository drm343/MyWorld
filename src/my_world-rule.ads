with Ada.Containers.Doubly_Linked_Lists;
with My_World.Character_Status;

package My_World.Rule is
   package Character_Status renames My_World.Character_Status;

   package Map is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Character_Status.Character_Type,
      "=" => Character_Status."=");

   procedure Add (Object : in out Map.List;
                  Item : Character_Status.Character_Type);

   package Map_System is
      type Object is tagged
         record
            Coordinate     : Rectangle;
            All_Characters : Map.List;
            Inside_Map     : Map.List;
            Player         : Character_Status.Character_Type;
            Center_Point   : Point;
         end record;

      function Create_Map_System (Graph : Rectangle) return Object;


      procedure Set_Player (This : in out Object;
                     Item : in out Character_Status.Character_Type);

      procedure Add (This : in out Object;
                     Item : in out Character_Status.Character_Type);

      procedure Move_Up (This : in out Object);
      procedure Move_Down (This : in out Object);
      procedure Move_Left (This : in out Object);
      procedure Move_Right (This : in out Object);

      function Is_Inside (This : in out Object; Item : Point) return Boolean;
   end Map_System;
end My_World.Rule;
