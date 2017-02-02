with Ada.Containers.Doubly_Linked_Lists;
with My_World.Character_Sheet;

package My_World.Rule is
   pragma Assertion_Policy (Check);

   package Character_Sheet renames My_World.Character_Sheet;

   package Map is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Character_Sheet.Status,
      "="          => Character_Sheet."=");

   procedure Add (Map_List : in out Map.List; Item : Character_Sheet.Status);

   package Map_System is
      type Map_Information is tagged record
         Coordinate        : Rectangle;
         All_Characters    : Map.List;
         Insight_Charaters : Map.List;
         Player            : Character_Sheet.Status;
         Center_Point      : Point;
      end record;

      function Create_Map_System (Graph : Rectangle) return Map_Information;

      procedure Refresh (Information : in out Map_Information);

      procedure Set_Player
        (Information : in out Map_Information;
         Item        : in out Character_Sheet.Status);

      procedure Add
        (Information : in out Map_Information;
         Item        : in out Character_Sheet.Status);

      procedure Move_Up (Information : in out Map_Information) with
         Pre => Nobody_Occupy_Position_Or_Crossable
           (Information,
            Information.Player.Get_Up_Position);
      procedure Move_Down (Information : in out Map_Information) with
         Pre => Nobody_Occupy_Position_Or_Crossable
           (Information,
            Information.Player.Get_Down_Position);
      procedure Move_Left (Information : in out Map_Information) with
         Pre => Nobody_Occupy_Position_Or_Crossable
           (Information,
            Information.Player.Get_Left_Position);
      procedure Move_Right (Information : in out Map_Information) with
         Pre => Nobody_Occupy_Position_Or_Crossable
           (Information,
            Information.Player.Get_Right_Position);

      procedure Attack_Up (Information : in out Map_Information) with
         Pre => Somebody_Occupy_Position_And_Attackable
           (Information,
            Information.Player.Get_Up_Position);
      procedure Attack_Down (Information : in out Map_Information) with
         Pre => Somebody_Occupy_Position_And_Attackable
           (Information,
            Information.Player.Get_Down_Position);
      procedure Attack_Left (Information : in out Map_Information) with
         Pre => Somebody_Occupy_Position_And_Attackable
           (Information,
            Information.Player.Get_Left_Position);
      procedure Attack_Right (Information : in out Map_Information) with
         Pre => Somebody_Occupy_Position_And_Attackable
           (Information,
            Information.Player.Get_Right_Position);

      function Nobody_Occupy_Position_Or_Crossable
        (Information : in out Map_Information;
         Item        :        Point) return Truth;
      function Somebody_Occupy_Position_And_Attackable
        (Information : in out Map_Information;
         Item        :        Point) return Truth;

      function Is_Inside
        (Information : in out Map_Information;
         Item        :        Point) return Boolean;
   end Map_System;
end My_World.Rule;
