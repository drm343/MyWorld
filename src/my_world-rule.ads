with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Numerics.Discrete_Random;
with My_World.Character_Sheet;

package My_World.Rule is
   pragma Assertion_Policy (Check);

   Miss_Hit : Exception;

   type Roll_Result is (Failed, Success);
   subtype D6_Range is Integer range 1 .. 6;
   package D6_Type is new Ada.Numerics.Discrete_Random (D6_Range);
   package Character_Sheet renames My_World.Character_Sheet;

   package D6_Numbers is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => D6_Range);
   package D6_Numbers_Sort is new D6_Numbers.Generic_Sorting;

   package Map is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Character_Sheet.Status,
      "="          => Character_Sheet."=");

   procedure Raise_Miss_Hit (Message : String);
   function Is_Success (Result : Roll_Result) return Boolean;
   function Is_Failed (Result : Roll_Result) return Boolean;

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

      function Attack_Target
        (Information : in out Map_Information;
         Position    :        My_World.Point) return Map.Cursor with
         Pre => Somebody_Occupy_Position_And_Attackable
           (Information,
            Position);

      function Hit_Check
        (Information : in out Map_Information;
         Cursor      :        Map.Cursor) return Roll_Result;

      procedure Deal_Wound
        (Information : in out Map_Information;
         Cursor      :        Map.Cursor;
         Damage      :        Natural);

      function Nobody_Occupy_Position_Or_Crossable
        (Information : in out Map_Information;
         Item        :        Point) return Truth;
      function Somebody_Occupy_Position_And_Attackable
        (Information : in out Map_Information;
         Item        :        Point) return Truth;

      function Is_Insight
        (Information : in out Map_Information;
         Item        :        Point) return Boolean;
   end Map_System;

   function Static_Rolls
     (Numbers : Positive;
      Target  : Positive) return Roll_Result;
   function Opposed_Rolls
     (Self    : Positive;
      Opposed : Positive) return Roll_Result;

end My_World.Rule;
