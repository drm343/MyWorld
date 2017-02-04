with My_World.Ability;

package My_World.Character_Sheet is
   pragma Assertion_Policy (Check);

   package User_Ability renames My_World.Ability;

   type Faction_Type is (Player, Ally, Enemy, Neutral);
   subtype Relation_Type is Faction_Type range Ally .. Neutral;

   type Status is tagged record
      Mark           : Character;
      Real_Position  : Point;
      Graph_Position : Point;
      Faction        : Faction_Type;
      Damage         : Natural;
      Ability        : User_Ability.Ability_Type;
      Crossable      : Boolean;
      Attackable     : Boolean;
   end record;

   function "="
     (This_Character  : Status;
      Other_Character : Status) return Boolean;

   function "="
     (This_Faction  : Faction_Type;
      Other_Faction : Faction_Type) return Boolean;

   function Create_Player (Mark : Character := '@') return Status with
      Post => User_Ability.Is_Correct_Rank (Create_Player'Result.Ability);
   function Create_NPC
     (Mark     : Character;
      Relation : Relation_Type) return Status with
      Post => User_Ability.Is_Correct_Rank (Create_NPC'Result.Ability);

   procedure Set_Real_Point
     (This_Character : in out Status;
      Point          :        My_World.Point);

   procedure Set_Real_Point
     (This_Character : in out Status;
      X              :        Natural;
      Y              :        Natural);

   procedure Set_Graph_Point
     (This_Character : in out Status;
      Point          :        My_World.Point);

   procedure Set_Graph_Point
     (This_Character : in out Status;
      X              :        Natural;
      Y              :        Natural);

   function Get_Up_Position (This_Character : in Status) return Point;
   function Get_Down_Position (This_Character : in Status) return Point;
   function Get_Left_Position (This_Character : in Status) return Point;
   function Get_Right_Position (This_Character : in Status) return Point;

   procedure Be_Wounded (This_Character : in out Status; Damage : Natural);

   function Get_Power (This_Character : Status) return Integer;
   function Get_Tough (This_Character : Status) return Integer;
   function Get_Wise (This_Character : Status) return Integer;
end My_World.Character_Sheet;
