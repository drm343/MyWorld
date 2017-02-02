package body My_World.Character_Sheet is
   function "="
     (This_Character  : Status;
      Other_Character : Status) return Boolean
   is
   begin
      if This_Character.Mark = Other_Character.Mark and
        This_Character.Real_Position = Other_Character.Real_Position
      then
         return True;
      else
         return False;
      end if;
   end "=";

   function "="
     (This_Faction  : Faction_Type;
      Other_Faction : Faction_Type) return Boolean
   is
   begin
      if Faction_Type'Image (This_Faction) =
        Faction_Type'Image (Other_Faction)
      then
         return True;
      else
         return False;
      end if;
   end "=";

   function Create_Player (Mark : Character := '@') return Status is
      User_Player : Status;
   begin
      User_Player.Mark           := Mark;
      User_Player.Real_Position  := Create_Point (0, 0);
      User_Player.Graph_Position := Create_Point (0, 0);
      User_Player.Faction        := Player;
      User_Player.Damage         := 0;
      User_Player.Crossable      := False;
      User_Player.Attackable     := False;

      return User_Player;
   end Create_Player;

   function Create_NPC
     (Mark     : Character;
      Relation : Relation_Type) return Status
   is
      NPC : Status;

      New_Point : Point;
   begin
      NPC.Mark          := Mark;
      NPC.Real_Position := Create_Point (0, 0);

      New_Point            := Create_Point (10, 36);
      NPC.Graph_Position.X := New_Point.X;
      NPC.Graph_Position.Y := New_Point.Y;
      NPC.Faction          := Faction_Type (Relation);
      NPC.Damage           := 0;
      NPC.Crossable        := False;
      NPC.Attackable       := True;

      return NPC;
   end Create_NPC;

   procedure Set_Real_Point
     (This_Character : in out Status;
      Point          :        My_World.Point)
   is
   begin
      This_Character.Real_Position.X := Point.X;
      This_Character.Real_Position.Y := Point.Y;
   end Set_Real_Point;

   procedure Set_Real_Point
     (This_Character : in out Status;
      X              :        Natural;
      Y              :        Natural)
   is
   begin
      This_Character.Real_Position.X := X;
      This_Character.Real_Position.Y := Y;
   end Set_Real_Point;

   procedure Set_Graph_Point
     (This_Character : in out Status;
      Point          :        My_World.Point)
   is
   begin
      This_Character.Graph_Position.X := Point.X;
      This_Character.Graph_Position.Y := Point.Y;
   end Set_Graph_Point;

   procedure Set_Graph_Point
     (This_Character : in out Status;
      X              :        Natural;
      Y              :        Natural)
   is
   begin
      This_Character.Graph_Position.X := X;
      This_Character.Graph_Position.Y := Y;
   end Set_Graph_Point;

   function Get_Up_Position
     (This_Character : in Status) return Point
   is
      Position : Point := This_Character.Real_Position;
   begin
      Move_Up (Position);

      return Position;
   end Get_Up_Position;

   function Get_Down_Position
     (This_Character : in Status) return Point
   is
      Position : Point := This_Character.Real_Position;
   begin
      Move_Down (Position);

      return Position;
   end Get_Down_Position;

   function Get_Left_Position
     (This_Character : in Status) return Point
   is
      Position : Point := This_Character.Real_Position;
   begin
      Move_Left (Position);

      return Position;
   end Get_Left_Position;

   function Get_Right_Position
     (This_Character : in Status) return Point
   is
      Position : Point := This_Character.Real_Position;
   begin
      Move_Right (Position);

      return Position;
   end Get_Right_Position;

end My_World.Character_Sheet;
