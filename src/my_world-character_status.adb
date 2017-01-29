package body My_World.Character_Status is
   function "=" (This_Character : Character_Type;
                 Other_Character : Character_Type)
                 return Boolean is
   begin
      if This_Character.Mark = Other_Character.Mark and
        This_Character.Real_Position = Other_Character.Real_Position then
         return True;
      else
         return False;
      end if;
   end "=";

   function Create_Player (Mark : Character := '@') return Character_Type is
      User_Player : Character_Type;
   begin
      User_Player.Mark           := Mark;
      User_Player.Real_Position  := Create_Point (0, 0);
      User_Player.Is_Visible     := True;
      User_Player.Graph_Position := Create_Point (0, 0);
      User_Player.Faction        := Player;
      User_Player.Damage         := 0;

      return User_Player;
   end Create_Player;

   function Create_NPC (Mark : Character;
                        Relation : Relation_Type)
                        return Character_Type is
      NPC : Character_Type;

      New_Point : Point;
   begin
      NPC.Mark           := Mark;
      NPC.Real_Position  := Create_Point (0, 0);
      NPC.Is_Visible     := True;

      New_Point := Create_Point (10, 36);
      NPC.Graph_Position.X := New_Point.X;
      NPC.Graph_Position.Y := New_Point.Y;
      NPC.Faction        := Faction_Type (Relation);
      NPC.Damage         := 0;

      return NPC;
   end Create_NPC;

   procedure Set_Real_Point (This_Character : in out Character_Type;
                             Point : My_World.Point) is
   begin
      This_Character.Real_Position.X := Point.X;
      This_Character.Real_Position.Y := Point.Y;
   end Set_Real_Point;

   procedure Set_Real_Point (This_Character : in out Character_Type;
                             X : Natural;
                             Y : Natural) is
   begin
      This_Character.Real_Position.X := X;
      This_Character.Real_Position.Y := Y;
   end Set_Real_Point;

   procedure Set_Graph_Point (This_Character : in out Character_Type;
                              Point : My_World.Point) is
   begin
      This_Character.Graph_Position.X := Point.X;
      This_Character.Graph_Position.Y := Point.Y;
   end Set_Graph_Point;

   procedure Set_Graph_Point (This_Character : in out Character_Type;
                              X : Natural;
                              Y : Natural) is
   begin
      This_Character.Graph_Position.X := X;
      This_Character.Graph_Position.Y := Y;
   end Set_Graph_Point;

end My_World.Character_Status;
