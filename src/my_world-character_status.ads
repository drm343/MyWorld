with My_World.Ability;

package My_World.Character_Status is
   package User_Ability renames My_World.Ability;

   type Faction_Type is (Player, Ally, Enemy, Neutral);
   subtype Relation_Type is Faction_Type range Ally .. Neutral;

   type Character_Type is tagged
      record
         Mark           : Character;
         Real_Position  : Point;
         Graph_Position : Point;
         Faction        : Faction_Type;
         Damage         : Natural;
         Ability        : User_Ability.Ability_Type;
      end record;

   function "=" (This_Character : Character_Type;
                 Other_Character : Character_Type)
                 return Boolean;

   function "=" (This_Faction  : Faction_Type;
                 Other_Faction : Faction_Type)
                 return Boolean;

   function Create_Player (Mark : Character := '@') return Character_Type;
   function Create_NPC (Mark : Character;
                        Relation : Relation_Type)
                        return Character_Type;

   procedure Set_Real_Point (This_Character : in out Character_Type;
                             Point : My_World.Point);

   procedure Set_Real_Point (This_Character : in out Character_Type;
                             X : Natural;
                             Y : Natural);

   procedure Set_Graph_Point (This_Character : in out Character_Type;
                              Point : My_World.Point);

   procedure Set_Graph_Point (This_Character : in out Character_Type;
                              X : Natural;
                              Y : Natural);

end My_World.Character_Status;
