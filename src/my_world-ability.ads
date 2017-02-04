package My_World.Ability is
   Rank_Error : exception;

   subtype Rank_Range is Integer range 0 .. 8;
   type Ability_Type is private;

   procedure Set_Rank (Ability_Status : out Ability_Type; Rank : Rank_Range);
   function Is_Correct_Rank (Ability_Status : in Ability_Type) return Truth;

   function Get_Power (Ability_Status : Ability_Type) return Integer;
   function Get_Tough (Ability_Status : Ability_Type) return Integer;
   function Get_Speed (Ability_Status : Ability_Type) return Integer;
   function Get_Wise (Ability_Status : Ability_Type) return Integer;

   procedure Set_Weak (Ability_Status : out Ability_Type);
   procedure Set_Power_Normal (Ability_Status : out Ability_Type);
   procedure Set_Powerful (Ability_Status : out Ability_Type);
   procedure Set_Vary_Powerful (Ability_Status : out Ability_Type);

   procedure Set_Sickly (Ability_Status : out Ability_Type);
   procedure Set_Tough_Normal (Ability_Status : out Ability_Type);
   procedure Set_Tough (Ability_Status : out Ability_Type);
   procedure Set_Vary_Tough (Ability_Status : out Ability_Type);

   procedure Set_Slow (Ability_Status : out Ability_Type);
   procedure Set_Speed_Normal (Ability_Status : out Ability_Type);
   procedure Set_Fast (Ability_Status : out Ability_Type);
   procedure Set_Vary_Fast (Ability_Status : out Ability_Type);

   procedure Set_Foolish (Ability_Status : out Ability_Type);
   procedure Set_Wise_Normal (Ability_Status : out Ability_Type);
   procedure Set_Wise (Ability_Status : out Ability_Type);
   procedure Set_Vary_Wise (Ability_Status : out Ability_Type);
private
   type Power_Type is (Weak, Normal, Powerful, Vary_Powerful);
   type Tough_Type is (Sickly, Normal, Tough, Vary_Tough);
   type Speed_Type is (Slow, Normal, Fast, Vary_Fast);
   type Wise_Type is (Foolish, Normal, Wise, Vary_Wise);

   type Ability_Type is record
      Power : Power_Type := Normal;
      Tough : Tough_Type := Normal;
      Speed : Speed_Type := Normal;
      Wise  : Wise_Type  := Normal;
      Rank  : Rank_Range := 0;
   end record;

end My_World.Ability;
