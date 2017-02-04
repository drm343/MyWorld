package body My_World.Ability is
   procedure Set_Rank (Ability_Status : out Ability_Type; Rank : Rank_Range) is
   begin
      Ability_Status.Rank := Rank;
   end Set_Rank;

   function Is_Correct_Rank (Ability_Status : in Ability_Type) return Truth is
      Totoal_Rank : Integer :=
        Get_Power (Ability_Status) +
        Get_Speed (Ability_Status) +
        Get_Tough (Ability_Status) +
        Get_Wise (Ability_Status) -
        4;
   begin
      if Totoal_Rank /= Ability_Status.Rank then
         raise Rank_Error with "Rank is not correct";
      end if;

      return True;
   end Is_Correct_Rank;

   function Get_Power (Ability_Status : Ability_Type) return Integer is
   begin
      return Power_Type'Pos (Ability_Status.Power);
   end Get_Power;

   function Get_Speed (Ability_Status : Ability_Type) return Integer is
   begin
      return Speed_Type'Pos (Ability_Status.Speed);
   end Get_Speed;

   function Get_Tough (Ability_Status : Ability_Type) return Integer is
   begin
      return Tough_Type'Pos (Ability_Status.Tough);
   end Get_Tough;

   function Get_Wise (Ability_Status : Ability_Type) return Integer is
   begin
      return Wise_Type'Pos (Ability_Status.Wise);
   end Get_Wise;

   procedure Set_Weak (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Power := Weak;
   end Set_Weak;

   procedure Set_Power_Normal (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Power := Normal;
   end Set_Power_Normal;

   procedure Set_Powerful (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Power := Powerful;
   end Set_Powerful;

   procedure Set_Vary_Powerful (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Power := Vary_Powerful;
   end Set_Vary_Powerful;

   procedure Set_Sickly (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Tough := Sickly;
   end Set_Sickly;

   procedure Set_Tough_Normal (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Tough := Normal;
   end Set_Tough_Normal;

   procedure Set_Tough (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Tough := Tough;
   end Set_Tough;

   procedure Set_Vary_Tough (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Tough := Vary_Tough;
   end Set_Vary_Tough;

   procedure Set_Slow (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Speed := Slow;
   end Set_Slow;

   procedure Set_Speed_Normal (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Speed := Normal;
   end Set_Speed_Normal;

   procedure Set_Fast (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Speed := Fast;
   end Set_Fast;

   procedure Set_Vary_Fast (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Speed := Vary_Fast;
   end Set_Vary_Fast;

   procedure Set_Foolish (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Wise := Foolish;
   end Set_Foolish;

   procedure Set_Wise_Normal (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Wise := Normal;
   end Set_Wise_Normal;

   procedure Set_Wise (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Wise := Wise;
   end Set_Wise;

   procedure Set_Vary_Wise (Ability_Status : out Ability_Type) is
   begin
      Ability_Status.Wise := Vary_Wise;
   end Set_Vary_Wise;
end My_World.Ability;
