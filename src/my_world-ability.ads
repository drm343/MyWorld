package My_World.Ability is
   type Power_Type is (Weak, Normal, Powerful, Vary_Powerful);
   type Tough_Type is (Sickly, Normal, Tough, Vary_Tough);
   type Speed_Type is (Slow, Normal, Fast, Vary_Fast);
   type Wise_Type is (Foolish, Normal, Wise, Vary_Wise);

   type Ability_Type is
      record
         Power : Power_Type;
         Tough : Tough_Type;
         Speed : Speed_Type;
         Wise  : Wise_Type;
      end record;
end My_World.Ability;
