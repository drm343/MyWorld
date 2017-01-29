package body My_World.Rule is
   procedure Add (Object : in out Map.List;
                  Item : Character_Status.Character_Type) is
   begin
      Object.Append (Item);
   end Add;
end My_World.Rule;
