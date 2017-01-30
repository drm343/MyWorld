package body My_World.Rule is
   procedure Add (Object : in out Map.List;
                  Item : Character_Status.Character_Type) is
   begin
      Object.Append (Item);
   end Add;

   package body Map_System is
      function Create_Map_System (Graph : Rectangle) return Object is
         Map : Object;
      begin
         Map.Coordinate := Graph;
         return Map;
      end Create_Map_System;

      procedure Refresh (This : in out Object) is
         Item   : Character_Status.Character_Type;

         First_Point : Point := This.Coordinate.First_Point;
      begin
         This.Inside_Map.Clear;

         for Cursor in This.All_Characters.Iterate loop
            Item := Map.Element (Cursor);

            if This.Is_Inside (Item.Real_Position) then
               Item.Set_Graph_Point
                 (Item.Real_Position.X - First_Point.X,
                  Item.Real_Position.Y - First_Point.Y);

               This.Inside_Map.Append (Item);
            end if;
         end loop;
      end Refresh;

      procedure Set_Player (This : in out Object;
                            Item : in out Character_Status.Character_Type) is
         use Character_Status;
      begin
         if Item.Faction = Player then
            This.Player := Item;
            This.Center_Point := Item.Graph_Position;
         end if;
      end Set_Player;

      procedure Add (This : in out Object;
                     Item : in out Character_Status.Character_Type) is
         First_Point : Point := This.Coordinate.First_Point;
      begin
         This.All_Characters.Append (Item);
         Item.Set_Graph_Point
           (Item.Real_Position.X - First_Point.X,
            Item.Real_Position.Y - First_Point.Y);

         if This.Is_Inside (Item.Real_Position) then
            This.Inside_Map.Append (Item);
         end if;
      end Add;

      procedure Move_Up (This : in out Object) is
      begin
         Move_Up (This.Player.Real_Position);

         if This.Coordinate.First_Point.Y > 0 then
            Move_Up (This.Coordinate.First_Point);
            Move_Up (This.Coordinate.Last_Point);
         else
            Move_Up (This.Player.Graph_Position);
         end if;

         Refresh (This);
      exception
         when My_World.Value_Error =>
            null;
      end Move_Up;

      procedure Move_Down (This : in out Object) is
      begin
         Move_Down (This.Player.Real_Position);

         if This.Center_Point.Y > This.Player.Real_Position.Y then
            Move_Down (This.Player.Graph_Position);
         else
            Move_Down (This.Coordinate.First_Point);
            Move_Down (This.Coordinate.Last_Point);
         end if;

         Refresh (This);
      exception
         when My_World.Value_Error =>
            null;
      end Move_Down;

      procedure Move_Left (This : in out Object) is
      begin
         Move_Left (This.Player.Real_Position);

         if This.Coordinate.First_Point.X > 0 then
            Move_Left (This.Coordinate.First_Point);
            Move_Left (This.Coordinate.Last_Point);
         else
            Move_Left (This.Player.Graph_Position);
         end if;

         Refresh (This);
      exception
         when My_World.Value_Error =>
            null;
      end Move_Left;

      procedure Move_Right (This : in out Object) is
      begin
         Move_Right (This.Player.Real_Position);

         if This.Center_Point.X > This.Player.Real_Position.X then
            Move_Right (This.Player.Graph_Position);
         else
            Move_Right (This.Coordinate.First_Point);
            Move_Right (This.Coordinate.Last_Point);
         end if;

         Refresh (This);
      exception
         when My_World.Value_Error =>
            null;
      end Move_Right;

      function Is_Inside (This : in out Object; Item : Point) return Boolean is
         First_Point : Point := This.Coordinate.First_Point;
         Last_Point  : Point := This.Coordinate.Last_Point;

         Relative_Point : Point;
      begin
         declare
         begin
            Relative_Point := Create_Point (Item, First_Point);
         exception
            when My_World.Value_Error =>
               Relative_Point := Last_Point;
         end;

         if Is_In_Rectangle (This.Coordinate, Item) and Relative_Point /= Last_Point then
            return True;
         else
            return False;
         end if;
      end Is_Inside;
   end Map_System;

end My_World.Rule;
