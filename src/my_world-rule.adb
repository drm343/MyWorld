package body My_World.Rule is
   procedure Add
     (Map_List : in out Map.List;
      Item     :        Character_Sheet.Status)
   is
   begin
      Map_List.Append (Item);
   end Add;

   function Is_Used_By_Other_Character
     (Map_List : in Map.List;
      Item     :    Point) return Boolean
   is
      Current_Item : Character_Sheet.Status;
   begin
      for Cursor in Map_List.Iterate loop
         Current_Item := Map.Element (Cursor);

         if Current_Item.Real_Position = Item then
            return True;
         end if;
      end loop;

      return False;
   end Is_Used_By_Other_Character;

   package body Map_System is
      function Create_Map_System (Graph : Rectangle) return Map_Information is
         Map : Map_Information;
      begin
         Map.Coordinate := Graph;
         return Map;
      end Create_Map_System;

      procedure Refresh (Information : in out Map_Information) is
         First_Point : Point := Information.Coordinate.First_Point;

         Relative_Position : Point;
      begin
         Information.Insight_Charaters.Clear;

         for Item of Information.All_Characters loop
            if Information.Is_Inside (Item.Real_Position) then
               Relative_Position :=
                 Create_Point
                   (Item.Real_Position.X - First_Point.X,
                    Item.Real_Position.Y - First_Point.Y);

               Item.Set_Graph_Point (Relative_Position);
               Information.Insight_Charaters.Append (Item);
            end if;
         end loop;
      end Refresh;

      procedure Set_Player
        (Information : in out Map_Information;
         Item        : in out Character_Sheet.Status)
      is
         use Character_Sheet;
      begin
         if Item.Faction = Player then
            Information.Player       := Item;
            Information.Center_Point := Item.Graph_Position;
         end if;
      end Set_Player;

      procedure Add
        (Information : in out Map_Information;
         Item        : in out Character_Sheet.Status)
      is
         First_Point : Point := Information.Coordinate.First_Point;

         Relative_Position : Point;
      begin
         Information.All_Characters.Append (Item);

         if Information.Is_Inside (Item.Real_Position) then
            Relative_Position :=
              Create_Point
                (Item.Real_Position.X - First_Point.X,
                 Item.Real_Position.Y - First_Point.Y);
            Item.Set_Graph_Point (Relative_Position);

            Information.Insight_Charaters.Append (Item);
         end if;
      end Add;

      procedure Move_Up (Information : in out Map_Information) is
      begin
         if Information.Coordinate.First_Point.Y > 0 then
            Move_Up (Information.Player.Real_Position);
            Move_Up (Information.Coordinate.First_Point);
            Move_Up (Information.Coordinate.Last_Point);
         else
            if Information.Player.Graph_Position.Y > 1 then
               Move_Up (Information.Player.Real_Position);
               Move_Up (Information.Player.Graph_Position);
            end if;
         end if;
      end Move_Up;

      procedure Move_Down (Information : in out Map_Information) is
      begin
         Move_Down (Information.Player.Real_Position);

         if Information.Player.Real_Position.Y >
           Information.Center_Point.Y
         then
            Move_Down (Information.Coordinate.First_Point);
            Move_Down (Information.Coordinate.Last_Point);
         else
            Move_Down (Information.Player.Graph_Position);
         end if;
      end Move_Down;

      procedure Move_Left (Information : in out Map_Information) is
      begin
         if Information.Coordinate.First_Point.X > 0 then
            Move_Left (Information.Player.Real_Position);
            Move_Left (Information.Coordinate.First_Point);
            Move_Left (Information.Coordinate.Last_Point);
         else
            if Information.Player.Graph_Position.X > 1 then
               Move_Left (Information.Player.Real_Position);
               Move_Left (Information.Player.Graph_Position);
            end if;
         end if;
      end Move_Left;

      procedure Move_Right (Information : in out Map_Information) is
      begin
         Move_Right (Information.Player.Real_Position);

         if Information.Player.Real_Position.X >
           Information.Center_Point.X
         then
            Move_Right (Information.Coordinate.First_Point);
            Move_Right (Information.Coordinate.Last_Point);
         else
            Move_Right (Information.Player.Graph_Position);
         end if;
      end Move_Right;

      procedure Attack_Up (Information : in out Map_Information) is
         Position : My_World.Point := Information.Player.Get_Up_Position;
      begin
         for Current_Item of Information.All_Characters loop
            if Current_Item.Real_Position = Position then
               Current_Item.Damage := Current_Item.Damage + 1;
            end if;
         end loop;
      end Attack_Up;

      procedure Attack_Down (Information : in out Map_Information) is
         Position : My_World.Point := Information.Player.Get_Down_Position;
      begin
         for Current_Item of Information.All_Characters loop
            if Current_Item.Real_Position = Position then
               Current_Item.Damage := Current_Item.Damage + 1;
            end if;
         end loop;
      end Attack_Down;

      procedure Attack_Left (Information : in out Map_Information) is
         Position : My_World.Point := Information.Player.Get_Left_Position;
      begin
         for Current_Item of Information.All_Characters loop
            if Current_Item.Real_Position = Position then
               Current_Item.Damage := Current_Item.Damage + 1;
            end if;
         end loop;
      end Attack_Left;

      procedure Attack_Right (Information : in out Map_Information) is
         Position : My_World.Point := Information.Player.Get_Right_Position;
      begin
         for Current_Item of Information.All_Characters loop
            if Current_Item.Real_Position = Position then
               Current_Item.Damage := Current_Item.Damage + 1;
            end if;
         end loop;
      end Attack_Right;

      function Nobody_Occupy_Position_Or_Crossable
        (Information : in out Map_Information;
         Item        :        Point) return Truth
      is
      begin
         for Current_Item of Information.All_Characters loop
            if Current_Item.Real_Position = Item then
               Raise_Value_Error
                 (Current_Item.Crossable,
                  "Someone occupy this position");
            end if;
         end loop;

         Raise_Value_Error
           (Information.Player.Real_Position /= Item,
            "Player occupy this position");
         return True;
      end Nobody_Occupy_Position_Or_Crossable;

      function Somebody_Occupy_Position_And_Attackable
        (Information : in out Map_Information;
         Item        :        Point) return Truth
      is
      begin
         for Current_Item of Information.All_Characters loop
            if Current_Item.Real_Position = Item then
               return Raise_Value_Error
                 (Current_Item.Attackable,
                  "Can not attack this target");
            end if;
         end loop;

         return Raise_Value_Error (False, "No target can attack");
      end Somebody_Occupy_Position_And_Attackable;

      function Is_Inside
        (Information : in out Map_Information;
         Item        :        Point) return Boolean
      is
         First_Point : Point := Information.Coordinate.First_Point;
         Last_Point  : Point := Information.Coordinate.Last_Point;

         Relative_Point : Point;
      begin
         declare
         begin
            Relative_Point := Create_Point (Item, First_Point);
         exception
            when My_World.Value_Error =>
               Relative_Point := Last_Point;
         end;

         if Is_In_Rectangle (Information.Coordinate, Item) and
           Relative_Point /= Last_Point
         then
            return True;
         else
            return False;
         end if;
      end Is_Inside;
   end Map_System;

end My_World.Rule;
