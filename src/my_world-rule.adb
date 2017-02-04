package body My_World.Rule is
   procedure Raise_Miss_Hit (Message : String) is
   begin
      raise Miss_Hit with Message;
   end Raise_Miss_Hit;

   function Is_Success (Result : Roll_Result) return Boolean is
   begin
      case Result is
         when Success =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Success;

   function Is_Failed (Result : Roll_Result) return Boolean is
   begin
      case Result is
         when Failed =>
            return True;
         when others =>
            return False;
      end case;
   end Is_Failed;

   procedure Add (Map_List : in out Map.List; Item : Character_Sheet.Status) is
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
            if Information.Is_Insight (Item.Real_Position) then
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

         if Information.Is_Insight (Item.Real_Position) then
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

      function Attack_Target
        (Information : in out Map_Information;
         Position    :        My_World.Point) return Map.Cursor
      is
         Item   : Character_Sheet.Status;
         Cursor : Map.Cursor;
      begin
         for Current_Cursor in Information.All_Characters.Iterate loop
            Item := Map.Element (Current_Cursor);

            if Item.Real_Position = Position then
               Cursor := Current_Cursor;
               exit;
            end if;
         end loop;

         return Cursor;
      end Attack_Target;

      function Hit_Check
        (Information : in out Map_Information;
         Cursor      :        Map.Cursor) return Roll_Result
      is
         function Total_Dices
           (Ability : Integer;
            Skill   : Integer) return Positive
         is
            Value : Integer := Ability + Skill;
         begin
            if Value <= 0 then
               return 1;
            else
               return Value;
            end if;
         end Total_Dices;

         Opposer : Character_Sheet.Status := Map.Element (Cursor);

         Self_Numbers : Positive :=
           Total_Dices (Character_Sheet.Get_Power (Information.Player), 0);
         Opposer_Numbers : Positive :=
           Total_Dices (Character_Sheet.Get_Power (Opposer), 0);
      begin
         return My_World.Rule.Opposed_Rolls (Self_Numbers, Opposer_Numbers);
      end Hit_Check;

      procedure Deal_Wound
        (Information : in out Map_Information;
         Cursor      :        Map.Cursor;
         Damage      :        Natural)
      is
         procedure Process (Element : in out Character_Sheet.Status) is
         begin
            Element.Be_Wounded (Damage);
         end Process;
      begin
         Information.All_Characters.Update_Element (Cursor, Process'Access);
      end Deal_Wound;

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

      function Is_Insight
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
      end Is_Insight;
   end Map_System;

   function ">" (Left, Right : D6_Numbers.Vector) return Roll_Result is
      Self_Cursor    : D6_Numbers.Cursor := Left.First;
      Opposed_Cursor : D6_Numbers.Cursor := Right.First;

      Self_Result    : D6_Range;
      Opposed_Result : D6_Range;
   begin
      loop
         Self_Result    := D6_Numbers.Element (Self_Cursor);
         Opposed_Result := D6_Numbers.Element (Opposed_Cursor);

         if Self_Result > Opposed_Result then
            goto Self_Win;
         elsif Self_Result < Opposed_Result then
            goto Self_Lose;
         else
            D6_Numbers.Next (Self_Cursor);
            D6_Numbers.Next (Opposed_Cursor);

            if D6_Numbers.Has_Element (Self_Cursor) then
               if D6_Numbers.Has_Element (Opposed_Cursor) then
                  null;
               else
                  goto Self_Win;
               end if;
            else
               if D6_Numbers.Has_Element (Opposed_Cursor) then
                  goto Self_Lose;
               else
                  goto Self_Win;
               end if;
            end if;
         end if;
      end loop;

      <<Self_Win>>
      return Success;

      <<Self_Lose>>
      return Failed;
   end ">";

   function Static_Rolls
     (Numbers : Positive;
      Target  : Positive) return Roll_Result
   is
      Generator : D6_Type.Generator;
      Value     : D6_Range;
      Left      : D6_Numbers.Vector := D6_Numbers.Empty_Vector;
      Right     : D6_Numbers.Vector := D6_Numbers.Empty_Vector;
      TMP       : Positive          := Target;
   begin
      D6_Type.Reset (Gen => Generator);

      Init_Target :
      loop
         if TMP >= 7 then
            Right.Append (6);
            TMP := TMP - 6;
         else
            Right.Append (TMP);
            exit;
         end if;
      end loop Init_Target;
      D6_Numbers_Sort.Sort (Right);
      Right.Reverse_Elements;

      Roll_Dices :
      for Count in Positive range 1 .. Numbers loop
         Value := D6_Type.Random (Gen => Generator);
         Left.Append (Value);
      end loop Roll_Dices;
      D6_Numbers_Sort.Sort (Left);
      Left.Reverse_Elements;

      return Left > Right;
   end Static_Rolls;

   function Opposed_Rolls
     (Self    : Positive;
      Opposed : Positive) return Roll_Result
   is
      Generator : D6_Type.Generator;
      Result    : D6_Range;

      Self_Cursor    : D6_Numbers.Cursor;
      Opposed_Cursor : D6_Numbers.Cursor;

      Self_Vector    : D6_Numbers.Vector := D6_Numbers.Empty_Vector;
      Opposed_Vector : D6_Numbers.Vector := D6_Numbers.Empty_Vector;

      Count : Positive := 1;
   begin
      D6_Type.Reset (Gen => Generator);

      Self_Roll :
      for Count in Positive range 1 .. Self loop
         Result := D6_Type.Random (Gen => Generator);
         Self_Vector.Append (Result);
      end loop Self_Roll;
      D6_Numbers_Sort.Sort (Self_Vector);

      Opposed_Roll :
      for Count in Positive range 1 .. Opposed loop
         Result := D6_Type.Random (Gen => Generator);
         Opposed_Vector.Append (Result);
      end loop Opposed_Roll;
      D6_Numbers_Sort.Sort (Opposed_Vector);

      return Self_Vector > Opposed_Vector;
   end Opposed_Rolls;

end My_World.Rule;
