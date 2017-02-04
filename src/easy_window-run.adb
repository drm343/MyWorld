separate (Easy_Window)
procedure Run
  (Main_Window : in out Main_Window_Type;
   Message     :        Easy_Window.Key_Code)
is
   procedure Move (Message : Easy_Window.Key_Code) is
   begin
      case Message is
         when NCurses.KEY_LEFT =>
            Map_System.Move_Left (Main_Window.Map);
         when NCurses.KEY_RIGHT =>
            Map_System.Move_Right (Main_Window.Map);
         when NCurses.KEY_UP =>
            Map_System.Move_Up (Main_Window.Map);
         when NCurses.KEY_DOWN =>
            Map_System.Move_Down (Main_Window.Map);
         when others =>
            null;
      end case;
   end Move;

   procedure Attack (Message : Easy_Window.Key_Code) is
      Target   : Double_List.Cursor;
      Position : My_World.Point;
   begin
      case Message is
         when NCurses.KEY_LEFT =>
            Position := Main_Window.Map.Player.Get_Left_Position;
            goto Attack_Step;
         when NCurses.KEY_RIGHT =>
            Position := Main_Window.Map.Player.Get_Right_Position;
            goto Attack_Step;
         when NCurses.KEY_UP =>
            Position := Main_Window.Map.Player.Get_Up_Position;
            goto Attack_Step;
         when NCurses.KEY_DOWN =>
            Position := Main_Window.Map.Player.Get_Down_Position;
            goto Attack_Step;
         when others =>
            goto Done;
      end case;

      <<Attack_Step>>
      Target := Map_System.Attack_Target (Main_Window.Map, Position);

      case Map_System.Hit_Check (Main_Window.Map, Target) is
         when My_World.Rule.Success =>
            Map_System.Deal_Wound (Main_Window.Map, Target, 1);
         when others =>
            My_World.Rule.Raise_Miss_Hit ("Attack Miss");
      end case;

      declare
         Item : Character_Sheet.Status := My_World.Rule.Map.Element (Target);
      begin
         Main_Window.Update_Message
         ("attack enemy, damage " & Natural'Image (Item.Damage));
      end;

      <<Done>>
      null;
   end Attack;
begin
   <<Move_Action>>
   declare
   begin
      Move (Message);
      goto Done;
   exception
      when Error : My_World.Value_Error =>
         null;
         --         Main_Window.Update_Message (Exception_Message (Error));
   end;

   <<Attack_Action>>
   declare
   begin
      Attack (Message);
   exception
      when Error : My_World.Value_Error =>
         Main_Window.Update_Message (Exception_Message (Error));
      when Error : My_World.Rule.Miss_Hit =>
         Main_Window.Update_Message (Exception_Message (Error));
   end;

   <<Done>>
   null;
end Run;
