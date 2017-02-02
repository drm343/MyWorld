with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Exceptions;      use Ada.Exceptions;

with Interfaces.C;

with Easy_Window;
with Terminal_Interface.Curses_Constants;
with My_World.Key_Code;
with My_World.Character_Sheet;

procedure Main is
   package NCurses renames Terminal_Interface.Curses_Constants;
   package NKeys renames My_World.Key_Code;
   package Character_Sheet renames My_World.Character_Sheet;

   Main_Window    : Easy_Window.Main_Window_Type;
   Create_Success : Boolean;

   Key_Code : Easy_Window.Key_Code;

   Player : Character_Sheet.Status := Character_Sheet.Create_Player;
   Gnome  : Character_Sheet.Status;

   package Action is
      procedure Run (Message : Easy_Window.Key_Code);
   end Action;

   package body Action is
      procedure Move (Message : Easy_Window.Key_Code) is
      begin
         case Message is
            when NCurses.KEY_LEFT =>
               Main_Window.Move_Graph_Left;
            when NCurses.KEY_RIGHT =>
               Main_Window.Move_Graph_Right;
            when NCurses.KEY_UP =>
               Main_Window.Move_Graph_Up;
            when NCurses.KEY_DOWN =>
               Main_Window.Move_Graph_Down;
            when others =>
               null;
         end case;
      end Move;

      procedure Attack (Message : Easy_Window.Key_Code) is
      begin
         case Message is
            when NCurses.KEY_LEFT =>
               Main_Window.Attack_Graph_Left;
            when NCurses.KEY_RIGHT =>
               Main_Window.Attack_Graph_Right;
            when NCurses.KEY_UP =>
               Main_Window.Attack_Graph_Up;
            when NCurses.KEY_DOWN =>
               Main_Window.Attack_Graph_Down;
            when others =>
               null;
         end case;
      end Attack;

      procedure Run (Message : Easy_Window.Key_Code) is
      begin
         <<Move_Action>>
         declare
         begin
            Move (Message);
            goto Done;
         exception
            when Error : My_World.Value_Error =>
               Main_Window.Update_Message (Exception_Message (Error));
         end;

         <<Attack_Action>>
         declare
         begin
            Attack (Message);
         exception
            when Error : My_World.Value_Error =>
               Main_Window.Update_Message (Exception_Message (Error));
         end;

         <<Done>>
         null;
      end Run;
   end Action;

begin
   --  char const *previous_locale = setlocale (LC_ALL, "");
   --  ref: http://panathenaia.halfmoon.jp/alang/ada/character-handling.html
   declare
      LC_ALL : constant Interfaces.C.int := 0;
      function setlocale
        (category : Interfaces.C.int;
         locale   : access constant Interfaces.C.char)
         return access constant Interfaces.C.char;
      pragma Import (C, setlocale);
      Empty : aliased Interfaces.C.char_array := (0 => Interfaces.C.nul);
      Previous_Locale : access constant Interfaces.C.char;
   begin
      Previous_Locale := setlocale (LC_ALL, Empty (0)'Access);
   end;

   Easy_Window.Init_Screen;

   Main_Window :=
     Easy_Window.Create_Main_Window (Create_Success, Player, 75, 20);

   if Create_Success then
      for Count in Natural range 0 .. 5 loop
         Gnome := Character_Sheet.Create_NPC ('G', Character_Sheet.Enemy);

         Gnome.Set_Real_Point (80, 10 + Count * 2);
         Main_Window.Add_Character (Gnome);
      end loop;

      for Count in Natural range 0 .. 3 loop
         Gnome := Character_Sheet.Create_NPC ('G', Character_Sheet.Enemy);

         Gnome.Set_Real_Point (36 + Count * 2, 5);
         Main_Window.Add_Character (Gnome);
      end loop;

      loop
         --           Main_Window.Debug;
         Main_Window.Clear_Graph;
         Main_Window.Update_Graph;
         Main_Window.Refresh_Skill_Information;
         Key_Code := Main_Window.Get_Keystroke;

         case Key_Code is
            when NKeys.Key_q =>
               Main_Window.Delete;
               exit;
            when others =>
               Action.Run (Key_Code);
         end case;

         Main_Window.Move_Character;
      end loop;
   end if;

   Easy_Window.End_Screen;
end Main;
