with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Interfaces.C;

with Easy_Window;
with Terminal_Interface.Curses_Constants;
with My_World.Key_Code;
with My_World.Character_Status;
with My_World.Rule;

procedure Main is
   package NCurses renames Terminal_Interface.Curses_Constants;
   package NKeys renames My_World.Key_Code;
   package Character_Status renames My_World.Character_Status;
   package Rule renames My_World.Rule;
   package Map renames Rule.Map;

   Main_Window : Easy_Window.Main_Window_Type;
   Create_Success : Boolean;

   Key_Code : Easy_Window.Key_Code;

   Player   : Character_Status.Character_Type := Character_Status.Create_Player;
   Gnome    : Character_Status.Character_Type := Character_Status.Create_NPC
     ('a',
      Character_Status.Enemy);

   Outside_Map : Map.List   := Map.Empty_List;
   Border_Map  : Map.List   := Map.Empty_List;
   Inside_Map  : Map.List   := Map.Empty_List;
   Cursor      : Map.Cursor := Outside_Map.Last;
begin
   --  char const *previous_locale = setlocale (LC_ALL, "");
   --  ref: http://panathenaia.halfmoon.jp/alang/ada/character-handling.html
   declare
      LC_ALL : constant Interfaces.C.int := 0;
      function setlocale (category : Interfaces.C.int;
                          locale : access constant Interfaces.C.char)
                          return access constant Interfaces.C.char;
      pragma Import (C, setlocale);
      Empty : aliased Interfaces.C.char_array := (0 => Interfaces.C.nul);
      Previous_Locale : access constant Interfaces.C.char;
   begin
      Previous_Locale := setlocale (LC_ALL, Empty (0)'Access);
   end;

   Easy_Window.Init_Screen;

   Main_Window := Easy_Window.Create_Main_Window (Create_Success, Player, 22, 75);

   if Create_Success then
      Gnome.Set_Real_Point (10, 5);
      Inside_Map.Append (Player);
      Border_Map.Append (Gnome);

      loop
         Key_Code := Main_Window.Get_Keystroke;

         case Key_Code is
         when NCurses.KEY_LEFT =>
            Main_Window.Move_Graph_Right;
         when NCurses.KEY_RIGHT =>
            Main_Window.Move_Graph_Left;
         when NCurses.KEY_UP =>
            Main_Window.Move_Graph_Down;
         when NCurses.KEY_DOWN =>
            Main_Window.Move_Graph_Up;
         when NKeys.Key_q =>
            Main_Window.Delete;
            exit;
         when others =>
            null;
         end case;

         Main_Window.Clear_Graph;
         Main_Window.Update_Graph (Gnome);
         Main_Window.Update_Graph (Player);

      end loop;
   end if;

   Easy_Window.End_Screen;
end Main;
