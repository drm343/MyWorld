with Ada.Exceptions; use Ada.Exceptions;

with Terminal_Interface.Curses;
with My_World.Sub_Window;
with My_World.Character_Sheet;
with My_World.Rule;

with Interfaces.C;

package Easy_Window is
   package NCurses renames Terminal_Interface.Curses;
   package Sub_Window renames My_World.Sub_Window;
   package Character_Sheet renames My_World.Character_Sheet;
   package Double_List renames My_World.Rule.Map;
   package Map_System renames My_World.Rule.Map_System;

   package Skill_System is
      type Skill_Information is tagged record
         Current_Position : My_World.Point;
         Window           : Sub_Window.Sub_Window_Type;
      end record;

      procedure Clear (Skill_Window : in out Skill_Information);
      procedure Update
        (Skill_Window : in out Skill_Information;
         Message      :        String);
   end Skill_System;

   package Message_System is
      type Message_Information is tagged record
         Current_Position : My_World.Point;
         Window           : Sub_Window.Sub_Window_Type;
      end record;

      procedure Clear (Message_Window : in out Message_Information);
      procedure Update
        (Message_Window : in out Message_Information;
         Message        :        String);
   end Message_System;

   MIN_COLUMN : constant Integer := 80;
   MIN_LINE   : constant Integer := 25;

   subtype Key_Code is NCurses.Key_Code;
   type Main_Window_Type is tagged private;

   procedure Debug (Main_Window : in out Main_Window_Type);

   function Create_Main_Window
     (Create_Success :    out Boolean;
      User_Player    : in out Character_Sheet.Status;
      Split_Column   :        Positive;
      Split_Line     :        Positive) return Main_Window_Type;

   procedure Delete (Main_Window : in out Main_Window_Type);

   procedure Init_Screen renames NCurses.Init_Screen;
   procedure End_Screen renames NCurses.End_Screen;

   function Get_Keystroke
     (Main_Window : in out Main_Window_Type) return NCurses.Real_Key_Code;

   procedure Add_Character
     (Main_Window : in out Main_Window_Type;
      Character   : in out Character_Sheet.Status);
   procedure Move_Graph_Up (Main_Window : in out Main_Window_Type);
   procedure Move_Graph_Down (Main_Window : in out Main_Window_Type);
   procedure Move_Graph_Left (Main_Window : in out Main_Window_Type);
   procedure Move_Graph_Right (Main_Window : in out Main_Window_Type);

   procedure Attack_Graph_Up (Main_Window : in out Main_Window_Type);
   procedure Attack_Graph_Down (Main_Window : in out Main_Window_Type);
   procedure Attack_Graph_Left (Main_Window : in out Main_Window_Type);
   procedure Attack_Graph_Right (Main_Window : in out Main_Window_Type);

   procedure Update_Graph (Main_Window : in out Main_Window_Type);

   procedure Center_Graph
     (Main_Window : in out Main_Window_Type;
      Message     :        Character);

   procedure Refresh_Skill_Information (Main_Window : in out Main_Window_Type);
   procedure Update_Message
     (Main_Window : in out Main_Window_Type;
      Message     :        String);

   procedure Clear_Graph (Main_Window : in out Main_Window_Type);
   procedure Clear_All (Main_Window : in out Main_Window_Type);

   procedure Move_Character (Main_Window : in out Main_Window_Type);

private

   type Main_Window_Type is tagged record
      Window       : NCurses.Window;
      Max_Width    : Integer;
      Max_Height   : Integer;
      Split_Width  : Integer;
      Split_Height : Integer;
      Map          : Map_System.Map_Information;
      Skill        : Skill_System.Skill_Information;
      Message      : Message_System.Message_Information;
      Graph_Window : Sub_Window.Sub_Window_Type;
   end record;

end Easy_Window;
