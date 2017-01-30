with Terminal_Interface.Curses;
with My_World.Sub_Window;
with My_World.Character_Status;
with My_World.Rule;

with Interfaces.C;

package Easy_Window is
   package NCurses renames Terminal_Interface.Curses;
   package Sub_Window renames My_World.Sub_Window;
   package Character_Status renames My_World.Character_Status;
   package Double_List renames My_World.Rule.Map;
   package Map_System renames My_World.Rule.Map_System;

   MIN_COLUMN    : constant Integer := 80;
   MIN_LINE      : constant Integer := 25;

   subtype Key_Code is NCurses.Key_Code;
   type Main_Window_Type is tagged private;

   procedure Debug (Main_Window : in out Main_Window_Type);

   function Create_Main_Window (Create_Success : out Boolean;
                                User_Player    : in out Character_Status.Character_Type;
                                Split_Column   : Positive;
                                Split_Line     : Positive)
                                return Main_Window_Type;

   procedure Delete (Main_Window : in out Main_Window_Type);

   procedure Init_Screen renames NCurses.Init_Screen;
   procedure End_Screen renames NCurses.End_Screen;

   function Get_Keystroke (Main_Window : in out Main_Window_Type)
                           return NCurses.Real_Key_Code;

   procedure Add_Character (Main_Window : in out Main_Window_Type;
                            Character : in out Character_Status.Character_Type);
   procedure Move_Graph_Up (Main_Window : in out Main_Window_Type);
   procedure Move_Graph_Down (Main_Window : in out Main_Window_Type);
   procedure Move_Graph_Left (Main_Window : in out Main_Window_Type);
   procedure Move_Graph_Right (Main_Window : in out Main_Window_Type);

   procedure Update_Graph (Main_Window : in out Main_Window_Type);

   procedure Center_Graph (Main_Window : in out Main_Window_Type;
                           Message     : Character);

   procedure Update_Skill (Main_Window : in out Main_Window_Type;
                           Message : String);

   procedure Update_Message (Main_Window : in out Main_Window_Type;
                             Message : String);

   procedure Clear_Graph (Main_Window : in out Main_Window_Type);
   procedure Clear_All (Main_Window : in out Main_Window_Type);
private

   type Main_Window_Type is tagged
      record
         Window         : NCurses.Window;
         Max_Width      : Integer;
         Max_Height     : Integer;
         Split_Width    : Integer;
         Split_Height   : Integer;
         Map            : Map_System.Object;
         Graph_Window   : Sub_Window.Sub_Window_Type;
         Skill_Window   : Sub_Window.Sub_Window_Type;
         Message_Window : Sub_Window.Sub_Window_Type;
      end record;

end Easy_Window;
