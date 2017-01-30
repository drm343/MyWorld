package body Easy_Window is
   procedure Debug (Main_Window : in out Main_Window_Type) is
   begin
      Main_Window.Message_Window.Clean;
      Main_Window.Update_Message(Integer'Image (Integer (Main_Window.Map.Inside_Map.Length)));
      Main_Window.Update_Message(Integer'Image (Integer (Main_Window.Map.All_Characters.Length)));
   end Debug;

   function Create_Main_Window (Create_Success : out Boolean;
                                User_Player    : in out Character_Status.Character_Type;
                                Split_Column   : Positive;
                                Split_Line     : Positive)
                                return Main_Window_Type is
      Main_Window : Main_Window_Type;

      Show_Cursor : NCurses.Cursor_Visibility := NCurses.Invisible;

      Check_Columns : NCurses.Column_Position;
      Check_Lines   : NCurses.Line_Position;
      Center_Point  : My_World.Point := My_World.Create_Point
        ((Split_Column - 2) / 2,
         (Split_Line - 2) / 2);

      First_Point   : My_World.Point     := My_World.Create_Point (0, 0);
      Last_Point    : My_World.Point     := My_World.Create_Point (Split_Column - 2, Split_Line - 2);
      Map_Rectangle : My_World.Rectangle := My_World.Create_Rectangle (First_Point, Last_Point);
      Map : Map_System.Object := Map_System.Create_Map_System (Map_Rectangle);
   begin
      Main_Window.Window := NCurses.Standard_Window;

      NCurses.Get_Size (Main_Window.Window, Check_Lines, Check_Columns);

      Main_Window.Max_Width  := Integer (Check_Columns);
      Main_Window.Max_Height := Integer (Check_Lines);

      if Main_Window.Max_Width >= MIN_COLUMN and Main_Window.Max_Height >= MIN_LINE then
         NCurses.Set_Cursor_Visibility (Show_Cursor);
         NCurses.Set_KeyPad_Mode (Main_Window.Window, True);
         NCurses.Set_Cbreak_Mode (True);
         NCurses.Set_Echo_Mode (False);

         Main_Window.Split_Width  := Split_Column;
         Main_Window.Split_Height := Split_Line;

         Main_Window.Message_Window := Sub_Window.Create_Sub_Window_Without_Border
           (Main_Window.Window,
            Main_Window.Max_Height - Split_Line,
            Main_Window.Max_Width,
            Split_Line,
            0);

         Main_Window.Graph_Window := Sub_Window.Create_Sub_Window
           (Main_Window.Window,
            Split_Line,
            Split_Column,
            0,
            0);

         Main_Window.Skill_Window := Sub_Window.Create_Sub_Window
           (Main_Window.Window,
            Split_Line,
            Main_Window.Max_Width - Split_Column,
            0,
            Split_Column);

         User_Player.Set_Graph_Point (Center_Point);
         User_Player.Set_Real_Point (Center_Point);

         Main_Window.Map := Map;
         Main_Window.Map.Set_Player (User_Player);
         Create_Success := True;
      else
         NCurses.Delete (Main_Window.Window);
         Create_Success := False;
      end if;

      return Main_Window;
   end Create_Main_Window;

   procedure Delete (Main_Window : in out Main_Window_Type) is
   begin
      Sub_Window.Delete (Main_Window.Graph_Window);
      Sub_Window.Delete (Main_Window.Skill_Window);
      Sub_Window.Delete (Main_Window.Message_Window);
      NCurses.Delete (Main_Window.Window);
   end Delete;

   function Get_Keystroke (Main_Window : in out Main_Window_Type)
                           return NCurses.Real_Key_Code
   is
   begin
      return NCurses.Get_Keystroke (Main_Window.Window);
   end Get_Keystroke;

   procedure Add_Character (Main_Window : in out Main_Window_Type;
                            Character : in out Character_Status.Character_Type) is
   begin
      Main_Window.Map.Add (Character);
   end Add_Character;

   procedure Move_Graph_Up (Main_Window : in out Main_Window_Type) is
   begin
      Map_System.Move_Up (Main_Window.Map);
   end Move_Graph_Up;

   procedure Move_Graph_Down (Main_Window : in out Main_Window_Type) is
   begin
      Map_System.Move_Down (Main_Window.Map);
   end Move_Graph_Down;

   procedure Move_Graph_Left (Main_Window : in out Main_Window_Type) is
   begin
      Map_System.Move_Left (Main_Window.Map);
   end Move_Graph_Left;

   procedure Move_Graph_Right (Main_Window : in out Main_Window_Type) is
   begin
      Map_System.Move_Right (Main_Window.Map);
   end Move_Graph_Right;

   procedure Update_Graph (Main_Window : in out Main_Window_Type) is
      procedure Run_Iterate (List : Double_List.List) is
         Position : My_World.Point;
         Message  : Character;
         Item     : Character_Status.Character_Type;
      begin
         for C in List.Iterate loop
            Item := Double_List.Element (C);

            Message  := Item.Mark;
            Position := Item.Graph_Position;

            Main_Window.Graph_Window.Update (Position, Message);
         end loop;
      end Run_Iterate;
   begin
      Run_Iterate (Main_Window.Map.Inside_Map);
      Main_Window.Graph_Window.Update
        (Main_Window.Map.Player.Graph_Position, Main_Window.Map.Player.Mark);
   end Update_Graph;

   procedure Update_Graph (Main_Window : in out Main_Window_Type;
                           Item : Character_Status.Character_Type) is
   begin
      Main_Window.Graph_Window.Update (Item.Graph_Position, Item.Mark);
   end Update_Graph;

   procedure Center_Graph (Main_Window : in out Main_Window_Type;
                           Message     : Character) is
   begin
      Main_Window.Graph_Window.Center;
      Main_Window.Graph_Window.Update (Message);
   end Center_Graph;

   procedure Update_Skill (Main_Window : in out Main_Window_Type;
                           Message : String) is
   begin
      Main_Window.Skill_Window.Update (Message);
   end Update_Skill;

   procedure Update_Skill (Main_Window : in out Main_Window_Type;
                           Position    : My_World.Point;
                           Message     : String) is
   begin
      Main_Window.Skill_Window.Update (Position, Message);
   end Update_Skill;

   procedure Update_Message (Main_Window : in out Main_Window_Type;
                             Message : String) is
   begin
      Main_Window.Message_Window.Update (Message);
   end Update_Message;

   procedure Update_Message (Main_Window : in out Main_Window_Type;
                             Position    : My_World.Point;
                             Message     : String) is
   begin
      Main_Window.Message_Window.Update (Position, Message);
   end Update_Message;

   procedure Clear_Graph (Main_Window : in out Main_Window_Type) is
   begin
      Main_Window.Graph_Window.Clean;
   end Clear_Graph;

   procedure Clear_All (Main_Window : in out Main_Window_Type) is
   begin
      Main_Window.Graph_Window.Clean;
      Main_Window.Skill_Window.Clean;
      Main_Window.Message_Window.Clean;
   end Clear_All;

end Easy_Window;
