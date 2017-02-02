package body My_World.Sub_Window is
   function Create_Sub_Window
     (Main_Window  : NCurses.Window;
      Lines        : Integer;
      Columns      : Integer;
      Start_Line   : Integer;
      Start_Column : Integer) return Sub_Window_Type
   is
      --        Window     : NCurses.Window;
      Sub_Window : Sub_Window_Type;

      Max_Lines   : NCurses.Line_Position;
      Max_Columns : NCurses.Column_Position;
   begin
      Sub_Window.Start_Line   := Start_Line + 1;
      Sub_Window.Start_Column := Start_Column + 1;
      Sub_Window.Lines        := Lines - 2;
      Sub_Window.Columns      := Columns - 2;

      Sub_Window.Window :=
        NCurses.Derived_Window
          (Main_Window,
           NCurses.Line_Position (Lines),
           NCurses.Column_Position (Columns),
           NCurses.Line_Position (Start_Line),
           NCurses.Column_Position (Start_Column));

      NCurses.Box (Sub_Window.Window);
      NCurses.Refresh (Sub_Window.Window);

      NCurses.Get_Size (Sub_Window.Window, Max_Lines, Max_Columns);
      Sub_Window.Max_Size := Integer (Max_Lines) * Integer (Max_Columns);

      return Sub_Window;
   end Create_Sub_Window;

   function Create_Sub_Window_Without_Border
     (Main_Window  : NCurses.Window;
      Lines        : Integer;
      Columns      : Integer;
      Start_Line   : Integer;
      Start_Column : Integer) return Sub_Window_Type
   is
      Sub_Window : Sub_Window_Type;
   begin
      Sub_Window.Start_Line   := Start_Line;
      Sub_Window.Start_Column := Start_Column;
      Sub_Window.Lines        := Lines;
      Sub_Window.Columns      := Columns;

      Sub_Window.Window :=
        NCurses.Derived_Window
          (Main_Window,
           NCurses.Line_Position (Lines),
           NCurses.Column_Position (Columns),
           NCurses.Line_Position (Start_Line),
           NCurses.Column_Position (Start_Column));

      return Sub_Window;
   end Create_Sub_Window_Without_Border;

   procedure Delete (Sub_Window : in out Sub_Window_Type) is
   begin
      NCurses.Delete (Sub_Window.Window);
   end Delete;

   procedure Draw_Line (Sub_Window : in out Sub_Window_Type) is
      Useless_Line  : NCurses.Line_Position;
      Target_Column : NCurses.Column_Position;
   begin
      NCurses.Get_Cursor_Position
        (Sub_Window.Window,
         Useless_Line,
         Target_Column);
      Sub_Window.Draw_Line (Integer (Target_Column));
   end Draw_Line;

   procedure Draw_Line
     (Sub_Window : in out Sub_Window_Type;
      Column     :        Integer)
   is
      Max_Lines     : Integer                 := Sub_Window.Lines - 1;
      Target_Column : NCurses.Column_Position :=
        NCurses.Column_Position (Column);
   begin
      for counter in Integer range 0 .. Max_Lines loop
         NCurses.Add
           (Sub_Window.Window,
            NCurses.Line_Position (counter),
            Target_Column,
            '|');
      end loop;
   end Draw_Line;

   procedure Draw_Column (Sub_Window : in out Sub_Window_Type) is
      Useless_Column : NCurses.Column_Position;
      Target_Line    : NCurses.Line_Position;
   begin
      NCurses.Get_Cursor_Position
        (Sub_Window.Window,
         Target_Line,
         Useless_Column);
      Sub_Window.Draw_Column (Integer (Target_Line));
   end Draw_Column;

   procedure Draw_Column
     (Sub_Window : in out Sub_Window_Type;
      Line       :        Integer)
   is
      Max_Columns : Integer               := Sub_Window.Columns - 1;
      Target_Line : NCurses.Line_Position := NCurses.Line_Position (Line);
   begin
      for counter in Integer range 0 .. Max_Columns loop
         NCurses.Add
           (Sub_Window.Window,
            Target_Line,
            NCurses.Column_Position (counter),
            '-');
      end loop;
   end Draw_Column;

   procedure Update
     (Sub_Window : in out Sub_Window_Type;
      Message    :        Character)
   is
   begin
      NCurses.Add (Sub_Window.Window, Message);
      NCurses.Refresh (Sub_Window.Window);
   end Update;

   procedure Update
     (Sub_Window : in out Sub_Window_Type;
      Position   :        Point;
      Message    :        Character)
   is
   begin
      if Position.X >= 0 and
        Position.X <= Sub_Window.Columns and
        Position.Y >= 0 and
        Position.Y <= Sub_Window.Lines
      then
         NCurses.Add
           (Sub_Window.Window,
            NCurses.Line_Position (Position.Y),
            NCurses.Column_Position (Position.X),
            Message);
         NCurses.Refresh (Sub_Window.Window);
      end if;
   end Update;

   procedure Update (Sub_Window : in out Sub_Window_Type; Message : String) is
   begin
      NCurses.Add (Sub_Window.Window, Message);
      NCurses.Refresh (Sub_Window.Window);
   end Update;

   procedure Update
     (Sub_Window : in out Sub_Window_Type;
      Position   :        Point;
      Message    :        String)
   is
      Lines   : NCurses.Line_Position;
      Columns : NCurses.Column_Position;

      Max_Position : Integer;
   begin
      NCurses.Get_Size (Sub_Window.Window, Lines, Columns);
      Max_Position :=
        Position.X + Position.Y * Integer (Columns) + Integer (Message'Length);

      if Position.X >= 0 and
        Position.X < Sub_Window.Columns and
        Position.Y >= 0 and
        Position.Y < Sub_Window.Lines and
        Max_Position <= Sub_Window.Max_Size
      then
         NCurses.Add
           (Sub_Window.Window,
            NCurses.Line_Position (Position.Y),
            NCurses.Column_Position (Position.X),
            Message);
         NCurses.Refresh (Sub_Window.Window);
      end if;
   end Update;

   procedure Clear (Sub_Window : in out Sub_Window_Type) is
   begin
      NCurses.Clear (Sub_Window.Window);
      NCurses.Refresh (Sub_Window.Window);
   end Clear;

   procedure Box (Sub_Window : in out Sub_Window_Type) is
   begin
      NCurses.Box (Sub_Window.Window);
   end Box;
   procedure Center (Sub_Window : in out Sub_Window_Type) is
      Center_Line   : Integer := Sub_Window.Lines / 2;
      Center_Column : Integer := Sub_Window.Columns / 2;
   begin
      NCurses.Move_Cursor
        (Sub_Window.Window,
         NCurses.Line_Position (Center_Line),
         NCurses.Column_Position (Center_Column));
   end Center;

end My_World.Sub_Window;
