with Terminal_Interface.Curses;
with Terminal_Interface.Curses.Text_IO;
with Terminal_Interface.Curses.Text_IO.Integer_IO;

package My_World.Sub_Window is
   subtype Line_Number is Integer range 1 .. 100;

   package NCurses renames Terminal_Interface.Curses;
   package Text_IO renames Terminal_Interface.Curses.Text_IO;
   package Integer_IO is new Terminal_Interface.Curses.Text_IO.Integer_IO (Num => Line_Number);

   type Sub_Window_Type is tagged
      record
         Window        : NCurses.Window;
         Start_Line    : Integer;
         Start_Column  : Integer;
         Lines         : Integer;
         Columns       : Integer;
         Max_Size      : Integer;
      end record;

   function Create_Sub_Window (Main_Window  : NCurses.Window;
                               Lines        : Integer;
                               Columns      : Integer;
                               Start_Line   : Integer;
                               Start_Column : Integer)
                               return Sub_Window_Type;

   function Create_Sub_Window_Without_Border
     (Main_Window  : NCurses.Window;
      Lines        : Integer;
      Columns      : Integer;
      Start_Line   : Integer;
      Start_Column : Integer)
      return Sub_Window_Type;

   procedure Clean (Sub_Window  : in out Sub_Window_Type);
   procedure Delete (Sub_Window : in out Sub_Window_Type);

   procedure Draw_Line (Sub_Window : in out Sub_Window_Type);
   procedure Draw_Line (Sub_Window : in out Sub_Window_Type;
                        Column     : Integer);

   procedure Draw_Column (Sub_Window : in out Sub_Window_Type);
   procedure Draw_Column (Sub_Window : in out Sub_Window_Type;
                          Line       : Integer);

   procedure Update (Sub_Window : in out Sub_Window_Type;
                     Message    : Character);

   procedure Update (Sub_Window : in out Sub_Window_Type;
                     Position   : Point;
                     Message    : Character);

   procedure Update (Sub_Window : in out Sub_Window_Type;
                     Message : String);

   procedure Update (Sub_Window : in out Sub_Window_Type;
                     Position   : Point;
                     Message    : String);

   procedure Center (Sub_Window : in out Sub_Window_Type);
end My_World.Sub_Window;
