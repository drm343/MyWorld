with Terminal_Interface.Curses;

package My_World.Key_Code is
    package NCurses renames Terminal_Interface.Curses;

    Key_q    : constant NCurses.Key_Code := 113;
end My_World.Key_Code;
