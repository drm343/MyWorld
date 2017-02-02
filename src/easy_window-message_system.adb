separate (Easy_Window)
package body Message_System is
   procedure Clear (Message_Window : in out Message_Information) is
   begin
      Message_Window.Window.Clear;
      My_World.Set_X (Message_Window.Current_Position, 0);
      My_World.Set_Y (Message_Window.Current_Position, 0);
   end Clear;

   procedure Update
     (Message_Window : in out Message_Information;
      Message        :        String)
   is
   begin
      if My_World.Get_Y (Message_Window.Current_Position) >= 6 then
         Message_Window.Clear;
      end if;

      Message_Window.Window.Update (Message_Window.Current_Position, Message);
      My_World.Set_X (Message_Window.Current_Position, 0);
      My_World.Move_Down (Message_Window.Current_Position);
   end Update;

end Message_System;
