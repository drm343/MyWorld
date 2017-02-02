separate (Easy_Window)
package body Skill_System is
   procedure Clear (Skill_Window : in out Skill_Information) is
   begin
      Skill_Window.Window.Clear;
      Skill_Window.Window.Box;
      My_World.Set_X (Skill_Window.Current_Position, 1);
      My_World.Set_Y (Skill_Window.Current_Position, 1);
   end Clear;

   procedure Update
     (Skill_Window : in out Skill_Information;
      Message      :        String)
   is
   begin
      Skill_Window.Window.Update (Skill_Window.Current_Position, Message);
      My_World.Set_X (Skill_Window.Current_Position, 1);
      My_World.Move_Down (Skill_Window.Current_Position);
   end Update;

end Skill_System;
