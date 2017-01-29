separate (Easy_Window)
package body Graph_Window is
   function Create_Graph (Window : Sub_Window.Sub_Window_Type)
                          return Object is
      New_Window : Object;
   begin
      New_Window.Window := Window;
      New_Window.First_Point := My_World.Create_Point (0, 0);
      New_Window.Last_Point  := My_World.Create_Point (Window.Lines - 2,
                                                       Window.Columns - 2);

      return New_Window;
   end Create_Graph;
end Graph_Window;
