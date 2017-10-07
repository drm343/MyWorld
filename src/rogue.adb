with Ada.Text_IO; use Ada.Text_IO;
with Ada.Command_Line;
with Ada.Directories;

with Interfaces.C; use Interfaces.C;


procedure Rogue is
  package CML renames Ada.Command_Line;
  package Dir renames Ada.Directories;

  procedure Submain(Root_Dir : char_array; Init_Config : char_array; NPC_Config : char_array);
  pragma Import(C, Submain, "submain");

  Root_Dir : String := Dir.Containing_Directory(Dir.Containing_Directory(CML.Command_Name));
  Init_Config : String := Root_Dir & "/config/init.cfg";
  NPC_Config : String := Root_Dir & "/config/npc.cfg";
begin
  Submain(To_C(Root_Dir), To_C(Init_Config), To_C(NPC_Config));
end Rogue;
