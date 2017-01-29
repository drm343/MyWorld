with Ada.Containers.Doubly_Linked_Lists;
with My_World.Character_Status;

package My_World.Rule is
   package Character_Status renames My_World.Character_Status;

   package Map is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Character_Status.Character_Type,
      "=" => Character_Status."=");

   procedure Add (Object : in out Map.List;
                  Item : Character_Status.Character_Type);
end My_World.Rule;
