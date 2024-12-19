--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Containers.Generic_Array_Sort;
with Ada.Containers.Indefinite_Ordered_Maps;

procedure Day_01 is
   File : File_Type;
   -- Name : constant String := "input/day_01_example.txt";
   Name : constant String := "input/day_01.txt";

   package Distance_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Integer);

   package Distance_Vector_Sorting is new Distance_Vectors.Generic_Sorting;
   use Distance_Vector_Sorting;

   Distance : Integer;
   V0, V1   : Distance_Vectors.Vector;

   package Distance_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type => Integer, Element_Type => Integer);
   use Distance_Maps;

   M : Map;

   Result_Part1 : Integer := 0;
   Result_Part2 : Integer := 0;

begin
   -- NOTE: Read the input values into memory.
   Open (File, In_File, Name);
   while not End_Of_File (File) loop
      Ada.Integer_Text_IO.Get (File, Distance);
      V0.Append (Distance);

      Ada.Integer_Text_IO.Get (File, Distance);
      V1.Append (Distance);
   end loop;
   Close (File);

   -- NOTE: Part 1.
   Sort (V0);
   Sort (V1);

   pragma Assert (V0.Length = V1.Length);
   for I in V0.First_Index .. V0.Last_Index loop
      Result_Part1 := Result_Part1 + abs (V0 (I) - V1 (I));
   end loop;

   Put_Line ("Day 01, Part 1:" & Integer'Image (Result_Part1));

   -- NOTE: Part 2.
   for I of V1 loop
      if M.Contains (I) then
         M (I) := M (I) + 1;
      else
         M.Include (I, 1);
      end if;
   end loop;

   for I of V0 loop
      if M.Contains (I) then
         Result_Part2 := Result_Part2 + (I * M (I));
      end if;
   end loop;

   Put_Line ("Day 01, Part 2:" & Integer'Image (Result_Part2));

end Day_01;
