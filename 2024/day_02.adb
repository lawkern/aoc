--------------------------------------------------------------------------------
-- (c) copyright 2024 Lawrence D. Kern /////////////////////////////////////////
--------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Integer_Text_IO;

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

procedure Day_02 is
   File : Ada.Text_IO.File_Type;
   -- Name : constant String := "input/day_02_example.txt";
   Name : constant String := "input/day_02.txt";

   package Level_Vectors is new Ada.Containers.Vectors
     (Index_Type => Natural, Element_Type => Integer);

   function Report_Is_Safe
     (Report : Level_Vectors.Vector; Skip_Index : Integer := -1) return Boolean
   is
      First_Index      : Natural;
      Curr, Prev, Diff : Integer;
      Incr, Decr, Same : Integer := 0;
      Spiked           : Boolean := False;
   begin
      First_Index :=
        (if Skip_Index /= Report.First_Index then Report.First_Index
         else Natural'Succ (Report.First_Index));

      Prev := Report (First_Index);
      for I in Integer'Succ (First_Index) .. Report.Last_Index loop
         if I /= Skip_Index then
            Curr := Report (I);

            if Curr > Prev then
               Incr := Incr + 1;
            elsif Curr < Prev then
               Decr := Decr + 1;
            else
               Same := Same + 1;
            end if;

            Diff := abs (Curr - Prev);
            if Diff < 1 or Diff > 3 then
               Spiked := True;
            end if;

            Prev := Curr;
         end if;
      end loop;

      return not Spiked and Same = 0 and (Decr = 0 or Incr = 0);
   end Report_Is_Safe;

   function Dampened_Report_Is_Safe
     (Report : Level_Vectors.Vector; Skip_Index : Integer := -1) return Boolean
   is
   begin
      if Report_Is_Safe (Report) then
         return True;
      end if;

      for I in Report.First_Index .. Report.Last_Index loop
         if Report_Is_Safe (Report, I) then
            return True;
         end if;
      end loop;

      return False;
   end Dampened_Report_Is_Safe;

   Result_Part1 : Integer := 0;
   Result_Part2 : Integer := 0;

begin
   -- NOTE: Read the input values into memory.
   Ada.Text_IO.Open (File, Ada.Text_IO.In_File, Name);

   while not Ada.Text_IO.End_Of_File (File) loop
      declare
         Report : Level_Vectors.Vector;
      begin
         loop
            declare
               Chara : Character;
               Level : Integer;
               EOL   : Boolean;
            begin
               Ada.Text_IO.Look_Ahead (File, Chara, EOL);
               exit when EOL;

               Ada.Integer_Text_IO.Get (File, Level);
               Report.Append (Level);
            end;
         end loop;

         Ada.Text_IO.Skip_Line (File);

         if Report_Is_Safe (Report) then
            Result_Part1 := Result_Part1 + 1;
         end if;

         if Dampened_Report_Is_Safe (Report) then
            Result_Part2 := Result_Part2 + 1;
         end if;
      end;
   end loop;

   Ada.Text_IO.Close (File);

   Ada.Text_IO.Put_Line ("Day 02, Part 1:" & Integer'Image (Result_Part1));
   Ada.Text_IO.Put_Line ("Day 02, Part 2:" & Integer'Image (Result_Part2));

end Day_02;
