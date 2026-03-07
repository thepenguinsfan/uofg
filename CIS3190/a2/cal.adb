with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Cal is
   --define the calender grind
   --day grid is a 6x7(max size of a month) array of natural numbers
   type Day_Grid is array (1 .. 6, 1 .. 7) of Natural;
   --calender grid is an array of 12 day grids
   type Calender_Grid is array (1 .. 12) of Day_Grid;
   type Language_Type is (English, French);

   subtype Digit_Line is String (1 .. 8);
   type Digit_Font is array (0 .. 9, 1 .. 10) of Digit_Line;
   --month name is a fixed length string of 12 characters
   type Month_Name is new String (1 .. 12);
   --month array is an array of 12 month names
   type Month_Array is array (1 .. 12) of Month_Name;

   --english months
   English_Months : constant Month_Array := (
      "January     ", "February    ", "March       ",
      "April       ", "May         ", "June        ",
      "July        ", "August      ", "September   ",
      "October     ", "November    ", "December    "
   );

   --french months
   French_Months : constant Month_Array := (
      "Janvier     ", "Fevrier     ", "Mars        ",
      "Avril       ", "Mai         ", "Juin        ",
      "Juillet     ", "Aout        ", "Septembre   ",
      "Octobre     ", "Novembre    ", "Decembre    "
   );

   --respective english and french days
   English_Days : constant String := "Su Mo Tu We Th Fr Sa";
   French_Days  : constant String := "Di Lu Ma Me Je Ve Sa";

   Banner_Font : Digit_Font;
   Calendar    : Calender_Grid;
   Year        : Integer;
   First_Day   : Integer;
   Language    : Language_Type;


   --subprogram to determine if the entered year is valid
   function IsValid (Year : in Integer) return Boolean is
   begin
      return Year >= 1582;
   end IsValid;

   --subprogram to determine if the entered year is a leap year
   function LeapYear (Year : in Integer) return Boolean is
   begin
      if Year mod 400 = 0 then
         return True;
      elsif Year mod 100 = 0 then
         return False;
      elsif Year mod 4 = 0 then
         return True;
      else
         return False;
      end if;
   end LeapYear;

   --subprogram to return the number of days in a given month and year
   function NumDaysInMonth (M : in Integer; Year : in Integer) return Integer is
   begin
      case M is
         when 1 | 3 | 5 | 7 | 8 | 10 | 12 =>
            return 31;
         when 4 | 6 | 9 | 11 =>
            return 30;
         when 2 =>
            if LeapYear(Year) then
               return 29;
            else
               return 28;
            end if;
         when others =>
            return 0;
      end case;
   end NumDaysInMonth;

   --subprogram to read year and language from the user and compute the day of the week for January 1st
   procedure ReadCalInfo (Year     : out Integer; DayForFirst  : out Integer; Lang     : out Language_Type) is
      Temp    : Integer;
      YM1     : Integer;
      Choice  : Character;
      Valid   : Boolean := False;

   begin
      --keep promting for a valid year until one is entered
      while not Valid loop
         Put("Enter a year (>= 1582): ");
         Get(Temp);
         --skip the newline character
         Skip_Line;
         if IsValid(Temp) then
            Valid := True;
         else
            Put_Line("Invalid year. Must be 1582 or later.");
         end if;
      end loop;
      Year := Temp;

      --calculate the day of the week for January 1st
      YM1 := Year - 1;
      DayForFirst := (36 + YM1 + (YM1 / 4) - (YM1 / 100) + (YM1 / 400)) mod 7;
      --prompt for the language, keep looping until a valid choice is entered
      Valid := False;
      while not Valid loop
         Put("Language (E)nglish or (F)rench? ");
         Get(Choice);
         Skip_Line;
         if Choice = 'F' or Choice = 'f' then
            Lang := French;
            Valid := True;
         elsif Choice = 'E' or Choice = 'e' then
            Lang := English;
            Valid := True;
         else
            Put_Line("Invalid choice. Please enter E or F.");
         end if;
      end loop;
   end ReadCalInfo;

   --subprogram to build the calendar
   procedure BuildCalendar is
      Day_Of_Week : Integer;
      Days        : Integer;
   begin
      Day_Of_Week := First_Day;
      --initialize the calendar grid to 0
      for M in 1 .. 12 loop
         for W in 1 .. 6 loop
            for D in 1 .. 7 loop
               Calendar(M)(W, D) := 0;
            end loop;
         end loop;

         --get the number of days in the month
         Days := NumDaysInMonth(M, Year);

         --place the days of the month in the calendar grid
         for Dy in 1 .. Days loop
            declare
               Week : constant Integer := ((Day_Of_Week + Dy - 1) / 7) + 1;
               Col  : constant Integer := ((Day_Of_Week + Dy - 1) mod 7) + 1;
            begin
               Calendar(M)(Week, Col) := Dy;
            end;
         end loop;

         --update the day of the week for the next month
         Day_Of_Week := (Day_Of_Week + Days) mod 7;
      end loop;
   end BuildCalendar;


     --subprogram to load the banner data from banner.dat
   procedure Load_Banner is
      F    : File_Type;
      Line : String (1 .. 80);
      Last : Natural;
   begin
      --initialize all slots to blank in case file lines are shorter than 8 characters
      for D in 0 .. 9 loop
         for R in 1 .. 10 loop
            Banner_Font(D, R) := "        ";
         end loop;
      end loop;

      --open the banner.dat file
      Open(F, In_File, "banner.dat");

      --read the banner data from the file
      for D in 0 .. 9 loop
         for R in 1 .. 10 loop
            Get_Line(F, Line, Last);
            if Last > 8 then
               Last := 8;
            end if;

            --place the banner data in the banner font array
            for I in 1 .. Last loop
               Banner_Font(D, R)(I) := Line(I);
            end loop;
         end loop;
      end loop;
      Close(F);
   end Load_Banner;


   --subprogram to print the calendar banner
   procedure Banner (Year : in Integer; Indent : in Integer) is
      Digs   : array (1 .. 4) of Integer;
      Temp   : Integer := Year;

   begin
      --convert the year to a array of digits(2026 -> [2, 0, 2, 6])
      for I in reverse 1 .. 4 loop
         Digs(I) := Temp mod 10;
         Temp := Temp / 10;
      end loop;

      --print the banner
      for Row in 1 .. 10 loop
         for I in 1 .. Indent loop
            Put(' ');
         end loop;
         --print the digits
         for D in 1 .. 4 loop
            Put(Banner_Font(Digs(D), Row));
            --print a space between the digits
            if D < 4 then
               Put("  ");
            end if;

         end loop;
         New_Line;
      end loop;
   end Banner;

   --subprogram to print the month, name, and week headings for a row of 3 months
   procedure PrintRowHeading (Start_Month : in Integer) is
      Day_Header : String (1 .. 20);
      Names   : Month_Array;

   begin
      if Language = English then
         Day_Header := English_Days;
         Names := English_Months;
      else
         Day_Header := French_Days;
         Names := French_Months;
      end if;

      --center the month names in the row
      for M in Start_Month .. Start_Month + 2 loop
         declare
            Name    : constant String := String(Names(M));
            Trim_Len : Natural := Name'Length;
            Pad     : Natural;

         begin
            --trim the trailing spaces from the month name
            while Trim_Len > 0 and then Name(Trim_Len) = ' ' loop
               Trim_Len := Trim_Len - 1;
            end loop;

            --calculate the number of spaces to pad the month name to center it
            Pad := (20 - Trim_Len) / 2;
            for I in 1 .. Pad loop
               Put(' ');
            end loop;

            Put(Name(1 .. Trim_Len));

            --print the trailing spaces to center the month name
            for I in 1 .. (20 - Pad - Trim_Len) loop
               Put(' ');
            end loop;
         end;

         --print a space between the month names
         if M < Start_Month + 2 then
            Put("   ");
         end if;
      end loop;
      New_Line;

      --print the day headers
      for M in Start_Month .. Start_Month + 2 loop
         Put(Day_Header);
         if M < Start_Month + 2 then
            Put("   ");
         end if;
      end loop;
      New_Line;
   end PrintRowHeading;

  --subprogram to print the dats of the month in a row of 3 months
   procedure PrintRowMonth (Start_Month : in Integer) is
   begin
      for W in 1 .. 6 loop
         declare
            All_Empty : Boolean := True;
         begin

            --check if the week-row is empty 
            for M in Start_Month .. Start_Month + 2 loop
               for D in 1 .. 7 loop
                  if Calendar(M)(W, D) /= 0 then
                     All_Empty := False;
                  end if;
               end loop;
            end loop;

            --if the week-row is not empty, print the days of the month
            if not All_Empty then
               for M in Start_Month .. Start_Month + 2 loop

                  --if the day is empty, print 2 spaces
                  for D in 1 .. 7 loop
                     if Calendar(M)(W, D) = 0 then
                        Put("  ");
                     else
                        --if the day is not empty, print the day number
                        Put(Calendar(M)(W, D), Width => 2);
                     end if;

                     if D < 7 then
                        Put(' ');
                     end if;
                  end loop;
                  
                  --print a space between the month names
                  if M < Start_Month + 2 then
                     Put("   ");
                  end if;
               end loop;
               New_Line;
            end if;
         end;
      end loop;
   end PrintRowMonth;
--main program
begin
   Load_Banner;
   ReadCalInfo(Year, First_Day, Language);
   BuildCalendar;

   New_Line;
   Banner(Year, 16);
   New_Line;

   --print 4 row of 3 months each
   for Row in 0 .. 3 loop
      declare
         Start_M : constant Integer := Row * 3 + 1;
      begin
         PrintRowHeading(Start_M);
         PrintRowMonth(Start_M);
         New_Line;
      end;
   end loop;

end Cal;
