with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;

procedure Cal is

   type Day_Grid is array (1 .. 6, 1 .. 7) of Natural;
   type Cal_Grid is array (1 .. 12) of Day_Grid;
   type Lang_Type is (English, French);

   subtype Digit_Line is String (1 .. 8);
   type Digit_Font is array (0 .. 9, 1 .. 10) of Digit_Line;

   type Month_Name is new String (1 .. 12);
   type Month_Name_Array is array (1 .. 12) of Month_Name;

   English_Months : constant Month_Name_Array := (
      "January     ", "February    ", "March       ",
      "April       ", "May         ", "June        ",
      "July        ", "August      ", "September   ",
      "October     ", "November    ", "December    "
   );

   French_Months : constant Month_Name_Array := (
      "Janvier     ", "Fevrier     ", "Mars        ",
      "Avril       ", "Mai         ", "Juin        ",
      "Juillet     ", "Aout        ", "Septembre   ",
      "Octobre     ", "Novembre    ", "Decembre    "
   );

   English_Days : constant String := "Su Mo Tu We Th Fr Sa";
   French_Days  : constant String := "Di Lu Ma Me Je Ve Sa";

   Banner_Font : Digit_Font;
   Calendar    : Cal_Grid;
   Year        : Integer;
   First_Day   : Integer;
   Lang        : Lang_Type;

   -------------------------------------------------------
   -- Load the banner font data from banner.dat
   -------------------------------------------------------
   procedure Load_Banner is
      F    : File_Type;
      Line : String (1 .. 80);
      Last : Natural;
   begin
      for D in 0 .. 9 loop
         for R in 1 .. 10 loop
            Banner_Font(D, R) := "        ";
         end loop;
      end loop;

      Open(F, In_File, "banner.dat");
      for D in 0 .. 9 loop
         for R in 1 .. 10 loop
            Get_Line(F, Line, Last);
            if Last > 8 then
               Last := 8;
            end if;
            for I in 1 .. Last loop
               Banner_Font(D, R)(I) := Line(I);
            end loop;
         end loop;
      end loop;
      Close(F);
   end Load_Banner;

   -------------------------------------------------------
   -- Determine if a year is valid (Gregorian: >= 1582)
   -------------------------------------------------------
   function IsValid (Y : in Integer) return Boolean is
   begin
      return Y >= 1582;
   end IsValid;

   -------------------------------------------------------
   -- Determine if a year is a leap year
   -------------------------------------------------------
   function LeapYear (Y : in Integer) return Boolean is
   begin
      if Y mod 400 = 0 then
         return True;
      elsif Y mod 100 = 0 then
         return False;
      elsif Y mod 4 = 0 then
         return True;
      else
         return False;
      end if;
   end LeapYear;

   -------------------------------------------------------
   -- Return the number of days in a given month/year
   -------------------------------------------------------
   function NumDaysInMonth (M : in Integer; Y : in Integer) return Integer is
   begin
      case M is
         when 1 | 3 | 5 | 7 | 8 | 10 | 12 =>
            return 31;
         when 4 | 6 | 9 | 11 =>
            return 30;
         when 2 =>
            if LeapYear(Y) then
               return 29;
            else
               return 28;
            end if;
         when others =>
            return 0;
      end case;
   end NumDaysInMonth;

   -------------------------------------------------------
   -- Read the year and language from the user, compute
   -- the day of the week for January 1st.
   -------------------------------------------------------
   procedure ReadCalInfo (Y     : out Integer;
                          FDay  : out Integer;
                          L     : out Lang_Type) is
      Temp    : Integer;
      YM1     : Integer;
      Choice  : Character;
      Valid   : Boolean := False;
   begin
      while not Valid loop
         Put("Enter a year (>= 1582): ");
         Get(Temp);
         Skip_Line;
         if IsValid(Temp) then
            Valid := True;
         else
            Put_Line("Invalid year. Must be 1582 or later.");
         end if;
      end loop;
      Y := Temp;

      YM1 := Y - 1;
      FDay := (36 + YM1 + (YM1 / 4) - (YM1 / 100) + (YM1 / 400)) mod 7;

      Put("Language (E)nglish or (F)rench? ");
      Get(Choice);
      Skip_Line;
      if Choice = 'F' or Choice = 'f' then
         L := French;
      else
         L := English;
      end if;
   end ReadCalInfo;

   -------------------------------------------------------
   -- Build the calendar: fill a 6x7 grid for each month
   -------------------------------------------------------
   procedure BuildCalendar is
      Day_Of_Week : Integer;
      Days        : Integer;
   begin
      Day_Of_Week := First_Day;

      for M in 1 .. 12 loop
         for W in 1 .. 6 loop
            for D in 1 .. 7 loop
               Calendar(M)(W, D) := 0;
            end loop;
         end loop;

         Days := NumDaysInMonth(M, Year);
         for Dy in 1 .. Days loop
            declare
               Week : constant Integer := ((Day_Of_Week + Dy - 1) / 7) + 1;
               Col  : constant Integer := ((Day_Of_Week + Dy - 1) mod 7) + 1;
            begin
               Calendar(M)(Week, Col) := Dy;
            end;
         end loop;

         Day_Of_Week := (Day_Of_Week + Days) mod 7;
      end loop;
   end BuildCalendar;

   -------------------------------------------------------
   -- Print the banner for the year in large block digits
   -------------------------------------------------------
   procedure Banner_Print (Y : in Integer; Indent : in Integer) is
      Digs   : array (1 .. 4) of Integer;
      Temp   : Integer := Y;
   begin
      for I in reverse 1 .. 4 loop
         Digs(I) := Temp mod 10;
         Temp := Temp / 10;
      end loop;

      for Row in 1 .. 10 loop
         for I in 1 .. Indent loop
            Put(' ');
         end loop;

         for D in 1 .. 4 loop
            Put(Banner_Font(Digs(D), Row));
            if D < 4 then
               Put("  ");
            end if;
         end loop;
         New_Line;
      end loop;
   end Banner_Print;

   -------------------------------------------------------
   -- Print the heading for a row of three months
   -------------------------------------------------------
   procedure PrintRowHeading (Start_Month : in Integer) is
      Day_Hdr : String (1 .. 20);
      Names   : Month_Name_Array;
   begin
      if Lang = English then
         Day_Hdr := English_Days;
         Names := English_Months;
      else
         Day_Hdr := French_Days;
         Names := French_Months;
      end if;

      for M in Start_Month .. Start_Month + 2 loop
         declare
            Name    : constant String := String(Names(M));
            Trim_Len : Natural := Name'Length;
            Pad     : Natural;
         begin
            while Trim_Len > 0 and then Name(Trim_Len) = ' ' loop
               Trim_Len := Trim_Len - 1;
            end loop;
            Pad := (20 - Trim_Len) / 2;
            for I in 1 .. Pad loop
               Put(' ');
            end loop;
            Put(Name(1 .. Trim_Len));
            for I in 1 .. (20 - Pad - Trim_Len) loop
               Put(' ');
            end loop;
         end;
         if M < Start_Month + 2 then
            Put("   ");
         end if;
      end loop;
      New_Line;

      for M in Start_Month .. Start_Month + 2 loop
         Put(Day_Hdr);
         if M < Start_Month + 2 then
            Put("   ");
         end if;
      end loop;
      New_Line;
   end PrintRowHeading;

   -------------------------------------------------------
   -- Print the date rows for a group of three months
   -------------------------------------------------------
   procedure PrintRowMonth (Start_Month : in Integer) is
   begin
      for W in 1 .. 6 loop
         declare
            All_Empty : Boolean := True;
         begin
            for M in Start_Month .. Start_Month + 2 loop
               for D in 1 .. 7 loop
                  if Calendar(M)(W, D) /= 0 then
                     All_Empty := False;
                  end if;
               end loop;
            end loop;

            if not All_Empty then
               for M in Start_Month .. Start_Month + 2 loop
                  for D in 1 .. 7 loop
                     if Calendar(M)(W, D) = 0 then
                        Put("  ");
                     else
                        Put(Calendar(M)(W, D), Width => 2);
                     end if;
                     if D < 7 then
                        Put(' ');
                     end if;
                  end loop;
                  if M < Start_Month + 2 then
                     Put("   ");
                  end if;
               end loop;
               New_Line;
            end if;
         end;
      end loop;
   end PrintRowMonth;

begin
   Load_Banner;
   ReadCalInfo(Year, First_Day, Lang);
   BuildCalendar;

   New_Line;
   Banner_Print(Year, 16);
   New_Line;

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
