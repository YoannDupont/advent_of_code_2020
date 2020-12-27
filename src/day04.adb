with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

use type Ada.Strings.Unbounded.Unbounded_String;

procedure Day04 is
    package TIO renames Ada.Text_IO;
    package ASU renames Ada.Strings.Unbounded;
    package ASF renames Ada.Strings.Fixed;

    function "+"(source : in String) return ASU.Unbounded_String renames ASU.To_Unbounded_String;
    function "+"(source : in ASU.Unbounded_String) return String renames ASU.To_String;

    type Passport_Field is (byr, iyr, eyr, hgt, hcl, ecl, pid, cid);
    subtype Required_Passport_Field is Passport_Field range byr .. pid;
    subtype Birth_Year is Natural range 1920 .. 2002;
    subtype Issue_Year is Natural range 2010 .. 2020;
    subtype Expiration_Year is Natural range 2020 .. 2030;
    subtype Centimiter_Height is Natural range 150 .. 193;
    subtype Inch_Height is Natural range 59 .. 76;
    type Eye_Color is (amb, blu, brn, gry, grn, hzl, oth);

    type Passport is array(Passport_Field'Range) of ASU.Unbounded_String;
    Empty_Passport : constant Passport := (others => ASU.Null_Unbounded_String);

    package Field_IO is new Ada.Text_IO.Enumeration_IO(Passport_Field);

    function Part_1(pspt : in Passport) return Boolean is
    begin
        for field in Required_Passport_Field loop
            if pspt(field) = ASU.Null_Unbounded_String then
                return False;
            end if;
        end loop;
        return True;
    end Part_1;

    function Part_2(pspt : in Passport) return Boolean is
    begin
        for field in Required_Passport_Field loop
            if pspt(field) = ASU.Null_Unbounded_String then
                return False;
            end if;
            case field is
                when byr =>
                    if Natural'Value(+pspt(field)) not in Birth_Year then
                        return False;
                    end if;
                when iyr =>
                    if Natural'Value(+pspt(field)) not in Issue_Year then
                        return False;
                    end if;
                when eyr =>
                    if Natural'Value(+pspt(field)) not in Expiration_Year then
                        return False;
                    end if;
                when hgt =>
                    declare
                        str : constant String := +pspt(field);
                    begin
                        if str(str'Last - 1 .. str'Last) = "cm" then
                            if Natural'Value(str(str'First .. str'Last - 2)) not in Centimiter_Height then
                                return False;
                            end if;
                        elsif str(str'Last - 1 .. str'Last) = "in" then
                            if Natural'Value(str(str'First .. str'Last - 2)) not in Inch_Height then
                                return False;
                            end if;
                        else
                            return False;
                        end if;
                    end;
                when hcl =>
                    if ASU.Length(pspt(field)) /= 7 then
                        return False;
                    end if;
                    declare
                        str : constant String := +pspt(field);
                    begin
                        if str(str'First) /= '#' then
                            return False;
                        end if;
                        for C of str(str'First + 1 .. str'Last) loop
                            if not (C in '0' .. '9' or C in 'a' .. 'f') then
                                return False;
                            end if;
                        end loop;
                    end;
                when ecl =>
                    declare
                        color : Eye_Color := Eye_Color'Value(+pspt(field));
                    begin
                        null;
                    end;
                when pid =>
                    if ASU.Length(pspt(field)) /= 9 then
                        return False;
                    end if;
                    declare
                        str : constant String := +pspt(field);
                    begin
                        for C of str loop
                            if C not in '0' .. '9' then
                                return False;
                            end if;
                        end loop;
                    end;
            end case;
        end loop;
        return True;
    exception
        when Constraint_Error =>
            return False;
    end Part_2;

    procedure Read(file : in TIO.File_Type; pspt : out Passport; success : out Boolean) is
        first : Natural;
        last : Natural;
        field : Passport_Field;
    begin
        pspt := Empty_Passport;
        success := False;
        while not TIO.End_Of_File(file) loop
            declare
                line : constant String := TIO.Get_Line(file);
            begin
                if line'Length = 0 then
                    return;
                end if;
                success := True;
                first := line'First;
                while first < line'Last loop
                    Field_IO.Get(line(first .. line'Last), field, last);
                    first := last + 2;
                    begin
                        last := ASF.Index(line, " ", first) - 1;
                    exception
                        when Constraint_Error => last := line'Last;
                    end;
                    pspt(field) := +line(first .. last);
                    first := last + 2;
                end loop;
            end;
        end loop;
    end Read;

    pass : Passport;
    F : TIO.File_Type;
    filepath : constant String := Ada.Command_Line.Argument(1);
    success : Boolean;
    part_1_result : Natural := 0;
    part_2_result : Natural := 0;
begin
    TIO.Put_Line("--- Day 4: Passport Processing ---");

    TIO.Open(F, TIO.In_File, filepath);
    while not TIO.End_Of_File(F) loop
        Read(F, pass, success);
        if success and Part_1(pass) then
            part_1_result := part_1_result + 1;
        end if;
        if success and Part_2(pass) then
            part_2_result := part_2_result + 1;
        end if;
    end loop;
    TIO.Close(F);
    TIO.Put_Line("Count the number of valid passports - those that have all required fields. Treat cid as optional. In your batch file, how many passports are valid?");
    TIO.Put_Line(part_1_result'Img);
    TIO.New_Line;
    TIO.Put_Line("Count the number of valid passports - those that have all required fields and valid values. Continue to treat cid as optional. In your batch file, how many passports are valid?");
    TIO.Put_Line(part_2_result'Img);
end Day04;
