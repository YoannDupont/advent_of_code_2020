with Ada.Text_IO;
with Ada.Long_Long_Integer_Text_IO;
with Ada.Command_Line;

procedure Day09 is
    package TIO renames Ada.Text_IO;
    package LLITIO renames Ada.Long_Long_Integer_Text_IO;

    type Series_Of_Numbers is array(Positive range <>) of Long_Long_Integer;
    Empty_Series : constant Series_Of_Numbers(1 .. 0) := (others => <>);

    type Positive_Couple is array(1 .. 2) of Positive;

    function Get(F : in TIO.File_Type) return Series_Of_Numbers is
        function Get_Rec(acc : Series_Of_Numbers) return Series_Of_Numbers is
            LLI : Long_Long_Integer;
        begin
            if TIO.End_Of_File(F) then
                return acc;
            end if;
            LLITIO.Get(F, LLI);
            return Get_Rec(acc & LLI);
        end Get_Rec;
    begin
        return Get_Rec(Empty_Series);
    end Get;

    function Find(SON : in Series_Of_Numbers; N : in Long_Long_Integer) return Boolean is
    begin
        for I in SON'First .. SON'Last - 1 loop
            for J in I + 1 .. SON'Last loop
                if SON(I) + SON(J) = N then
                    return True;
                end if;
            end loop;
        end loop;
        return False;
    end Find;

    procedure Part_1(SON : in Series_Of_Numbers; preamble_size : in Positive; index : out Positive; invalid : out Long_Long_Integer) is
        preamble : Series_Of_Numbers(1 .. preamble_size) := SON(SON'First .. SON'First + preamble_size - 1);
    begin
        for I in preamble_size + 1 .. SON'Last loop
            if not Find(preamble, SON(I)) then
                index := I;
                invalid := SON(I);
                return;
            end if;
            preamble(1 .. preamble_size - 1) := preamble(2 .. preamble_size);
            preamble(preamble_size) := SON(I);
        end loop;
        raise Constraint_Error with "Found no solutions";
    end Part_1;

    function Part_2(SON : in Series_Of_Numbers; index : in Positive; invalid : in Long_Long_Integer) return Long_Long_Integer is
        before : Series_Of_Numbers(SON'First .. index - 1) := SON(SON'First .. index - 1);
        after : Series_Of_Numbers(index + 1 .. SON'Last) := SON(index + 1 .. SON'Last);
        lohi : Positive_Couple;
        found : Boolean := False;
        smallest, largest : Long_Long_Integer;

        procedure Search(series : in out Series_Of_Numbers; lohi : out Positive_Couple; found : out Boolean) is
            split : Positive;
        begin
            found := False;
            for I in series'First + 1 .. series'Last loop
                series(I) := series(I) + series(I-1);
                if series(I) < invalid then
                    split := I;
                end if;
            end loop;
            split := split + 1;
            for last in split .. series'Last loop
                for first in series'First .. last - 1 loop
                    if series(last) - series(first) = invalid then
                        lohi := (first + 1, last);
                        found := True;
                        return;
                    elsif series(last) - series(first) < invalid then
                        exit;
                    end if;
                end loop;
            end loop;
        end Search;
    begin
        Search(before, lohi, found);
        if not found then
            Search(after, lohi, found);
        end if;
        if not found then
            raise Constraint_Error with "No solution found";
        end if;

        smallest := SON(lohi(1));
        largest := SON(lohi(1));
        for I in lohi(1)+1 .. lohi(2) loop
            if SON(I) < smallest then
                smallest := SON(I);
            end if;
            if largest < SON(I) then
                largest := SON(I);
            end if;
        end loop;
        return smallest + largest;
    end Part_2;

    filepath : constant String := Ada.Command_Line.Argument(1);
    preamble_size : Positive;
    F : TIO.File_Type;
begin
    TIO.Put_Line("--- Day 9: Encoding Error ---");

    if Ada.Command_Line.Argument_Count < 2 then
        preamble_size := 25;
    else
        preamble_size := Positive'Value(Ada.Command_Line.Argument(2));
    end if;

    TIO.Open(F, TIO.In_File, filepath);
    declare
        SON : constant Series_Of_Numbers := Get(F);
        index : Positive;
        invalid : Long_Long_Integer;
        encryption_weakness : Long_Long_Integer;
    begin
        Part_1(SON, preamble_size, index, invalid);
        encryption_weakness := Part_2(SON, index, invalid);
        TIO.Put_Line("The first step of attacking the weakness in the XMAS data is to find the first number in the list (after the preamble) which is not the sum of two of the 25 numbers before it. What is the first number that does not have this property?");
        LLITIO.Put(invalid, width => 0);
        TIO.New_Line;
        TIO.New_Line;
        TIO.Put_Line("What is the encryption weakness in your XMAS-encrypted list of numbers?");
        TIO.Put_Line(encryption_weakness'Img);
    end;
    TIO.Close(F);
end Day09;
