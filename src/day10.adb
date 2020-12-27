with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Containers.Generic_Array_Sort;

procedure Day10 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;

    type Joltage_Array is array(Positive range <>) of Natural;
    Empty_Joltage_Array : constant Joltage_Array(1 .. 0) := (others => <>);

    subtype Long_Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

    procedure Sort is new Ada.Containers.Generic_Array_Sort(Positive, Natural, Joltage_Array);

    function Get(F : in TIO.File_Type) return Joltage_Array is
        function Get_Rec(acc : Joltage_Array) return Joltage_Array is
            P : Positive;
        begin
            if TIO.End_Of_File(F) then
                return acc;
            end if;
            ITIO.Get(F, P);
            return Get_Rec(acc & P);
        end Get_Rec;
    begin
        return Get_Rec(Empty_Joltage_Array);
    end Get;

    procedure Part_1(adapters : in Joltage_Array; ones, threes : out Natural) is
        data : Joltage_Array := adapters;
        previous : Natural;
    begin
        ones := 0;
        threes := 1;
        Sort(data);
        previous := 0;
        for joltage of data loop
            if joltage - previous = 1 then
                ones := ones + 1;
            elsif joltage - previous = 3 then
                threes := threes + 1;
            end if;
            previous := joltage;
        end loop;
    end Part_1;

    procedure Part_2(adapters : in Joltage_Array; number_of_arrangements : out Long_Long_Natural) is
        data : Joltage_Array := adapters & (1 => 0);
        number_of_alternatives : array(data'Range) of Long_Long_Natural := (others => 0);
    begin
        Sort(data);
        number_of_alternatives(number_of_alternatives'First) := 1;
        for I in data'First + 1 .. data'Last loop
            for J in reverse Positive'Max(I - 3, data'First) .. I - 1 loop
                if data(I) - data(J) <= 3 then
                    number_of_alternatives(I) := number_of_alternatives(I) + number_of_alternatives(J);
                end if;
            end loop;
        end loop;
        number_of_arrangements := number_of_alternatives(number_of_alternatives'Last);
    end Part_2;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
begin
    TIO.Put_Line("--- Day 10: Adapter Array ---");

    TIO.Open(F, TIO.In_File, filepath);
    declare
        adapters : constant Joltage_Array := Get(F);
        ones, threes : Natural;
        number_of_arrangements : Long_Long_Natural;
    begin
        Part_1(adapters, ones, threes);
        TIO.Put_Line("What is the number of 1-jolt differences multiplied by the number of 3-jolt differences?");
        ITIO.Put(ones * threes, width => 0);
        TIO.New_Line;
        TIO.New_Line;
        Part_2(adapters, number_of_arrangements);
        TIO.Put_Line("What is the total number of distinct ways you can arrange the adapters to connect the charging outlet to your device?");
        TIO.Put(number_of_arrangements'Img);
    end;
    TIO.Close(F);
end Day10;
