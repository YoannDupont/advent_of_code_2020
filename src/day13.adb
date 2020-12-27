with Ada.Text_IO;
with Ada.Long_Long_Integer_Text_IO;
with Ada.Command_Line;

procedure Day13 is
    package TIO renames Ada.Text_IO;
    package LLITIO renames Ada.Long_Long_Integer_Text_IO;

    subtype Long_Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
    subtype Long_Long_Positive is Long_Long_Natural range 1 .. Long_Long_Natural'Last;

    type Bus is record
        id : Long_Long_Positive;
        offset : Long_Long_Natural;
    end record;
    type Bus_Array is array(Positive range <>) of Bus;

    function To_Bus_Array(input : in String) return Bus_Array is
        cleaned : String := input;
        comma_counts : array(input'Range) of Long_Long_Natural := (others => 0);
        buffer : Bus_Array(1 .. input'Length / 2);
        current : Positive := buffer'First;
        id : Long_Long_Positive;
        last : Positive := cleaned'First;
    begin
        for I in cleaned'Range loop
            if cleaned(I) = ',' then
                cleaned(I) := ' ';
                comma_counts(I) := comma_counts(I-1) + 1;
            elsif cleaned(I) = 'x' then
                cleaned(I) := ' ';
                comma_counts(I) := comma_counts(I-1);
            elsif I > cleaned'First then
                comma_counts(I) := comma_counts(I-1);
            end if;
        end loop;

        while last <= cleaned'Last loop
            LLITIO.Get(cleaned(last .. cleaned'Last), Item => id, last => last);
            buffer(current) := (id, comma_counts(last));
            last := last + 1;
            current := current + 1;
        end loop;

        return buffer(buffer'First .. current - 1);
    end To_Bus_Array;

    procedure Part_1(expected : in Long_Long_Positive; buses : Bus_Array; id_best, diff_best : out Long_Long_Positive) is
        diff : Long_Long_Natural;
    begin
        diff_best := Long_Long_Positive'Last;
        for bus of buses loop
            diff := bus.id - (expected rem bus.id);
            if diff < diff_best then
                diff_best := diff;
                id_best := bus.id;
            end if;
        end loop;
    end Part_1;

    procedure Part_2(buses : Bus_Array; timestamp : out Long_Long_Positive) is
        other_buses : constant Bus_Array := buses(buses'First + 1 .. buses'Last);
        step : Long_Long_Positive := buses(buses'First).id;
    begin
        timestamp := step;
        for bus of other_buses loop
            while (timestamp + bus.offset) rem bus.id /= 0 loop
                timestamp := timestamp + step;
            end loop;
            step := step * bus.id;
        end loop;
    end Part_2;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    expected_departure : Long_Long_Positive;
begin
    TIO.Put_Line("--- Day 13: Shuttle Search ---");

    TIO.Open(F, TIO.In_File, filepath);
    LLITIO.Get(F, expected_departure);
    if TIO.End_Of_Line(F) then
        TIO.Skip_Line(F);
    end if;
    declare
        line : constant String := TIO.Get_Line(F);
        buses : constant Bus_Array := To_Bus_Array(line);
        p1_id, p1_wait : Long_Long_Positive;
        timestamp : Long_Long_Positive;
    begin
        Part_1(expected_departure, buses, p1_id, p1_wait);
        TIO.Put_Line(Long_Long_Positive'Image(p1_id * p1_wait));
        Part_2(buses, timestamp);
        TIO.Put_Line(timestamp'Img);
    end;
    TIO.Close(F);
end Day13;
