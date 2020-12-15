with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Hashed_Maps;
with Ada.Command_Line;

procedure Day15 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;

    type Natural_Array is array(Positive range <>) of Natural;
    Empty_Array : Constant Natural_Array(1 .. 0) := (others => <>);

    function Hash(N : in Natural) return Ada.Containers.Hash_Type is (Ada.Containers.Hash_Type(N));

    package N2N_Maps is new Ada.Containers.Hashed_Maps(Natural, Natural, Hash, "=", "=");
    use N2N_Maps;

    function Get(F : in TIO.File_Type) return Natural_Array is
        line : constant String := TIO.Get_Line(F);
        function Get_Rec(from : in Positive; accumulator : in Natural_Array) return Natural_Array is
            N : Natural;
            last : Natural;
        begin
            if from > line'Last then
                return accumulator;
            end if;
            ITIO.Get(line(from .. line'Last), N, last);
            return Get_Rec(last + 2, accumulator & N);
        end Get_Rec;
    begin
        return Get_Rec(line'First, Empty_Array);
    end Get;

    function Play(input : in Natural_Array; nth : in Natural) return Natural is
        most_recently_spoken : Natural := input(input'Last);
        last_time_spoken : Map;
        laps : Natural;
    begin
        for I in input'Range loop
            last_time_spoken.Include(input(I), I);
        end loop;

        for I in input'Last + 1 .. input'First + nth - 1 loop
            if last_time_spoken.Contains(most_recently_spoken) then
                laps := I - last_time_spoken.Element(most_recently_spoken) - 1;
                last_time_spoken.Include(most_recently_spoken, I - 1);
                most_recently_spoken := laps;
            else
                last_time_spoken.Include(most_recently_spoken, I - 1);
                most_recently_spoken := 0;
            end if;
        end loop;
        return most_recently_spoken;
    end Play;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
begin
    TIO.Open(F, TIO.In_File, filepath);
    declare
        input : constant Natural_Array := Get(F);
    begin
        TIO.Put_Line("Given your starting numbers, what will be the 2020th number spoken?");
        TIO.Put_Line(Play(input, 2020)'Img);
        TIO.Put_Line("Given your starting numbers, what will be the 30000000th number spoken?");
        TIO.Put_Line(Play(input, 30000000)'Img);
    end;
    TIO.Close(F);
end Day15;
