with Ada.Text_IO;
with Ada.Assertions;
with Ada.Command_Line;

procedure Day05 is
    package TIO renames Ada.Text_IO;

    type Seat_ID is new Natural range 0 .. 1023;
    type Seat_Row is new Natural range 0 .. 127;
    type Seat_Row_Range is record
        lo : Seat_Row := Seat_Row'First;
        hi : Seat_Row := Seat_Row'Last;
    end record;
    type Seat_Column is new Natural range 0 .. 7;
    type Seat_Column_Range is record
        lo : Seat_Column := Seat_Column'First;
        hi : Seat_Column := Seat_Column'Last;
    end record;
    type Seat is record
        row : Seat_Row;
        column : Seat_Column;
    end record;
    type Binary_Space_Partitioning is (F, B, L, R);
    type Seat_Location is array(1 .. 10) of Binary_Space_Partitioning;

    function To_Seat_Location(s : in String) return Seat_Location is
        partitioning : Seat_Location;
    begin
        for shift in 0 .. partitioning'Length - 1 loop
            partitioning(shift + 1) := Binary_Space_Partitioning'Value((1 => s(s'First + shift)));
        end loop;
        return partitioning;
    end To_Seat_Location;

    function Get_Seat(location : in Seat_Location) return Seat is
        the_seat : Seat;
        row : Seat_Row_Range;
        column : Seat_Column_Range;
    begin
        for partition of location loop
            case partition is
                when F =>
                    row.hi := (row.lo + row.hi + 1) / 2;
                when B =>
                    row.lo := (row.lo + row.hi + 1) / 2;
                when L =>
                    column.hi := (column.lo + column.hi + 1) / 2;
                when R =>
                    column.lo := (column.lo + column.hi + 1) / 2;
            end case;
        end loop;
        the_seat.row := row.lo;
        the_seat.column := column.lo;
        return the_seat;
    end Get_Seat;

    function Get_ID(s : in Seat) return Seat_ID is
    begin
        return Seat_ID(s.row) * 8 + Seat_ID(s.column);
    end;

    filepath : constant String := Ada.Command_Line.Argument(1);
    file : TIO.File_Type;
    highest : Seat_ID := 0;
    cmp : Seat_ID := 0;
    attributed : array(Seat_ID'Range) of Boolean := (others => False);
    my_seat : Seat_ID;
begin
    Ada.Assertions.Assert(Get_ID(Get_Seat(To_Seat_Location("BFFFBBFRRR"))) = 567);
    Ada.Assertions.Assert(Get_ID(Get_Seat(To_Seat_Location("FFFBBBFRRR"))) = 119);
    Ada.Assertions.Assert(Get_ID(Get_Seat(To_Seat_Location("BBFFBBFRLL"))) = 820);

    TIO.Open(file, TIO.In_File, filepath);
    while not TIO.End_Of_File(file) loop
        cmp := Get_ID(Get_Seat(To_Seat_Location(TIO.Get_Line(file))));
        if highest < cmp then
            highest := cmp;
        end if;
        attributed(cmp) := True;
    end loop;
    TIO.Close(file);
    TIO.Put_Line("What is the highest seat ID on a boarding pass?");
    TIO.Put_Line(highest'Img);
    for I in Seat_ID'First + 1 .. highest loop
        if attributed(I - 1) and not attributed(I) and attributed(I + 1) then
            my_seat := I;
            exit;
        end if;
    end loop;
    TIO.New_Line;
    TIO.Put_Line("What is the ID of your seat?");
    TIO.Put_Line(my_seat'Img);
end Day05;
