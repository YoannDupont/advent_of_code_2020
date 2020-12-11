with Ada.Text_IO;
with Ada.Command_Line;

procedure Day11 is
    package TIO renames Ada.Text_IO;

    type State is ('.', 'L', '#');
    subtype Seat_State is State range 'L' .. '#';
    type Grid is array(Positive range <>, Positive range <>) of State;
    type Integer_Couple is array(1 .. 2) of Integer;

    function To_State(C : in Character) return State is
    begin
        return State'Value(''' & C & ''');
    end To_State;

    function To_Value(S : in State) return Natural is
    begin
        case S is
            when '#' => return 1;
            when '.' | 'L' => return 0;
        end case;
    end To_Value;

    function Line_Count(F : in out TIO.File_Type) return Natural is
        count : Natural := 0;
    begin
        TIO.Reset(F, TIO.In_File);
        while not TIO.End_Of_File(F) loop
            count := count + 1;
            TIO.Skip_Line(F);
        end loop;
        TIO.Reset(F, TIO.In_File);
        return count;
    end Line_Count;

    function Line_Length(F : in out TIO.File_Type) return Natural is
        length : Natural;
    begin
        TIO.Reset(F, TIO.In_File);
        length := TIO.Get_Line(F)'Length;
        TIO.Reset(F, TIO.In_File);
        return length;
    end Line_Length;

    function Get(F : in out TIO.File_Type) return Grid is
        LC : constant Positive := Line_Count(F);
        LL : constant Positive := Line_Length(F);
        buffer : String(1 .. LL);
        G : Grid(1 .. LC, 1 .. LL);
    begin
        for I in G'Range(1) loop
            TIO.Get(F, buffer);
            for J in buffer'Range loop
                G(I, J) := To_State(buffer(J));
            end loop;
            if TIO.End_Of_Line(F) then
                TIO.Skip_Line(F);
            end if;
        end loop;
        return G;
    end Get;

    function Part_1(G: in Grid; I, J : in Positive) return Natural is
        count : Natural := 0;
    begin
        for x in Positive'Max(G'First(1), I -1) .. Positive'Min(G'Last(1), I + 1) loop
            for y in Positive'Max(G'First(2), J - 1) .. Positive'Min(G'Last(2), J + 1) loop
                if x /= I or y /= J then
                    count := count + To_Value(G(x, y));
                end if;
            end loop;
        end loop;
        return count;
    end Part_1;

    function Part_2(G: in Grid; I, J : in Positive) return Natural is
        shifts : constant array(1 .. 8) of Integer_Couple := (
            (-1, -1), (-1, 0), (-1, 1),
            (0, -1), (0, 1),
            (1, -1), (1, 0), (1, 1)
        );
        count : Natural := 0;
        x, y : Natural;
    begin
        for shift of shifts loop
            x := I + shift(1);
            y := J + shift(2);
            while x in G'Range(1) and y in G'Range(2) loop
                if G(x, y) in Seat_State then
                    count := count + To_Value(G(x, y));
                    exit;
                end if;
                x := x + shift(1);
                y := y + shift(2);
            end loop;
        end loop;
        return count;
    end Part_2;

    function Next(
        G : in Grid;
        Count_Neighbours : access function(G : in Grid; x,y : Positive) return Natural;
        tolerance : in Natural
    ) return Grid is
        nxt : Grid(G'Range(1), G'Range(2)) := G;
        count : Natural;
    begin
        for I in G'Range(1) loop
            for J in G'Range(2) loop
                case G(I, J) is
                    when '.' => null;
                    when 'L' =>
                        count := Count_Neighbours(G, I, J);
                        if count = 0 then
                            nxt(I, J) := '#';
                        end if;
                    when '#' =>
                        count := Count_Neighbours(G, I, J);
                        if count >= tolerance then
                            nxt(I, J) := 'L';
                        end if;
                end case;
            end loop;
        end loop;
        return nxt;
    end Next;

    function Occupied_Seats(G : Grid) return Natural is
        count : Natural := 0;
    begin
        for seat of G loop
            count := count + To_Value(seat);
        end loop;
        return count;
    end Occupied_Seats;

    function Equilibirum_Occupancy(
        G : in Grid;
        Count_Neighbours : access function(G : in Grid; x,y : Positive) return Natural;
        tolerance : in Natural
    ) return Natural is
        current : Grid := G;
        nxt : Grid(G'Range(1), G'Range(2));
    begin
        loop
            nxt := Next(current, Count_Neighbours, tolerance);
            exit when current = nxt;
            current := nxt;
        end loop;
        return Occupied_Seats(current);
    end Equilibirum_Occupancy;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
begin
    TIO.Open(F, TIO.In_File, filepath);
    declare
        G : constant Grid := Get(F);
        p1 : constant Natural := Equilibirum_Occupancy(G, Part_1'Access, 4);
        p2 : constant Natural := Equilibirum_Occupancy(G, Part_2'Access, 5);
    begin
        TIO.Put_Line("How many seats end up occupied?");
        TIO.Put_Line(p1'Img);
        TIO.New_Line;
        TIO.Put_Line("Given the new visibility method and the rule change for occupied seats becoming empty, once equilibrium is reached, how many seats end up occupied?");
        TIO.Put_Line(p2'Img);
    end;
    TIO.Close(F);
end Day11;
