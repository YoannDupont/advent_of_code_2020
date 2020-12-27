with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Command_Line;

procedure Day17 is
    package TIO renames Ada.Text_IO;

    subtype Dimension is Positive range 1 .. 4;
    type Coordinate is array(Dimension'Range) of Integer;
    Origin : constant Coordinate := (others => 0);
    type Coordinate_Array is array(Positive range <>) of Coordinate;

    package Coordinate_Sets is new Ada.Containers.Ordered_Sets(Coordinate);
    subtype Grid4D is Coordinate_Sets.Set;

    package Coordinate_To_Integer is new Ada.Containers.Ordered_Maps(Coordinate, Integer);
    subtype C2I is Coordinate_To_Integer.Map;

    function "+"(L, R : in Coordinate) return Coordinate is
        C : Coordinate := Origin;
    begin
        for I in Dimension loop
            C(I) := L(I) + R(I);
        end loop;
        return C;
    end "+";

    function Combinations_With_Replacement
       (size : Dimension;
        lo, hi: in Integer;
        keep_origin : Boolean := True) return Coordinate_Array
    is
        buffer : Coordinate_Array(1 .. (hi - lo + 1)**size);
        current : Coordinate := (others => 0);
        index : Positive := buffer'First;

        procedure Generate(nth : Dimension) is
        begin
            for I in lo .. hi loop
                current(nth) := I;
                if nth = size then
                    if current /= Origin or else keep_origin then
                        buffer(index) := current;
                        index := index + 1;
                    end if;
                else
                    Generate(nth + 1);
                end if;
            end loop;
        end Generate;
    begin
        Generate(Dimension'First);
        return buffer(buffer'first .. index - 1);
    end Combinations_With_Replacement;

    function Get(F : in TIO.File_Type) return Grid4D is
        grid : Grid4D;
        x, y : Integer;
    begin
        y := 0;
        while not TIO.End_Of_File(F) loop
            x := 0;
            for C of TIO.Get_Line(F) loop
                if C = '#' then
                    grid.Insert((x, y, 0, 0));
                end if;
                x := x + 1;
            end loop;
            y := y + 1;
        end loop;
        return grid;
    end Get;

    procedure Increment(Key : in Coordinate; Element : in out Integer) is
    begin
        Element := Element + 1;
    end Increment;

    function Count_Immediate_Neighbours(grid : in Grid4D; n_dims : Dimension) return C2I is
        counts : C2I;
        neighbour : Coordinate;
        C : Coordinate_To_Integer.Cursor;
    begin
        for coord of grid loop
            for shift of Combinations_With_Replacement(n_dims, -1, 1, False) loop
                neighbour := coord + shift;
                C := counts.Find(neighbour);
                if Coordinate_To_Integer.Has_Element(C) then
                    counts.Update_Element(C, Increment'Access);
                else
                    counts.Insert(neighbour, 1);
                end if;
            end loop;
        end loop;
        return counts;
    end Count_Immediate_Neighbours;

    function Next(grid : in Grid4D; n_dims : Dimension) return Grid4D is
        nxt : Grid4D;
        counts : constant C2I := Count_Immediate_Neighbours(grid, n_dims);
        C : Coordinate_To_Integer.Cursor := counts.First;
        cell : Coordinate;
    begin
        while Coordinate_To_Integer.Has_Element(C) loop
            cell := Coordinate_To_Integer.Key(C);
            if grid.Contains(cell) then
                if Coordinate_To_Integer.Element(C) in 2 .. 3 then
                    nxt.Insert(cell);
                end if;
            else
                if Coordinate_To_Integer.Element(C) = 3 then
                    nxt.Insert(cell);
                end if;
            end if;
            Coordinate_To_Integer.Next(C);
        end loop;
        return nxt;
    end Next;

    function Cycle(G : in Grid4D; n_dims : Dimension) return Grid4D is
        final : Grid4D := G;
    begin
        for I in 1 .. 6 loop
            final := Next(final, n_dims);
        end loop;
        return Final;
    end Cycle;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    grid : Grid4D;
begin
    TIO.Put_Line("--- Day 17: Conway Cubes ---");

    TIO.Open(F, TIO.In_File, filepath);
    grid := Get(F);
    TIO.Close(F);

    TIO.Put_Line("Starting with your given initial configuration, simulate six cycles. How many cubes are left in the active state after the sixth cycle?");
    TIO.Put_Line(Cycle(grid, 3).Length'Img);
    TIO.New_Line;

    TIO.Put_Line("Starting with your given initial configuration, simulate six cycles in a 4-dimensional space. How many cubes are left in the active state after the sixth cycle?");
    TIO.Put_Line(Cycle(grid, 4).Length'Img);
end Day17;
