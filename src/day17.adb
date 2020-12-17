with Ada.Text_IO;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Command_Line;

procedure Day17 is
    package TIO renames Ada.Text_IO;

    type Coordinate is array(1 .. 4) of Integer;

    package Coordinate_Sets is new Ada.Containers.Ordered_Sets(Coordinate);
    subtype Grid4D is Coordinate_Sets.Set;

    package Coordinate_To_Integer is new Ada.Containers.Ordered_Maps(Coordinate, Integer);
    subtype C2I is Coordinate_To_Integer.Map;

    function Get(F : in TIO.File_Type) return Grid4D is
        grid : Grid4D;
        x, y : Integer;
    begin
        y := 0;
        while not TIO.End_Of_File(F) loop
            declare
                line : constant String := TIO.Get_Line(F);
            begin
                x := 0;
                for C of line loop
                    if C = '#' then
                        grid.Insert((x, y, 0, 0));
                    end if;
                    x := x + 1;
                end loop;
                y := y + 1;
            end;
        end loop;
        return grid;
    end Get;

    procedure Increment(Key : in Coordinate; Element : in out Integer) is
    begin
        Element := Element + 1;
    end Increment;

    function Count_Immediate_Neighbours(grid : in Grid4D; use_w : Boolean) return C2I is
        counts : C2I;
        neighbour : Coordinate;
    begin
        for coord of grid loop
            for x in -1 .. 1 loop
                for y in -1 .. 1 loop
                    for z in -1 .. 1 loop
                        if not use_w then
                            if not(x = 0 and y = 0 and z = 0) then
                                neighbour := (coord(1)+x, coord(2)+y, coord(3)+z, 0);
                                if counts.Contains(neighbour) then
                                    counts.Update_Element(counts.Find(neighbour), Increment'Access);
                                else
                                    counts.Insert(neighbour, 1);
                                end if;
                            end if;
                        else
                            for w in -1 .. 1 loop
                                if not(x = 0 and y = 0 and z = 0 and w = 0) then
                                    neighbour := (coord(1)+x, coord(2)+y, coord(3)+z, coord(4)+w);
                                    if counts.Contains(neighbour) then
                                        counts.Update_Element(counts.Find(neighbour), Increment'Access);
                                    else
                                        counts.Insert(neighbour, 1);
                                    end if;
                                end if;
                            end loop;
                        end if;
                    end loop;
                end loop;
            end loop;
        end loop;
        return counts;
    end Count_Immediate_Neighbours;

    function Next(grid : in Grid4D; use_w : Boolean) return Grid4D is
        nxt : Grid4D;
        counts : constant C2I := Count_Immediate_Neighbours(grid, use_w);
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

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    grid : Grid4D;
    part_1 : Grid4D;
    part_2 : Grid4D;
begin
    TIO.Open(F, TIO.In_File, filepath);
    grid := Get(F);
    TIO.Close(F);

    part_1 := grid.Copy;
    for I in 1 .. 6 loop
        part_1 := Next(part_1, False);
    end loop;
    TIO.Put_Line("Starting with your given initial configuration, simulate six cycles. How many cubes are left in the active state after the sixth cycle?");
    TIO.Put_Line(part_1.Length'Img);

    part_2 := grid.Copy;
    for I in 1 .. 6 loop
        part_2 := Next(part_2, True);
    end loop;
    TIO.Put_Line("Starting with your given initial configuration, simulate six cycles in a 4-dimensional space. How many cubes are left in the active state after the sixth cycle?");
    TIO.Put_Line(part_2.Length'Img);
end Day17;
