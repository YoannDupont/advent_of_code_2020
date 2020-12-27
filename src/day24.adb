with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Containers.Ordered_Sets;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Indefinite_Vectors;

use type Ada.Containers.Hash_Type;

procedure Day24 is
    package TIO renames Ada.Text_IO;

    type Cardinal is (N, S, E, W, None);
    subtype True_Cardinal is Cardinal range N .. W;
    type Distance is new Integer;
    type Direction is array(1 .. 2) of Cardinal;
    type Tile is array(1 .. 2) of Distance;
    Origin : constant Tile := (0, 0);

    type Direction_Array is array(Positive range <>) of Direction;
    Empty_Direction_Array : constant Direction_Array(1 .. 0) := (others => <>);
    type Tile_Array is array(Positive range <>) of Tile;

    package TAV is new Ada.Containers.Indefinite_Vectors(Positive, Tile_Array);
    subtype TA_Vector is TAV.Vector;

    function "<"(L,R : Tile) return Boolean is
    begin
        return (L(1) < R(1)) or else (L(1) = R(1) and L(2) < R(2));
    end "<";

    package Tile_Sets is new Ada.Containers.Ordered_Sets(Tile);
    package T2I_Maps is new Ada.Containers.Ordered_Maps(Tile, Natural);

    function To_Shift(C : Cardinal) return Distance is
    begin
        case C is
            when N | E => return 2;
            when S | W => return -2;
            when None => return 0;
        end case;
    end To_Shift;

    function To_Tile(D : in Direction) return Tile is
        C : constant Tile := (To_Shift(D(1)), To_Shift(D(2)));
    begin
        if C(1) /= 0 then
            return (C(1), C(2) / 2);
        else
            return C;
        end if;
    end To_Tile;

    function To_Tiles(DA : in Direction_Array) return Tile_Array is
        CA : Tile_Array(DA'range);
    begin
        for I in DA'Range loop
            CA(I) := To_Tile(DA(I));
        end loop;
        return CA;
    end To_Tiles;

    function "+"(L, R : in Tile) return Tile is
    begin
        return (L(1) + R(1), L(2) + R(2));
    end "+";

    function Get(F : in TIO.File_Type) return True_Cardinal is
        C : Character;
    begin
        TIO.Get(F, C);
        return True_Cardinal'Value((1 => C));
    end Get;

    function Get(F : in TIO.File_Type) return Direction is
        C1, C2 : Cardinal;
    begin
        C1 := Get(F);
        case True_Cardinal(C1) is
            when N | S =>
                C2 := Get(F);
            when E | W =>
                C2 := C1;
                C1 := None;
        end case;
        return (C1, C2);
    end Get;

    function Get(F : in TIO.File_Type) return Direction_Array is
        function Get_Rec(accumulator : in Direction_Array) return Direction_Array is
        begin
            if TIO.End_Of_Line(F) then
                if not TIO.End_Of_File(F) then
                    TIO.Skip_Line(F);
                end if;
                return accumulator;
            end if;
            return Get_Rec(accumulator & Direction_Array'(1 => Get(F)));
        end Get_Rec;
    begin
        return Get_Rec(Empty_Direction_Array);
    end Get;

    function Get(F : in TIO.File_Type) return TA_Vector is
        result : TA_Vector;
    begin
        while not TIO.End_Of_File(F) loop
            result.Append(To_Tiles(Get(F)));
        end loop;
        return result;
    end Get;

    function Move(C : in Tile; coordinates : in Tile_Array) return Tile is
        result : Tile := C;
    begin
        for coord of coordinates loop
            result := result + coord;
        end loop;
        return result;
    end Move;

    function Part_1(moves_vector : in TA_Vector) return Tile_Sets.Set is
        blacks : Tile_Sets.Set;
        T : Tile;
    begin
        for arr of moves_vector loop
            T := Move(Origin, arr);
            if not blacks.Contains(T) then
                blacks.Insert(T);
            else
                blacks.Delete(T);
            end if;
        end loop;
        return blacks;
    end Part_1;

    procedure Increment(Key : in Tile; Element : in out Natural) is
    begin
        Element := Element + 1;
    end Increment;

    function Part_2(start : Tile_Sets.Set; days : Positive) return Tile_Sets.Set is
        blacks : Tile_Sets.Set := start;
        tmp : Tile_Sets.Set;
        neighbourhood : constant Tile_Array := (
            To_Tile((None, E)), To_Tile((None, W)),
            To_Tile((N, E)), To_Tile((N, W)),
            To_Tile((S, E)), To_Tile((S, W))
        );
        neighbour : Tile;
        current : Tile;
        value : Natural;
        counts : T2I_Maps.Map;
        C : T2I_Maps.Cursor;
    begin
        for day in 1 .. days loop
            counts.Clear;
            for tl of blacks loop
                for shift of neighbourhood loop
                    neighbour := tl + shift;
                    C := counts.Find(neighbour);
                    if T2I_Maps.Has_Element(C) then
                        counts.Update_Element(C, Increment'Access);
                    else
                        counts.Insert(neighbour, 1);
                    end if;
                end loop;
            end loop;

            C := counts.First;
            while T2I_Maps.Has_Element(C) loop
                current := T2I_Maps.Key(C);
                value := T2I_Maps.Element(C);
                if blacks.Contains(current) then
                    if value in 1 .. 2 then
                        tmp.Insert(current);
                    end if;
                elsif value = 2 then
                    tmp.Insert(current);
                end if;
                T2I_Maps.Next(C);
            end loop;
            blacks := tmp;
            tmp.Clear;
        end loop;
        return blacks;
    end Part_2;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    puzzle_input : TA_Vector;
    p1, p2 : Tile_Sets.Set;
begin
    TIO.Put_Line("--- Day 24: Lobby Layout ---");

    TIO.Open(F, TIO.In_File, filepath);
    puzzle_input := Get(F);
    TIO.Close(F);

    TIO.Put_Line("Go through the renovation crew's list and determine which tiles they need to flip. After all of the instructions have been followed, how many tiles are left with the black side up?");
    p1 := Part_1(puzzle_input);
    TIO.Put_Line(p1.Length'Img);

    TIO.Put_Line("How many tiles will be black after 100 days?");
    p2 := Part_2(p1, 100);
    TIO.Put_Line(p2.Length'Img);
end Day24;
