with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Containers.Hashed_Sets;
with Ada.Numerics.Generic_Elementary_Functions;

procedure Day20 is
    package TIO renames Ada.Text_IO;
    package ANGEF is new Ada.Numerics.Generic_Elementary_Functions(Float);

    type Image_Type is array(Positive range <>, Positive range <>) of Boolean;
    subtype Tile_Range is Positive range 1 .. 10;
    subtype Tile is Image_Type(Tile_Range, Tile_Range);
    type Slice is array(Tile_Range) of Boolean;

    type Neighbour_Type is (Up, Down, Left, Right);
    type Sides is array(Neighbour_Type) of Natural;
    type Neighbouring is array(1 .. 4) of Natural;

    type Camera is
    record
        id : Long_Long_Integer;
        image : Tile := (others => (others => False));
        neighbours : Neighbouring := (others => 0);
        neighbours_sided : Sides := (others => 0);
        flipped : Boolean := False;
        n_found : Natural := 0;
    end record;

    type Camera_Array is array(Positive range <>) of Camera;
    Empty_Camera_Array : constant Camera_Array(1 .. 0) := (others => <>);

    type Flip_Orientation is (None, Horizontal, Vertical, L_90, R_90);
    type Flip_Kind is array(1 .. 2) of Flip_Orientation;
    type Flip_Array is array(Positive range <>) of Flip_Kind;
    Flips : constant Flip_Array := (
        (None, None),
        (Horizontal, None),
        (Vertical, None),
        (Horizontal, Vertical),
        (L_90, None),
        (R_90, None),
        (R_90, R_90),
        (R_90, Horizontal),
        (L_90, Horizontal)--,
        -- (R_90, Vertical),
        -- (L_90, Vertical)
    );

    type Natural_Couple is array (1 .. 2) of Natural;
    type Pattern is array(Positive range <>) of Natural_Couple;

    function Hash(I : in Integer) return Ada.Containers.Hash_Type is (Ada.Containers.Hash_Type(I));
    package Integer_Sets is new Ada.Containers.Hashed_Sets(Integer, Hash, "=", "=");

    function "-"(neighbour : in Neighbour_Type) return Neighbour_Type is
    begin
        case neighbour is
            when Up | Left =>
                return Neighbour_Type'Succ(neighbour);
            when Down | Right =>
                return Neighbour_Type'Pred(neighbour);
        end case;
    end "-";

    function Get_Camera(F : in TIO.File_Type) return Camera is
        cam : Camera;
        line : String(Tile_Range);
    begin
        while TIO.End_Of_Line(F) loop
            TIO.Skip_Line(F);
        end loop;
        declare
            tile_line : constant String := TIO.Get_Line(F);
        begin
            cam.id := Long_Long_Integer'Value(tile_line(tile_line'First + 5 .. tile_line'Last - 1));
        end;
        for Y in Tile_Range loop
            line := TIO.Get_Line(F);
            for X in Tile_Range loop
                cam.image(Y, X) := line(X) = '#';
            end loop;
        end loop;
        return cam;
    end Get_Camera;

    function Get(F : in TIO.File_Type) return Camera_Array is
        function Get_Rec(accumulator : Camera_Array) return Camera_Array is
        begin
            if TIO.End_Of_File(F) then
                return accumulator;
            end if;
            return Get_Rec(accumulator & Get_Camera(F));
        end Get_Rec;
    begin
        return Get_Rec(Empty_Camera_Array);
    end Get;

    function Flip(input : in Image_Type; kind : Flip_Kind) return Image_Type is
        output : Image_Type := input;
        tmp : Boolean;
    begin
        for k of kind loop
            case k is
                when None => null;
                when Horizontal =>
                    for Y in input'Range loop
                        for X in input'First(2) .. input'First(2) + (input'Length(2) / 2) - 1 loop
                            tmp := output(Y, X);
                            output(Y, X) := output(Y, output'Last(2) - X + 1);
                            output(Y, output'Last(2) - X + 1) := tmp;
                        end loop;
                    end loop;
                when Vertical =>
                    for X in input'Range(2) loop
                        for Y in input'First .. input'First + (input'Length / 2) - 1 loop
                            tmp := output(Y, X);
                            output(Y, X) := output(output'Last - Y + 1, X);
                            output(output'Last - Y + 1, X) := tmp;
                        end loop;
                    end loop;
                when L_90 =>
                    for Y in input'Range loop
                        for X in input'Range(2) loop
                            output(input'Last(1) - X + 1, Y) := input(Y, X);
                        end loop;
                    end loop;
                when R_90 =>
                    for Y in input'Range loop
                        for X in input'Range(2) loop
                            output(X, input'Last(2) - Y + 1) := input(Y, X);
                        end loop;
                    end loop;
            end case;
        end loop;
        return output;
    end Flip;

    function Get_Slice(input : in Tile; position : Neighbour_Type) return Slice is
        X, Y : Positive;
        S : Slice;
    begin
        case position is
            when Up | Down =>
                Y := (if position = Up then 1 else input'Last);
                for X in input'Range(2) loop
                    S(X) := input(Y, X);
                end loop;
            when Left | Right =>
                X := (if position = Left then 1 else input'Last(2));
                for Y in input'Range loop
                    S(Y) := input(Y, X);
                end loop;
        end case;
        return S;
    end Get_Slice;

    procedure Check_Neighbour(cameras : in out Camera_Array; L_i, R_i : in Positive; attach : Boolean; where : out Neighbour_Type; Found : out Boolean) is
        L : Camera renames cameras(L_i);
        R : Camera renames cameras(R_i);
        L_tmp, R_tmp : Tile;
        L_slice, R_slice : Slice;
        L_flips : constant Flip_Array := (if attach and L.flipped then Flips(1 .. 1) else Flips);
        R_flips : constant Flip_Array := (if attach and R.flipped then Flips(1 .. 1) else Flips);
    begin
        where := Up;
        found := False;
        if not(attach) and (L.n_found = 4 or R.n_found = 4) then
            return;
        end if;

        main:for L_flip of L_flips loop
            for R_Flip of R_flips loop
                L_tmp := Flip(L.image, L_flip);
                R_tmp := Flip(R.image, R_flip);
                for neighbour in Neighbour_Type loop
                    L_slice := Get_Slice(L_tmp, neighbour);
                    R_slice := Get_Slice(R_tmp, -neighbour);
                    if L_slice = R_slice then
                        if attach then
                            L.flipped := True;
                            L.image := L_tmp;
                            R.flipped := True;
                            R.image := R_tmp;
                            L.neighbours_sided(neighbour) := R_i;
                            R.neighbours_sided(-neighbour) := L_i;
                        else
                            L.n_found := L.n_found + 1;
                            L.neighbours(L.n_found) := R_i;
                            R.n_found := R.n_found + 1;
                            R.neighbours(R.n_found) := L_i;
                        end if;

                        where := neighbour;
                        found := True;
                        exit main;
                    end if;
                end loop;
            end loop;
        end loop main;
    end Check_Neighbour;

    procedure Order(cameras : in out Camera_Array) is
        start : Positive;
        where : Neighbour_Type;
        found : Boolean;
        seen : Integer_Sets.Set;

        procedure Do_Check(index : in Positive) is
            C : Camera renames cameras(index);
        begin
            seen.Include(index);
            for I in 1 .. C.n_found loop
                if not seen.Contains(C.neighbours(I)) then
                    Check_Neighbour(cameras, index, C.neighbours(I), True, where, found);
                end if;
            end loop;
            for I in 1 .. C.n_found loop
                if not seen.Contains(C.neighbours(I)) then
                    Do_Check(C.neighbours(I));
                end if;
            end loop;
        end Do_Check;
    begin
        for I in cameras'Range loop
            if cameras(I).n_found = 2 then
                start := I;
                exit;
            end if;
        end loop;
        Do_Check(start);
    end Order;

    function Assemble(cameras : in Camera_Array) return Image_Type is
        SIZE : constant Integer := Positive(ANGEF.Sqrt(Float(cameras'Length))) * 8;
        image : Image_Type(1 .. SIZE, 1 .. SIZE);
        line, col : Natural;
        nth_line, nth_col : Natural := 0;
    begin
        -- finding upper left angle
        for C in cameras'Range loop
            if cameras(C).n_found = 2 and cameras(C).neighbours_sided(Down) /= 0 and cameras(C).neighbours_sided(Right) /= 0 then
                line := C;
                exit;
            end if;
        end loop;
        nth_line := 0;
        while line /= 0 loop
            col := line;
            nth_col := 0;
            while col /= 0 loop
                for L in 1 .. 8 loop
                    for C in 1 .. 8 loop
                        image(image'First + (nth_line * 8) + L - 1, image'First + (nth_col * 8) + C - 1) := cameras(col).image(L + 1, C + 1);
                    end loop;
                end loop;
                col := cameras(col).neighbours_sided(Right);
                nth_col := nth_col + 1;
            end loop;
            line := cameras(line).neighbours_sided(Down);
            nth_line := nth_line + 1;
        end loop;
        return image;
    end Assemble;

    function Check_Pattern(pat : in Pattern; img : in Image_Type; y, x: Positive) return Boolean is
    begin
        for NC of pat loop
            if y + NC(1) > img'Last or else x + NC(2) > img'Last(2) or else not img(y + NC(1), x + NC(2)) then
                return False;
            end if;
        end loop;
        return True;
    end Check_Pattern;

    procedure Remove_Pattern(pat : in Pattern; img : in out Image_Type; y, x: Positive) is
    begin
        for NC of pat loop
            img(y + NC(1), x + NC(2)) := False;
        end loop;
    end Remove_Pattern;

    procedure Part_1(cameras : in out Camera_Array; product : out Long_Long_Integer) is
        where : Neighbour_Type;
        found : Boolean;
    begin
        -- finding neighbours without ordering them
        for L in cameras'First .. cameras'Last - 1 loop
            for R in L + 1 .. cameras'Last loop
                Check_Neighbour(cameras, L, R, False, where, found);
            end loop;
        end loop;
        Order(cameras);

        product := 1;
        for cam of cameras loop
            if cam.n_found = 2 then
                product := product * cam.id;
            end if;
        end loop;
    end Part_1;

    function Part_2(image : Image_Type; pat : in Pattern) return Natural is
        tmp : Image_Type(image'Range, image'Range(2));
        total : Natural := 0;
        found : Boolean := False;
    begin
        for flipping of Flips loop
            tmp := Flip(image, flipping);
            for Y in image'Range loop
                for X in image'Range(2) loop
                    if Check_Pattern(pat, tmp, Y, X) then
                        Remove_Pattern(pat, tmp, Y, X);
                        found := True;
                    end if;
                end loop;
            end loop;
            exit when found;
        end loop;

        total := 0;
        for Y in tmp'Range loop
            for X in tmp'Range(2) loop
                if tmp(Y, X) then
                    total := total + 1;
                end if;
            end loop;
        end loop;
        return total;
    end Part_2;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;

    dragon : constant Pattern := (
        (0, 18),
        (1, 0), (1, 5), (1, 6), (1, 11), (1, 12), (1, 17), (1, 18), (1, 19),
        (2, 1), (2, 4), (2, 7), (2, 10), (2, 13), (2, 16)
    );
begin
    TIO.Put_Line("--- Day 20: Jurassic Jigsaw ---");

    TIO.Open(F, TIO.In_File, filepath);
    declare
        cameras : Camera_Array := Get(F);
        p1 : Long_Long_Integer;
    begin
        TIO.Put_Line("Assemble the tiles into an image. What do you get if you multiply together the IDs of the four corner tiles?");
        Part_1(cameras, p1);
        TIO.Put_Line(p1'Img);
        TIO.Put_Line("How many # are not part of a sea monster?");
        TIO.Put_Line(Part_2(Assemble(cameras), dragon)'Img);
    end;
    TIO.Close(F);
end Day20;
