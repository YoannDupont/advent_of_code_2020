with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Assertions;
with Ada.Command_Line;
with Ada.Containers.Indefinite_Vectors;

procedure Day03 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;

    subtype Long_Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

    type Slope is record
        right : Positive;
        down : Positive;
    end record;

    type Slope_Array is array(Positive range <>) of Slope;

    package Maps is new Ada.Containers.Indefinite_Vectors(Positive, String);
    subtype Map is Maps.Vector;

    function Element(pattern : in String; index : in Positive) return Character is
    begin
        return pattern(pattern'First + ((index - 1) rem pattern'Length));
    end Element;

    function Part_1(M : in Map; s : in Slope) return Natural is
        how_many : Natural := 0;
        C : Maps.Cursor := M.First;
        x : Natural := 1;
    begin
        loop
            for I in 1 .. s.down loop
                Maps.Next(C);
            end loop;
            exit when not Maps.Has_Element(C);
            x := x + s.right;
            if Element(Maps.Element(C), x) = '#' then
                how_many := how_many + 1;
            end if;
        end loop;
        return how_many;
    end Part_1;

    function Part_2(M : in Map; slopes : in Slope_Array) return Long_Long_Natural is
        product : Long_Long_Natural := 1;
    begin
        for S of slopes loop
            product := product * Long_Long_Natural(Part_1(M, S));
        end loop;
        return product;
    end Part_2;

    test_1_map : Map;
    test_1_Slope : constant Slope := (3, 1);

    filename : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    input : Map;
    part_1_slope : constant Slope := (3, 1);
    part_2_slopes : constant Slope_Array := ((1, 1), (3, 1), (5, 1), (7, 1), (1, 2));
begin
    test_1_map.Append("..##.......");
    test_1_map.Append("#...#...#..");
    test_1_map.Append(".#....#..#.");
    test_1_map.Append("..#.#...#.#");
    test_1_map.Append(".#...##..#.");
    test_1_map.Append("..#.##.....");
    test_1_map.Append(".#.#.#....#");
    test_1_map.Append(".#........#");
    test_1_map.Append("#.##...#...");
    test_1_map.Append("#...##....#");
    test_1_map.Append(".#..#...#.#");
    Ada.Assertions.Assert(Part_1(test_1_map, Test_1_Slope) = 7);
    Ada.Assertions.Assert(Part_2(test_1_map, part_2_slopes) = 336);

    TIO.Open(F, TIO.In_File, filename);
    while not TIO.End_Of_File(F) loop
        input.Append(TIO.Get_Line(F));
    end loop;
    TIO.Close(F);
    
    TIO.Put_Line("Starting at the top-left corner of your map and following a slope of right 3 and down 1, how many trees would you encounter?");
    ITIO.Put(Part_1(input, part_1_slope), width => 0);
    TIO.New_Line;
    TIO.New_Line;
    
    TIO.Put_Line("What do you get if you multiply together the number of trees encountered on each of the listed slopes?");
    TIO.Put(Part_2(input, part_2_slopes)'Img);
    TIO.New_Line;
end Day03;
