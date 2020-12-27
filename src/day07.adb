with Ada.Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Command_Line;

use Ada.Containers;

use type Ada.Text_IO.Positive_Count;

procedure Day07 is
    package TIO renames Ada.Text_IO;
    package NIO is new Ada.Text_IO.Integer_IO(Natural);

    type Color_Shade is (
        bright, clear, dark, dim, dotted, drab, dull, faded, light, mirrored, muted, pale, plaid,
        posh, shiny, striped, vibrant, wavy
    );
    type Color_Name is (
        aqua, beige, black, blue, bronze, brown, chartreuse, coral, crimson, cyan, fuchsia, gold,
        gray, green, indigo, lavender, lime, magenta, maroon, olive, orange, plum, purple, red,
        salmon, silver, tan, teal, tomato, turquoise, violet, white, yellow
    );
    type Color is record
        shade : Color_Shade;
        name : Color_Name;
    end Record;

    function "<"(L,R : Color) return Boolean is
    begin
        return (L.shade < R.shade) or else (L.shade = R.shade and L.name < R.name);
    end "<";

    package Capacity_Maps is new Ordered_Maps(Color, Natural);
    subtype Capacity is Capacity_Maps.Map;
    package Rules_Maps is new Ordered_Maps(Color, Capacity_Maps.Map, "<", Capacity_Maps."=");
    subtype Rules is Rules_Maps.Map;
    package Color_Sets is new Ordered_Sets(Color);

    package Shade_IO is new TIO.Enumeration_IO(Color_Shade);
    package Name_IO is new TIO.Enumeration_IO(Color_Name);

    procedure Get(F : in TIO.File_Type; colr : out Color; C : out Capacity) is
        CHR : Character;
        EOL : Boolean;
        how_many : Natural;
        current_shade : Color_Shade;
        current_name : Color_Name;
    begin
        C.Clear;
        Shade_IO.Get(F, colr.shade);
        Name_IO.Get(F, colr.name);
        TIO.Set_Col(F, TIO.Col(F) + 14); -- " bags contain "
        TIO.Look_Ahead(F, CHR, EOL);
        if CHR = 'n' then -- "no other bags"
            TIO.Skip_Line(F);
            return;
        end if;
        while not TIO.End_Of_Line(F) loop
            NIO.Get(F, how_many);
            Shade_IO.Get(F, current_shade);
            Name_IO.Get(F, current_name);
            for I in 1 .. 6 - (if how_many = 1 then 1 else 0) loop -- " bags," or "bag, "
                TIO.Get(F, CHR);
            end loop;
            C.Insert((current_shade, current_name), how_many);
        end loop;
    end Get;

    function Part_1(R : in Rules; C : in Color) return Natural is
        seen, now, next, ok : Color_Sets.Set;
        cursor : Rules_Maps.Cursor;
        cap : Capacity;
        current_color : Color;
    begin
        seen.Insert(C);
        cursor := R.First;
        while Rules_Maps.Has_Element(cursor) loop
            cap := Rules_Maps.Element(cursor);
            if cap.Contains(C) then
                current_color := (Rules_Maps.Key(cursor).shade, Rules_Maps.Key(cursor).name);
                seen.Insert(current_color);
                now.Insert(current_color);
                ok.Insert(current_color);
            end if;
            Rules_Maps.Next(cursor);
        end loop;

        while not now.Is_Empty loop
            next.Clear;
            for clr of now loop
                cursor := R.First;
                while Rules_Maps.Has_Element(cursor) loop
                    cap := Rules_Maps.Element(cursor);
                    if cap.Contains(clr) then
                        current_color := (Rules_Maps.Key(cursor).shade, Rules_Maps.Key(cursor).name);
                        if not seen.Contains(current_color) then
                            seen.Insert(current_color);
                            next.Insert(current_color);
                            ok.Insert(current_color);
                        end if;
                    end if;
                    Rules_Maps.Next(cursor);
                end loop;
            end loop;
            now.Clear;
            now.Union(next);
        end loop;

        return Natural(ok.Length);
    end Part_1;

    function Part_2(R : in Rules; C : in Color) return Natural is
        function Bags_For(C : in Color; factor : in Positive := 1) return Natural is
            cap : Capacity;
            cursor : Capacity_Maps.Cursor;
            bags : Natural := 0;
            current_color : Color;
        begin
            cap := R.Element(C);
            cursor := cap.First;
            while Capacity_Maps.Has_Element(cursor) loop
                current_color := (Capacity_Maps.Key(cursor).shade, Capacity_Maps.Key(cursor).name);
                bags := bags + factor * Capacity_Maps.Element(cursor);
                bags := bags + Bags_For(current_color, factor * Capacity_Maps.Element(cursor));
                Capacity_Maps.Next(cursor);
            end loop;
            return bags;
        end Bags_For;
    begin
        return Bags_For(C);
    end Part_2;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    colr : Color;
    cap : Capacity;
    rls : Rules;
begin
    TIO.Put_Line("--- Day 7: Handy Haversacks ---");

    TIO.Open(F, TIO.In_File, filepath);
    while not TIO.End_Of_File(F) loop
        Get(F, colr, cap);
        rls.Insert(colr, cap);
    end loop;
    TIO.Close(F);

    TIO.Put_Line("How many bag colors can eventually contain at least one shiny gold bag?");
    TIO.Put_Line(Part_1(rls, (shiny, gold))'Img);

    TIO.Put_Line("How many individual bags are required inside your single shiny gold bag?");
    TIO.Put_Line(Part_2(rls, (shiny, gold))'Img);
end Day07;
