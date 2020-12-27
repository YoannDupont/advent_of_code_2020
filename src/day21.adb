with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Sets;
with Ada.Command_Line;

use type Ada.Containers.Count_Type;

procedure Day21 is
    package TIO renames Ada.Text_IO;
    package ASU renames Ada.Strings.Unbounded;

    subtype Ingredient is ASU.Unbounded_String;
    No_Ingredient : Ingredient renames ASU.Null_Unbounded_String;
    function "<"(L,R : Ingredient) return Boolean renames ASU."<";
    function "="(L,R : Ingredient) return Boolean renames ASU."=";
    function "+"(input : Ingredient) return String renames ASU.To_String;
    package Ingredient_Sets is new Ada.Containers.Ordered_Sets(Ingredient);
    subtype Ingredient_Set is Ingredient_Sets.Set;

    type Allergen is (Dairy, Eggs, Fish, Nuts, Peanuts, Sesame, Shellfish, Soy);
    package Allergen_IO is new Ada.Text_IO.Enumeration_IO(Allergen);
    package Allergen_Sets is new Ada.Containers.Ordered_Sets(Allergen);
    subtype Allergen_Set is Allergen_Sets.Set;

    type A2IS is array(Allergen) of Ingredient_Set;

    type Food is
    record
        ingredients : Ingredient_Set;
        allergens : Allergen_Set;
    end record;
    type Food_Array is array(Positive range <>) of Food;
    Empty_Food_Array : constant Food_Array(1 .. 0) := (others => <>);

    function Get(F : in TIO.File_Type) return Ingredient is
        ing : Ingredient;
        C : Character;
        EOF : Boolean;
    begin
        TIO.Look_Ahead(F, C, EOF);
        if C = ' ' or C = '(' then
            return No_Ingredient;
        end if;
        while C /= ' ' loop
            TIO.Get(F, C);
            ASU.Append(ing, C);
            TIO.Look_Ahead(F, C, EOF);
        end loop;
        TIO.Get(F, C);
        return ing;
    end Get;

    function Get_Food(F : in TIO.File_Type) return Food is
        R : Food;
        A : Allergen;
        C : Character;
    begin
        loop
            declare
                tmp : constant Ingredient := Get(F);
            begin
                if tmp /= No_Ingredient then
                    R.ingredients.Include(tmp);
                else
                    exit;
                end if;
            end;
        end loop;
        TIO.Set_Col(F, TIO."+"(TIO.Col(F), 10));
        loop
            Allergen_IO.Get(F, A);
            R.allergens.Include(A);
            TIO.Get(F, C);
            exit when C = ')';
        end loop;
        return R;
    end Get_Food;

    function Get(F : in TIO.File_Type) return Food_Array is
        function Get_Rec(accumulator : Food_Array) return Food_Array is
        begin
            if TIO.End_Of_File(F) then
                return accumulator;
            end if;
            return Get_Rec(accumulator & Get_Food(F));
        end Get_Rec;
    begin
        return Get_Rec(Empty_Food_Array);
    end Get;

    procedure Part_1
       (recipes    : in     Food_Array;
        candidates :    out A2IS;
        allergenic :    out Ingredient_Set;
        count      :    out Ada.Containers.Count_Type)
    is
    begin
        candidates := (others => Ingredient_Sets.Empty_Set);
        for R of recipes loop
            for allerg of R.allergens loop
                candidates(allerg).Union(R.ingredients);
            end loop;
        end loop;
        for R of recipes loop
            for allerg of R.allergens loop
                candidates(allerg).Intersection(R.ingredients);
            end loop;
        end loop;
        -- assuming you decide only 1 new candidate per iteration
        -- this is the maximum number of iterations needed.
        for unused_var in Allergen loop
            for allerg in Allergen loop
                if candidates(allerg).Length = 1 then
                    for allerg1 in Allergen loop
                        if allerg1 /= allerg then
                            candidates(allerg1).Difference(candidates(allerg));
                        end if;
                    end loop;
                end if;
            end loop;
        end loop;

        allergenic := Ingredient_Sets.Empty_Set;
        count := 0;
        for allerg in Allergen loop
            allergenic.Union(candidates(allerg));
        end loop;
        for R of recipes loop
            count := count + R.ingredients.Difference(allergenic).Length;
        end loop;
        TIO.Put_Line(count'Img);
    end Part_1;

    procedure Part_2(candidates : in A2IS) is
    begin
        for allerg in Allergen loop
            if not candidates(allerg).Is_Empty then
                TIO.Put(+candidates(allerg).First_Element);
                TIO.Put(",");
            end if;
        end loop;
        TIO.New_Line;
    end Part_2;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
begin
    TIO.Put_Line("--- Day 21: Allergen Assessment ---");

    TIO.Open(F, TIO.In_File, filepath);
    declare
        recipes : constant Food_Array := Get(F);
        candidates : A2IS;
        allergenic : Ingredient_Set;
        count : Ada.Containers.Count_Type;
    begin
        TIO.Put_Line("Determine which ingredients cannot possibly contain any of the allergens in your list. How many times do any of those ingredients appear?");
        Part_1(recipes, candidates, allergenic, count);
        TIO.New_Line;
        TIO.Put_Line("Time to stock your raft with supplies. What is your canonical dangerous ingredient list?");
        Part_2(candidates);
    end;
    TIO.Close(F);
end Day21;
