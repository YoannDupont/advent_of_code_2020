with Ada.Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Fixed;
with Ada.Unchecked_Conversion;
with Ada.Command_Line;

procedure Day14 is
    package TIO renames Ada.Text_IO;
    package ASF renames Ada.Strings.Fixed;

    subtype Long_Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;

    subtype Mask_Range is Natural range 1 .. 36;
    type Bit is mod 2;
    for Bit'Size use 1;
    type Bit_Array is array(Mask_Range) of Bit;
    pragma Pack(Bit_Array);
    type Unsigned is mod 2**Mask_Range'Last;

    package Masks is new Ada.Containers.Ordered_Maps(Mask_Range, Bit);
    subtype Mask is Masks.Map;
    package Unsigned_Maps is new Ada.Containers.Ordered_Maps(Unsigned, Unsigned);
    subtype Memory is Unsigned_Maps.Map;

    type Positive_Array is array(Positive range <>) of Positive;
    Empty_Array : constant Positive_Array(1 .. 0) := (others => <>);
    type Floating_Masks_Array is array(Positive range <>) of Mask;

    function To_Bit_Array is new Ada.Unchecked_Conversion(Unsigned, Bit_Array);
    function To_Unsigned is new Ada.Unchecked_Conversion(Bit_Array, Unsigned);

    function To_Mask(input : in String) return Mask is
        M : Mask;
    begin
        for shift in 0 .. input'Length - 1 loop
            if input(input'Last - shift) /= 'X' then
                M.Insert(shift + 1, Bit'Value((1 => input(input'Last - shift))));
            end if;
        end loop;
        return M;
    end To_Mask;

    function Right(L, R : in Bit) return Bit is (R);
    function Or_Op(L, R : in Bit) return Bit is (L or R);

    function Apply(M : in Mask; target : in Bit_Array; Operator : access function(L, R : in Bit) return Bit) return Bit_Array is
        output : Bit_Array := target;
        C : Masks.Cursor := M.First;
    begin
        while Masks.Has_Element(C) loop
            output(Masks.Key(C)) := Operator(output(Masks.Key(C)), Masks.Element(C));
            Masks.Next(C);
        end loop;
        return output;
    end Apply;

    procedure Make_Alternatives(input : in String; buffer : out Floating_Masks_Array; alternatives : out Natural) is
        function Make_Indices(index : Positive; accumulator : in Positive_Array) return Positive_Array is
        begin
            if index > input'Last then
                return accumulator;
            elsif input(index) /= 'X' then
                return Make_Indices(index + 1, accumulator);
            else
                return Make_Indices(index + 1, accumulator & (index - input'First + 1));
            end if;
        end Make_Indices;

        indices : constant Positive_Array := Make_Indices(input'First, Empty_Array);
        M_alts : Mask;

        procedure Next(index : in Positive) is
        begin
            if index > indices'Last then
                buffer(buffer'First + alternatives) := M_alts.Copy;
                alternatives := alternatives + 1;
            else
                M_alts.Insert(Mask_Range'Last - indices(index) + 1, 0);
                Next(index + 1);
                M_alts.Delete(Mask_Range'Last - indices(index) + 1);
                M_alts.Insert(Mask_Range'Last - indices(index) + 1, 1);
                Next(index + 1);
                M_alts.Delete(Mask_Range'Last - indices(index) + 1);
            end if;
        end Next;
    begin
        alternatives := 0;
        Next(indices'First);
    end Make_Alternatives;

    procedure Part_1(F : in out TIO.File_Type; output : out Long_Long_Natural) is
        M : Mask;
        address, value : Unsigned;
        mem : Memory;
    begin
        TIO.Reset(F);
        output := 0;
        while not TIO.End_Of_File(F) loop
            declare
                line : constant String := TIO.Get_Line(F);
                space : constant Natural := ASF.Index(line, " ");
            begin
                if line(line'First .. space - 1) = "mask" then
                    M := To_Mask(line(space + 3 .. line'Last));
                else
                    address := Unsigned'Value(line(ASF.Index(line, "[") + 1 .. ASF.Index(line, "]") - 1));
                    value := Unsigned'Value(line(space + 3 .. line'Last));
                    mem.Include(address, To_Unsigned(Apply(M, To_Bit_Array(value), Right'Access)));
                end if;
            end;
        end loop;
        for value of mem loop
            output := output + Long_Long_Natural(value);
        end loop;
    end Part_1;

    procedure Part_2(F : in out TIO.File_Type; output : out Long_Long_Natural) is
        function Max_Alternatives return Natural is
            total : Natural := 0;
        begin
            TIO.Reset(F);
            while not TIO.End_Of_File(F) loop
                total := Natural'Max(total, ASF.Count(TIO.Get_Line(F), "X"));
            end loop;
            return 2 ** total;
        end Max_Alternatives;

        floating_masks : Floating_Masks_Array(1 .. Max_Alternatives);
        M : Mask;
        n_alts : Natural;
        address, value : Unsigned; 
        mem : Memory;
    begin
        TIO.Reset(F);
        output := 0;
        while not TIO.End_Of_File(F) loop
            declare
                line : constant String := TIO.Get_Line(F);
                space : constant Natural := ASF.Index(line, " ");
            begin
                if line(line'First .. space - 1) = "mask" then
                    M := To_Mask(line(space + 3 .. line'Last));
                    Make_Alternatives(line(space + 3 .. line'Last), floating_masks, n_alts);
                else
                    address := Unsigned'Value(line(ASF.Index(line, "[") + 1 .. ASF.Index(line, "]") - 1));
                    value := Unsigned'Value(line(space + 3 .. line'Last));
                    address := To_Unsigned(Apply(M, To_Bit_Array(address), Or_Op'Access));
                    for I in 1 .. n_alts loop
                        mem.Include(To_Unsigned(Apply(floating_masks(I), To_Bit_Array(address), Right'Access)), value);
                    end loop;
                end if;
            end;
        end loop;
        for value of mem loop
            output := output + Long_Long_Natural(value);
        end loop;
    end Part_2;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
begin
    TIO.Open(F, TIO.In_File, filepath);
    declare
        p1 : Long_Long_Natural;
        p2 : Long_Long_Natural;
    begin
        TIO.Put_Line("Execute the initialization program. What is the sum of all values left in memory after it completes?");
        Part_1(F, p1);
        TIO.Put_Line(p1'Img);
        TIO.Put_Line("Execute the initialization program using an emulator for a version 2 decoder chip. What is the sum of all values left in memory after it completes?");
        Part_2(F, p2);
        TIO.Put_Line(p2'Img);
    end;
    TIO.Close(F);
end Day14;
