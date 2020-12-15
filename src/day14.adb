with Ada.Text_IO;
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Fixed;
with Ada.Command_Line;

procedure Day14 is
    package TIO renames Ada.Text_IO;
    package ASF renames Ada.Strings.Fixed;

    subtype Long_Long_Natural is Long_Long_Integer range 0 .. Long_Long_Integer'Last;
    subtype Mask_Range is Natural range 1 .. 36;
    type Unsigned is mod 2**Mask_Range'Last;
    type Unsigned_Array is array(Positive range <>) of Unsigned;
    type Positive_Array is array(Positive range <>) of Positive;
    Empty_Array : constant Positive_Array(1 .. 0) := (others => <>);

    package Unsigned_Maps is new Ada.Containers.Ordered_Maps(Unsigned, Unsigned);
    subtype Memory is Unsigned_Maps.Map;

    function To_Mask(input : in String; filter_in : in Character) return Unsigned is
        mask : Unsigned := 0;
    begin
        for shift in 0 .. input'Length - 1 loop
            if input(input'Last - shift) = filter_in then
                mask := mask + 2 ** shift;
            end if;
        end loop;
        return mask;
    end To_Mask;

    procedure Make_Masks(input : in String; masks_positives, masks_negatives : out Unsigned_Array; alternatives : out Natural) is
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
        mask_negative : Unsigned := 0;
        mask_positive : Unsigned := 0;

        procedure Fill(index : in Positive) is
        begin
            if index > indices'Last then
                masks_positives(masks_positives'First + alternatives) := mask_positive;
                masks_negatives(masks_negatives'First + alternatives) := mask_negative;
                alternatives := alternatives + 1;
            else
                mask_negative := mask_negative + 2 ** (Mask_Range'Last - indices(index));
                Fill(index + 1);
                mask_negative := mask_negative - 2 ** (Mask_Range'Last - indices(index));
                mask_positive := mask_positive + 2 ** (Mask_Range'Last - indices(index));
                Fill(index + 1);
                mask_positive := mask_positive - 2 ** (Mask_Range'Last - indices(index));
            end if;
        end Fill;
    begin
        alternatives := 0;
        Fill(indices'First);
    end Make_Masks;

    procedure Part_1(F : in out TIO.File_Type; output : out Long_Long_Natural) is
        mask_p, m_n, masked : Unsigned;
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
                    mask_p := To_Mask(line(space + 3 .. line'Last), '1');
                    m_n := To_Mask(line(space + 3 .. line'Last), '0');
                else
                    address := Unsigned'Value(line(ASF.Index(line, "[") + 1 .. ASF.Index(line, "]") - 1));
                    value := Unsigned'Value(line(space + 3 .. line'Last));
                    masked := (value or mask_p) and not m_n;
                    mem.Include(address, masked);
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

        floating_p, floating_n : Unsigned_Array(1 .. Max_Alternatives);
        mask_p : Unsigned;
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
                    mask_p := To_Mask(line(space + 3 .. line'Last), '1');
                    Make_Masks(line(space + 3 .. line'Last), floating_p, floating_n, n_alts);
                else
                    address := Unsigned'Value(line(ASF.Index(line, "[") + 1 .. ASF.Index(line, "]") - 1));
                    value := Unsigned'Value(line(space + 3 .. line'Last));
                    address := address or mask_p;
                    for I in 1 .. n_alts loop
                        mem.Include((address or floating_p(I)) and not floating_n(I), value);
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
