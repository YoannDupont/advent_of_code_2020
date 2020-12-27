with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Indefinite_Vectors;
with Ada.Strings.Fixed;
with Ada.Command_Line;

use type Ada.Containers.Count_Type;

procedure Day19 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;
    package ASF renames Ada.Strings.Fixed;

    subtype Short_Natural is Natural range 0 .. 255;
    type SN_Array is array(Positive range <>) of Short_Natural;
    package SN_Vector_Vectors is new Ada.Containers.Indefinite_Vectors(Positive, SN_Array);
    package String_Vectors is new Ada.Containers.Indefinite_Vectors(Positive, String);
    type Production_Kind is (Terminal, Alternatives);
    type Production(kind : Production_Kind := Alternatives) is
    record
        case kind is
            when Terminal => value : Character;
            when Alternatives => choices : SN_Vector_Vectors.Vector;
        end case;
    end record;
    -- hold the set of production rules
    type Grammar is array(Short_Natural'Range) of Production;
    Default_Grammar : constant Grammar := (others => <>);

    procedure Get(F : TIO.File_Type; target : out Grammar) is
    begin
        target := Default_Grammar;
        while not TIO.End_Of_Line(F) loop
            declare
                line : constant String := TIO.Get_Line(F);
                ident, buffer : Short_Natural;
                last : Positive;
                rule_buffer : SN_Array(1 .. 3) := (others => <>);
                lst : Natural := 0;
            begin
                last := ASF.Index(line, ":");
                ident := Short_Natural'Value(line(line'First .. last - 1));
                if ASF.Index(line, """") /= 0 then
                    target(ident) := (Terminal, line(last + 3));
                else
                    last := last + 2;
                    while last <= line'Last loop
                        if line(last) = '|' then
                            target(ident).choices.Append(rule_buffer(1 .. lst));
                            lst := 0;
                        else
                            ITIO.Get(line(last .. line'Last), buffer, last => last);
                            -- vals.Append(buffer);
                            lst := lst + 1;
                            rule_buffer(lst) := buffer;
                        end if;
                        last := last + 2;
                    end loop;
                    if lst /= 0 then
                        target(ident).choices.Append(rule_buffer(1 .. lst));
                        lst := 0;
                    end if;
                end if;
            end;
        end loop;
    end Get;

    function Match(input : in String; cfg : in Grammar; start : in Short_Natural := 0) return Boolean is
        function Do_Match(current : in Positive; stack : in SN_Array) return Boolean is
            rhs : Production;
            choices : SN_Vector_Vectors.Vector;
        begin
            if input'Last < current then
                return stack'Length = 0;
            elsif stack'Length = 0 then
                return False;
            end if;

            rhs := cfg(stack(stack'First));
            case rhs.kind is
                when Terminal =>
                    if input(current) /= rhs.value then
                        return False;
                    end if;
                    return Do_Match(current + 1, stack(stack'First + 1 .. stack'Last));
                when Alternatives =>
                    choices := rhs.choices;
                    for alternative of choices loop
                        if Do_Match(current, alternative & stack(stack'First + 1 .. stack'Last)) then
                            return True;
                        end if;
                    end loop;
                    return False;
            end case;
        end Do_Match;
    begin
        return Do_Match(input'First, (1 => start));
    end Match;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    cfg : Grammar;
    sum : Natural;
    inputs : String_Vectors.Vector;
begin
    TIO.Put_Line("--- Day 19: Monster Messages ---");

    TIO.Open(F, TIO.In_File, filepath);
    Get(F, cfg);
    TIO.Skip_Line(F);
    while not TIO.End_Of_File(F) loop
        inputs.Append(TIO.Get_Line(F));
    end loop;
    TIO.Close(F);

    TIO.Put_Line("How many messages completely match rule 0?");
    sum := 0;
    for input of inputs loop
        if Match(input, cfg) then
            sum := sum + 1;
        end if;
    end loop;
    TIO.Put_Line(sum'Img);

    TIO.Put_Line("After updating rules 8 and 11, how many messages completely match rule 0?");
    cfg(8).choices.Append((42, 8));
    cfg(11).choices.Append((42, 11, 31));
    sum := 0;
    for input of inputs loop
        if Match(input, cfg) then
            sum := sum + 1;
        end if;
    end loop;
    TIO.Put_Line(sum'Img);
end Day19;
