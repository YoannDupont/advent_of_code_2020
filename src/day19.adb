with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Command_Line;

with GNAT.Regexp;

use Ada.Strings.Unbounded;
use type Ada.Containers.Count_Type;

procedure Day19 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;
    package ASF renames Ada.Strings.Fixed;

    subtype Short_Natural is Natural range 0 .. 255;
    package SN_Vectors is new Ada.Containers.Vectors(Positive, Short_Natural);
    package SN_Vector_Vectors is new Ada.Containers.Vectors(Positive, SN_Vectors.Vector, SN_Vectors."=");
    type Production_Kind is (Terminal, Alternatives);
    type Production(kind : Production_Kind := Alternatives) is
    record
        case kind is
            when Terminal => value : Character;
            when Alternatives => choices : SN_Vector_Vectors.Vector;
        end case;
    end record;
    -- hold the set of production rules
    type Production_Set is array(Short_Natural'Range) of Production;
    Default_Production_Set : constant Production_Set := (others => <>);

    procedure Get(F : TIO.File_Type; target : out Production_Set) is
    begin
        target := Default_Production_Set;
        while not TIO.End_Of_Line(F) loop
            declare
                line : constant String := TIO.Get_Line(F);
                ident, buffer : Short_Natural;
                last : Positive;
                vals : SN_Vectors.Vector;
            begin
                last := ASF.Index(line, ":");
                ident := Short_Natural'Value(line(line'First .. last - 1));
                if ASF.Index(line, """") /= 0 then
                    target(ident) := (Terminal, line(last + 3));
                else
                    last := last + 2;
                    while last <= line'Last loop
                        if line(last) = '|' then
                            target(ident).choices.Append(vals.Copy);
                            vals.Clear;
                        else
                            ITIO.Get(line(last .. line'Last), buffer, last => last);
                            vals.Append(buffer);
                        end if;
                        last := last + 2;
                    end loop;
                    if not vals.Is_Empty then
                        target(ident).choices.Append(vals.Copy);
                        vals.Clear;
                    end if;
                end if;
            end;
        end loop;
    end Get;

    function To_Pattern(PA : in Production_Set; start : Short_Natural) return String is
        function To_Pattern_Rec(current : Short_Natural) return String is
            buffer : Unbounded_String;
            tmp : SN_Vectors.Vector;
            index : Natural;
        begin
            case PA(current).kind is
                when Terminal =>
                    return (1 => PA(current).value);
                when Alternatives =>
                    tmp := PA(current).choices.Last_Element.Copy;
                    index := SN_Vectors.To_Index(tmp.Find(current));
                    if index /= 0 then
                        Append(buffer, "(");
                        for J in 1 .. 5 loop
                            Append(buffer, "(");
                            for K in 1 .. J loop
                                if index > 1 then
                                    Append(buffer, "(");
                                    for I in 1 .. index - 1 loop
                                        Append(buffer, To_Pattern_Rec(tmp.Element(I)));
                                    end loop;
                                    Append(buffer, ")");
                                end if;
                            end loop;
                            for K in 1 .. J loop
                                if index < Natural(tmp.length) then
                                    Append(buffer, "(");
                                    for I in index + 1 .. Natural(tmp.Length) loop
                                        Append(buffer, To_Pattern_Rec(tmp.Element(I)));
                                    end loop;
                                    Append(buffer, ")");
                                end if;
                            end loop;
                            Append(buffer, ")|");
                        end loop;
                        Delete(buffer, Length(buffer), Length(buffer));
                        Append(buffer, ")");
                    else
                        if PA(current).choices.Length > 1 then
                            Append(buffer, "(");
                        end if;
                        for V of PA(current).choices loop
                            for nxt of V loop
                                Append(buffer, To_Pattern_Rec(nxt));
                            end loop;
                            Append(buffer, '|');
                        end loop;
                        Delete(buffer, Length(buffer), Length(buffer));
                        if PA(current).choices.Length > 1 then
                            Append(buffer, ")");
                        end if;
                    end if;
                    return To_String(buffer);
            end case;
        end To_Pattern_Rec;
    begin
        return To_Pattern_Rec(start);
    end To_Pattern;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;

    test_case : Production_Set;
    lst : SN_Vectors.Vector;
    reg : GNAT.Regexp.Regexp;
    sum : Natural := 0;
begin
    TIO.Open(F, TIO.In_File, filepath);
    Get(F, test_case);
    reg := GNAT.Regexp.Compile(To_Pattern(test_case, 0));
    TIO.Skip_Line(F);
    while not TIO.End_Of_File(F) loop
        if GNAT.Regexp.Match(TIO.Get_Line(F), reg) then
            sum := sum + 1;
        end if;
    end loop;
    TIO.Put_Line(sum'Img);
    TIO.Close(F);
end Day19;
