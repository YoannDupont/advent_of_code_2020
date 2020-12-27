with Ada.Text_IO;
with Ada.Command_Line;

procedure Day06 is
    package TIO renames Ada.Text_IO;

    subtype Question_Index is Character range 'a' .. 'z';
    type Form is array(Question_Index'Range) of Boolean;
    All_Noes : constant Form := (others => False);
    All_Yesses : constant Form := (others => True);

    function To_Form(source : in String) return Form is
        F : Form := All_Noes;
    begin
        for C of source loop
            F(C) := True;
        end loop;
        return F;
    end To_Form;

    function Count(F : in Form; value : Boolean) return Natural is
        total : Natural := 0;
    begin
        for item of F loop
            if item = value then
                total := total + 1;
            end if;
        end loop;
        return total;
    end Count;

    function Form_Group(
        F : in TIO.File_Type;
        Compare : access function(L, R : in Boolean) return Boolean;
        base_value : Form
    ) return Form is
        end_form : Form := base_value;
        current : Form;
    begin
        while not TIO.End_Of_File(F) loop
            declare
                line : constant String := TIO.Get_Line(F);
            begin
                exit when line'Length = 0;
                current := To_Form(line);
                for I in end_form'Range loop
                    end_form(I) := Compare(end_form(I), current(I));
                end loop;
            end;
        end loop;
        return end_form;
    end Form_Group;

    -- "or" and "and" operators are intrinsic, we cannot use the function'Access idiom.
    -- We define some overlay function so we can use function'Access idiom.
    function part_1_cmp(L, R : in Boolean) return Boolean is (L or R) with Inline;
    function part_2_cmp(L, R : in Boolean) return Boolean is (L and R) with Inline;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    part_1_sum : Natural := 0;
    part_2_sum : Natural := 0;
begin
    TIO.Put_Line("--- Day 6: Custom Customs ---");

    TIO.Open(F, TIO.In_File, filepath);
    while not TIO.End_Of_File(F) loop
        part_1_sum := part_1_sum + Count(Form_Group(F, part_1_cmp'Access, All_Noes), True);
    end loop;
    TIO.Put_Line("For each group, count the number of questions to which anyone answered ""yes"". What is the sum of those counts?");
    TIO.Put_Line(part_1_sum'Img);
    TIO.Close(F);

    TIO.New_Line;
    TIO.Open(F, TIO.In_File, filepath);
    while not TIO.End_Of_File(F) loop
        part_2_sum := part_2_sum + Count(Form_Group(F, part_2_cmp'Access, All_Yesses), True);
    end loop;
    TIO.Put_Line("For each group, count the number of questions to which everyone answered ""yes"". What is the sum of those counts?");
    TIO.Put_Line(part_2_sum'Img);
    TIO.Close(F);
end Day06;
