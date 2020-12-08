with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Assertions;
with Ada.Command_Line;

procedure Day01 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;

    No_Solution : exception;

    type Natural_Array is array(Positive range <>) of Natural;
    Empty_Arr : constant Natural_Array(1 .. 0) := (others => <>);

    function Read(F : in TIO.File_Type) return Natural_Array is
        function Read_Rec(acc : Natural_Array) return Natural_Array is
            N : Natural;
        begin
            if TIO.End_Of_File(F) then
                return acc;
            end if;
            ITIO.Get(F, N);
            return Read_Rec(acc & N);
        end Read_Rec;
    begin
        return Read_Rec(Empty_Arr);
    end Read;

    function Part_1(arr : in Natural_Array; goal : in Natural) return Natural is
    begin
        for I in arr'First .. arr'Last - 1 loop
            for J in I+1 .. arr'Last loop
                if arr(I) + arr(J) = goal then
                    return arr(I) * arr(J);
                end if;
            end loop;
        end loop;
        raise No_Solution with "found no solution for part 1";
    end Part_1;

    function Part_2(arr : in Natural_Array; goal : in Natural) return Natural is
    begin
        for I in arr'First .. arr'Last - 2 loop
            for J in I+1 .. arr'Last - 1 loop
                for K in J+1 .. arr'Last loop
                    if arr(I) + arr(J) + arr(K) = goal then
                        return arr(I) * arr(J) * arr(K);
                    end if;
                end loop;
            end loop;
        end loop;
        raise No_Solution with "found no solution for part 2";
    end Part_2;

    Test_Input : constant Natural_Array := (1721, 979, 366, 299, 675, 1456);
    Test_1_Output : constant Natural := Part_1(Test_Input, 2020);
    Test_2_Output : constant Natural := Part_2(Test_Input, 2020);

    File_Name : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
begin
    Ada.Assertions.Assert(Test_1_Output = 514579);
    Ada.Assertions.Assert(Test_2_Output = 241861950);
    
    TIO.Open(F, TIO.In_File, File_Name);
    declare
        expense_report : constant Natural_Array := Read(F);
        part_1_output : constant Natural := Part_1(expense_report, 2020);
        part_2_output : constant Natural := Part_2(expense_report, 2020);
    begin
        TIO.Put_Line("Find the two entries that sum to 2020; what do you get if you multiply them together?");
        ITIO.Put(part_1_output, width => 0);
        TIO.New_Line;
        TIO.New_Line;
        TIO.Put_Line("what is the product of the three entries that sum to 2020?");
        ITIO.Put(part_2_output, width => 0);
        TIO.New_Line;
    end;
    TIO.Close(F);
end Day01;
