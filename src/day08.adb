with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;

procedure Day08 is
    package TIO renames Ada.Text_IO;

    type Operation is (acc, jmp, nop);

    type Instruction is record
        op : Operation;
        argument : Integer;
    end record;

    type Instruction_Array is array(Positive range <>) of Instruction;
    Empty_Instruction_Array : constant Instruction_Array(1 .. 0) := (others => <>);

    type Boot_Code(size : Natural) is record
        accumulator : Integer;
        instructions : Instruction_Array(1 .. size);
    end record;

    package Operation_IO is new Ada.Text_IO.Enumeration_IO(Operation);

    function Read(F : in TIO.File_Type) return Boot_Code is
        function Get_Instructions(accumulator : Instruction_Array) return Instruction_Array is
            op : Operation;
            arg : Integer;
        begin
            if TIO.End_Of_File(F) then
                return accumulator;
            end if;
            Operation_IO.Get(F, op);
            Ada.Integer_Text_IO.Get(F, arg);
            return Get_Instructions(accumulator & Instruction'(op, arg));
        end Get_Instructions;
        instructions : constant Instruction_Array := Get_Instructions(Empty_Instruction_Array);
    begin
        return Boot_Code'(instructions'Length, 0, instructions);
    end Read;

    procedure Part_1(code : in out Boot_Code; looped : out Boolean) is
        seen : array(code.instructions'range) of Boolean := (others => False);
        I : Positive := code.instructions'First;
    begin
        code.accumulator := 0;
        while I in code.instructions'Range and then not(seen(I)) loop
            seen(I) := True;
            case code.instructions(I).op is
                when acc =>
                    code.accumulator := code.accumulator + code.instructions(I).argument;
                    I := I + 1;
                when jmp =>
                    I := I + code.instructions(I).argument;
                when nop =>
                    I := I + 1;
            end case;
            looped := I in code.instructions'Range and then seen(I);
        end loop;
    end Part_1;

    procedure Part_2(code : in out Boot_Code; changed : out Positive) is
        tmp : Operation;
        looped : Boolean;
    begin
        for I in code.instructions'Range loop
            changed := I;
            tmp := code.instructions(I).op;
            case tmp is
                when acc => null;
                when jmp =>
                    code.instructions(I).op := nop;
                    Part_1(code, looped);
                    if looped then
                        code.instructions(I).op := jmp;
                    end if;
                when nop =>
                    code.instructions(I).op := jmp;
                    Part_1(code, looped);
                    if looped then
                        code.instructions(I).op := nop;
                    end if;
            end case;
            exit when not looped;
        end loop;
    end Part_2;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
begin
    TIO.Put_Line("--- Day 8: Handheld Halting ---");

    TIO.Open(F, TIO.In_File, filepath);
    declare
        code : Boot_Code := Read(F);
        looped : Boolean;
        changed : Positive;
    begin
        Part_1(code, looped);
        TIO.Put_Line("Run your copy of the boot code. Immediately before any instruction is executed a second time, what value is in the accumulator?");
        TIO.Put_Line(code.accumulator'Img);
        TIO.New_Line;
        TIO.Put_Line("Fix the program so that it terminates normally by changing exactly one jmp (to nop) or nop (to jmp). What is the value of the accumulator after the program terminates?");
        Part_2(code, changed);
        TIO.Put_Line(code.accumulator'Img & " after changing instruction" & changed'Img);
    end;
    TIO.Close(F);
end Day08;
