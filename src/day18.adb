with Ada.Text_IO;
with Ada.Long_Long_Integer_Text_IO;
with Ada.Containers.Vectors;
with Ada.Command_Line;

procedure Day18 is
    package TIO renames Ada.Text_IO;
    package LLITIO renames Ada.Long_Long_Integer_Text_IO;

    function Plus(L,R : Long_Long_Integer) return Long_Long_Integer is (L + R);
    function Times(L,R : Long_Long_Integer) return Long_Long_Integer is (L * R);

    type Symbol_Kind is (Plus_Op, Times_Op, Number, Open_Paren, Close_Paren);
    type Token(kind : Symbol_Kind := Number) is record
        case kind is
            when Number => value : Long_Long_Integer;
            when others => null;
        end case;
    end record;

    package Expression_Lists is new Ada.Containers.Vectors(Positive, Token);
    subtype Expression is Expression_Lists.Vector;

    type Expression_Array is array(Positive range <>) of Expression;
    Empty_Expression_Array : constant Expression_Array(1 .. 0) := (others => <>);

    To_Op : constant array(Plus_Op .. Times_Op) of access function(L,R : Long_Long_Integer) return Long_Long_Integer :=
       (Plus_Op => Plus'Access,
        Times_Op => Times'Access);
    
    function Parse(input : in String) return Expression is
        expr : Expression;
        index : Positive := input'First;
        tmp : Long_Long_Integer;
    begin
        while index <= input'Last loop
            case input(index) is
                when ' ' => null;
                when '+' => expr.Append((Kind => Plus_Op));
                when '*' => expr.Append((Kind => Times_Op));
                when '(' => expr.Append((Kind => Open_Paren));
                when ')' => expr.Append((Kind => Close_Paren));
                when '0' .. '9' =>
                    LLITIO.Get(input(index .. input'Last), tmp, last => index);
                    expr.Append((Number, tmp));
                when others =>
                    raise Constraint_Error with "Invalid character: " & input(index);
            end case;
            index := index + 1;
        end loop;
        return expr;
    end Parse;

    function Get(F : in TIO.File_Type) return Expression_Array is
        function Get(accumulator : in Expression_Array) return Expression_Array is
        begin
            if TIO.End_Of_File(F) then
                return accumulator;
            end if;
            return Get(accumulator & Parse(TIO.Get_Line(F)));
        end Get;
    begin
        return Get(Empty_Expression_Array);
    end Get;

    function Evaluate(expr : in Expression; advanced : Boolean) return Long_Long_Integer is
        actual : Expression := expr.Copy;
        function Eval return Long_Long_Integer is
            ops : array(1 .. Natural(expr.Length)) of Symbol_Kind;
            last_op : Natural := 0;
            nums : array(ops'Range) of Long_Long_Integer;
            last_num : Natural := 0;
        begin
            while not actual.Is_Empty loop
                case actual.First_Element.kind is
                    when Plus_Op .. Times_Op =>
                        last_op := last_op + 1;
                        ops(last_op) := actual.First_Element.kind;
                        actual.Delete_First;
                    when Number =>
                        last_num := last_num + 1;
                        nums(last_num) := actual.First_Element.value;
                        actual.Delete_First;
                        if advanced then
                            -- handling + precedence by consuming them immediately
                            if last_op > 0 and then ops(last_op) = Plus_Op then
                                nums(last_num - 1) := nums(last_num - 1) + nums(last_num);
                                last_num := last_num - 1;
                                last_op := last_op - 1;
                            end if;
                        elsif last_num = 2 then
                            nums(last_num - 1) := To_Op(ops(last_op))(nums(last_num - 1), nums(last_num));
                            last_num := last_num - 1;
                            last_op := last_op - 1;
                        end if;
                    when Open_Paren =>
                        actual.Delete_First;
                        if advanced then
                            last_num := last_num + 1;
                            nums(last_num) := Eval;
                            if last_op > 0 and then ops(last_op) = Plus_Op then
                                nums(last_num - 1) := nums(last_num - 1) + nums(last_num);
                                last_num := last_num - 1;
                                last_op := last_op - 1;
                            end if;
                        else
                            if last_num /= 0 then
                                nums(last_num) := To_Op(ops(last_op))(nums(last_num), Eval);
                                last_op := last_op - 1;
                            else
                                last_num := last_num + 1;
                                nums(last_num) := Eval;
                            end if;
                        end if;
                    when Close_Paren =>
                        actual.Delete_First;
                        exit when advanced;
                        return nums(last_num);
                end case;
            end loop;
            if advanced then -- handle remaining * on the stack
                while last_op /= 0 loop
                    nums(last_num - 1) := nums(last_num - 1) * nums(last_num);
                    last_op := last_op - 1;
                    last_num := last_num - 1;
                end loop;
            end if;
            return nums(1);
        end Eval;
    begin
        return Eval;
    end Evaluate;

    function Sum_Expressions(expressions : in Expression_Array; advanced : Boolean) return Long_Long_Integer is
        sum : Long_Long_Integer := 0;
    begin
        for expression of expressions loop
            sum := sum + Evaluate(expression, advanced);
        end loop;
        return sum;
    end Sum_Expressions;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
begin
    TIO.Open(F, TIO.In_File, filepath);
    declare
        expressions : constant Expression_Array := Get(F);
    begin
        TIO.Put_Line(Sum_Expressions(expressions, False)'Img);
        TIO.Put_Line(Sum_Expressions(expressions, True)'Img);
    end;
    TIO.Close(F);
end Day18;
