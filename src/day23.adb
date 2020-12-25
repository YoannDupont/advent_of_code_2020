with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Long_Long_Integer_Text_IO;
with Ada.Command_Line;

procedure Day23 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;
    package LLITIO renames Ada.Long_Long_Integer_Text_IO;

    subtype Cup is Positive range 1 .. 1_000_000;
    type Cup_Array is array(Cup range <>) of Cup;
    type Game(Length : Cup) is
    record
        cups : Cup_Array(1 .. Length) := (others => <>);
    end record;

    function Previous(C : in Cup; G : in Game) return Cup is
    begin
        if C = 1 then
            return G.Length;
        else
            return C - 1;
        end if;
    end Previous;

    function Next(C : in Cup; G : in Game) return Cup is
    begin
        if C = G.Length then
            return 1;
        else
            return C + 1;
        end if;
    end Next;

    function In_Next_3(C : in Cup; G : in Game; start : in Cup) return Boolean is
        I : Cup := start;
    begin
        for J in 1 .. 3 loop
            I := G.cups(I);
            if I = C then
                return True;
            end if;
        end loop;
        return False;
    end In_Next_3;

    procedure Move_3(G : in out Game; start : in Cup; destination : in Cup) is
        first : constant Cup := G.cups(start);
        last : constant Cup := G.cups(G.cups(G.cups(start)));
    begin
        G.cups(start) := G.cups(last);
        G.cups(last) := G.cups(destination);
        G.cups(destination) := first;
    end Move_3;

    function Get(F : in TIO.File_Type) return Cup_Array is
        input : constant String := TIO.Get_Line(F);
        CA : Cup_Array(1 .. input'Length);
    begin
        for I in 1 .. CA'Length loop
            CA(I) := Cup'Value((1 => input(input'First + I - 1)));
        end loop;
        return CA;
    end Get;

    function Fill(input : in Cup_Array; to : Cup) return Cup_Array is
        CA : Cup_Array(1 .. to);
    begin
        CA(1 .. input'Length) := input;
        for I in input'Length + 1 .. to loop
            CA(I) := I;
        end loop;
        return CA;
    end Fill;

    function To_Game(CA : in Cup_Array) return Game is
        G : Game(CA'Length);
    begin
        for I in 1 .. G.Length loop
            G.cups(CA(I)) := CA(Next(I, G));
        end loop;
        return G;
    end To_Game;

    procedure Play(G : in out Game; first : Cup; N : Positive) is
        destination : Cup;
        cand : Cup;
        source : Cup := first;
    begin
        for I in 1 .. N loop
            cand := Previous(source, G);
            while In_Next_3(cand, G, source) loop
                cand := Previous(cand, G);
            end loop;
            destination := cand;
            Move_3(G, source, destination);
            source := G.cups(source);
        end loop;
    end Play;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
begin
    TIO.Open(F, TIO.In_File, filepath);
    declare
        CA : constant Cup_Array := Get(F);
        G : Game := To_Game(CA);
        G2 : Game := To_Game(Fill(CA, 1_000_000));
        first : constant Cup := CA(1);
        tmp : Cup;
        p2 : Long_Long_Integer := 1;
    begin
        TIO.Put_Line("Using your labeling, simulate 100 moves. What are the labels on the cups after cup 1?");
        Play(G, first, 100);
        TIO.Put("Cups: ");
        tmp := G.cups(1);
        for I in 2 .. G.Length loop
            ITIO.Put(tmp, width => 0);
            tmp := G.cups(tmp);
        end loop;
        TIO.New_Line;
        TIO.New_Line;

        TIO.Put_Line("Determine which two cups will end up immediately clockwise of cup 1. What do you get if you multiply their labels together?");
        Play(G2, first, 10_000_000);
        TIO.Put("Cups: ");
        tmp := G2.cups(1);
        for I in 1 .. 2 loop
            ITIO.Put(tmp, width => 0);
            TIO.Put(" ");
            p2 := p2 * Long_Long_Integer(tmp);
            tmp := G2.cups(tmp);
        end loop;
        TIO.New_Line;
        LLITIO.Put(p2, width => 0);
    end;
    TIO.Close(F);
end Day23;
