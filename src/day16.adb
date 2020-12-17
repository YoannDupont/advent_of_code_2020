with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Strings.Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Command_Line;

procedure Day16 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;
    package ASF renames Ada.Strings.Fixed;

    type Ticket_Field is
       (departure_location, departure_station, departure_platform, departure_track, departure_date,
        departure_time, arrival_location, arrival_station, arrival_platform, arrival_track, class,
        duration, price, route, row, seat, train, type_field, wagon, zone);
    subtype Departure is Ticket_Field range departure_location .. departure_time;
    type Ticket is array(1 .. Ticket_Field'Pos(Ticket_Field'Last) + 1) of Natural;
    type Ticket_Array is array(Positive range <>) of Ticket;
    Empty_Ticket_Array : constant Ticket_Array(1 .. 0) := (others => <>);

    type Value_Range is record
        from : Natural := 0;
        to : Natural := 0;
    end record;

    type Field_Value_Ranges is record
        first : Value_Range;
        last : Value_Range;
    end record;
    Empty_Ranges : constant Field_Value_Ranges := ((0, 0), (0, 0));

    type Ticket_Field_Ranges is array(Ticket_Field'Range) of Field_Value_Ranges;

    package Field_Sets is new Ada.Containers.Ordered_Sets(Ticket_Field);

    type Field_Set_Array is array (Positive range <>) of Field_Sets.Set;

    function To_Set(ranges : in Ticket_Field_Ranges) return Field_Sets.Set is
        S : Field_Sets.Set;
    begin
        for F in Ticket_Field loop
            if ranges(F) /= Empty_Ranges then
                S.Insert(F);
            end if;
        end loop;
        return S;
    end To_Set;

    function To_Ticket_Field(source : in String) return Ticket_Field is
    begin
        if source = "type" then
            return type_field;
        else
            return Ticket_Field'Value(ASF.Translate(source, Ada.Strings.Maps.To_Mapping(" ", "_")));
        end if;
    end To_Ticket_Field;

    procedure Get_Ticket_Field_Ranges
       (F : in TIO.File_Type;
        allowed : out Ticket_Field_Ranges;
        size : out Natural)
    is
    begin
        size := 0;
        while not TIO.End_Of_Line(F) loop
            declare
                line : constant String := TIO.Get_Line(F);
                index : Natural := ASF.Index(line, ":");
                field : constant Ticket_Field := To_Ticket_Field(line(line'First .. index - 1));
                ranges : Field_Value_Ranges := Empty_Ranges;
            begin
                size := size + 1;
                ITIO.Get(line(index + 1 .. line'Last), ranges.first.from, last => index);
                ITIO.Get(line(index + 2 .. line'Last), ranges.first.to, last => index);
                ITIO.Get(line(index + 5 .. line'Last), ranges.last.from, last => index);
                ITIO.Get(line(index + 2 .. line'Last), ranges.last.to, last => index);
                allowed(field) := ranges;
            end;
        end loop;
    end Get_Ticket_Field_Ranges;

    function Get_Ticket(F : in TIO.File_Type; size : in Natural) return Ticket is
        the_ticket : Ticket := (others => 0);
        line : constant String := TIO.Get_Line(F);
        start : Natural := line'First;
        index : Natural := ASF.Index(line, ",");
    begin
        for I in 1 .. size loop
            the_ticket(I) := Natural'Value(line(start .. index - 1));
            start := index + 1;
            index := ASF.Index(line(start .. line'Last), ",");
            if index = 0 then
                index := line'Last + 1;
            end if;
        end loop;
        return the_ticket;
    end Get_Ticket;

    function Get_Nearby_Tickets(F : in TIO.File_Type; size : in Natural) return Ticket_Array is
        function Get_Tickets(accumulator : in Ticket_Array) return Ticket_Array is
        begin
            if TIO.End_Of_File(F) then
                return accumulator;
            end if;
            return Get_Tickets(accumulator & Get_Ticket(F, size));
        end Get_Tickets;
    begin
        return Get_Tickets(Empty_Ticket_Array);
    end Get_Nearby_Tickets;

    function Is_In(N : in Natural; allowed : in Field_Value_Ranges) return Boolean is
    begin
        return 
            ((allowed.first.from <= N and N <= allowed.first.to)
                or (allowed.last.from <= N and N <= allowed.last.to))
            or else (N = 0);
    end Is_In;

    function Is_Valid_Any(N : in Natural; allowed : in Ticket_Field_Ranges) return Boolean is
    begin
        if N = 0 then
            return True;
        end if;
        for field in Ticket_Field loop
            if Is_In(N, allowed(field)) then
                return True;
            end if;
        end loop;
        return False;
    end Is_Valid_Any;

    function Is_Valid(T : in Ticket; allowed : in Ticket_Field_Ranges) return Boolean is
    begin
        for value of T loop
            if not Is_Valid_Any(value, allowed) then
                return False;
            end if;
        end loop;
        return True;
    end Is_Valid;

    function Filter_Out(tickets : in Ticket_Array; allowed : in Ticket_Field_Ranges) return Ticket_Array is
        remaining : Ticket_Array(tickets'Range);
        I : Positive := tickets'First;
    begin
        for t of tickets loop
            if Is_Valid(t, allowed) then
                remaining(I) := t;
                I := I + 1;
            end if;
        end loop;
        return remaining(remaining'First .. I - 1);
    end Filter_Out;

    function Find_Valids
       (tickets : in Ticket_Array;
        allowed : in Ticket_Field_Ranges;
        size : in Positive) return Field_Set_Array
    is
        valids : Field_Set_Array(1 .. size) := (others => To_Set(allowed));
        to_remove : Field_Sets.Set := Field_Sets.Empty_Set;
    begin
        for I in 1 .. size loop
            for J in 1 .. size loop
                to_remove.Clear;
                for t of tickets loop
                    for field of valids(J) loop
                        if not Is_In(t(J), allowed(field)) then
                            to_remove.Include(field);
                        end if;
                    end loop;
                end loop;
                valids(J).Difference(to_remove);
                if Natural(valids(J).Length) = 1 then
                    for K in 1 .. size loop
                        if K /= J then
                            valids(K).Difference(valids(J));
                        end if;
                    end loop;
                end if;
            end loop;
        end loop;
        return valids;
    end Find_Valids;

    function Part_1(tickets : in Ticket_Array; allowed : in Ticket_Field_Ranges) return Natural is
        sum : Natural := 0;
    begin
        for ticket of tickets loop
            for value of ticket loop
                if not Is_Valid_Any(value, allowed) then
                    sum := sum + value;
                end if;
            end loop;
        end loop;
        return sum;
    end Part_1;

    function Part_2(my_ticket : in Ticket; valids : in Field_Set_Array) return Long_Long_Integer is
        product : Long_Long_Integer := 1;
    begin
        for I in valids'Range loop
            if valids(I).First_Element in Departure then
                product := product * Long_Long_Integer(my_ticket(I));
            end if;
        end loop;
        return product;
    end Part_2;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    allowed : Ticket_Field_Ranges;
    size : Natural;
    my_ticket : Ticket;
begin
    TIO.Open(F, TIO.In_File, filepath);
    Get_Ticket_Field_Ranges(F, allowed, size);
    TIO.Skip_Line(F);
    TIO.Skip_Line(F);
    my_ticket := Get_Ticket(F, size);
    TIO.Skip_Line(F);
    TIO.Skip_Line(F);
    declare
        nearby_tickets : constant Ticket_Array := Get_Nearby_Tickets(F, size);
        valid_nearby_tickets : constant Ticket_Array := Filter_Out(nearby_tickets, allowed);
        valids : constant Field_Set_Array := Find_Valids(my_ticket & valid_nearby_tickets, allowed, size);
        p2 : constant Long_Long_Integer := Part_2(my_ticket, valids);
    begin
        TIO.Put_Line(Part_1(nearby_tickets, allowed)'Img);
        TIO.New_Line;
        TIO.Put_Line(p2'Img);
    end;
    TIO.Close(F);
end Day16;
