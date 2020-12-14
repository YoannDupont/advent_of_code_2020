with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;

procedure Day12 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;

    -- N E S W to allows to use -1 and +1 as left and right respectively in unsigned type.
    type Orientation is (N, E, S, W, L, R, F);
    subtype Absolute_Orientation is Orientation range N .. W;
    subtype Relative_Orientation is Orientation range L .. F;
    subtype Waypoint_Orientation is Orientation range N .. R;
    subtype Absolute_Waypoint_Orientation is Waypoint_Orientation range N .. W;
    subtype Relative_Waypoint_Orientation is Waypoint_Orientation range L .. R;
    type Absolute_Orientation_Modulus is mod 4;

    type Action is record
        where : Orientation;
        value : Positive;
    end record;

    type Action_Array is array(Positive range <>) of Action;
    Empty_Action_Array : constant Action_Array(1 .. 0) := (others => <>);

    type Position is record
        x, y : Integer;
    end record;

    function "+"(L, R : in Position) return Position is
    begin
        return (L.x + R.x, L.y + R.y);
    end "+";

    function "*"(P : in Position; V : in Integer) return Position is
    begin
        return (p.x * V, p.y * V);
    end "*";

    Direction : constant array(Absolute_Waypoint_Orientation'Range) of Position := ((0, 1), (1, 0), (0, -1), (-1, 0));

    type Ship is record
        facing : Absolute_Orientation;
        pos : Position;
        waypoint : Position;
    end record;

    function Get(F : in TIO.File_Type) return Action_Array is
        function Get_Rec(acc : Action_Array) return Action_Array is
            O : Orientation;
            P : Positive;
            C : Character;
        begin
            if TIO.End_Of_File(F) then
                return acc;
            end if;
            TIO.Get(F, C);
            O := Orientation'Value((1 => C));
            ITIO.Get(F, P);
            return Get_Rec(acc & Action'(O, P));
        end Get_Rec;
    begin
        return Get_Rec(Empty_Action_Array);
    end Get;

    function Turn(O : in Absolute_Orientation; towards : in Relative_Orientation; value : In positive) return Absolute_Orientation is
        aom : Absolute_Orientation_Modulus := Absolute_Orientation_Modulus(Absolute_Orientation'Pos(O));
    begin
        case towards is
            when L =>
                aom := aom - Absolute_Orientation_Modulus(value / 90);
                return Absolute_Orientation'Val(aom);
            when R =>
                aom := aom + Absolute_Orientation_Modulus(value / 90);
                return Absolute_Orientation'Val(aom);
            when F => return O;
        end case;
    end Turn;

    procedure Rotate(ferry : in out Ship; where : in Waypoint_Orientation; value : in Positive) is
        lr : Relative_Waypoint_Orientation;
        tmp : Integer;
    begin
        case where is
            when Absolute_Waypoint_Orientation =>
                ferry.waypoint := ferry.waypoint + Direction(where) * value;
            when Relative_Waypoint_Orientation =>
                if value = 180 then
                    ferry.waypoint := ferry.waypoint * (-1);
                else
                    if value = 270 then
                        lr := (if where = L then R else L);
                    else
                        lr := where;
                    end if;
                    case lr is
                        when L =>
                            ferry.waypoint.y := -ferry.waypoint.y;
                        when R =>
                            ferry.waypoint.x := -ferry.waypoint.x;
                    end case;
                    tmp := ferry.waypoint.x;
                    ferry.waypoint.x := ferry.waypoint.y;
                    ferry.waypoint.y := tmp;
                end if;
        end case;
    end Rotate;

    function Manhattan_Distance(p1, p2 : Position) return Natural is
    begin
        return abs(p1.x - p2.x) + abs(p1.y - p2.y);
    end Manhattan_Distance;

    procedure Part_1(ferry : in out Ship; act : in Action) is
        new_orientation : Absolute_Orientation;
    begin
        if act.where in Relative_Orientation then
            new_orientation := Turn(ferry.facing, act.where, act.value);
            ferry.facing := new_orientation;
        else
            new_orientation := act.where;
        end if;
        if act.where in Absolute_Orientation or act.where = F then
            ferry.pos := ferry.pos + Direction(new_orientation) * act.value;
        end if;
    end Part_1;

    procedure Part_2(ferry : in out Ship; act : in Action) is
    begin
        if act.where in Waypoint_Orientation then
            Rotate(ferry, Waypoint_Orientation(act.where), act.value);
        else
            ferry.pos := ferry.pos + ferry.waypoint * act.value;
        end if;
    end Part_2;

    function Sail(actions : in Action_Array; Move : access procedure (ferry : in out Ship; act : in Action)) return Natural is
        ferry : Ship := (E, (0, 0), (10, 1));
    begin
        for action of actions loop
            Move(ferry, action);
        end loop;
        return Manhattan_Distance(ferry.pos, (0, 0));
    end Sail;

    filepath : constant String := Ada.Command_Line.Argument(1);
    file : TIO.File_Type;
begin
    TIO.Open(file, TIO.In_File, filepath);
    declare
        actions : constant Action_Array := Get(file);
        p1 : constant Natural := Sail(actions, Part_1'Access);
        p2 : constant Natural := Sail(actions, Part_2'Access);
    begin
        TIO.Put_Line("Figure out where the navigation instructions lead. What is the Manhattan distance between that location and the ship's starting position?");
        ITIO.Put(p1, width => 0);
        TIO.New_Line;
        TIO.New_Line;
        TIO.Put_Line("Figure out where the navigation instructions actually lead. What is the Manhattan distance between that location and the ship's starting position?");
        ITIO.Put(p2, width => 0);
    end;
    TIO.Close(file);
end Day12;
