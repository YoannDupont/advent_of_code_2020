with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Assertions;
with Ada.Command_Line;
with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;
with Ada.Containers.Vectors;

use type Ada.Text_IO.Positive_Count;

procedure Day02 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;
    package ASU renames Ada.Strings.Unbounded;
    package ASF renames Ada.Strings.Fixed;

    function "+"(source : in String) return ASU.Unbounded_String renames ASU.To_Unbounded_String;
    function "+"(source : in ASU.Unbounded_String) return String renames ASU.To_String;

    type Corporate_Policy is record
        lowest : Positive;
        highest : Positive;
        letter : Character;
    end record;

    type Database_Entry is record
        policy : Corporate_Policy;
        password : ASU.Unbounded_String;
    end record;

    function Part_1(DB_Entry : in Database_Entry) return Boolean is
        count : constant Natural := ASF.Count(
            source => +DB_Entry.password,
            Pattern => (1 => DB_Entry.policy.letter)
        );
    begin
        return DB_Entry.policy.lowest <= count and count <= DB_Entry.policy.highest;
    end Part_1;

    function Part_2(DB_Entry : in Database_Entry) return Boolean is
        password : constant String := +DB_Entry.password;
        position1 : constant Boolean := password(password'First + DB_Entry.policy.lowest - 1) = DB_Entry.policy.letter;
        position2 : constant Boolean := password(password'First + DB_Entry.policy.highest - 1) = DB_Entry.policy.letter;
    begin
        return position1 xor position2;
    end Part_2;

    package Databases is new Ada.Containers.Vectors(Positive, Database_Entry);
    subtype Database is Databases.Vector;

    function Read(F : in TIO.File_Type) return Database is
        policy : Corporate_Policy;
        db : Database;
        C : Character;
    begin
        while not TIO.End_Of_File(F) loop
            ITIO.Get(F, policy.lowest);
            TIO.Get(F, C); -- '-'
            ITIO.Get(F, policy.highest);
            TIO.Get(F, C); -- ' '
            TIO.Get(F, policy.letter);
            TIO.Get(F, C); -- ': '
            TIO.Get(F, C); -- ': '
            db.Append((policy, +TIO.Get_Line(F)));
        end loop;
        return db;
    end Read;

    function Check(DB : in Database; Is_Valid : access function(input : in Database_Entry) return Boolean) return Natural is
        number_valid : Natural := 0;
    begin
        for db_entry of DB loop
            if Is_Valid(db_entry) then
                number_valid := number_valid + 1;
            end if;
        end loop;
        return number_valid;
    end Check;

    Test_Input : Database;
    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
begin
    Test_Input.Append(((1, 3, 'a'), +"abcde"));
    Test_Input.Append(((1, 3, 'b'), +"cdefg"));
    Test_Input.Append(((2, 9, 'c'), +"ccccccccc"));

    Ada.Assertions.Assert(Check(Test_Input, Part_1'Access) = 2);
    Ada.Assertions.Assert(Check(Test_Input, Part_2'Access) = 1);

    TIO.Open(F, TIO.In_File, filepath);
    declare
        DB : constant Database := Read(F);
    begin
        TIO.Put_Line("How many passwords are valid according to their policies?");
        ITIO.Put(Check(DB, Part_1'Access), width => 0);
        TIO.New_Line;
        TIO.New_Line;
        TIO.Put_Line("How many passwords are valid according to the new interpretation of the policies?");
        ITIO.Put(Check(DB, Part_2'Access), width => 0);
        TIO.New_Line;
    end;
    TIO.Close(F);
end Day02;
