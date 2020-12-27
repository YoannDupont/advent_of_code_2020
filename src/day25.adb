with Ada.Text_IO;
with Ada.Command_Line;

procedure Day25 is
    package TIO renames Ada.Text_IO;

    type Modulo is mod 20201227;

    package Modulo_IO is new TIO.Modular_IO(Modulo);

    procedure Find_Loop_Value(loop_value : out Modulo; public_key : Modulo; subject_number : Modulo := 7) is
        value : Modulo := 1;
    begin
        loop_value := 0;
        while value /= public_key loop
            loop_value := loop_value + 1;
            value := (value * subject_number);
        end loop;
    end Find_Loop_Value;

    procedure Encrypt(key : out Modulo; subject_number, loop_size : in Modulo) is
    begin
        key := subject_number ** Natural(loop_size);
    end Encrypt;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    card_public_key, card_loop_size, card_encryption_key : Modulo;
    door_public_key, door_loop_size, door_encryption_key : Modulo;
begin
    TIO.Put_Line("--- Day 25: Combo Breaker ---");

    TIO.Open(F, TIO.In_File, filepath);
    Modulo_IO.Get(F, card_public_key);
    Modulo_IO.Get(F, door_public_key);
    TIO.Close(F);

    -- Find_Loop_Value(card_loop_size, card_public_key);
    Find_Loop_Value(door_loop_size, door_public_key);

    Encrypt(card_encryption_key, card_public_key, door_loop_size);
    -- Encrypt(door_encryption_key, door_public_key, card_loop_size);

    TIO.Put_Line("What encryption key is the handshake trying to establish?");
    TIO.Put_Line(card_encryption_key'Img);
    -- TIO.Put_Line(door_encryption_key'Img);
end Day25;
