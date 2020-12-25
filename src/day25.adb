with Ada.Text_IO;
with Ada.Command_Line;

procedure Day25 is
    package TIO renames Ada.Text_IO;

    type Modulus is mod 20201227;

    package Modulus_IO is new TIO.Modular_IO(Modulus);

    procedure Find_Loop(loop_value : out Modulus; public_key : Modulus; subject_number : Modulus := 7) is
        value : Modulus := 1;
    begin
        loop_value := 0;
        while value /= public_key loop
            loop_value := loop_value + 1;
            value := (value * subject_number);
        end loop;
    end Find_Loop;

    procedure Encrypt(key : out Modulus; subject_number, loop_size : in Modulus) is
    begin
        key := subject_number ** Natural(loop_size);
    end Encrypt;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    card_public_key, card_loop_size, card_encryption_key : Modulus;
    door_public_key, door_loop_size, door_encryption_key : Modulus;
begin
    TIO.Open(F, TIO.In_File, filepath);
    Modulus_IO.Get(F, card_public_key);
    Modulus_IO.Get(F, door_public_key);
    TIO.Close(F);

    Find_Loop(card_loop_size, card_public_key);
    Find_Loop(door_loop_size, door_public_key);

    Encrypt(card_encryption_key, card_public_key, door_loop_size);
    Encrypt(door_encryption_key, door_public_key, card_loop_size);

    TIO.Put_Line("What encryption key is the handshake trying to establish?");
    TIO.Put_Line(card_encryption_key'Img);
    TIO.Put_Line(door_encryption_key'Img);
end Day25;
