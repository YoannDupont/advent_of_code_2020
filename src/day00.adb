with Ada.Text_IO;
with Ada.Command_Line;

procedure Day00 is
    package TIO renames Ada.Text_IO;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
begin
    TIO.Put_Line("--- Day 0: Template Templar ---");

    TIO.Open(F, TIO.In_File, filepath);
    TIO.Close(F);

    TIO.Put_Line("This is a placeholder for part 1.");

    TIO.Put_Line("This is a placeholder for part 2.");
end Day00;
