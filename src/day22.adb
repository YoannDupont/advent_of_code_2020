with Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Command_Line;
with Ada.Containers.Hashed_Sets;

use type Ada.Containers.Hash_Type;

procedure Day22 is
    package TIO renames Ada.Text_IO;
    package ITIO renames Ada.Integer_Text_IO;

    subtype Card is Positive;
    type Card_Array is array(Positive range <>) of Card;
    SIZE : constant Positive := 128;
    subtype Card_Buffer is Card_Array(1 .. SIZE);
    type Card_Deck is
    record
        buffer : Card_Buffer;
        first : Positive := SIZE / 2;
        last : Natural := (SIZE / 2) - 1;
    end record;

    function "="(L, R : in Card_Deck) return Boolean is
    begin
        return L.buffer(L.first .. L.last) = R.buffer(R.first .. R.last);
    end "=";

    -- see https://stackoverflow.com/a/11358544/4869962
    function Hash(CB : in Card_Deck) return Ada.Containers.Hash_Type is
        val : Ada.Containers.Hash_Type := 1;
    begin
        for I in CB.first .. CB.last loop
           val := (val * 31) + Ada.Containers.Hash_Type(CB.buffer(I));
        end loop;
        return val;
    end Hash;

    function Is_Empty(CD : in Card_Deck) return Boolean is
    begin
        return CD.last < CD.first;
    end Is_Empty;

    function Length(CD : in Card_Deck) return Natural is
    begin
        if Is_Empty(CD) then
            return 0;
        else
            return CD.last - CD.first + 1;
        end if;
    end Length;

    function Is_Full(CD : in Card_Deck) return Boolean is
    begin
        return CD.first = CD.buffer'First and CD.last = CD.buffer'Last;
    end Is_Full;

    function Pop(CD : in out Card_Deck) return Card is
    begin
        if Is_Empty(CD) then
            raise Constraint_Error;
        end if;
        CD.last := CD.last - 1;
        return CD.buffer(CD.last + 1);
    end Pop;

    procedure Delete_First(CD : in out Card_Deck) is
    begin
        if Is_Empty(CD) then
            raise Constraint_Error;
        end if;
        CD.first := CD.first + 1;
    end Delete_First;

    procedure Prepend(CD : in out Card_Deck; element : in Card) is
    begin
        if Is_Full(CD) then
            raise Constraint_Error;
        end if;
        if CD.first = CD.buffer'First then
            CD.buffer(CD.first + 1 .. CD.last + 1) := CD.buffer(CD.first .. CD.last);
            CD.first := CD.first + 1;
            CD.last := CD.last + 1;
        end if;
        CD.first := CD.first - 1;
        CD.buffer(CD.first) := element;
    end Prepend;

    function Get(F : in TIO.File_Type) return Card_Deck is
        the_deck : Card_Deck;
        C : Card;
    begin
        TIO.Skip_Line(F);
        while not TIO.End_Of_Line(F) loop
            ITIO.Get(F, C);
            Prepend(the_deck, C);
            if not TIO.End_Of_File(f) then
                TIO.Skip_Line(F);
            end if;
        end loop;
        TIO.Skip_Line(F);
        return the_deck;
    end Get;

    package Card_Deck_Sets is new Ada.Containers.Hashed_Sets(Card_Deck, Hash, "=", "=");
    subtype Card_Deck_Set is Card_Deck_Sets.Set;

    procedure Part_1(player1, player2 : in Card_Deck; hand : out Card_Deck) is
        deck1 : Card_Deck := player1;
        deck2 : Card_Deck := player2;
        C1, C2 : Card;
    begin
        while not(Is_Empty(deck1) or Is_Empty(deck2)) loop
            C1 := Pop(deck1);
            C2 := Pop(deck2);
            if C1 < C2 then
                Prepend(deck2, C2);
                Prepend(deck2, C1);
            else
                Prepend(deck1, C1);
                Prepend(deck1, C2);
            end if;
        end loop;
        
        hand := (if Is_Empty(deck1) then deck2 else deck1);
    end Part_1;

    procedure Part_2(player1, player2 : in Card_Deck; winner : out Positive; hand : out Card_Deck) is
        deck1 : Card_Deck := player1;
        deck2 : Card_Deck := player2;
        seen_d1, seen_d2 : Card_Deck_Set;
        card1, card2 : Card;
        game_winner : Positive;
        holder : Card_Deck;
    begin
        while not(Is_Empty(deck1) or Is_Empty(deck2)) loop
            if seen_d1.Contains(deck1) or seen_d2.Contains(deck2) then
                winner := 1;
                hand := deck1;
                return;
            end if;
            seen_d1.Insert(deck1);
            seen_d2.Insert(deck2);
            card1 := Pop(deck1);
            card2 := Pop(deck2);
            if card1 <= Length(deck1) and card2 <= Length(deck2) then
                declare
                    r_deck1 : Card_Deck := deck1;
                    r_deck2 : Card_Deck := deck2;
                begin
                    while Length(r_deck1) /= card1 loop
                        Delete_First(r_deck1);
                    end loop;
                    while Length(r_deck2) /= card2 loop
                        Delete_First(r_deck2);
                    end loop;
                    Part_2(r_deck1, r_deck2, game_winner, holder);
                end;
                if game_winner = 1 then
                    Prepend(deck1, card1);
                    Prepend(deck1, card2);
                else
                    Prepend(deck2, card2);
                    Prepend(deck2, card1);
                end if;
            else
                if card1 < card2 then
                    Prepend(deck2, card2);
                    Prepend(deck2, card1);
                else
                    Prepend(deck1, card1);
                    Prepend(deck1, card2);
                end if;
            end if;
        end loop;

        winner := (if Is_Empty(deck1) then 2 else 1);
        hand := (if Is_Empty(deck1) then deck2 else deck1);
    end Part_2;

    function Score(the_deck : in Card_Deck) return Natural is
        amount : Natural := 0;
        factor : Natural := 1;
    begin
        for I in the_deck.first .. the_deck.last loop
            amount := amount + (factor * the_deck.buffer(I));
            factor := factor + 1;
        end loop;
        return amount;
    end Score;

    filepath : constant String := Ada.Command_Line.Argument(1);
    F : TIO.File_Type;
    player1, player2, hand : Card_Deck;
    winner : Positive;
begin
    TIO.Open(F, TIO.In_File, filepath);
    player1 := Get(F);
    player2 := Get(F);
    TIO.Close(F);

    TIO.Put_Line("Play the small crab in a game of Combat using the two decks you just dealt. What is the winning player's score?");
    Part_1(player1, player2, hand);
    TIO.Put_Line(Score(hand)'Img);
    TIO.New_Line;

    TIO.Put_Line("Defend your honor as Raft Captain by playing the small crab in a game of Recursive Combat using the same two decks as before. What is the winning player's score?");
    Part_2(player1, player2, winner, hand);
    TIO.Put_Line(Score(hand)'Img);
end Day22;
