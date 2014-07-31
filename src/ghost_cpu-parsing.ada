With
Ada.Strings.Equal_Case_Insensitive,
Ada.Characters.Handling,
Ada.Characters.Latin_1;

separate(Ghost_CPU)
Package Body Parsing Is

    use Ada.Streams, Ada.Characters.Handling;

    Function Trim(Input : String) return String is
      (if Input'Length = 0 then ""
       elsif Is_Graphic( Input(Input'First) ) then Input
       else  Trim( Input(Positive'Succ(Input'First)..Input'Last) )
      );


    function Input(Stream : not null access Root_Stream_Type'Class)
                   return  Code_Chunk is
        Function Get_Instruction(Stream : not null access
                                          Root_Stream_Type'Class:= Input.Stream)
                                 return Instruction is
            Mnemonic_Length : constant :=
                                Instruction_Set'Width-Instruction_Prefix'Length;
            Use Ada.Characters.Latin_1, Ada.Characters.Handling;
            Mnemonic : String(1..Mnemonic_Length):= (others => NUL);
            Index    : Positive range Mnemonic'Range:= Mnemonic'First;

            procedure Strip_Whitespace is
            begin
                -- Strip leading white-space
                while not Is_Graphic(Mnemonic(Index)) or Mnemonic(Index) = ' ' loop
                    Character'Read( Stream, Mnemonic(Index) );
                end loop;
            end Strip_Whitespace;

            procedure Ignore_Line is
            begin
                while Mnemonic(Index) /= CR or Mnemonic(Index) = LF loop
                    Character'Read( Stream, Mnemonic(Index) );
                end loop;
            end Ignore_Line;

            -- ::WARNING:: -- Not Implemented!
            Procedure Get_Parameter(A : out Argument_Style;
                                    V : out Byte) is
                pragma Unreferenced (A, V);

                Procedure Reset is
                begin
                    -- Reset the temp-string.
                    for C of Mnemonic loop
                        C:= NUL;
                    end loop;
                    -- Reset the index.
                    Index:= Mnemonic'First;
                end Reset;

                Indirect : Boolean:= False;
            begin
                raise Program_Error with "::WARNING:: -- Not Implemented!";

                Reset;
                Strip_Whitespace;
                if Mnemonic(Index) = '[' then
                    Indirect:= True;
                    Reset;
                    Strip_Whitespace;
                end if;

                loop
                    exit when Index = Mnemonic'Last or
                              not Is_Alphanumeric( Mnemonic(Index) );
                    Index:= Positive'Succ(Index);
                    Character'Read( Stream, Mnemonic(Index) );
                end loop;


                if Indirect then
                    if Mnemonic(Index) /= ']' then
                        raise Instruction_Encode_Error;
                    end if;
                end if;
            end Get_Parameter;


            Procedure Get_Parameter(P   : Parameter_Index;
                                    Ins : in out Instruction) is
            begin
                case P is
                    when 1 => Get_Parameter(A => Ins.A1, V => Ins.V1);
                    when 2 => Get_Parameter(A => Ins.A2, V => Ins.V2);
                    when 3 => Get_Parameter(A => Ins.A3, V => Ins.V3);
                end case;
            end Get_Parameter;


            function "="(Left, Right : String) return Boolean renames
              Ada.Strings.Equal_Case_Insensitive;

        begin
            -- Read the first character.
            loop
                Strip_Whitespace;
                if Mnemonic(Index) = ';' then
                    Ignore_Line;
                end if;
                exit when Is_Graphic( Mnemonic(Index) );
            end loop;

            -- Read the second character.
            Index:= Positive'Succ(Index);
            Character'Read( Stream, Mnemonic(Index) );

            -- OR is the only two-character instruction.
            if Mnemonic(1..Index) /= "OR" then
                Index:= Positive'Succ(Index);
                Character'Read( Stream, Mnemonic(Index) );
            end if;


            return Result: Instruction(Mnemonic =>
                  Instruction_Set'Value(Instruction_Prefix & Mnemonic(1..Index))
                                      ) do
                declare
                    subtype Parameters is Parameter_Index
                        range 1..(case Result.Mnemonic is
                                  when arity_0 => 0,
                                  when arity_1 => 1,
                                  when arity_2 => 2,
                                  when arity_3 => 3
                                 );
                begin
                    For Index in Parameters loop
                        Get_Parameter(Index, Result);
                    end loop;
                end;
            end return;
        end Get_Instruction;

        Parameters : Array(1..3) of Access String:= (Others => null);
    begin

        Return (2..1 => <>);
    end Input;

    procedure Output(Stream : not null access Root_Stream_Type'Class;
                     Item   : in  Code_Chunk) is

        Function As_Register(Value : Byte) return Register
          with Pre => Value in Register_Range;

        Function Bracket(Input : String) return String is
          ("["& Input &"]");

        Function As_Register(Value : Byte) return Register is
          ( Register'Val(Value) );

        Function As_Register(Value : Byte) return String is
          ( Trim(Register'Image( As_Register(Value) )) );

        Function Write_Param(Index : Parameter_Index;
                             Op    : Instruction) return String
          with Pre => Arity_Check(Index, Op);

        Function Write_Param(Value : Byte;
                             Style : Argument_Style) return String is
          (case Style is
           when Register_Argument => As_Register(Value),
           when Indirect_Argument => Bracket(As_Register(Value)),
           when Constant_Argument => Trim(Byte'Image(Value)),
           when Data_Argument     => Bracket(Trim(Byte'Image(Value)))
          );

        Function Write_Param(Index : Parameter_Index;
                             Op    : Instruction) return String is
          (case Index is
           when 1 => Write_Param(Value => Op.V1, Style => Op.A1),
           when 2 => Write_Param(Value => Op.V2, Style => Op.A2),
           when 3 => Write_Param(Value => Op.V3, Style => Op.A3),
           when others => raise Instruction_Decode_Error
          );


        Space     : Character renames Ada.Characters.Latin_1.HT;
        Tab       : Character renames Ada.Characters.Latin_1.Space;
        Semicolon : Character renames Ada.Characters.Latin_1.Semicolon;
        New_Line  : Constant String:= Ada.Characters.Latin_1.CR &
                                      Ada.Characters.Latin_1.LF;
    begin
        for Op of Item loop
            declare
                Prefix    : String renames Instruction_Prefix;
                Raw_Image : String renames Instruction_Set'Image(Op.Mnemonic);
                Image     : String renames
                                     Raw_Image(Prefix'Length..Raw_Image'Last);
                subtype Parameters is Parameter_Index
                  range 1..(case Op.Mnemonic is
                            when arity_0 => 0,
                            when arity_1 => 1,
                            when arity_2 => 2,
                            when arity_3 => 3
                           );
            begin
                String'Write	(Stream, Image);
                for Index in Parameters loop
                    Character'Write(Stream, (if Index = 1 then Space else ','));
                    String'Write   (Stream, Write_Param(Index,Op.ALL));
                end loop;
                Character'Write	(Stream, Semicolon);
                String'Write    (Stream, New_Line);
            end;
        end loop;
    end Output;

End Parsing;
