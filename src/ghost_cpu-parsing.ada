With
Ada.Characters.Handling,
Ada.Characters.Latin_1;

separate(Ghost_CPU)
Package Body Parsing Is

    use Ada.Characters.Handling;

    Function Trim(Input : String) return String is
      (if Input'Length = 0 then ""
       elsif Is_Graphic( Input(Input'First) ) then Input
       else  Trim( Input(Positive'Succ(Input'First)..Input'Last) )
      );


    function Input(Stream : not null access Ada.Streams.Root_Stream_Type'Class)
                   return  Code_Chunk is
    begin
        Return (2..1 => <>);
    end Input;

    procedure Output(Stream : not null access Ada.Streams.Root_Stream_Type'Class;
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
