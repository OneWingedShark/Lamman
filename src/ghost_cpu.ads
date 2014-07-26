With
Interfaces;

Package Ghost_CPU with Pure is

    -- Subtype rename of unsigned, 8-bit, modulo 256 numbers.
    Subtype Byte is Interfaces.Unsigned_8;

    -- Prefix for the identifiers of the instruction-set,
    Instruction_Prefix : Constant String := "nm_";

    -- Enumeration of the instruction-set mnemonics.
    Type Instruction_Set is (
        -- 0-Argumant Instructions
        nm_HLT,
        -- 1-Argumant Instructions
        nm_INC, nm_DEC, nm_INT,
        -- 2-Argumant Instructions
        nm_MOV, nm_ADD, nm_SUB, nm_MUL, nm_DIV, nm_AND, nm_OR, nm_XOR,
        -- 3-Argumant Instructions
        nm_JLT, nm_JEQ, nm_JGT
                            );

    -- Represents an instruction associated with a mnemonic.
    Type Instruction( Mnemonic : Instruction_Set ) is private;

    Instruction_Encode_Error,
    Instruction_Decode_Error  : Exception;

    -- Enumeration of the registers.
    Type Register is (A, B, C, D, E, F, G, H, PC);

    -- General purpose registers.
    Subtype GP_Register is Register Range A..H;

    -- Types representing memory of the emulates CPU.
    Type Code_Chunk     is Array(Byte range <>) of not null access Instruction;
    Type Memory_Chunk   is Array(Byte range <>) of Byte;
    Type Register_Bank  is Array(Register) of Byte;
    Subtype Code_Bank   is Code_Chunk(Byte);
    Subtype Memory_Bank is Memory_Chunk(Byte);

    -- Represents a processor.
    Type CPU is tagged record
        Cycle     : Integer Range 1..1024:= 1;
        Registers : Register_Bank:= (Others => 0);
        Data      : Memory_Bank:=   (Others => 0);
        Code      : Code_Bank; -- Code is to be initialized by start-up.
    end record;

    -- Load a program into Code memory.
    Procedure Init( Processor : in out CPU; Code : Code_Chunk );

    -- Run an instruction.
    Procedure Exec( Processor : in out CPU );

Private
    use all type Byte;

    -- Arity-sets for given instructions.
    Subtype Arity_0 is Instruction_Set range nm_HLT..nm_HLT;
    Subtype Arity_1 is Instruction_Set range nm_INC..nm_INT;
    Subtype Arity_2 is Instruction_Set range nm_MOV..nm_XOR;
    Subtype Arity_3 is Instruction_Set range nm_JLT..nm_JGT;

    -- Ensure the above sets fully cover the instruction set.
    Pragma Assert((for all I in Instruction_Set =>
                     I in Arity_0 | Arity_1 | Arity_2 | Arity_3),
                  "Error: not all instructions are covered."
                 );

    -- The valid byte-values representing registers.
    subtype Register_Range is Byte range
      Register'Pos(Register'First)..Register'Pos(Register'Last);

    -- The valid interrupts.
    subtype Interrupt_Range is Byte range 0..8;

    -- Governs how a value is to be interpreted:
    -- Register, Indirection [Code or Data], or Constant.
    Type Argument_Style is (
      Register_Argument, Indirect_Argument, Constant_Argument, Data_Argument
                           );

    Type Instruction( Mnemonic : Instruction_Set ) is record
        case Mnemonic is
            when Arity_0 => null;
            when others =>
                A1 : Argument_Style;
                V1 : Byte;
                case Mnemonic is
                    when Arity_1 | Arity_0 => null;
                    when others =>
                        A2 : Argument_Style;
                        V2 : Byte;
                        case Mnemonic is
                            when Arity_2 | Arity_1 | Arity_0 => null;
                            when Arity_3 =>
                                A3 : Argument_Style;
                                V3 : Byte;
                        end case;
                end case;
        end case;
    end record
      with Type_Invariant =>
        (Case Instruction.Mnemonic is
         when nm_HLT => True,
         when nm_INC | nm_DEC =>
           Instruction.A1 /= Constant_Argument and
             (if   Instruction.A1 = Register_Argument
              then Instruction.V1 /= Register'pos(PC)),
         when nm_INT =>
           Instruction.V1 in Interrupt_Range,
         when nm_MOV =>
           Instruction.A1 /= Constant_Argument,
         when nm_ADD | nm_SUB | nm_MUL | nm_DIV |
              nm_AND | nm_OR  | nm_XOR =>
           Instruction.A1 /= Constant_Argument and
             (if   Instruction.A1 = Register_Argument
              then Instruction.V1 /= Register'pos(PC)),
         when nm_JLT | nm_JEQ | nm_JGT =>
           Instruction.A1 = Constant_Argument,
         when others => raise Program_Error
        );


end Ghost_CPU;
