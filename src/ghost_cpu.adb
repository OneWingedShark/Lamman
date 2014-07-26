Package Body Ghost_CPU Is

    Procedure Init ( Processor : in out CPU; Code : Code_Chunk ) is
    Begin
        Processor.Code:= Code &
        (Code'Length..Byte'Last => new Instruction'(Mnemonic => nm_HLT));
    End Init;

    Procedure Exec( Processor : in out CPU ) is

        -- Possible indices for parameters.
        Type Parameter_Index is range 1..3;

        -- Shorthand for the current instruction.
        Instruction : Ghost_CPU.Instruction renames
          Processor.Code(Processor.Registers(PC)).all;

        -- The original value of the program-counter.
        Start_PC    : Constant Byte:= Processor.Registers(PC);

        -------------------------------------------------------------
        --  Forward Declaration of Functions (Plus Documentation)  --
        -------------------------------------------------------------

        -- Check that index is in 1..arity of the current instruction.
        Function Arity_Check(Index : Parameter_Index) return Boolean;

        -- Retrieves the value indicated by Value and the given style.
        Function Fetch(Value : Byte; Style : Argument_Style) return Byte;

        -- Writes Value to the indicated location [based on Style and state].
        Procedure Write(Value : Byte; Style : Argument_Style; Loc : Byte);

        -- Retrieves the value of the indicated parameter; shorthand for Fetch.
        Function Param(Index : Parameter_Index) return Byte
          with pre => Arity_Check(Index);

        -- Retrieves the style associated with the parameter of the given index.
        Function Param_Style(Index : Parameter_Index) return Argument_Style
          with pre => Arity_Check(Index);

        -- Indicates a binary operation to be performed.
        Generic
            with function "+"(Left,Right : Byte) return Byte;
        Procedure Bin_Op;

        -- Indicates a conditional branch to be performed.
        Generic
            with function "<"(Left,Right : Byte) return Boolean;
        Procedure Conditional;

        -------------------------------
        --  Utility Function Bodies  --
        -------------------------------

        Function Arity_Check(Index : Parameter_Index) return Boolean is
          (case Index is
           when 1 => Instruction.Mnemonic not in Arity_0,
           when 2 => Instruction.Mnemonic not in Arity_0 | Arity_1,
           when 3 => Instruction.Mnemonic not in Arity_0 | Arity_1 | Arity_2,
           when others => raise Program_Error
          );

        Function Fetch(Value : Byte; Style : Argument_Style) return Byte is
        (case Style is
         when Register_Argument =>
           (if   Value in Register_Range then Value
            else raise Program_Error),
         when Indirect_Argument =>
           (if   Value in Register_Range and Value /= Register'Pos(PC) then
              Processor.Data(Processor.Registers(Register'Val(Value)))
            else raise Program_Error),
         when Constant_Argument => Value,
         when Data_Argument     => Processor.Data(Value)
        );

        Procedure Write(Value : Byte; Style : Argument_Style; Loc : Byte) is
        begin
            case Style is
            when Register_Argument =>
                if Loc not in Register_Range then
                    raise Program_Error with "Invalid register location.";
                else
                    Processor.Registers( Register'Val(Loc) ) := Value;
                end if;
            when Indirect_Argument =>
                if Loc not in Register_Range then
                    raise Program_Error with "Invalid register location.";
                elsif Value /= Register'Pos(PC) then
                    raise Program_Error with "Cannot use PC for indirection.";
                else
                    declare
                        Register_Name : constant Register := Register'Val(Loc);
                        Indirection   : Byte renames
                                          Processor.Registers(Register_Name);
                    begin
                        Processor.Data(Indirection):= Value;
                    end;
                end if;
            when Constant_Argument =>
                raise Program_Error with
                  "Why are you trying to assign values to a constant?";
            when Data_Argument     =>
                Processor.Data(Loc):= Value;
            end case;
        End Write;

        Function Param(Index : Parameter_Index) return Byte is
          (case Index is
           when 1 => Fetch(Instruction.V1, Param_Style(Index)),
           when 2 => Fetch(Instruction.V2, Param_Style(Index)),
           when 3 => Fetch(Instruction.V3, Param_Style(Index))
          );

        Function Param_Style(Index : Parameter_Index) return Argument_Style is
          (case Index is
           when 1 => Instruction.A1,
           when 2 => Instruction.A2,
           when 3 => Instruction.A3
          );

        ---------------------
        -- Generic Bodies  --
        ---------------------

        Procedure Bin_Op is
        begin
            Write(
                  Value => Param(1)+Param(2),
                  Style => Param_Style(1),
                  Loc   => Instruction.V1
                 );
        End Bin_Op;

        Procedure Conditional is
            Target : Byte renames Param(1);
            X      : Byte Renames Param(2);
            Y      : Byte Renames Param(3);
        begin
            if X < Y then
                Processor.Registers(PC):= Target;
            End If;
        End Conditional;

        ----------------------
        --  Instantiations  --
        ----------------------

        Procedure Do_Add is new Bin_Op("+");
        Procedure Do_Sub is new Bin_Op("-");
        Procedure Do_Mul is new Bin_Op("*");
        Procedure Do_Div is new Bin_Op("/");
        Procedure Do_And is new Bin_Op("And");
        Procedure Do_Or  is new Bin_Op("Or" );
        Procedure Do_Xor is new Bin_Op("Xor");

        Procedure Do_JLT is new Conditional("<");
        Procedure Do_JEQ is new Conditional("=");
        Procedure Do_JGT is new Conditional(">");


    Begin
        DECODE:
        declare
        begin
            Case Instruction.Mnemonic is
                when nm_HLT => null;
                when nm_INC => Write(Param(1)+1,Param_Style(1),Instruction.V1);
                when nm_DEC => Write(Param(1)-1,Param_Style(1),Instruction.V1);
                when nm_INT =>
                    case Interrupt_Range(Param(1)) is
                        when 0 => null;
                        when 1 => null;
                        when 2 => null;
                        when 3 => null;
                        when 4 => null;
                        when 5 => null;
                        when 6 => null;
                        when 7 => null;
                        when 8 => null;
                    end case;
                when nm_MOV => Write(Param(2),Param_Style(1),Instruction.V1);
                when nm_ADD => Do_Add;
                when nm_SUB => Do_Sub;
                when nm_MUL => Do_Mul;
                when nm_DIV => Do_Div;
                when nm_AND => Do_And;
                when nm_OR  => Do_Or;
                when nm_XOR => Do_Xor;
                when nm_JLT => Do_JLT;
                when nm_JEQ => Do_JEQ;
                when nm_JGT => Do_JGT;
            end case;
        End DECODE;

        Processor.Cycle:= (if Processor.Cycle < 1024 then
                             Integer'Succ(Processor.Cycle)
                           else 1
                           );

        if Processor.Registers(PC) = Start_PC then
           Processor.Registers(PC):= Processor.Registers(PC) + 1;
        end if;

    exception
            when Constraint_Error => Null;
    End Exec;


End Ghost_CPU;
