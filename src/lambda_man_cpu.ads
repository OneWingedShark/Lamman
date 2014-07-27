With
Lambda_Man_Components.Stack;

Package Lambda_Man_CPU with Pure is

    Type Register is (C, S, D, E);

    Type Instruction_Set is (
            -- 0-arg
            ADD,  SUB,  MUL,  DIV,  CEQ,  CGT,  CGTE, ATOM,
            CONS, CAR,  CDR,  JOIN, RTN,  STOP, DBUG, BRK,
            -- 1-arg
            LDC,  LDF,  AP,   DUM,  RAP,  TAP,  TRAP,
            -- 2-arg
            LD,   SEL,  TSEL, ST
                            );

End Lambda_Man_CPU;
