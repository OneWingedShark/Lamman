With
Ada.Finalization,
Ada.Iterator_Interfaces,
Ada.Containers;

Generic
    Type Element(<>) is private;
    Maximum : Positive := 1024;
Package Lambda_Man_Components.Stack with Pure is



    Type Stack(<>) is new Ada.Finalization.Controlled with private;

    Function  Count   ( Container :        Stack ) return Natural;

    Function  Peek    ( Container :        Stack ) return Element
    with
      Pre  => Container.Count in Positive or else raise Stack_Underflow,
      Post => Peek'Result = Peek'Result;


    Function  Pop     ( Container : in out Stack ) return Element
    with
      Pre  => Container.Count in Positive or else raise Stack_Underflow,
      Post => Container'Old.Count - 1 = Container.Count and
              Container'Old.Peek = Pop'Result;

    Procedure Push    ( Container : in out Stack; Item : aliased Element)
    with
      Pre  => Container.Count < Maximum or else raise Stack_Overflow,
      Post => Container'Old.Count + 1 = Container.Count and
              Container.Peek = Push.Item;


Private
    Use Ada.Finalization;

    subtype Index_Type is Positive range 1..Maximum;

    type Element_Access is access constant Element;
    type Element_Array  is array (Index_Type range <>) of Element_Access;


    Type Stack is new Ada.Finalization.Controlled with record
        Data     : Element_Array(Index_Type):= (Others => Null);
        Last     : Natural := 0;
    end record;


    Function  Count( Container : Stack ) return Natural is
      (Container.Last);


End Lambda_Man_Components.Stack;
