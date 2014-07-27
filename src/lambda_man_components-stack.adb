Package Body Lambda_Man_Components.Stack is

    Function  Peek ( Container : Stack ) return Element is
      ( Container.Data(Container.Last).all );


    Function  Pop     ( Container : in out Stack ) return Element is
    begin
        Return Result : constant Element:= Container.Data(Container.Last).all do
            Container.Last:= Positive'Pred( Container.Last );
        end return;
    End Pop;


    Procedure Push ( Container : in out Stack; Item : aliased Element) is
    begin
        Container.Last:= Positive'Succ( Container.Last );
        Container.Data(Container.Last):= Item'Unchecked_Access;
    End Push;



End Lambda_Man_Components.Stack;
