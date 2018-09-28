program ObervableArrayTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,

  Pengine.EventCollections;

type

  TMyArray = TObservableArray<string>;

procedure OnChange;
begin
  Writeln('OnChange');
end;

procedure OnAdd;
begin
  Writeln('OnAdd');
end;

procedure OnRemove;
begin
  Writeln('OnRemove');
end;

procedure OnChangeIndex;
begin
  Writeln('OnChangeIndex');
end;

procedure OnSwap;
begin
  Writeln('OnSwap');
end;

procedure OnSort;
begin
  Writeln('OnSort');
end;

procedure OnClear;
begin
  Writeln('OnClear');
end;

var
  MyList: TMyArray;
begin
  try

    MyList := TMyArray.Create;
    MyList.OnChange.Add(OnChange);
    MyList.OnAdd.Add(OnAdd);
    MyList.OnRemove.Add(OnRemove);
    MyList.OnChangeIndex.Add(OnChangeIndex);
    MyList.OnSwap.Add(OnSwap);
    MyList.OnSort.Add(OnSort);
    MyList.OnClear.Add(OnClear);

    MyList.Add('hello');
    MyList.Add('this');
    MyList.Add('is');
    MyList.Insert('awesome', 0);
    MyList.Clear;

    Readln;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

