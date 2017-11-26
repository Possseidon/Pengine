unit GameBaseDefine;

interface

uses
  Lists;

type

  ILuaCoded = interface
    procedure Update;
  end;

  TGameBase = class abstract
  private
    FLuaObjects: TInterfaceArray<ILuaCoded>;
  public

  end;

implementation

end.

