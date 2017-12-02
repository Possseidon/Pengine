unit Pengine.UnitTesting;

interface

uses
  TestFramework,

  System.SysUtils;

type

  TTestCase = class(TestFramework.TTestCase)
  public
    procedure CheckSameValueS(expected, actual: Single; epsilon: Single = 0; msg: string = '');
    procedure CheckSameValueD(expected, actual: Double; epsilon: Double = 0; msg: string = '');
    procedure CheckSameValueE(expected, actual: Extended; epsilon: Extended = 0; msg: string = '');
  end;

implementation

uses
  Math;

procedure TTestCase.CheckSameValueS(expected, actual: Single; epsilon: Single = 0; msg: string = '');
begin
  if not SameValue(expected, actual, epsilon) then
    Fail(notEqualsErrorMessage(expected.ToString, actual.ToString, msg), ReturnAddress);
end;

procedure TTestCase.CheckSameValueD(expected, actual: Double; epsilon: Double; msg: string);
begin
  if not SameValue(expected, actual, epsilon) then
    Fail(notEqualsErrorMessage(expected.ToString, actual.ToString, msg), ReturnAddress);
end;

procedure TTestCase.CheckSameValueE(expected, actual: Extended; epsilon: Extended; msg: string);
begin
  if not SameValue(expected, actual, epsilon) then
    Fail(notEqualsErrorMessage(expected.ToString, actual.ToString, msg), ReturnAddress);
end;

end.

