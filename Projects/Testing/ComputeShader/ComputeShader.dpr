program ComputeShader;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,

  System.SysUtils,

  Pengine.IntMaths,
  Pengine.GLState,
  Pengine.GLContext,
  Pengine.GLProgram;

var
  WND: HWND;
  DC: HDC;
  Context: TGLContext;
  State: TGLState;
  Prog: TGLProgram;
begin
  try

    Writeln('Started...');

    Context := TGLContext.Create(0, 0, nil);
    State := TGLState.Create;

    Writeln('GLState created.');

    Prog := TGLProgram.Create(State);

    Writeln('Program created.');

    Prog.AddShaderFromText(stCompute,
      '#version 420' + sLineBreak +
      'void main() { }');

    Writeln('Added program data.');

    Context.Free;
    Prog.Free;
    State.Free;

  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.

