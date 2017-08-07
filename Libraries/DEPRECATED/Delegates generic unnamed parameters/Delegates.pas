unit Delegates;

interface

uses
  SysUtils;
  
type

  EDelegateNotFound = class(Exception)
  public
    constructor Create;
  end;

  {$DEFINE INTERFACE_SECTION}

  {$DEFINE DELEGATE1}{$I DelegateHelp}
  {$DEFINE DELEGATE2}{$I DelegateHelp}
  {$DEFINE DELEGATE3}{$I DelegateHelp}
  {$DEFINE DELEGATE4}{$I DelegateHelp}
  {$DEFINE DELEGATE5}{$I DelegateHelp}
  {$DEFINE DELEGATE6}{$I DelegateHelp}
  {$DEFINE DELEGATE7}{$I DelegateHelp}
  {$DEFINE DELEGATE8}{$I DelegateHelp}
  {$UNDEF DELEGATE1}
  {$UNDEF DELEGATE2}
  {$UNDEF DELEGATE3}
  {$UNDEF DELEGATE4}
  {$UNDEF DELEGATE5}
  {$UNDEF DELEGATE6}
  {$UNDEF DELEGATE7}
  {$UNDEF DELEGATE8}

  {$UNDEF INTERFACE_SECTION}

implementation

{ EDelegateNotFound }

constructor EDelegateNotFound.Create;
begin
  inherited Create('Could not remove Delegate, as it does not exist in the Event-List');
end;

  {$DEFINE IMPLEMENTATION_SECTION}

  {$DEFINE DELEGATE1}{$I DelegateHelp}
  {$DEFINE DELEGATE2}{$I DelegateHelp}
  {$DEFINE DELEGATE3}{$I DelegateHelp}
  {$DEFINE DELEGATE4}{$I DelegateHelp}
  {$DEFINE DELEGATE5}{$I DelegateHelp}
  {$DEFINE DELEGATE6}{$I DelegateHelp}
  {$DEFINE DELEGATE7}{$I DelegateHelp}
  {$DEFINE DELEGATE8}{$I DelegateHelp}
  {$UNDEF DELEGATE1}
  {$UNDEF DELEGATE2}
  {$UNDEF DELEGATE3}
  {$UNDEF DELEGATE4}
  {$UNDEF DELEGATE5}
  {$UNDEF DELEGATE6}
  {$UNDEF DELEGATE7}
  {$UNDEF DELEGATE8}

  {$UNDEF IMPLEMENTATION_SECTION}

end.
