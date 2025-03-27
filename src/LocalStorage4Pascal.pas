unit LocalStorage4Pascal;

interface

uses
  LocalStorage4Pascal.Interfaces;

type

  TLocalStorage = class
    class function InitLocalStorage4Pascal(const FileName: string = 'localstorage.json'; SaveToFileDefaultValues: boolean = false): iLocalStorage4Pascal;
  end;

var
  LocalStorage4Delphi: iLocalStorage4Pascal;
  LocalStorage4Lazarus: iLocalStorage4Pascal;

implementation

uses
  {$IFDEF FPC}
  LocalStorage4Pascal.Lazarus;
  {$ELSE}
  LocalStorage4Pascal.Delphi;
  {$ENDIF}

class function TLocalStorage.InitLocalStorage4Pascal(const FileName: string = 'localstorage.json'; SaveToFileDefaultValues: boolean = false): iLocalStorage4Pascal;
begin
  {$IFDEF FPC}
  if not Assigned(LocalStorage4Lazarus) then
    LocalStorage4Lazarus := TLocalStorage4Pascal.New(FileName, SaveToFileDefaultValues);

  Result := LocalStorage4Lazarus;
  {$ELSE}
  if not Assigned(LocalStorage4Delphi) then
    LocalStorage4Delphi := TLocalStorage4Pascal.New(FileName, SaveToFileDefaultValues);

  Result := LocalStorage4Delphi;
  {$ENDIF}
end;

end.
