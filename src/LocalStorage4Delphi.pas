unit LocalStorage4Delphi;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
  fpjson,
  jsonparser
  {$ELSE}
  System.JSON
  {$ENDIF},
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF FMX}
  FMX.Dialogs
  {$ELSE}
  Vcl.Dialogs
  {$ENDIF};

type

  iLocalStorage4Delphi = interface
    ['{076E3E57-880E-4744-B3B4-2514FC4F79C2}']
    procedure SetValue(const Key: string; const Value: string); overload;
    procedure SetValue(const Key: string; const Value: Integer); overload;
    procedure SetValue(const Key: string; const Value: Boolean); overload;
    procedure SetValue(const Key: string; const Value: Double); overload;
    procedure SetValue(const Key: string; const Value: TJSONObject); overload;
    procedure SetValue(const Key: string; const Value: TJSONArray); overload;

    function GetString(const Key: string; const Default: string = ''): string;
    function GetInteger(const Key: string; const Default: Integer = 0): Integer;
    function GetBoolean(const Key: string; const Default: Boolean = False): Boolean;
    function GetDouble(const Key: string; const Default: Double = 0.0): Double;
    function GetJSONObject(const Key: string): TJSONObject;
    function GetJSONArray(const Key: string): TJSONArray;
    function RemoveValue(const Key: string): Boolean;
    function KeyExists(const Key: string): Boolean;
    function Clear: Boolean;
  end;

  TLocalStorage4Delphi = class(TInterfacedObject, iLocalStorage4Delphi)
  private
    FStorage: {$IFDEF FPC} TJSONObject {$ELSE} System.JSON.TJSONObject {$ENDIF};
    FFilePath: string;
    function GetStoragePath(const FileName: string = 'localstorage.json'): string;
    procedure LoadFromFile;
    procedure SaveToFile;
    function GetValue<T>(const key: string; Default: T): T;
  public
    class var FInstance: iLocalStorage4Delphi;
    constructor Create(const FileName: string = 'LocalStorage.json');
    destructor Destroy; override;
    class function New(const FileName: string = 'LocalStorage.json'): iLocalStorage4Delphi;
    procedure SetValue(const Key: string; const Value: string); overload;
    procedure SetValue(const Key: string; const Value: Integer); overload;
    procedure SetValue(const Key: string; const Value: Boolean); overload;
    procedure SetValue(const Key: string; const Value: Double); overload;
    procedure SetValue(const Key: string; const Value: TJSONObject); overload;
    procedure SetValue(const Key: string; const Value: TJSONArray); overload;

    function GetString(const Key: string; const Default: string = ''): string;
    function GetInteger(const Key: string; const Default: Integer = 0): Integer;
    function GetBoolean(const Key: string; const Default: Boolean = False): Boolean;
    function GetDouble(const Key: string; const Default: Double = 0.0): Double;
    function GetJSONObject(const Key: string): TJSONObject;
    function GetJSONArray(const Key: string): TJSONArray;
    function RemoveValue(const Key: string): Boolean;
    function KeyExists(const Key: string): Boolean;
    function Clear: Boolean;
  end;

implementation

uses
  System.IOUtils;

{ TLocalStorage }

{ TLocalStorage4Delphi }

function TLocalStorage4Delphi.Clear: Boolean;
begin

end;

constructor TLocalStorage4Delphi.Create(const FileName: string);
begin
  FFilePath := GetStoragePath(FileName);
  FStorage := {$IFDEF FPC} TJSONObject.Create {$ELSE} System.JSON.TJSONObject.Create {$ENDIF};
  LoadFromFile;
end;

destructor TLocalStorage4Delphi.Destroy;
begin
  SaveToFile;
  FStorage.Free;
  inherited;
end;

function TLocalStorage4Delphi.GetBoolean(const Key: string;
  const Default: Boolean): Boolean;
begin
  if FStorage.FindValue(Key) <> nil then
    Result := FStorage.Values[key].AsType<Boolean>
  else
   begin
    SetValue(key, Default);
    Result := Default;
   end;
end;

function TLocalStorage4Delphi.GetDouble(const Key: string;
  const Default: Double): Double;
begin
  if FStorage.FindValue(Key) <> nil then
    Result := FStorage.Values[key].AsType<Double>
  else
   begin
    SetValue(key, Default);
    Result := Default;
   end;
end;

function TLocalStorage4Delphi.GetInteger(const Key: string;
  const Default: Integer): Integer;
begin
  if FStorage.FindValue(Key) <> nil then
    Result := FStorage.Values[key].AsType<integer>
  else
   begin
    SetValue(key, Default);
    Result := Default;
   end;
end;

function TLocalStorage4Delphi.GetJSONArray(const Key: string): TJSONArray;
begin

end;

function TLocalStorage4Delphi.GetJSONObject(const Key: string): TJSONObject;
begin
  Result := GetValue(key, TJSONObject.Create);
end;

function TLocalStorage4Delphi.GetStoragePath(const FileName: string): string;
begin
  {$IFDEF MSWINDOWS}
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + FileName;
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(TPath.GetHomePath) + FileName;
  {$ENDIF}
end;

function TLocalStorage4Delphi.GetString(const Key, Default: string): string;
begin
  if FStorage.FindValue(Key) <> nil then
    Result := FStorage.Values[key].AsType<string>
  else
   begin
    SetValue(key, Default);
    Result := Default;
   end;
end;

function TLocalStorage4Delphi.GetValue<T>(const key: string; Default: T): T;
begin
  if FStorage.FindValue(Key) <> nil then
    Result := FStorage.Values[key].AsType<T>
  else
   begin
    //SetValue(key, Default);
    Result := Default;
   end;
end;

function TLocalStorage4Delphi.KeyExists(const Key: string): Boolean;
begin
  Result := assigned(FStorage.FindValue(key));
end;

procedure TLocalStorage4Delphi.LoadFromFile;
var
  JSONStr: string;
  FileStream: TStreamReader;
  JSONObj: TJSONObject;
begin
  if not FileExists(FFilePath) then Exit;

  try
    FileStream := TStreamReader.Create(FFilePath, TEncoding.UTF8);
    try
      JSONStr := FileStream.ReadToEnd;
    finally
      FileStream.Free;
    end;

    if JSONStr.Trim = '' then Exit;

    JSONObj := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
    if Assigned(JSONObj) then
    begin
      FStorage.Free;
      FStorage := JSONObj;
    end;
  except
    // Tratamento de erro opcional
  end;
end;

class function TLocalStorage4Delphi.New(
  const FileName: string): iLocalStorage4Delphi;
begin
  if(not Assigned(FInstance))then
    FInstance := TLocalStorage4Delphi.Create(FileName);
  Result := FInstance;
end;

function TLocalStorage4Delphi.RemoveValue(const Key: string): Boolean;
begin

end;

procedure TLocalStorage4Delphi.SaveToFile;
var
  JSONStr: string;
  FileStream: TStreamWriter;
begin
  JSONStr := FStorage.ToJSON;
  try
    FileStream := TStreamWriter.Create(FFilePath, False, TEncoding.UTF8);
    try
      FileStream.Write(JSONStr);
    finally
      FileStream.Free;
    end;
  except
    // Tratamento de erro opcional
  end;
end;

procedure TLocalStorage4Delphi.SetValue(const Key: string;
  const Value: Integer);
begin
  FStorage.RemovePair(key);
  FStorage.AddPair(Key, Value);
end;

procedure TLocalStorage4Delphi.SetValue(const Key, Value: string);
begin
  FStorage.RemovePair(key);
  FStorage.AddPair(Key, Value);
end;

procedure TLocalStorage4Delphi.SetValue(const Key: string;
  const Value: TJSONArray);
begin

end;

procedure TLocalStorage4Delphi.SetValue(const Key: string;
  const Value: TJSONObject);
begin
  FStorage.RemovePair(key);
  FStorage.AddPair(Key, Value);
end;

procedure TLocalStorage4Delphi.SetValue(const Key: string; const Value: Double);
begin
  FStorage.RemovePair(key);
  FStorage.AddPair(Key, Value);
end;

procedure TLocalStorage4Delphi.SetValue(const Key: string;
  const Value: Boolean);
begin
  FStorage.RemovePair(key);
  FStorage.AddPair(Key, Value);
end;

end.

