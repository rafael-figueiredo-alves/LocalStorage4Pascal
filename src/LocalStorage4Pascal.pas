unit LocalStorage4Pascal;

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

  iLocalStorage4Pascal = interface
    ['{076E3E57-880E-4744-B3B4-2514FC4F79C2}']
    {$region 'SetValue methods'}
    procedure SetValue(const Key: string; const Value: string); overload;
    procedure SetValue(const Key: string; const Value: Integer); overload;
    procedure SetValue(const Key: string; const Value: Boolean); overload;
    procedure SetValue(const Key: string; const Value: Double); overload;
    procedure SetValue(const Key: string; const Value: TJSONObject); overload;
    procedure SetValue(const Key: string; const Value: TJSONArray); overload;
    {$endregion}

    {$region 'GetValue Methods'}
    function GetString(const Key: string; const Default: string = ''): string;
    function GetInteger(const Key: string; const Default: Integer = 0): Integer;
    function GetBoolean(const Key: string; const Default: Boolean = False): Boolean;
    function GetDouble(const Key: string; const Default: Double = 0.0): Double;
    function GetJSONObject(const Key: string; const Default: TJSONObject = nil): TJSONObject;
    function GetJSONArray(const Key: string; const Default: TJSONArray = nil): TJSONArray;
    {$endregion}

    function RemoveValue(const Key: string): Boolean;
    function KeyExists(const Key: string): Boolean;
    function Clear: Boolean;
  end;

  TLocalStorage4Pascal = class(TInterfacedObject, iLocalStorage4Pascal)
  private
    FStorage: {$IFDEF FPC} TJSONObject {$ELSE} System.JSON.TJSONObject {$ENDIF};
    FFilePath: string;
    function GetStoragePath(const FileName: string = 'localstorage.json'): string;
    procedure LoadFromFile;
    procedure SaveToFile;
    function GetValue<T>(const key: string; Default: T): T;
    function SetValue<T>(const key: string; const Value: T): T; overload;
  public
    class var FInstance: iLocalStorage4Pascal;
    constructor Create(const FileName: string = 'LocalStorage.json');
    destructor Destroy; override;
    class function New(const FileName: string = 'LocalStorage.json'): iLocalStorage4Pascal;
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
    function GetJSONObject(const Key: string; const Default: TJSONObject = nil): TJSONObject;
    function GetJSONArray(const Key: string; const Default: TJSONArray = nil): TJSONArray;
    function RemoveValue(const Key: string): Boolean;
    function KeyExists(const Key: string): Boolean;
    function Clear: Boolean;
  end;

implementation

uses
  System.IOUtils, System.TypInfo, System.Rtti;

{ TLocalStorage4Pascal }

function TLocalStorage4Pascal.GetValue<T>(const key: string; Default: T): T;
begin
  if FStorage.FindValue(Key) <> nil then
    Result := FStorage.Values[key].AsType<T>
  else
   begin
    SetValue(key, Default);
    Result := Default;
   end;
end;

function TLocalStorage4Pascal.SetValue<T>(const key: string; const Value: T): T;
var
  StoredValue: TValue;
begin
  StoredValue := TValue.From<T>(Value);

  if StoredValue.IsObject then
    begin
      if StoredValue.AsObject is TJSONObject then
        FStorage.AddPair(key, TJSONObject(StoredValue.AsObject))
      else if StoredValue.AsObject is TJSONArray then
        FStorage.AddPair(key, TJSONArray(StoredValue.AsObject));
    end
   else
    begin

      case PTypeInfo(TypeInfo(T))^.Kind of
        tkInteger:
          FStorage.AddPair(key, StoredValue.AsInteger);
        tkFloat:
          FStorage.AddPair(key, StoredValue.AsExtended);
        tkUString, tkString, tkLString:
          FStorage.AddPair(key, StoredValue.AsString);
        tkWString:
          FStorage.AddPair(key, StoredValue.AsString);
        tkEnumeration:
          if TypeInfo(T) = TypeInfo(Boolean) then
            FStorage.AddPair(key, StoredValue.AsBoolean);
      else
        raise Exception.CreateFmt('Tipo não suportado: %s', [PTypeInfo(TypeInfo(T))^.Name]);
      end;
    end;

  Result := T(value);
end;

function TLocalStorage4Pascal.GetStoragePath(const FileName: string): string;
begin
  {$IFDEF MSWINDOWS}
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + FileName;
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(TPath.GetHomePath) + FileName;
  {$ENDIF}
end;

procedure TLocalStorage4Pascal.LoadFromFile;
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

procedure TLocalStorage4Pascal.SaveToFile;
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

constructor TLocalStorage4Pascal.Create(const FileName: string);
begin
  FFilePath := GetStoragePath(FileName);
  FStorage := {$IFDEF FPC} TJSONObject.Create {$ELSE} System.JSON.TJSONObject.Create {$ENDIF};
  LoadFromFile;
end;

destructor TLocalStorage4Pascal.Destroy;
begin
  SaveToFile;
  FStorage.Free;
  inherited;
end;

class function TLocalStorage4Pascal.New(const FileName: string): iLocalStorage4Pascal;
begin
  if(not Assigned(FInstance))then
    FInstance := TLocalStorage4Pascal.Create(FileName);
  Result := FInstance;
end;

function TLocalStorage4Pascal.Clear: Boolean;
var
  i: Integer;
begin
  for i := FStorage.Count - 1 downto 0 do
    FStorage.RemovePair(FStorage.Pairs[i].JsonString.Value); // Remove um por um
  SaveToFile;
end;

function TLocalStorage4Pascal.GetBoolean(const Key: string;const Default: Boolean): Boolean;
begin
  Result := GetValue<Boolean>(key, Default);
end;

function TLocalStorage4Pascal.GetDouble(const Key: string;const Default: Double): Double;
begin
  Result := GetValue<Double>(key, Default);
end;

function TLocalStorage4Pascal.GetInteger(const Key: string;const Default: Integer): Integer;
begin
  Result := GetValue<integer>(key, Default);
end;

function TLocalStorage4Pascal.GetJSONArray(const Key: string;const Default: TJSONArray): TJSONArray;
begin
  Result := GetValue<TJSONArray>(key, Default);
end;

function TLocalStorage4Pascal.GetJSONObject(const Key: string;const Default: TJSONObject): TJSONObject;
begin
  Result := GetValue<TJSONObject>(key, Default);
end;

function TLocalStorage4Pascal.GetString(const Key, Default: string): string;
begin
  Result := GetValue<string>(key, Default);
end;

function TLocalStorage4Pascal.KeyExists(const Key: string): Boolean;
begin
  Result := assigned(FStorage.FindValue(key));
end;

function TLocalStorage4Pascal.RemoveValue(const Key: string): Boolean;
begin
  Result := assigned(FStorage.RemovePair(key));
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string;const Value: Integer);
begin
  FStorage.RemovePair(key);
  SetValue<integer>(key, value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key, Value: string);
begin
  FStorage.RemovePair(key);
  SetValue<string>(key, value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string;const Value: TJSONArray);
begin
  FStorage.RemovePair(key);
  SetValue<TJSONArray>(key, value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string;const Value: TJSONObject);
begin
  FStorage.RemovePair(key);
  SetValue<TJSONObject>(key, value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Double);
begin
  FStorage.RemovePair(key);
  SetValue<double>(key, value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string;
  const Value: Boolean);
begin
  FStorage.RemovePair(key);
  SetValue<Boolean>(key, value);
end;

end.

