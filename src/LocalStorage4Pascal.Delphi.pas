unit LocalStorage4Pascal.Delphi;

interface

{$IFNDEF FPC}

uses
  Classes,
  SysUtils,
  System.JSON, LocalStorage4Pascal.Interfaces;

type
  TLocalStorage4Pascal = class(TInterfacedObject, iLocalStorage4Pascal)
  private
    FStorage: TJSONObject;
    FFilePath: string;
    function GetStoragePath(const FileName: string = 'localstorage.json'): string;
    procedure LoadFromFile;
    procedure SaveToFile;
    function GetValue<T>(const Key: string; Default: T): T;
    function SetValue<T>(const Key: string; const Value: T): T; overload;
  public
    constructor Create(const FileName: string = 'LocalStorage.json');
    destructor Destroy; override;
    class function New(const FileName: string = 'LocalStorage.json'): iLocalStorage4Pascal;
    procedure SetValue(const Key: string; const Value: string); overload;
    procedure SetValue(const Key: string; const Value: Integer); overload;
    procedure SetValue(const Key: string; const Value: Boolean); overload;
    procedure SetValue(const Key: string; const Value: Double); overload;
    procedure SetValue(const Key: string; const Value: TJSONObject); overload;
    procedure SetValue(const Key: string; const Value: TJSONArray); overload;
    procedure SetValue(const Key: string; const Value: Int64); overload;

    function GetString(const Key: string; const Default: string = ''): string;
    function GetInteger(const Key: string; const Default: Integer = 0): Integer;
    function GetBoolean(const Key: string; const Default: Boolean = False): Boolean;
    function GetDouble(const Key: string; const Default: Double = 0.0): Double;
    function GetJSONObject(const Key: string; const Default: TJSONObject = nil): TJSONObject;
    function GetJSONArray(const Key: string; const Default: TJSONArray = nil): TJSONArray;
    function GetInt64(const Key: string; const Default: Int64 = 0): Int64;

    function RemoveValue(const Key: string): Boolean;
    function KeyExists(const Key: string): Boolean;
    function Clear: Boolean;

    function Version: string;
  end;

{$ENDIF}

implementation

{$IFNDEF FPC}

uses
  System.IOUtils,
  System.TypInfo,
  System.Rtti;

{ TLocalStorage4Pascal }

function TLocalStorage4Pascal.GetValue<T>(const Key: string; Default: T): T;
begin
  if FStorage.FindValue(Key) <> nil then
    Result := FStorage.Values[Key].AsType<T>
  else
  begin
    SetValue(Key, Default);
    Result := Default;
  end;
end;

function TLocalStorage4Pascal.SetValue<T>(const Key: string; const Value: T): T;
var
  StoredValue: TValue;
begin
  FStorage.RemovePair(Key).Free;

  StoredValue := TValue.From<T>(Value);

  if StoredValue.IsObject then
  begin
    if StoredValue.AsObject is TJSONObject then
      FStorage.AddPair(Key, TJSONObject(StoredValue.AsObject).Clone as TJSONObject)
    else if StoredValue.AsObject is TJSONArray then
      FStorage.AddPair(Key, TJSONArray(StoredValue.AsObject).Clone as TJSONArray);
  end
  else
  begin
    case PTypeInfo(TypeInfo(T))^.Kind of
      tkInteger:
        FStorage.AddPair(Key, StoredValue.AsInteger);
      tkInt64:
        FStorage.AddPair(Key, StoredValue.AsInt64);
      tkFloat:
        FStorage.AddPair(Key, StoredValue.AsExtended);
      tkUString, tkString, tkLString:
        FStorage.AddPair(Key, StoredValue.AsString);
      tkWString:
        FStorage.AddPair(Key, StoredValue.AsString);
      tkEnumeration:
        if TypeInfo(T) = TypeInfo(Boolean) then
          FStorage.AddPair(Key, StoredValue.AsBoolean);
      else
        raise Exception.CreateFmt('Tipo não suportado: %s', [PTypeInfo(TypeInfo(T))^.Name]);
    end;
  end;

  SaveToFile;
  Result := T(Value);
end;

function TLocalStorage4Pascal.Version: string;
begin
  Result := LocaStorage4Pascal_Version;
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
    raise Exception.Create('Erro ao tentar salvar LocalStorage em arquivo.');
  end;
end;

constructor TLocalStorage4Pascal.Create(const FileName: string);
begin
  FFilePath := GetStoragePath(FileName);
  FStorage := TJSONObject.Create;
  LoadFromFile;
end;

destructor TLocalStorage4Pascal.Destroy;
begin
  SaveToFile;
  FreeAndNil(FStorage);
  inherited;
end;

class function TLocalStorage4Pascal.New(const FileName: string): iLocalStorage4Pascal;
begin
  Result := TLocalStorage4Pascal.Create(FileName);
end;

function TLocalStorage4Pascal.Clear: Boolean;
var
  i: Integer;
begin
  Result := False;
  try
    for i := FStorage.Count - 1 downto 0 do
      FStorage.RemovePair(FStorage.Pairs[i].JsonString.Value).Free; // Libera cada item
      
    SaveToFile;
    Result := True;
  except
    raise Exception.Create('Erro ao tentar limpar dados do LocalStorage');
  end;
end;

function TLocalStorage4Pascal.GetBoolean(const Key: string; const Default: Boolean): Boolean;
begin
  Result := GetValue<Boolean>(Key, Default);
end;

function TLocalStorage4Pascal.GetDouble(const Key: string; const Default: Double): Double;
begin
  Result := GetValue<Double>(Key, Default);
end;

function TLocalStorage4Pascal.GetInt64(const Key: string; const Default: Int64): Int64;
begin
  Result := GetValue<Int64>(Key, Default);
end;

function TLocalStorage4Pascal.GetInteger(const Key: string; const Default: Integer): Integer;
begin
  Result := GetValue<Integer>(Key, Default);
end;

function TLocalStorage4Pascal.GetJSONArray(const Key: string; const Default: TJSONArray): TJSONArray;
var
  Value: TJSONArray;
begin
  Value := GetValue<TJSONArray>(Key, Default);
  if Assigned(Value) then
    Result := Value.Clone as TJSONArray
  else
    Result := nil;
    
  Default.Free; // Apenas no Delphi, pois Default é passado como const e pode ser liberado
end;

function TLocalStorage4Pascal.GetJSONObject(const Key: string; const Default: TJSONObject): TJSONObject;
var
  Value: TJSONObject;
begin
  Value := GetValue<TJSONObject>(Key, Default);
  if Assigned(Value) then
    Result := Value.Clone as TJSONObject
  else
    Result := nil;

  Default.Free; // Apenas no Delphi, pois Default é passado como const e pode ser liberado
end;

function TLocalStorage4Pascal.GetString(const Key: string; const Default: string): string;
begin
  Result := GetValue<string>(Key, Default);
end;

function TLocalStorage4Pascal.KeyExists(const Key: string): Boolean;
begin
  Result := Assigned(FStorage.FindValue(Key));
end;

function TLocalStorage4Pascal.RemoveValue(const Key: string): Boolean;
var
  Pair: TJSONPair;
begin
  Pair := FStorage.RemovePair(Key);

  Result := Assigned(Pair);

  if Result then
    Pair.Free; // Libera o par removido para evitar vazamento
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Integer);
begin
  SetValue<Integer>(Key, Value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key, Value: string);
begin
  SetValue<string>(Key, Value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: TJSONArray);
begin
  SetValue<TJSONArray>(Key, Value);

  Value.Free; // Apenas no Delphi, pois Value é passado como const e pode ser liberado
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: TJSONObject);
begin
  SetValue<TJSONObject>(Key, Value);
  
  Value.Free; // Apenas no Delphi, pois Value é passado como const e pode ser liberado
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Double);
begin
  SetValue<Double>(Key, Value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Boolean);
begin
  SetValue<Boolean>(Key, Value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Int64);
begin
  SetValue<Int64>(Key, Value);
end;

{$ENDIF}

end.
