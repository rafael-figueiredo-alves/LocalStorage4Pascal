unit LocalStorage4Pascal.Lazarus;

interface

{$IFDEF FPC}
uses
  fpjson,
  jsonparser,
  LocalStorage4Pascal.Interfaces;

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
    function AsType<T>(JsonData: TJSONData): T;
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

{$IFDEF FPC}
uses
  TypInfo,
  RTTI,
  LazUTF8;

{ TLocalStorage4Pascal }

function TLocalStorage4Pascal.AsType<T>(JsonData: TJSONData): T;
begin
  if JsonData = nil then
    raise Exception.Create('Dados JSON não encontrados');

  case PTypeInfo(TypeInfo(T))^.Kind of
    tkInteger:
      Result := TypeCast<T>(JsonData.AsInteger);
    tkInt64:
      Result := TypeCast<T>(JsonData.AsInt64);
    tkFloat:
      Result := TypeCast<T>(JsonData.AsFloat);
    tkUString, tkString:
      Result := TypeCast<T>(JsonData.AsString);
    tkEnumeration:
      if TypeInfo(T) = TypeInfo(Boolean) then
        Result := TypeCast<T>(JsonData.AsBoolean)
      else
        raise Exception.Create('Tipo genérico não suportado: ' + PTypeInfo(TypeInfo(T))^.Name);
    else
      raise Exception.Create('Tipo genérico não suportado: ' + PTypeInfo(TypeInfo(T))^.Name);
  end;
end;

// Função auxiliar para cast genérico
function TypeCast<T>: T; inline;
begin
  Result := T(Value);
end;

function TLocalStorage4Pascal.GetValue<T>(const Key: string; Default: T): T;
var
  JsonData: TJSONData;
begin
  JsonData := FStorage.Find(Key);
  if JsonData <> nil then
    Result := AsType<T>(JsonData)
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
  FStorage.Delete(Key);

  StoredValue := TValue.From<T>(Value);

  if StoredValue.IsObject then
  begin
    if StoredValue.AsObject is TJSONObject then
      FStorage.Add(Key, TJSONObject(StoredValue.AsObject).Clone as TJSONObject)
    else if StoredValue.AsObject is TJSONArray then
      FStorage.Add(Key, TJSONArray(StoredValue.AsObject).Clone as TJSONArray);
  end
  else
  begin
    case PTypeInfo(TypeInfo(T))^.Kind of
      tkInteger:
        FStorage.Add(Key, StoredValue.AsInteger);
      tkInt64:
        FStorage.Add(Key, StoredValue.AsInt64);
      tkFloat:
        FStorage.Add(Key, StoredValue.AsExtended);
      tkUString, tkString, tkLString:
        FStorage.Add(Key, StoredValue.AsString);
      tkWString:
        FStorage.Add(Key, StoredValue.AsString);
      tkEnumeration:
        if TypeInfo(T) = TypeInfo(Boolean) then
          FStorage.Add(Key, StoredValue.AsBoolean)
        else
          raise Exception.CreateFmt('Tipo não suportado: %s', [PTypeInfo(TypeInfo(T))^.Name]);
      else
        raise Exception.CreateFmt('Tipo não suportado: %s', [PTypeInfo(TypeInfo(T))^.Name]);
    end;
  end;

  SaveToFile;
  Result := Value; // Ajustado de T(Value) para Value
end;

function TLocalStorage4Pascal.Version: string;
begin
  Result := LocaStorage4Pascal_Version;
end;

function TLocalStorage4Pascal.GetStoragePath(const FileName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(GetUserDir) + FileName; // Substituí TPath.GetHomePath
end;

procedure TLocalStorage4Pascal.LoadFromFile;
var
  JSONStr: string;
  FileStream: TFileStream;
  StringStream: TStringStream;
  JSONData: TJSONData;
begin
  if not FileExists(FFilePath) then Exit;

  try
    FileStream := TFileStream.Create(FFilePath, fmOpenRead or fmShareDenyWrite);
    try
      StringStream := TStringStream.Create('');
      try
        StringStream.CopyFrom(FileStream, FileStream.Size);
        JSONStr := StringStream.DataString;
      finally
        StringStream.Free;
      end;
    finally
      FileStream.Free;
    end;

    if Trim(JSONStr) = '' then Exit;

    JSONData := GetJSON(JSONStr);
    if Assigned(JSONData) and (JSONData.JSONType = jtObject) then
    begin
      JSONObj := JSONData as TJSONObject;
      FStorage.Free;
      FStorage := JSONObj;
    end
    else
    begin
      JSONData.Free; // Libera se não for um objeto válido
    end;
  except
    on E: Exception do
      Writeln('Erro ao carregar o arquivo: ', E.Message); // Tratamento básico de erro
  end;
end;

procedure TLocalStorage4Pascal.SaveToFile;
var
  JSONStr: string;
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  JSONStr := FStorage.FormatJSON; // Substituímos ToJSON por FormatJSON
  try
    FileStream := TFileStream.Create(FFilePath, fmCreate or fmShareDenyRead);
    try
      StringStream := TStringStream.Create(JSONStr);
      try
        FileStream.CopyFrom(StringStream, StringStream.Size);
      finally
        StringStream.Free;
      end;
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
      Writeln('Erro ao salvar o arquivo: ', E.Message); // Tratamento básico de erro
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
      FStorage.Delete(i)

    SaveToFile;
    Result := True;
  except
    // Tratamento de erro, se necessário
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
end;

function TLocalStorage4Pascal.GetString(const Key: string; const Default: string): string;
begin
  Result := GetValue<string>(Key, Default);
end;

function TLocalStorage4Pascal.KeyExists(const Key: string): Boolean;
begin
  Result := Assigned(FStorage.Find(Key));
end;

function TLocalStorage4Pascal.RemoveValue(const Key: string): Boolean;
var
  Index: Integer;
begin
  Index := FStorage.IndexOfName(Key);
  Result := Index >= 0;
  if Result then
  begin
    FStorage.Delete(Index);
    SaveToFile; // Necessário no Lazarus para persistir a remoção
  end;
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Integer);
begin
  SetValue(Key, Value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key, Value: string);
begin
  SetValue(Key, Value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: TJSONArray);
begin
  SetValue(Key, Value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: TJSONObject);
begin
  SetValue(Key, Value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Double);
begin
  SetValue(Key, Value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Boolean);
begin
  SetValue(Key, Value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Int64);
begin
  SetValue(Key, Value);
end;
{$endif}

end.
