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
    procedure SetValue(const Key: string; const Value: Int64); overload;
    {$endregion}

    {$region 'GetValue Methods'}
    function GetString(const Key: string; const Default: string = ''): string;
    function GetInteger(const Key: string; const Default: Integer = 0): Integer;
    function GetBoolean(const Key: string; const Default: Boolean = False): Boolean;
    function GetDouble(const Key: string; const Default: Double = 0.0): Double;
    function GetJSONObject(const Key: string; const Default: TJSONObject = nil): TJSONObject;
    function GetJSONArray(const Key: string; const Default: TJSONArray = nil): TJSONArray;
    function GetInt64(const Key: string; const Default: Int64 = 0): Int64;
    {$endregion}

    function RemoveValue(const Key: string): Boolean;
    function KeyExists(const Key: string): Boolean;
    function Clear: Boolean;

    function Version: string;
  end;

  TLocalStorage4Pascal = class(TInterfacedObject, iLocalStorage4Pascal)
  private
    FStorage: {$IFDEF FPC} TJSONObject {$ELSE} System.JSON.TJSONObject {$ENDIF};
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

  function InitLocalStorage4Pascal(const FileName: string = 'localstorage.json'): iLocalStorage4Pascal;

var
  LocalStorage4Delphi: iLocalStorage4Pascal;
  LocalStorage4Lazarus: iLocalStorage4Pascal;

const
  LocaStorage4Pascal_Version = '1.0.0';

implementation

uses
  {$IFDEF FPC}
  TypInfo,
  RTTI,
  LazUTF8
  {$ELSE}
  System.IOUtils,
  System.TypInfo,
  System.Rtti
  {$ENDIF};

function InitLocalStorage4Pascal(const FileName: string = 'localstorage.json'): iLocalStorage4Pascal;
begin
  {$IFDEF FPC}
  if not Assigned(LocalStorage4Lazarus) then
    LocalStorage4Lazarus := TLocalStorage4Pascal.New(FileName);
  Result := LocalStorage4Lazarus;
  {$ELSE}
  if not Assigned(LocalStorage4Delphi) then
    LocalStorage4Delphi := TLocalStorage4Pascal.New(FileName);
  Result := LocalStorage4Delphi;
  {$ENDIF}
end;

{ TLocalStorage4Pascal }
function TLocalStorage4Pascal.GetValue<T>(const Key: string; Default: T): T;
{$IFDEF FPC}
var
  JsonData: TJSONData;
{$ENDIF}
begin
  {$IFNDEF FPC}
  if FStorage.FindValue(Key) <> nil then
    Result := FStorage.Values[Key].AsType<T>
  else
  begin
    SetValue(Key, Default);
    Result := Default;
  end;
  {$ELSE}
  JsonData := FStorage.Find(Key);
    if JsonData <> nil then
    begin
      case PTypeInfo(TypeInfo(T))^.Kind of
        tkInteger:
          Result := Integer(JsonData.AsInteger);
        tkInt64:
          Result := Int64(JsonData.AsInt64);
        tkFloat:
          Result := Double(JsonData.AsFloat);
        tkUString, tkString:
          Result := String(JsonData.AsString);
        tkEnumeration:
          if TypeInfo(T) = TypeInfo(Boolean) then
            Result := Boolean(JsonData.AsBoolean)
          else
            raise Exception.Create('Tipo genérico não suportado: ' + PTypeInfo(TypeInfo(T))^.Name);
        else
          raise Exception.Create('Tipo genérico não suportado: ' + PTypeInfo(TypeInfo(T))^.Name);
      end;
    end
    else
    begin
      SetValue(Key, Default);
      Result := Default;
    end;;
  {$ENDIF}
end;

function TLocalStorage4Pascal.SetValue<T>(const Key: string; const Value: T): T;
var
  StoredValue: TValue;
begin
  {$IFDEF FPC}
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
  {$ELSE}
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
  {$ENDIF}
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
  {$IFDEF FPC}
  Result := IncludeTrailingPathDelimiter(GetUserDir) + FileName; // Substituí TPath.GetHomePath
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(TPath.GetHomePath) + FileName;
  {$ENDIF}
  {$ENDIF}
end;

procedure TLocalStorage4Pascal.LoadFromFile;
var
  JSONStr: string;
  FileStream: {$IFDEF FPC}TFileStream{$ELSE}TStreamReader{$ENDIF};
  {$IFDEF FPC}StringStream: TStringStream;{$ENDIF}
  {$IFDEF FPC}JSONData: TJSONData;{$ENDIF}
  JSONObj: TJSONObject;
begin
  if not FileExists(FFilePath) then Exit;

  {$IFDEF FPC}
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
  {$ELSE}
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
  {$ENDIF}
end;

procedure TLocalStorage4Pascal.SaveToFile;
var
  JSONStr: string;
  {$IFNDEF FPC}
  FileStream: TStreamWriter;
  {$ELSE}
  FileStream: TFileStream;
  StringStream: TStringStream;
  {$ENDIF}
begin
  {$IFDEF FPC}
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
  {$ELSE}
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
  {$ENDIF}
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
      {$IFNDEF FPC}
      FStorage.RemovePair(FStorage.Pairs[i].JsonString.Value).Free // Libera cada item
      {$ELSE}
      FStorage.Delete(i)
      {$ENDIF};
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
  {$IFNDEF FPC}
  Default.Free; // Apenas no Delphi, pois Default é passado como const e pode ser liberado
  {$ENDIF}
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
  {$IFNDEF FPC}
  Default.Free; // Apenas no Delphi, pois Default é passado como const e pode ser liberado
  {$ENDIF}
end;

function TLocalStorage4Pascal.GetString(const Key: string; const Default: string): string;
begin
  Result := GetValue<string>(Key, Default);
end;

function TLocalStorage4Pascal.KeyExists(const Key: string): Boolean;
begin
  {$IFNDEF FPC}
  Result := Assigned(FStorage.FindValue(Key));
  {$ELSE}
  Result := Assigned(FStorage.Find(Key));
  {$ENDIF}
end;

function TLocalStorage4Pascal.RemoveValue(const Key: string): Boolean;
var
  {$IFNDEF FPC}
  Pair: TJSONPair;
  {$ELSE}
  Index: Integer;
  {$ENDIF}
begin
  {$IFNDEF FPC}
  Pair := FStorage.RemovePair(Key);
  Result := Assigned(Pair);
  if Result then
    Pair.Free; // Libera o par removido para evitar vazamento
  {$ELSE}
  Index := FStorage.IndexOfName(Key);
  Result := Index >= 0;
  if Result then
  begin
    FStorage.Delete(Index);
    SaveToFile; // Necessário no Lazarus para persistir a remoção
  end;
  {$ENDIF}
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Integer);
begin
  {$IFNDEF FPC}
  SetValue<Integer>(Key, Value);
  {$ELSE}
  SetValue(Key, Value);
  {$ENDIF}
end;

procedure TLocalStorage4Pascal.SetValue(const Key, Value: string);
begin
  {$IFNDEF FPC}
  SetValue<string>(Key, Value);
  {$ELSE}
  SetValue(Key, Value);
  {$ENDIF}
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: TJSONArray);
begin
  {$IFNDEF FPC}
  SetValue<TJSONArray>(Key, Value);
  {$ELSE}
  SetValue(Key, Value);
  {$ENDIF}

  {$IFNDEF FPC}
  Value.Free; // Apenas no Delphi, pois Value é passado como const e pode ser liberado
  {$ENDIF}
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: TJSONObject);
begin
  {$IFNDEF FPC}
  SetValue<TJSONObject>(Key, Value);
  {$ELSE}
  SetValue(Key, Value);
  {$ENDIF}

  {$IFNDEF FPC}
  Value.Free; // Apenas no Delphi, pois Value é passado como const e pode ser liberado
  {$ENDIF}
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Double);
begin
  {$IFNDEF FPC}
  SetValue<Double>(Key, Value);
  {$ELSE}
  SetValue(Key, Value);
  {$ENDIF}
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Boolean);
begin
  {$IFNDEF FPC}
  SetValue<Boolean>(Key, Value);
  {$ELSE}
  SetValue(Key, Value);
  {$ENDIF}
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Int64);
begin
  {$IFNDEF FPC}
  SetValue<Int64>(Key, Value);
  {$ELSE}
  SetValue(Key, Value);
  {$ENDIF}
end;

end.
