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
    function GetInt64(const key: string; const Default: Int64): Int64;
    {$endregion}

    function RemoveValue(const Key: string): Boolean;
    function KeyExists(const Key: string): Boolean;
    function Clear: Boolean;

    function Version : string;
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
    function GetInt64(const key: string; const Default: Int64): Int64;

    function RemoveValue(const Key: string): Boolean;
    function KeyExists(const Key: string): Boolean;
    function Clear: Boolean;

    function Version : string;
  end;

  function InitLocalStorage4Pascal(const FileName: string = 'localstorage.json') : iLocalStorage4Pascal;

var
  LocalStorage4Delphi  : iLocalStorage4Pascal;
  LocalStorage4Lazarus : iLocalStorage4Pascal;

const
  LocaStorage4Pascal_Version = '1.0.0';

implementation

uses
  System.IOUtils, System.TypInfo, System.Rtti;

function InitLocalStorage4Pascal(const FileName: string = 'localstorage.json') : iLocalStorage4Pascal;
begin
  {$IFDEF FPC}
  if (not Assigned(LocalStorage4Lazarus)) then
   LocalStorage4Lazarus := TLocalStorage4Pascal.New(FileName);

  Result := LocalStorage4Lazarus;
  {$else}
  if (not Assigned(LocalStorage4Delphi)) then
   LocalStorage4Delphi := TLocalStorage4Pascal.New(FileName);

  Result := LocalStorage4Delphi;
  {$endif}
end;

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

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Int64);
begin
  SetValue<Int64>(key, Value);
end;

function TLocalStorage4Pascal.SetValue<T>(const key: string; const Value: T): T;
var
  StoredValue: TValue;
begin
  FStorage.RemovePair(key).Free;

  StoredValue := TValue.From<T>(Value);

  if StoredValue.IsObject then
    begin
      if StoredValue.AsObject is TJSONObject then
        FStorage.AddPair(key, TJSONObject(StoredValue.AsObject).Clone as TJSONObject)
      else if StoredValue.AsObject is TJSONArray then
        FStorage.AddPair(key, TJSONArray(StoredValue.AsObject).Clone as TJSONArray);
    end
   else
    begin

      case PTypeInfo(TypeInfo(T))^.Kind of
        tkInteger:
          FStorage.AddPair(key, StoredValue.AsInteger);
        tkInt64:
          FStorage.AddPair(key, StoredValue.AsInt64);
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

    SaveToFile;
    Result := T(value);
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
var
 i, a: integer;
 l : TJSONObject;
begin
  SaveToFile;
//  for I := 0 to FStorage.Count - 1 do
//   begin
//    if(FStorage.Pairs[i].JsonValue is TJSONObject)then
//     begin
//     L := FStorage.Pairs[i].JsonValue as TJSONObject;
//     for a := 0 to L.Count - 1 do
//       L.RemovePair(l.Pairs[a].JsonString.ToString).Free;
//     end;
//
//    FStorage.RemovePair(FStorage.Pairs[i].JsonString.ToString).Free;
//   end;
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
    // Tratamento de erro, se necessário
  end;
end;

function TLocalStorage4Pascal.GetBoolean(const Key: string;const Default: Boolean): Boolean;
begin
  Result := GetValue<Boolean>(key, Default);
end;

function TLocalStorage4Pascal.GetDouble(const Key: string;const Default: Double): Double;
begin
  Result := GetValue<Double>(key, Default);
end;

function TLocalStorage4Pascal.GetInt64(const key: string;const Default: Int64): Int64;
begin
  Result := GetValue<Int64>(key, Default);
end;

function TLocalStorage4Pascal.GetInteger(const Key: string;const Default: Integer): Integer;
begin
  Result := GetValue<integer>(key, Default);
end;

function TLocalStorage4Pascal.GetJSONArray(const Key: string;const Default: TJSONArray): TJSONArray;
begin
  Result := GetValue<TJSONArray>(key, Default).Clone as TJSONArray;

  Default.Free;
end;

function TLocalStorage4Pascal.GetJSONObject(const Key: string;const Default: TJSONObject): TJSONObject;
begin
  Result := GetValue<TJSONObject>(key, Default).Clone as TJSONObject;

  Default.Free;
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
var
  Pair: TJSONPair;
begin
  Pair := FStorage.RemovePair(Key);

  Result := Assigned(Pair);

  if Result then
    Pair.Free;  // Libera o par removido para evitar vazamento
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string;const Value: Integer);
begin
  SetValue<integer>(key, value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key, Value: string);
begin
  SetValue<string>(key, value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string;const Value: TJSONArray);
begin
  SetValue<TJSONArray>(key, value);

  Value.Free;
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string;const Value: TJSONObject);
begin
  SetValue<TJSONObject>(key, value);

  Value.Free;
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Double);
begin
  SetValue<double>(key, value);
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string;const Value: Boolean);
begin
  SetValue<Boolean>(key, value);
end;

end.

