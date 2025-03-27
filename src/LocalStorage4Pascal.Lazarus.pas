unit LocalStorage4Pascal.Lazarus;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

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
    fSaveToFileDefaultValues: Boolean;
    function GetStoragePath(const FileName: string = 'localstorage.json'): string;
    procedure LoadFromFile;
    procedure SaveToFile;
  public
    constructor Create(const FileName: string = 'LocalStorage.json'; SaveToFileDefaultValues: Boolean = false);
    destructor Destroy; override;
    class function New(const FileName: string = 'LocalStorage.json'; SaveToFileDefaultValues: Boolean = false): iLocalStorage4Pascal;
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
  Classes,
  TypInfo,
  SysUtils,
  Rtti,
  LazUTF8;

{ TLocalStorage4Pascal }

function TLocalStorage4Pascal.Version: string;
begin
  Result := LocaStorage4Pascal_Version;
end;

function TLocalStorage4Pascal.GetStoragePath(const FileName: string): string;
begin
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + FileName;
end;

procedure TLocalStorage4Pascal.LoadFromFile;
var
  JSONStr: string;
  FileStream: TFileStream;
  StringStream: TStringStream;
  JSONData: TJSONData;
  JSONObj: TJSONObject;
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
      JSONData.Free;
  except
    on E: Exception do
      Writeln('Erro ao carregar o arquivo: ', E.Message);
  end;
end;

procedure TLocalStorage4Pascal.SaveToFile;
var
  JSONStr: string;
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  JSONStr := FStorage.FormatJSON;
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
      Writeln('Erro ao salvar o arquivo: ', E.Message);
  end;
end;

constructor TLocalStorage4Pascal.Create(const FileName: string; SaveToFileDefaultValues: Boolean);
begin
  FFilePath := GetStoragePath(FileName);
  FStorage := TJSONObject.Create;
  fSaveToFileDefaultValues := SaveToFileDefaultValues;
  LoadFromFile;
end;

destructor TLocalStorage4Pascal.Destroy;
begin
  SaveToFile;
  FreeAndNil(FStorage);
  inherited;
end;

class function TLocalStorage4Pascal.New(const FileName: string; SaveToFileDefaultValues: Boolean): iLocalStorage4Pascal;
begin
  Result := TLocalStorage4Pascal.Create(FileName, SaveToFileDefaultValues);
end;

function TLocalStorage4Pascal.Clear: Boolean;
var
  i: Integer;
begin
  Result := False;
  try
    for i := FStorage.Count - 1 downto 0 do
      FStorage.Delete(i);
    SaveToFile;
    Result := True;
  except
    on E: Exception do
      Writeln('Erro ao limpar o armazenamento: ', E.Message);
  end;
end;

function TLocalStorage4Pascal.GetBoolean(const Key: string; const Default: Boolean): Boolean;
var
  JsonData: TJSONData;
begin
  JsonData := FStorage.Find(Key);
  if JsonData <> nil then
    Result := JsonData.AsBoolean
  else
  begin
    if(fSaveToFileDefaultValues)then
     SetValue(Key, Default);
    Result := Default;
  end;
end;

function TLocalStorage4Pascal.GetDouble(const Key: string; const Default: Double): Double;
var
  JsonData: TJSONData;
begin
  JsonData := FStorage.Find(Key);
  if JsonData <> nil then
    Result := JsonData.AsFloat
  else
  begin
    if(fSaveToFileDefaultValues)then
     SetValue(Key, Default);
    Result := Default;
  end;
end;

function TLocalStorage4Pascal.GetInt64(const Key: string; const Default: Int64): Int64;
var
  JsonData: TJSONData;
begin
  JsonData := FStorage.Find(Key);
  if JsonData <> nil then
    Result := JsonData.AsInt64
  else
  begin
    if(fSaveToFileDefaultValues)then
     SetValue(Key, Default);
    Result := Default;
  end;
end;

function TLocalStorage4Pascal.GetInteger(const Key: string; const Default: Integer): Integer;
var
  JsonData: TJSONData;
begin
  JsonData := FStorage.Find(Key);
  if JsonData <> nil then
    Result := JsonData.AsInteger
  else
  begin
    if(fSaveToFileDefaultValues)then
     SetValue(Key, Default);
    Result := Default;
  end;
end;

function TLocalStorage4Pascal.GetJSONArray(const Key: string; const Default: TJSONArray): TJSONArray;
var
  JsonData: TJSONData;
begin
  JsonData := FStorage.Find(Key);
  if JsonData <> nil then
    Result := JsonData.Clone as TJSONArray
  else
  begin
    if(fSaveToFileDefaultValues)then
     SetValue(Key, Default);
    if Assigned(Default) then
      Result := Default.Clone as TJSONArray
    else
      Result := nil;
  end;
end;

function TLocalStorage4Pascal.GetJSONObject(const Key: string; const Default: TJSONObject): TJSONObject;
var
  JsonData: TJSONData;
begin
  JsonData := FStorage.Find(Key);
  if JsonData <> nil then
    Result := JsonData.Clone as TJSONObject
  else
  begin
    if(fSaveToFileDefaultValues)then
     SetValue(Key, Default);
    if Assigned(Default) then
      Result := Default.Clone as TJSONObject
    else
      Result := nil;
  end;
end;

function TLocalStorage4Pascal.GetString(const Key: string; const Default: string): string;
var
  JsonData: TJSONData;
begin
  JsonData := FStorage.Find(Key);
  if JsonData <> nil then
    Result := JsonData.AsString
  else
  begin
    if(fSaveToFileDefaultValues)then
     SetValue(Key, Default);
    Result := Default;
  end;
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
    SaveToFile;
  end;
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Integer);
begin
  FStorage.Delete(Key);

  FStorage.Add(Key, Value);

  SaveToFile;
end;

procedure TLocalStorage4Pascal.SetValue(const Key, Value: string);
begin
  FStorage.Delete(Key);

  FStorage.Add(Key, Value);

  SaveToFile;
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: TJSONArray);
begin
  FStorage.Delete(Key);

  FStorage.Add(Key, Value);

  SaveToFile;
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: TJSONObject);
begin
  FStorage.Delete(Key);

  FStorage.Add(Key, Value);

  SaveToFile;
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Double);
begin
  FStorage.Delete(Key);

  FStorage.Add(Key, Value);

  SaveToFile;
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Boolean);
begin
  FStorage.Delete(Key);

  FStorage.Add(Key, Value);

  SaveToFile;
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: Int64);
begin
  FStorage.Delete(Key);

  FStorage.Add(Key, Value);

  SaveToFile;
end;

{$ENDIF}

end.
