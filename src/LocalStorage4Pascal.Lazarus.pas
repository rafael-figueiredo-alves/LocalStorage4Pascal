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

  { TLocalStorage4Pascal }

  TLocalStorage4Pascal = class(TInterfacedObject, iLocalStorage4Pascal)
  private
    FStorage: TJSONObject;
    FFilePath: string;
    fSaveToFileDefaultValues: Boolean;
    function GetStoragePath(const FileName: string = 'localstorage.json'): string;
    procedure LoadFromFile;
    procedure SaveToFile;
    function ToBase64(const Input: string): string;
    function FromBase64(const Input: string): string;
    function EncryptString(const Input: string; const Key: string): string;
    function DecryptString(const Input: string; const Key: string): string;
    function DateTimeToISO8601(const ADateTime: TDateTime): string;
    function DateToISO8601(const ADate: TDate): string;
    function TimeToISO8601(const ATime: TTime): string;
    procedure SetTimeValue(const Key: string; const Value: TTime);
    procedure SetDateValue(const Key: string; const Value: TDate);
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
    procedure SetValue(const Key: string; const Value: string; const SecretKey: string); overload;
    procedure SetValue(const Key: string; const Value: TDateTime); overload;
    procedure SetValue(const Key: string; const Value: TDate); overload;
    procedure SetValue(const Key: string; const Value: TTime); overload;

    function GetString(const Key: string; const Default: string = ''): string;
    function GetInteger(const Key: string; const Default: Integer = 0): Integer;
    function GetBoolean(const Key: string; const Default: Boolean = False): Boolean;
    function GetDouble(const Key: string; const Default: Double = 0.0): Double;
    function GetJSONObject(const Key: string; const Default: TJSONObject = nil): TJSONObject;
    function GetJSONArray(const Key: string; const Default: TJSONArray = nil): TJSONArray;
    function GetInt64(const Key: string; const Default: Int64 = 0): Int64;
    function GetEncryptedString(const Key: string; const SecretKey: string): string;
    function GetDateTime(const Key: string; const Default: TDateTime): TDateTime;
    function GetDate(const Key: string; const Default: TDate): TDate;
    function GetTime(const Key: string; const Default: TTime): TTime;

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
  LazUTF8,
  base64;

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

function TLocalStorage4Pascal.ToBase64(const Input: string): string;
begin
  Result := EncodeStringBase64(Input);
end;

function TLocalStorage4Pascal.FromBase64(const Input: string): string;
begin
  Result := DecodeStringBase64(Input);
end;

function TLocalStorage4Pascal.EncryptString(const Input: string;const Key: string): string;
var
  EachChar: Integer;
  KeyPos: Integer;
begin
  Result := '';
  KeyPos := 1;

  for EachChar := 1 to Length(Input) do
  begin
    Result := Result + Char(Byte(Input[EachChar]) xor Byte(Key[KeyPos]));
    Inc(KeyPos);
    if KeyPos > Length(Key) then
      KeyPos := 1;
  end;
end;

function TLocalStorage4Pascal.DecryptString(const Input: string;const Key: string): string;
var
  EachChar : Integer;
  KeyPos   : Integer;
begin
  Result := '';
  KeyPos := 1;

  for EachChar := 1 to Length(Input) do
  begin
    Result := Result + Char(Byte(Input[EachChar]) xor Byte(Key[KeyPos]));
    Inc(KeyPos);
    if KeyPos > Length(Key) then
      KeyPos := 1;
  end;
end;

function TLocalStorage4Pascal.DateTimeToISO8601(const ADateTime: TDateTime): string;
begin
  Result := DateTimeToStr(ADateTime);
end;

function TLocalStorage4Pascal.DateToISO8601(const ADate: TDate): string;
begin
  Result := DateToStr(ADate);
end;

function TLocalStorage4Pascal.TimeToISO8601(const ATime: TTime): string;
begin
  Result := TimeToStr(ATime);
end;

procedure TLocalStorage4Pascal.SetTimeValue(const Key: string;const Value: TTime);
begin
  FStorage.Delete(Key);

  FStorage.Add(Key, TimeToISO8601(Value));

  SaveToFile;
end;

procedure TLocalStorage4Pascal.SetDateValue(const Key: string;const Value: TDate);
begin
  FStorage.Delete(Key);

  FStorage.Add(Key, DateToISO8601(Value));

  SaveToFile;
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

function TLocalStorage4Pascal.GetEncryptedString(const Key: string;const SecretKey: string): string;
var
  JsonData: TJSONData;
begin
  JsonData := FStorage.Find(Key);
  if JsonData <> nil then
    Result := JsonData.AsString
  else
   begin
    if(fSaveToFileDefaultValues)then
      SetValue(Key, '');
    Result := '';
    end;

  if(Result <> '')then
   begin
     Result := DecryptString(FromBase64(Result), SecretKey);
   end;
end;

function TLocalStorage4Pascal.GetDateTime(const Key: string;const Default: TDateTime): TDateTime;
var
  JsonData: TJSONData;
begin
  JsonData := FStorage.Find(Key);
  if JsonData <> nil then
    Result := StrToDateTime(JsonData.AsString)
  else
  begin
    if(fSaveToFileDefaultValues)then
     SetValue(Key, Default);
    Result := Default;
  end;
end;

function TLocalStorage4Pascal.GetDate(const Key: string; const Default: TDate): TDate;
var
  JsonData: TJSONData;
begin
  JsonData := FStorage.Find(Key);
  if JsonData <> nil then
    Result := StrToDate(JsonData.AsString)
  else
  begin
    if(fSaveToFileDefaultValues)then
     SetValue(Key, Default);
    Result := Default;
  end;
end;

function TLocalStorage4Pascal.GetTime(const Key: string; const Default: TTime): TTime;
var
  JsonData: TJSONData;
begin
  JsonData := FStorage.Find(Key);
  if JsonData <> nil then
    Result := StrToTime(JsonData.AsString)
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

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: string);
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

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: string;const SecretKey: string);
var
  EncryptedValue : string;
begin
  EncryptedValue := '';

  if(Value <> '')then
   begin
     EncryptedValue := ToBase64(EncryptString(Value, SecretKey));
   end;

  FStorage.Delete(Key);

  FStorage.Add(Key, EncryptedValue);

  SaveToFile;
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string;const Value: TDateTime);
begin
    // Precisamos de uma heurística adicional para diferenciar
    if (Value = Trunc(Value)) and (Frac(Value) = 0) then
      SetDateValue(Key, Value) // Apenas data (parte inteira)
    else if (Trunc(Value) = 0) and (Frac(Value) > 0) then
      SetTimeValue(Key, Value) // Apenas hora (parte fracionária)
    else
      begin
        FStorage.Delete(Key);

        FStorage.Add(Key, DateTimeToISO8601(Value));

        SaveToFile;
      end;

end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: TDate);
begin
  FStorage.Delete(Key);

  FStorage.Add(Key, DateToISO8601(Value));

  SaveToFile;
end;

procedure TLocalStorage4Pascal.SetValue(const Key: string; const Value: TTime);
begin
  FStorage.Delete(Key);

  FStorage.Add(Key, TimeToISO8601(Value));

  SaveToFile;
end;

{$ENDIF}

end.
