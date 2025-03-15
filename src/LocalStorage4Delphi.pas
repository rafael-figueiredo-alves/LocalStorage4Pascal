unit LocalStorage4Delphi;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections, System.Rtti;

type

  iLocalStorage4Delphi = interface
    ['{076E3E57-880E-4744-B3B4-2514FC4F79C2}']
    function SetValue(const Key: string; const Value: TValue): iLocalStorage4Delphi;
    function GetValue(const Key: string; const Deafult: TValue): TValue;
    function RemoveValue(const Key: string): Boolean;
    function KeyExists(const Key: string): Boolean;
    function Clear: Boolean;
  end;

  TLocalStorage4Delphi = class(TInterfacedObject, iLocalStorage4Delphi)
  private
    class var FInstance: iLocalStorage4Delphi;
    FStorage: TJsonObject;
    class var FFilePath: string;
    procedure LoadFromFile;
    procedure SaveToFile;
    function TValueToJSON(const AValue: TValue): TJSONValue;
    function JSONToTValue(const AJsonValue: TJSONValue): TValue;
  public
    constructor Create(const FileName: string = 'LocalStorage.json');
    destructor Destroy; override;
    class function New(const FileName: string = 'LocalStorage.json'): iLocalStorage4Delphi;
    function SetValue(const Key: string; const Value: TValue): iLocalStorage4Delphi;
    function GetValue(const Key: string; const Default: TValue): TValue;
    function RemoveValue(const Key: string): Boolean;
    function KeyExists(const Key: string): Boolean;
    function Clear: Boolean;
  end;

implementation

function TLocalStorage4Delphi.Clear: Boolean;
var
  Pair : TJSONPair;
  ReturnBool : Boolean;
begin
  for pair in FStorage do
    ReturnBool := RemoveValue(pair.Value);

  Result := ReturnBool;
end;

{ TLocalStorage }

constructor TLocalStorage4Delphi.Create(const FileName: string);
begin
  inherited Create;
  FStorage := TJsonObject.Create;
  if FFilePath.IsEmpty then
   FFilePath := TPath.Combine(TPath.GetDocumentsPath, FileName);
  LoadFromFile;
end;

destructor TLocalStorage4Delphi.Destroy;
begin
  FStorage.Free;
  inherited;
end;

function TLocalStorage4Delphi.GetValue(const Key: string; const Default: TValue): TValue;
var
 JSONValue : TJSONValue;
begin
  JSONValue := FStorage.GetValue(Key);
  if(Assigned(JSONValue))then
   Result := JSONToTValue(JSONValue)
  else
   begin
     SetValue(key, Default);
     Result := Default;
   end;

  SaveToFile;
end;

function TLocalStorage4Delphi.JSONToTValue(const AJsonValue: TJSONValue): TValue;
begin
  if Assigned(AJsonValue) then
  begin
    if AJsonValue is TJSONNumber then
    begin
      if Pos('.', AJsonValue.Value) > 0 then
        Result := TValue.From<TValue>(TJSONNumber(AJsonValue).AsDouble)
      else
        Result := TValue.From<TValue>(TJSONNumber(AJsonValue).AsInt64);
    end
    else if AJsonValue is TJSONString then
      Result := TValue.From<string>(TJSONString(AJsonValue).Value)
    else if AJsonValue is TJSONBool then
      Result := TValue.From<Boolean>(TJSONBool(AJsonValue).AsBoolean)
    else
      raise Exception.CreateFmt('Unsupported type', []);
  end;
end;

function TLocalStorage4Delphi.KeyExists(const Key: string): Boolean;
begin
  Result := FStorage.GetValue(Key) <> nil;
end;

procedure TLocalStorage4Delphi.LoadFromFile;
var
  LFileStream: TFileStream;
  LStringReader: TStringStream;
begin
  if not FileExists(FFilePath) then Exit;

  LStringReader := TStringStream.Create('', TEncoding.UTF8);
  try
    LFileStream := TFileStream.Create(FFilePath, fmOpenRead);
    try
      LStringReader.LoadFromStream(LFileStream);
      FStorage := TJSONObject.ParseJSONValue(LStringReader.DataString) as TJSONObject;
    finally
      LFileStream.Free;
    end;
  finally
    LStringReader.Free;
  end;
end;

class function TLocalStorage4Delphi.New(const FileName: string): iLocalStorage4Delphi;
begin
  if not Assigned(FInstance) then
    FInstance := TLocalStorage4Delphi.Create(FileName);
  Result := FInstance;
end;

function TLocalStorage4Delphi.RemoveValue(const Key: string): Boolean;
begin
  Result := Assigned(FStorage.RemovePair(Key));

  SaveToFile;
end;

procedure TLocalStorage4Delphi.SaveToFile;
var
  LFileStream: TFileStream;
  LStringWriter: TStringStream;
begin
  LStringWriter := TStringStream.Create(FStorage.ToJSON, TEncoding.UTF8);
  try
    LFileStream := TFileStream.Create(FFilePath, fmCreate);
    try
      LStringWriter.SaveToStream(LFileStream);
    finally
      LFileStream.Free;
    end;
  finally
    LStringWriter.Free;
  end;
end;

function TLocalStorage4Delphi.SetValue(const Key: string; const Value: TValue): iLocalStorage4Delphi;
begin
  if Value.IsEmpty then
   Exit;

   FStorage.AddPair(key, TValueToJSON(Value));

   SaveToFile;
end;

function TLocalStorage4Delphi.TValueToJSON(const AValue: TValue): TJSONValue;
begin
  if AValue.IsEmpty then
    Exit;

  case AValue.Kind of
    tkInteger, tkInt64:
      Result := TJSONNumber.Create(AValue.AsInteger);
    tkFloat:
      Result := TJSONNumber.Create(AValue.AsExtended);
    tkUString:
      Result := TJSONString.Create(AValue.AsString);
    tkEnumeration:
      if AValue.TypeInfo = TypeInfo(Boolean) then
        Result := TJSONBool.Create(AValue.AsBoolean);
    else
      raise Exception.CreateFmt('Unsupported type', []);
  end;
end;

end.

