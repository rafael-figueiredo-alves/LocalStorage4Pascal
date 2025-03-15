unit Unit1;

interface

uses
  System.SysUtils,
  System.Classes,
  System.IOUtils,
  System.JSON,
  System.Generics.Collections;

type

  TLocalStorage4Delphi = class
  private
    class var FInstance: TLocalStorage4Delphi;
    FStorage: TDictionary<string, TJSONValue>;
    FFilePath: string;
    procedure LoadFromFile;
    procedure SaveToFile;
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TLocalStorage4Delphi;
    procedure SetValue<T>(const Key: string; const Value: T);
    function GetValue<T>(const Key: string; const Default: T): T;
    procedure RemoveValue(const Key: string);
    procedure Clear;
  end;

implementation

{ TLocalStorage }

constructor TLocalStorage4Delphi.Create;
begin
  inherited Create;
  FStorage := TDictionary<string, TJSONValue>.Create;
  FFilePath := TPath.Combine(TPath.GetDocumentsPath, 'localstorage.json');
  LoadFromFile;
end;

destructor TLocalStorage4Delphi.Destroy;
begin
  SaveToFile;
  FStorage.Free;
  inherited;
end;

class function TLocalStorage4Delphi.Instance: TLocalStorage4Delphi;
begin
  if not Assigned(FInstance) then
    FInstance := TLocalStorage4Delphi.Create;
  Result := FInstance;
end;

procedure TLocalStorage4Delphi.LoadFromFile;
var
  JSONText: string;
  JSONObject: TJSONObject;
  Pair: TJSONPair;
begin
  if not FileExists(FFilePath) then Exit;
  JSONText := TFile.ReadAllText(FFilePath, TEncoding.UTF8);
  JSONObject := TJSONObject.ParseJSONValue(JSONText) as TJSONObject;
  try
    if Assigned(JSONObject) then
    begin
      for Pair in JSONObject do
        FStorage.AddOrSetValue(Pair.JsonString.Value, Pair.JsonValue.Clone as TJSONValue);
    end;
  finally
    JSONObject.Free;
  end;
end;

procedure TLocalStorage4Delphi.SaveToFile;
var
  JSONObject: TJSONObject;
  Pair: TPair<string, TJSONValue>;
begin
  JSONObject := TJSONObject.Create;
  try
    for Pair in FStorage do
      JSONObject.AddPair(Pair.Key, Pair.Value.Clone as TJSONValue);
    TFile.WriteAllText(FFilePath, JSONObject.ToJSON, TEncoding.UTF8);
  finally
    JSONObject.Free;
  end;
end;

procedure TLocalStorage4Delphi.SetValue<T>(const Key: string; const Value: T);
var
  JSONValue: TJSONValue;
begin
  JSONValue := TJSONValue.From<T>(Value);
  FStorage.AddOrSetValue(Key, JSONValue);
  SaveToFile;
end;

function TLocalStorage4Delphi.GetValue<T>(const Key: string; const Default: T): T;
var
  JSONValue: TJSONValue;
begin
  if FStorage.TryGetValue(Key, JSONValue) then
    Result := JSONValue.AsType<T>
  else
    Result := Default;
end;

procedure TLocalStorage4Delphi.RemoveValue(const Key: string);
begin
  FStorage.Remove(Key);
  SaveToFile;
end;

procedure TLocalStorage4Delphi.Clear;
begin
  FStorage.Clear;
  SaveToFile;
end;

end.

