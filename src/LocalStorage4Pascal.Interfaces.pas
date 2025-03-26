unit LocalStorage4Pascal.Interfaces;

interface

uses
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

const
  LocaStorage4Pascal_Version = '1.0.0';

implementation

end.
