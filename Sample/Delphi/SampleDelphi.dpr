program SampleDelphi;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitSample in 'UnitSample.pas' {FormMain},
  LocalStorage4Pascal in '..\..\src\LocalStorage4Pascal.pas',
  LocalStorage4Pascal.Delphi in '..\..\src\LocalStorage4Pascal.Delphi.pas',
  LocalStorage4Pascal.Lazarus in '..\..\src\LocalStorage4Pascal.Lazarus.pas',
  LocalStorage4Pascal.Interfaces in '..\..\src\LocalStorage4Pascal.Interfaces.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  TLocalStorage.InitLocalStorage4Pascal('SampleDelphi.json');
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
