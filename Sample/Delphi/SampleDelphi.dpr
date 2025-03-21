program SampleDelphi;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitSample in 'UnitSample.pas' {FormMain},
  LocalStorage4Pascal in '..\..\src\LocalStorage4Pascal.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;
  InitLocalStorage4Pascal('SampleDelphi.json');
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
