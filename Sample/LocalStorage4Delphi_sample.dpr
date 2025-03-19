program LocalStorage4Delphi_sample;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMain in 'UnitMain.pas' {Form1},
  LocalStorage4Pascal in '..\src\LocalStorage4Pascal.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
