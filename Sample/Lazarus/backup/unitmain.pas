unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    chBool: TCheckBox;
    Label1: TLabel;
    Shape1: TShape;
    procedure Button1Click(Sender: TObject);
  private
    procedure SalvarNoLocalStorage;
    procedure LerDoLocalStorage;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  SalvarNoLocalStorage;
end;

procedure TForm1.SalvarNoLocalStorage;
begin
  LocalStorage4Lazarus.SetValue('chBool', chBool.Checked);
  //LocalStorage4Lazarus.SetValue('EdString', EdString.Text);
  //LocalStorage4Lazarus.SetValue('EdFloat', EdFloat.Value);
end;

procedure TForm1.LerDoLocalStorage;
begin

end;

end.

