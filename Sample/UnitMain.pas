unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.EditBox, FMX.SpinBox, FMX.Edit, FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    SpinBox1: TSpinBox;
    Button1: TButton;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  LocalStorage4Pascal, System.JSON;

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
begin
  tlocalstorage4pascal.new.SetValue('Check', CheckBox1.IsChecked);
  TLocalStorage4Pascal.new.SetValue('String', edit1.text);
  TLocalStorage4Pascal.new.SetValue('Number', spinbox1.Value);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  TLocalStorage4Pascal.new(Application.Title + '.json').Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  Oi : tjsonobject;
begin
  SpinBox1.Value := TLocalStorage4Pascal.new(Application.Title + '.json').GetDouble('Number', 10.4);
  edit1.Text     := TLocalStorage4Pascal.new(Application.Title + '.json').GetString('String', 'Texto1');
  checkbox1.IsChecked := TLocalStorage4Pascal.new(Application.Title + '.json').GetBoolean('Check', true);
  Oi := TLocalStorage4Pascal.new(Application.Title + '.json').GetJSONObject('JS', TJSONObject.Create(TJSONPair.Create('Teste', 'eTasks')));
  Form1.Caption := Oi.Values['Teste'].ToString;

//   checkbox1.IsChecked := TLocalStorage4Delphi.new.GetValue('Check', true).AsBoolean;
//   Edit1.Text :=  TLocalStorage4Delphi.new.GetValue('String', 'Texto 1').AsString;
//   SpinBox1.Value :=  TLocalStorage4Delphi.new.GetValue('Number', 10).AsType<Double>;
//   Teste := TLocalStorage4Delphi.new.GetValue('Number', 10).AsType<Integer>;
end;

end.
