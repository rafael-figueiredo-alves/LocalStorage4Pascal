unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtSalvar: TButton;
    BtLimpar: TButton;
    Button1: TButton;
    chBool: TCheckBox;
    EdNum1: TEdit;
    EdNum2: TEdit;
    EdNum3: TEdit;
    EdString: TEdit;
    EdFloat: TFloatSpinEdit;
    EdNome: TEdit;
    EdApelido: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    displayARRAY: TLabel;
    LblVersao: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Shape1: TShape;
    procedure BtLimparClick(Sender: TObject);
    procedure BtSalvarClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure GravarDadosDoLocalStorage;
    procedure LerDadosDoLocaStorage;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  LocalStorage4Pascal, fpJson;

{ TForm1 }

procedure TForm1.BtSalvarClick(Sender: TObject);
begin
  GravarDadosDoLocalStorage;
end;

procedure TForm1.Button1Click(Sender: TObject);
const
  SecretKey: string = '123456789';
var
  encString: string;
  DataHoraRec  : TDateTime;
  HoraRec      : ttime;
  DataRec      : TDate;
begin
  encString := 'Ola, mundo do Lazarus';

  ShowMessage(encString);
  ShowMessage(DateTimeToStr(now));
  ShowMessage(DateToStr(now));
  ShowMessage(TimeToStr(now));

  LocalStorage4Lazarus.SetValue('EncString', encString, SecretKey);
  LocalStorage4Lazarus.SetValue('DataHora', now);
  LocalStorage4Lazarus.SetValue('Data', StrToDate('23/01/2021'));
  LocalStorage4Lazarus.SetValue('Hora', StrToTime('11:28:45'));

  encString := LocalStorage4Lazarus.GetEncryptedString('EncString', SecretKey);
  DataHoraRec:= LocalStorage4Lazarus.GetDateTime('DataHora', now);
  DataRec:= LocalStorage4Lazarus.GetDate('Data', StrToDate('29/02/2024'));
  HoraRec:= LocalStorage4Lazarus.GetTime('Hora', StrToTime('19:38:43'));

  ShowMessage(encString);
  ShowMessage(DateTimeToStr(DataHoraRec));
  ShowMessage(DateToStr(DataRec));
  ShowMessage(TimeToStr(HoraRec));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LblVersao.Caption := 'LocalStorage4Pascal - version ' + LocalStorage4Lazarus.Version;

  LerDadosDoLocaStorage;
end;

procedure TForm1.BtLimparClick(Sender: TObject);
begin
  if(LocalStorage4Lazarus.Clear)then
   LerDadosDoLocaStorage;
end;

procedure TForm1.GravarDadosDoLocalStorage;
var
  ClasseJS : TJSONObject;
  ArrayJS  : TJSONArray;
  Obj: TJSONObject;
begin
  LocalStorage4Lazarus.SetValue('chBool', chBool.Checked);
  LocalStorage4Lazarus.SetValue('EdString', EdString.Text);
  LocalStorage4Lazarus.SetValue('EdFloat', EdFloat.Value);

  ClasseJS := TJSONObject.Create;
  ClasseJS.Add('Nome', EdNome.Text);
  ClasseJS.Add('Apelido', EdApelido.Text);

  LocalStorage4Lazarus.SetValue('JSObj', ClasseJS);

  ArrayJS := TJSONArray.Create;
  Obj := TJSONObject.Create;
  Obj.Add('Num', EdNum1.Text);
  ArrayJS.Add(Obj);

  Obj := TJSONObject.Create;
  Obj.Add('Num', EdNum2.Text);
  ArrayJS.Add(Obj);

  Obj := TJSONObject.Create;
  Obj.Add('Num', EdNum3.Text);
  ArrayJS.Add(Obj);

  LocalStorage4Lazarus.SetValue('JSArray', ArrayJS);

  LerDadosDoLocaStorage;
end;

procedure TForm1.LerDadosDoLocaStorage;
var
  ClasseJS : TJSONObject;
  ArrayJS  : TJSONArray;
  item     : integer;
  JSONObj  : TJSONObject;
  Obj: TJSONObject;
begin
  chBool.Checked := LocalStorage4Lazarus.GetBoolean('chBool', false);
  EdString.Text  := LocalStorage4Lazarus.GetString('EdString', '');
  EdFloat.Value  := LocalStorage4Lazarus.GetDouble('EdFloat', 3.14);

  JSONObj := TJSONObject.create;
  JSONObj.add('Nome', 'Rafael');
  JSONObj.add('Apelido', 'DevPegasus');

  classeJS := LocalStorage4Lazarus.GetJSONObject('JSObj', JSONObj);

  EdNome.Text := classeJS.Strings['Nome'];
  EdApelido.Text := ClasseJS.Strings['Apelido'];

  ArrayJS := TJSONArray.Create;
  Obj := TJSONObject.Create;
  Obj.Add('Num', 'Oi');
  ArrayJS.Add(Obj);

  Obj := TJSONObject.Create;
  Obj.Add('Num', 'Hi');
  ArrayJS.Add(Obj);

  Obj := TJSONObject.Create;
  Obj.Add('Num', 'Bye');
  ArrayJS.Add(Obj);

  ArrayJS := LocalStorage4Lazarus.GetJSONArray('JSArray', ArrayJS);

  if(ArrayJS.Count > 0)then
   begin
     for Item := 0 to ArrayJS.Count - 1 do
        begin
          if(item = 0)then
           begin
            displayARRAY.Caption := TJSONObject(ArrayJS.Items[Item]).Strings['Num'];
            EdNum1.Text :=  TJSONObject(ArrayJS.Items[Item]).Strings['Num'];
           end;
          if(Item = 1)then
           begin
            displayARRAY.Caption := displayARRAY.Caption + ' - ' + TJSONObject(ArrayJS.Items[Item]).Strings['Num'];
            EdNum2.Text :=  TJSONObject(ArrayJS.Items[Item]).Strings['Num'];
           end;
          if(Item = 2)then
           begin
            displayARRAY.Caption := displayARRAY.Caption + ' - ' + TJSONObject(ArrayJS.Items[Item]).Strings['Num'];
            EdNum3.Text :=  TJSONObject(ArrayJS.Items[Item]).Strings['Num'];
           end;
        end;
   end;

  ClasseJS.Free;
  ArrayJS.Free;
end;

end.

