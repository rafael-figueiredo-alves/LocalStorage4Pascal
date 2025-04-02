unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, DateTimePicker;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtSalvar: TButton;
    BtLimpar: TButton;
    chBool: TCheckBox;
    edDateTime: TDateTimePicker;
    edTime: TDateTimePicker;
    edDate: TDateTimePicker;
    EdNum1: TEdit;
    EdNum2: TEdit;
    EdNum3: TEdit;
    EdString: TEdit;
    EdFloat: TFloatSpinEdit;
    EdNome: TEdit;
    EdApelido: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    displayARRAY: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    LblVersao: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Shape1: TShape;
    procedure BtLimparClick(Sender: TObject);
    procedure BtSalvarClick(Sender: TObject);
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
  LocalStorage4Lazarus.SetValue('DataHora', edDateTime.DateTime);
  LocalStorage4Lazarus.SetValue('DataOnly', edDate.Date);
  LocalStorage4Lazarus.SetValue('TimeOnly', edTime.Time);

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

  edDateTime.DateTime:= LocalStorage4Lazarus.GetDateTime('DataHora', now);
  edTime.Time:= LocalStorage4Lazarus.GetTime('TimeOnly', Time);
  edDate.Date:= LocalStorage4Lazarus.GetDate('DataOnly', Date);

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

