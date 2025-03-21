unit UnitSample;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, FMX.Effects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit,
  FMX.EditBox, FMX.SpinBox;

type
  TFormMain = class(TForm)
    LayAppBar: TLayout;
    AppBar: TRectangle;
    SombraAppBAr: TShadowEffect;
    lblTitle: TLabel;
    LayMain: TLayout;
    Layout1: TLayout;
    Label1: TLabel;
    Layout2: TLayout;
    Label2: TLabel;
    Layout3: TLayout;
    Label3: TLabel;
    Layout4: TLayout;
    Label4: TLabel;
    Layout5: TLayout;
    Label5: TLabel;
    Layout6: TLayout;
    Label6: TLabel;
    chBool: TCheckBox;
    EdString: TEdit;
    EdJSNome: TEdit;
    EdJSApelido: TEdit;
    EdFloat: TSpinBox;
    Layout7: TLayout;
    btSalvar: TButton;
    Layout8: TLayout;
    btLimpar: TButton;
    Layout9: TLayout;
    LibVersion: TLabel;
    Layout10: TLayout;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Layout11: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure btSalvarClick(Sender: TObject);
    procedure btLimparClick(Sender: TObject);
  private
    { Private declarations }
    procedure LerDadosDoLocaStorage;
    procedure GravarDadosDoLocalStorage;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

Uses LocalStorage4Pascal, System.JSON;

{ TFormMain }

procedure TFormMain.btLimparClick(Sender: TObject);
begin
  if(LocalStorage4Delphi.Clear)then
   LerDadosDoLocaStorage;
end;

procedure TFormMain.btSalvarClick(Sender: TObject);
begin
  GravarDadosDoLocalStorage;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  LibVersion.Text := 'LocalStorage4Pascal - version ' + LocalStorage4Delphi.Version;

  LerDadosDoLocaStorage;
end;

procedure TFormMain.GravarDadosDoLocalStorage;
var
  ClasseJS : TJSONObject;
  ArrayJS  : TJSONArray;
begin
  LocalStorage4Delphi.SetValue('chBool', chBool.IsChecked);
  LocalStorage4Delphi.SetValue('EdString', EdString.Text);
  LocalStorage4Delphi.SetValue('EdFloat', EdFloat.Value);

  ClasseJS := TJSONObject.Create;
  ClasseJS.AddPair('Nome', EdJSNome.Text);
  ClasseJS.AddPair('Apelido', EdJSApelido.Text);

  LocalStorage4Delphi.SetValue('JSObj', ClasseJS);

  ArrayJS := TJSONArray.Create;
  ArrayJS.Add(TJSONObject.Create(TJSONPair.Create('Num', Edit1.Text)));
  ArrayJS.Add(TJSONObject.Create(TJSONPair.Create('Num', Edit2.Text)));
  ArrayJS.Add(TJSONObject.Create(TJSONPair.Create('Num', Edit3.Text)));

  LocalStorage4Delphi.SetValue('JSArray', ArrayJS);

  LerDadosDoLocaStorage;
end;

procedure TFormMain.LerDadosDoLocaStorage;
var
  ClasseJS : TJSONObject;
  ArrayJS  : TJSONArray;
  item     : integer;
begin
  chBool.IsChecked := LocalStorage4Delphi.GetBoolean('chBool', false);
  EdString.Text    := LocalStorage4Delphi.GetString('EdString', '');
  EdFloat.Value    := LocalStorage4Delphi.GetDouble('EdFloat', 3.14);

  classeJS := LocalStorage4Delphi.GetJSONObject('JSObj', TJSONObject.Create(TJSONPair.Create('Nome', 'Rafael')).AddPair('Apelido', 'DevPegasus'));

  EdJSNome.Text := classeJS.Values['Nome'].Value;
  EdJSApelido.Text := ClasseJS.Values['Apelido'].Value;

  ArrayJS := TJSONArray.Create;
  ArrayJS.add(TJSONObject.Create(TJSONPair.Create('Num', 'Oi')));
  ArrayJS.add(TJSONObject.Create(TJSONPair.Create('Num', 'Hi')));
  ArrayJS.add(TJSONObject.Create(TJSONPair.Create('Num', 'Bye')));

  ArrayJS := LocalStorage4Delphi.GetJSONArray('JSArray', ArrayJS);

  if(ArrayJS.Count > 0)then
   begin
     for Item := 0 to ArrayJS.Count - 1 do
        begin
          if(item = 0)then
           begin
            Label7.Text := TJSONObject(ArrayJS.Items[Item]).Values['Num'].Value;
            Edit1.Text :=  TJSONObject(ArrayJS.Items[Item]).Values['Num'].Value;
           end;
          if(Item = 1)then
           begin
            Label7.Text := Label7.Text + ' - ' + TJSONObject(ArrayJS.Items[Item]).Values['Num'].Value;
            Edit2.Text :=  TJSONObject(ArrayJS.Items[Item]).Values['Num'].Value;
           end;
          if(Item = 2)then
           begin
            Label7.Text := Label7.Text + ' - ' + TJSONObject(ArrayJS.Items[Item]).Values['Num'].Value;
            Edit3.Text :=  TJSONObject(ArrayJS.Items[Item]).Values['Num'].Value;
           end;
        end;
   end;

  ClasseJS.Free;
  ArrayJS.Free;
end;

end.
