unit mPassword;

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls;
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;
{$IFEND}


type
  TPasswordForm = class(TForm)
    Edit: TEdit;
    CheckBox: TCheckBox;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EditChange(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
  private
    { Private 宣言 }
  protected
    { Protected 宣言 }
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public 宣言 }
  end;

function Password(AOwner: TComponent; var APassword: string): Boolean;

var
  PasswordForm: TPasswordForm;

implementation

uses
  mCommon, mMain;

{$R *.dfm}


function Password(AOwner: TComponent; var APassword: string): Boolean;
begin
  with TPasswordForm.Create(AOwner) do
    try
      Edit.Text := APassword;
      Result := ShowModal = mrOk;
      if Result then
        APassword := Edit.Text;
    finally
      Release;
    end;
end;

{ TPasswordForm }

// -----------------------------------------------------------------------------
// フォーム生成時

procedure TPasswordForm.FormCreate(Sender: TObject);
begin
  Font.Assign(ArcBoxForm.Font);
  OKButton.Enabled := False;
  EnableWindow(TWinControl(Owner).Handle, False);
end;

// -----------------------------------------------------------------------------
// フォーム破棄時

procedure TPasswordForm.FormDestroy(Sender: TObject);
begin
  EnableWindow(TWinControl(Owner).Handle, True);
end;

// -----------------------------------------------------------------------------
// フォーム開始時

procedure TPasswordForm.FormShow(Sender: TObject);
begin
  //
end;

// -----------------------------------------------------------------------------
// フォーム終了時

procedure TPasswordForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

// -----------------------------------------------------------------------------
// エディット変更時

procedure TPasswordForm.EditChange(Sender: TObject);
begin
  OKButton.Enabled := Edit.Text <> '';
end;

// -----------------------------------------------------------------------------
// チェックボックスクリック時

procedure TPasswordForm.CheckBoxClick(Sender: TObject);
begin
  with Edit do
  begin
    if CheckBox.Checked then
      PasswordChar := #0
    else
      PasswordChar := '●';
  end;
end;

// -----------------------------------------------------------------------------
// フォームパラメータ作成

procedure TPasswordForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := TWinControl(Owner).Handle;
end;

end.
