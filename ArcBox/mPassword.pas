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
    { Private �錾 }
  protected
    { Protected �錾 }
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public �錾 }
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
// �t�H�[��������

procedure TPasswordForm.FormCreate(Sender: TObject);
begin
  Font.Assign(ArcBoxForm.Font);
  OKButton.Enabled := False;
  EnableWindow(TWinControl(Owner).Handle, False);
end;

// -----------------------------------------------------------------------------
// �t�H�[���j����

procedure TPasswordForm.FormDestroy(Sender: TObject);
begin
  EnableWindow(TWinControl(Owner).Handle, True);
end;

// -----------------------------------------------------------------------------
// �t�H�[���J�n��

procedure TPasswordForm.FormShow(Sender: TObject);
begin
  //
end;

// -----------------------------------------------------------------------------
// �t�H�[���I����

procedure TPasswordForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

// -----------------------------------------------------------------------------
// �G�f�B�b�g�ύX��

procedure TPasswordForm.EditChange(Sender: TObject);
begin
  OKButton.Enabled := Edit.Text <> '';
end;

// -----------------------------------------------------------------------------
// �`�F�b�N�{�b�N�X�N���b�N��

procedure TPasswordForm.CheckBoxClick(Sender: TObject);
begin
  with Edit do
  begin
    if CheckBox.Checked then
      PasswordChar := #0
    else
      PasswordChar := '��';
  end;
end;

// -----------------------------------------------------------------------------
// �t�H�[���p�����[�^�쐬

procedure TPasswordForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := TWinControl(Owner).Handle;
end;

end.
