unit mProgress;

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ComCtrls;
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;
{$IFEND}


type
  TProgressForm = class(TForm)
    ProcessingLabel: TLabel;
    ProgressBar: TProgressBar;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private �錾 }
  protected
    { Protected �錾 }
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public �錾 }
    Cancel: Boolean;
    procedure Progress(const S: string; APos, AMax: Int64);
  end;

var
  ProgressForm: TProgressForm;

implementation

uses
  mCommon, mMain;

{$R *.dfm}

{ TProgressForm }

// -----------------------------------------------------------------------------
// �t�H�[��������

procedure TProgressForm.FormCreate(Sender: TObject);
begin
  Font.Assign(ArcBoxForm.Font);
  Cancel := False;
  EnableWindow(TWinControl(Owner).Handle, False);
end;

// -----------------------------------------------------------------------------
// �t�H�[���j����

procedure TProgressForm.FormDestroy(Sender: TObject);
begin
  EnableWindow(TWinControl(Owner).Handle, True);
end;

// -----------------------------------------------------------------------------
// �t�H�[���J�n��

procedure TProgressForm.FormShow(Sender: TObject);
begin
  //
end;

// -----------------------------------------------------------------------------
// �t�H�[���I����

procedure TProgressForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Cancel := True;
end;

// -----------------------------------------------------------------------------
// �L�����Z���N���b�N��

procedure TProgressForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------
// �t�H�[���p�����[�^�쐬

procedure TProgressForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := TWinControl(Owner).Handle;
end;

// -----------------------------------------------------------------------------
// �i���ݒ�

procedure TProgressForm.Progress(const S: string; APos, AMax: Int64);
begin
  ProcessingLabel.Caption := S;
  with ProgressBar do
  begin
    Max := AMax;
    Position := APos;
  end;
  Application.ProcessMessages;
end;

end.
