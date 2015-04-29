unit mAbout;

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls;
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;
{$IFEND}


type
  TAboutForm = class(TForm)
    Image: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LinkLabel: TLinkLabel;
    OKButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure LinkLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
  private
    { Private 宣言 }
  protected
    { Protected 宣言 }
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public 宣言 }
  end;

function About(AOwner: TComponent): Boolean;

var
  AboutForm: TAboutForm;

implementation

uses
{$IF CompilerVersion > 22.9}
  System.Types, Winapi.ShellAPI,
{$ELSE}
  Types, ShellAPI,
{$IFEND}
  mConsts, mCommon, mMain;

{$R *.dfm}

// -----------------------------------------------------------------------------
// バージョン情報

function About(AOwner: TComponent): Boolean;
begin
  with TAboutForm.Create(AOwner) do
    try
      Result := ShowModal = mrOk;
    finally
      Release;
    end;
end;

{ TAboutForm }

// -----------------------------------------------------------------------------
// フォーム生成時

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Font.Assign(ArcBoxForm.Font);
  Image.Picture.Icon.Handle := GetIcon(128);
  Label1.Caption := SName;
  Label2.Caption := Format('Build: %s', [GetFileVersion]);
end;

// -----------------------------------------------------------------------------
// フォーム破棄時

procedure TAboutForm.FormDestroy(Sender: TObject);
begin
  //
end;

// -----------------------------------------------------------------------------
// フォーム開始時

procedure TAboutForm.FormShow(Sender: TObject);
begin
  //
end;

// -----------------------------------------------------------------------------
// フォーム終了時

procedure TAboutForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

// -----------------------------------------------------------------------------
// リンククリック時

procedure TAboutForm.LinkLabelLinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  ShellExecute(0, PChar('OPEN'), PChar(Link), '', '', SW_SHOW);
end;

// -----------------------------------------------------------------------------
// フォームパラメータ作成

procedure TAboutForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := TWinControl(Owner).Handle;
end;

end.
