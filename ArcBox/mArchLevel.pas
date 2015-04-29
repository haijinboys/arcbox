unit mArchLevel;

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
  TArchLevelForm = class(TForm)
    TrackBar: TTrackBar;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TrackBarChange(Sender: TObject);
  private
    { Private 宣言 }
  protected
    { Protected 宣言 }
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public 宣言 }
  end;

function Level(AOwner: TComponent; var APos: NativeInt): Boolean;

var
  ArchLevelForm: TArchLevelForm;

implementation

uses
  mConsts, mCommon, mMain;

{$R *.dfm}


function Level(AOwner: TComponent; var APos: NativeInt): Boolean;
begin
  with TArchLevelForm.Create(AOwner) do
    try
      TrackBar.Position := APos;
      Result := ShowModal = mrOk;
      if Result then
        APos := TrackBar.Position;
    finally
      Release;
    end;
end;

{ TLevelForm }

// -----------------------------------------------------------------------------
// フォーム生成時

procedure TArchLevelForm.FormCreate(Sender: TObject);
begin
  Font.Assign(ArcBoxForm.Font);
end;

// -----------------------------------------------------------------------------
// フォーム破棄時

procedure TArchLevelForm.FormDestroy(Sender: TObject);
begin
  //
end;

// -----------------------------------------------------------------------------
// フォーム開始時

procedure TArchLevelForm.FormShow(Sender: TObject);
begin
  //
end;

// -----------------------------------------------------------------------------
// フォーム終了時

procedure TArchLevelForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  //
end;

// -----------------------------------------------------------------------------
// トラックバー変更時

procedure TArchLevelForm.TrackBarChange(Sender: TObject);
begin
  Caption := Format(SArchLevel, [TrackBar.Position]);
end;

// -----------------------------------------------------------------------------
// フォームパラメータ作成

procedure TArchLevelForm.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.WndParent := TWinControl(Owner).Handle;
end;

end.
