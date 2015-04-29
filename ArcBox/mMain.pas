unit mMain;

{$WARN UNIT_PLATFORM OFF}

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Forms, Vcl.Controls, Vcl.Dialogs, Vcl.Menus,
  Vcl.StdCtrls,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Forms, Controls,
  Dialogs, Menus, StdCtrls,
{$IFEND}
  DragDrop, MeryCtrls, mArchive, mProgress;

const
  WM_ARCHIVE = (WM_USER + $4098);

type
  TArcBoxForm = class(TForm)
    PopupMenu: TPopupMenu;
    HelpAboutMenuItem: TMenuItem;
    DragDropTarget: TDragDropTarget;
    ArchDirGroupBox: TGroupBox;
    ArchDirComboBox: TAutoComboBox;
    ArchDirButton: TButton;
    UseArchDirCheckBox: TCheckBox;
    ArchFormatGroupBox: TGroupBox;
    ArchFormatZipRadioButton: TRadioButton;
    ArchFormat7zRadioButton: TRadioButton;
    ArchFormatTarRadioButton: TRadioButton;
    ArchFormatTgzRadioButton: TRadioButton;
    ArchFormatTbz2RadioButton: TRadioButton;
    ArchFormatTxzRadioButton: TRadioButton;
    ArchFormatWimRadioButton: TRadioButton;
    ArchLevelButton: TButton;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure HelpAboutMenuItemClick(Sender: TObject);
    procedure DragDropTargetDragDropFiles(Sender: TWinControl; X, Y: Integer;
      Files: TStrings);
    procedure ArchDirButtonClick(Sender: TObject);
    procedure UseArchDirCheckBoxClick(Sender: TObject);
    procedure ArchFormatZipRadioButtonClick(Sender: TObject);
    procedure ArchFormat7zRadioButtonClick(Sender: TObject);
    procedure ArchFormatTarRadioButtonClick(Sender: TObject);
    procedure ArchFormatTgzRadioButtonClick(Sender: TObject);
    procedure ArchFormatTbz2RadioButtonClick(Sender: TObject);
    procedure ArchFormatTxzRadioButtonClick(Sender: TObject);
    procedure ArchFormatWimRadioButtonClick(Sender: TObject);
    procedure ArchLevelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  private
    { Private 宣言 }
    FPoint: TPoint;
    FFiles: TStrings;
    FDirText: string;
    FDirList: TStrings;
    FUseArchDir: Boolean;
    FArchFormat: TArcFormat;
    FArchLevel: NativeInt;
    FActive: Boolean;
    FCancel: Boolean;
    FFileName: string;
    FProgress: TProgressForm;
    procedure ReadIni;
    procedure WriteIni;
    procedure ExecArchive;
    procedure OpenArchive(const AFileName: string);
    procedure SaveArchive(const AFileName: string);
    procedure ConvArchive(const AFileName: string);
    procedure SaveToFile(const APath, AFileName: string);
    procedure ArchivePassword(Sender: TObject; var APassword: string;
      var Abort: Boolean);
    procedure ArchiveProgress(Sender: TObject; APos, AMax: Int64;
      var Abort: Boolean);
    procedure UpdateFont;
    procedure WMCopyData(var Message: TWMCopyData); message WM_COPYDATA;
    procedure WMArchive(var Message: TMessage); message WM_ARCHIVE;
  public
    { Public 宣言 }
  end;

var
  ArcBoxForm: TArcBoxForm;

implementation

uses
{$IF CompilerVersion > 22.9}
  System.Types, System.IniFiles, System.IOUtils, Vcl.FileCtrl,
{$ELSE}
  Types, IniFiles, IOUtils, FileCtrl,
{$IFEND}
  mConsts, mCommon, mArchLevel, mPassword, mAbout;

{$R *.dfm}

{ TArcBoxForm }

// -----------------------------------------------------------------------------
// フォーム生成時

procedure TArcBoxForm.FormCreate(Sender: TObject);
var
  I: NativeInt;
begin
  UpdateFont;
  Caption := SName;
  FPoint := Point(Left, Top);
  FIniFailed := False;
  FMonitorCount := 1;
  FFiles := TStringList.Create;
  FDirText := '';
  FDirList := TStringList.Create;
  FUseArchDir := True;
  FArchFormat := afZip;
  FArchLevel := 5;
  FCancel := False;
  FActive := False;
  FFileName := '';
  ReadIni;
  Left := FPoint.X;
  Top := FPoint.Y;
  AssignList(ArchDirComboBox, FDirList);
  ArchDirComboBox.Text := FDirText;
  UseArchDirCheckBox.Checked := FUseArchDir;
  case FArchFormat of
    afZip:
      ArchFormatZipRadioButton.Checked := True;
    af7z:
      ArchFormat7zRadioButton.Checked := True;
    afTar:
      ArchFormatTarRadioButton.Checked := True;
    afTgz:
      ArchFormatTgzRadioButton.Checked := True;
    afTbz2:
      ArchFormatTbz2RadioButton.Checked := True;
    afTxz:
      ArchFormatTxzRadioButton.Checked := True;
    afWim:
      ArchFormatWimRadioButton.Checked := True;
  end;
  if ParamCount > 0 then
  begin
    Application.ShowMainForm := False;
    with TStringList.Create do
      try
        for I := 1 to ParamCount do
          Add(ParamStr(I));
        while Count > 0 do
        begin
          FFiles.Add(Strings[0]);
          Delete(0);
        end;
      finally
        Free;
      end;
    ExecArchive;
    PostQuitMessage(0);
  end;
end;

// -----------------------------------------------------------------------------
// フォーム破棄時

procedure TArcBoxForm.FormDestroy(Sender: TObject);
begin
  FPoint := Point(Left, Top);
  if Assigned(FDirList) then
    FreeAndNil(FDirList);
  if Assigned(FFiles) then
    FreeAndNil(FFiles);
end;

// -----------------------------------------------------------------------------
// フォーム開始時

procedure TArcBoxForm.FormShow(Sender: TObject);
begin
  //
end;

// -----------------------------------------------------------------------------
// フォーム終了時

procedure TArcBoxForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FDirText := ArchDirComboBox.Text;
  MergeList(FDirList, FDirText);
end;

// -----------------------------------------------------------------------------
// バージョン情報

procedure TArcBoxForm.HelpAboutMenuItemClick(Sender: TObject);
begin
  About(Self);
end;

// -----------------------------------------------------------------------------
// ファイルドラッグドロップ時

procedure TArcBoxForm.DragDropTargetDragDropFiles(Sender: TWinControl; X,
  Y: Integer; Files: TStrings);
var
  S: string;
begin
  for S in Files do
    FFiles.Add(S);
  PostMessage(Self.Handle, WM_ARCHIVE, 0, 0);
end;

// -----------------------------------------------------------------------------
// ...クリック時

procedure TArcBoxForm.ArchDirButtonClick(Sender: TObject);
var
  Path: string;
begin
  with ArchDirComboBox do
  begin
    Path := Text;
    if SelectDirectory(SSelectExtractDir, '', Path, [sdNewFolder, sdNewUI]) then
      Text := IncludeTrailingPathDelimiter(Path);
  end;
end;

// -----------------------------------------------------------------------------
// 変換元の書庫と同じ場所に変換するクリック時

procedure TArcBoxForm.UseArchDirCheckBoxClick(Sender: TObject);
begin
  with UseArchDirCheckBox do
  begin
    FUseArchDir := Checked;
    ArchDirComboBox.Enabled := not Checked;
    ArchDirButton.Enabled := not Checked;
  end;
end;

// -----------------------------------------------------------------------------
// ZIPクリック時

procedure TArcBoxForm.ArchFormatZipRadioButtonClick(Sender: TObject);
begin
  FArchFormat := afZip;
end;

// -----------------------------------------------------------------------------
// 7zクリック時

procedure TArcBoxForm.ArchFormat7zRadioButtonClick(Sender: TObject);
begin
  FArchFormat := af7z;
end;

// -----------------------------------------------------------------------------
// TARクリック時

procedure TArcBoxForm.ArchFormatTarRadioButtonClick(Sender: TObject);
begin
  FArchFormat := afTar;
end;

// -----------------------------------------------------------------------------
// TAR-GZクリック時

procedure TArcBoxForm.ArchFormatTgzRadioButtonClick(Sender: TObject);
begin
  FArchFormat := afTgz;
end;

// -----------------------------------------------------------------------------
// TAR-BZ2クリック時

procedure TArcBoxForm.ArchFormatTbz2RadioButtonClick(Sender: TObject);
begin
  FArchFormat := afTbz2;
end;

// -----------------------------------------------------------------------------
// TAR-XZクリック時

procedure TArcBoxForm.ArchFormatTxzRadioButtonClick(Sender: TObject);
begin
  FArchFormat := afTxz;
end;

// -----------------------------------------------------------------------------
// WIMクリック時

procedure TArcBoxForm.ArchFormatWimRadioButtonClick(Sender: TObject);
begin
  FArchFormat := afWim;
end;

// -----------------------------------------------------------------------------
// 設定クリック時

procedure TArcBoxForm.ArchLevelButtonClick(Sender: TObject);
begin
  Level(Self, FArchLevel);
end;

// -----------------------------------------------------------------------------
// OKクリック時

procedure TArcBoxForm.OKButtonClick(Sender: TObject);
begin
  WriteIni;
  Close;
end;

// -----------------------------------------------------------------------------
// キャンセルクリック時

procedure TArcBoxForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------
// INIファイル読込

procedure TArcBoxForm.ReadIni;
var
  S, CurrentDir: string;
  I: NativeInt;
begin
  if not GetIniFileName(S) then
    Exit;
  CurrentDir := GetCurrentDir;
  SetCurrentDir(ExtractFileDir(ParamStr(0)));
  with TMemIniFile.Create(S, TEncoding.UTF8) do
    try
      // ウィンドウ
      FPoint.X := ReadInteger('MainForm', 'Left', Left);
      FPoint.Y := ReadInteger('MainForm', 'Top', Top);
      // モニタ数
      FMonitorCount := ReadInteger('MainForm', 'MonitorCount', Screen.MonitorCount);
      // 変換先フォルダ
      FDirText := ReadString('MainForm', 'DirText', FDirText);
      // 変換元の書庫と同じ場所に変換する
      FUseArchDir := ReadBool('MainForm', 'UseArchDir', FUseArchDir);
      // 変換する書庫形式
      FArchFormat := TArcFormat(ReadInteger('MainForm', 'ArchFormat', NativeInt(FArchFormat)));
      // 圧縮レベル
      FArchLevel := ReadInteger('MainForm', 'ArchLevel', FArchLevel);
      for I := 0 to 31 do
      begin
        S := ReadString('MainForm', Format('DirList%d', [I]), '');
        if S <> '' then
          FDirList.Add(S);
      end;
    finally
      Free;
    end;
  SetCurrentDir(CurrentDir);
end;

// -----------------------------------------------------------------------------
// INIファイル書出

procedure TArcBoxForm.WriteIni;
var
  S: string;
  I: NativeInt;
begin
  if FIniFailed or (not GetIniFileName(S)) then
    Exit;
  try
    with TMemIniFile.Create(S, TEncoding.UTF8) do
      try
        // ウィンドウ
        WriteInteger('MainForm', 'Left', FPoint.X);
        WriteInteger('MainForm', 'Top', FPoint.Y);
        // モニタ数
        WriteInteger('MainForm', 'MonitorCount', Screen.MonitorCount);
        // 変換先フォルダ
        WriteString('MainForm', 'DirText', FDirText);
        // 変換元の書庫と同じ場所に変換する
        WriteBool('MainForm', 'UseArchDir', FUseArchDir);
        // 変換する書庫形式
        WriteInteger('MainForm', 'ArchFormat', NativeInt(FArchFormat));
        // 圧縮レベル
        WriteInteger('MainForm', 'ArchLevel', FArchLevel);
        for I := 0 to 31 do
        begin
          if I < FDirList.Count then
            S := FDirList[I]
          else
            S := '';
          WriteString('MainForm', Format('DirList%d', [I]), S);
        end;
        UpdateFile;
      finally
        Free;
      end;
  except
    FIniFailed := True;
  end;
end;

// -----------------------------------------------------------------------------
// アーカイブを処理

procedure TArcBoxForm.ExecArchive;
begin
  if FActive then
    Exit;
  FActive := True;
  try
    with FFiles do
      while Count > 0 do
      begin
        FFileName := Strings[0];
        Delete(0);
        OpenArchive(FFileName);
        if FCancel then
        begin
          Clear;
          Exit;
        end;
      end;
  finally
    FActive := False;
  end;
end;

// -----------------------------------------------------------------------------
// アーカイブを開く

procedure TArcBoxForm.OpenArchive(const AFileName: string);
begin
  if FileExists2(AFileName) then
    ConvArchive(AFileName)
  else if DirectoryExists2(AFileName) then
    SaveArchive(AFileName);
end;

// -----------------------------------------------------------------------------
// アーカイブを保存

procedure TArcBoxForm.SaveArchive(const AFileName: string);
var
  S, Path: string;
begin
  Path := ExcludeTrailingPathDelimiter(AFileName);
  if not DirectoryExists2(Path) then
    Exit;
  with ArchDirComboBox, UseArchDirCheckBox do
  begin
    if Checked then
      S := ExtractFilePath(Path)
    else
    begin
      S := '';
      if not DirectoryExists2(Text) then
        if SelectDirectory(SSelectExtractDir, '', S, [sdNewFolder, sdNewUI]) then
          Text := IncludeTrailingPathDelimiter(S);
      S := IncludeTrailingPathDelimiter(Text);
    end;
    if not DirectoryExists2(S) then
      Exit;
  end;
  FCancel := False;
  FProgress := TProgressForm.Create(Self);
  try
    FProgress.Show;
    SetForegroundWindow(FProgress.Handle);
    SaveToFile(Path, S + ExtractFileName(Path));
  finally
    FreeAndNil(FProgress);
  end;
end;

// -----------------------------------------------------------------------------
// アーカーブを変換

procedure TArcBoxForm.ConvArchive(const AFileName: string);
var
  S, Path: string;
  Guid: TGUID;
begin
  if not FileExists2(AFileName) then
    Exit;
  CreateGUID(Guid);
  Path := IncludeTrailingPathDelimiter(TPath.GetTempPath) + GUIDToString(Guid);
  with ArchDirComboBox, UseArchDirCheckBox do
  begin
    if Checked then
      S := ExtractFilePath(AFileName)
    else
    begin
      S := '';
      if not DirectoryExists2(Text) then
        if SelectDirectory(SSelectExtractDir, '', S, [sdNewFolder, sdNewUI]) then
          Text := IncludeTrailingPathDelimiter(S);
      S := IncludeTrailingPathDelimiter(Text);
    end;
    if not DirectoryExists2(S) then
      Exit;
  end;
  FCancel := False;
  FProgress := TProgressForm.Create(Self);
  try
    FProgress.Show;
    SetForegroundWindow(FProgress.Handle);
    with TInArchive.Create do
      try
        OnPassword := ArchivePassword;
        OnProgress := ArchiveProgress;
        Extract(AFileName, Path);
      finally
        Free;
      end;
    if FCancel then
      Exit;
    SaveToFile(Path, S + ExtractFileName(AFileName));
  finally
    if DirectoryExists2(Path) then
      TDirectory.Delete(Path, True);
    FreeAndNil(FProgress);
  end;
end;

// -----------------------------------------------------------------------------
// ファイルに保存

procedure TArcBoxForm.SaveToFile(const APath, AFileName: string);
var
  S: string;
begin
  S := ChangeFileExt(AFileName, '');
  case FArchFormat of
    afZip:
      S := GetUniqueFileName(S, '.zip');
    af7z:
      S := GetUniqueFileName(S, '.7z');
    afTar:
      S := GetUniqueFileName(S, '.tar');
    afTgz:
      S := GetUniqueFileName(S, '.tar.gz');
    afTbz2:
      S := GetUniqueFileName(S, '.tar.bz2');
    afTxz:
      S := GetUniqueFileName(S, '.tar.xz');
    afWim:
      S := GetUniqueFileName(S, '.wim');
  end;
  with TOutArchive.Create do
    try
      Format := FArchFormat;
      Level := FArchLevel;
      OnProgress := ArchiveProgress;
      try
        SaveToFile(APath, S);
      except
        if FileExists2(S) then
          DeleteFile(S);
      end;
    finally
      Free;
    end;
end;

// -----------------------------------------------------------------------------
// アーカイブパスワード時

procedure TArcBoxForm.ArchivePassword(Sender: TObject; var APassword: string;
  var Abort: Boolean);
begin
  if not Password(Self, APassword) then
    Abort := True;
end;

// -----------------------------------------------------------------------------
// アーカイブ進捗時

procedure TArcBoxForm.ArchiveProgress(Sender: TObject; APos, AMax: Int64;
  var Abort: Boolean);
begin
  if Assigned(FProgress) then
  begin
    FProgress.Progress(FFileName, APos, AMax);
    FCancel := FProgress.Cancel;
    Abort := FCancel;
  end;
end;

// -----------------------------------------------------------------------------
// フォント更新

procedure TArcBoxForm.UpdateFont;
var
  LF: TLogFont;
begin
  GetObject(Font.Handle, SizeOf(TLogFont), @LF);
  LF.lfQuality := NONANTIALIASED_QUALITY;
  Font.Handle := CreateFontIndirect(LF);
end;

// -----------------------------------------------------------------------------
// コピーデータ時

procedure TArcBoxForm.WMCopyData(var Message: TWMCopyData);
var
  S: string;
  P: PChar;
begin
  P := PChar(Message.CopyDataStruct^.lpData);
  S := P;
  if Length(S) > 0 then
    FFiles.Add(S);
  ExecArchive;
end;

// -----------------------------------------------------------------------------
// アーカイブ時

procedure TArcBoxForm.WMArchive(var Message: TMessage);
begin
  ExecArchive;
end;

end.
