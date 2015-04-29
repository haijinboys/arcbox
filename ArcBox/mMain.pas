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
    { Private �錾 }
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
    { Public �錾 }
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
// �t�H�[��������

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
// �t�H�[���j����

procedure TArcBoxForm.FormDestroy(Sender: TObject);
begin
  FPoint := Point(Left, Top);
  if Assigned(FDirList) then
    FreeAndNil(FDirList);
  if Assigned(FFiles) then
    FreeAndNil(FFiles);
end;

// -----------------------------------------------------------------------------
// �t�H�[���J�n��

procedure TArcBoxForm.FormShow(Sender: TObject);
begin
  //
end;

// -----------------------------------------------------------------------------
// �t�H�[���I����

procedure TArcBoxForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FDirText := ArchDirComboBox.Text;
  MergeList(FDirList, FDirText);
end;

// -----------------------------------------------------------------------------
// �o�[�W�������

procedure TArcBoxForm.HelpAboutMenuItemClick(Sender: TObject);
begin
  About(Self);
end;

// -----------------------------------------------------------------------------
// �t�@�C���h���b�O�h���b�v��

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
// ...�N���b�N��

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
// �ϊ����̏��ɂƓ����ꏊ�ɕϊ�����N���b�N��

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
// ZIP�N���b�N��

procedure TArcBoxForm.ArchFormatZipRadioButtonClick(Sender: TObject);
begin
  FArchFormat := afZip;
end;

// -----------------------------------------------------------------------------
// 7z�N���b�N��

procedure TArcBoxForm.ArchFormat7zRadioButtonClick(Sender: TObject);
begin
  FArchFormat := af7z;
end;

// -----------------------------------------------------------------------------
// TAR�N���b�N��

procedure TArcBoxForm.ArchFormatTarRadioButtonClick(Sender: TObject);
begin
  FArchFormat := afTar;
end;

// -----------------------------------------------------------------------------
// TAR-GZ�N���b�N��

procedure TArcBoxForm.ArchFormatTgzRadioButtonClick(Sender: TObject);
begin
  FArchFormat := afTgz;
end;

// -----------------------------------------------------------------------------
// TAR-BZ2�N���b�N��

procedure TArcBoxForm.ArchFormatTbz2RadioButtonClick(Sender: TObject);
begin
  FArchFormat := afTbz2;
end;

// -----------------------------------------------------------------------------
// TAR-XZ�N���b�N��

procedure TArcBoxForm.ArchFormatTxzRadioButtonClick(Sender: TObject);
begin
  FArchFormat := afTxz;
end;

// -----------------------------------------------------------------------------
// WIM�N���b�N��

procedure TArcBoxForm.ArchFormatWimRadioButtonClick(Sender: TObject);
begin
  FArchFormat := afWim;
end;

// -----------------------------------------------------------------------------
// �ݒ�N���b�N��

procedure TArcBoxForm.ArchLevelButtonClick(Sender: TObject);
begin
  Level(Self, FArchLevel);
end;

// -----------------------------------------------------------------------------
// OK�N���b�N��

procedure TArcBoxForm.OKButtonClick(Sender: TObject);
begin
  WriteIni;
  Close;
end;

// -----------------------------------------------------------------------------
// �L�����Z���N���b�N��

procedure TArcBoxForm.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

// -----------------------------------------------------------------------------
// INI�t�@�C���Ǎ�

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
      // �E�B���h�E
      FPoint.X := ReadInteger('MainForm', 'Left', Left);
      FPoint.Y := ReadInteger('MainForm', 'Top', Top);
      // ���j�^��
      FMonitorCount := ReadInteger('MainForm', 'MonitorCount', Screen.MonitorCount);
      // �ϊ���t�H���_
      FDirText := ReadString('MainForm', 'DirText', FDirText);
      // �ϊ����̏��ɂƓ����ꏊ�ɕϊ�����
      FUseArchDir := ReadBool('MainForm', 'UseArchDir', FUseArchDir);
      // �ϊ����鏑�Ɍ`��
      FArchFormat := TArcFormat(ReadInteger('MainForm', 'ArchFormat', NativeInt(FArchFormat)));
      // ���k���x��
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
// INI�t�@�C�����o

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
        // �E�B���h�E
        WriteInteger('MainForm', 'Left', FPoint.X);
        WriteInteger('MainForm', 'Top', FPoint.Y);
        // ���j�^��
        WriteInteger('MainForm', 'MonitorCount', Screen.MonitorCount);
        // �ϊ���t�H���_
        WriteString('MainForm', 'DirText', FDirText);
        // �ϊ����̏��ɂƓ����ꏊ�ɕϊ�����
        WriteBool('MainForm', 'UseArchDir', FUseArchDir);
        // �ϊ����鏑�Ɍ`��
        WriteInteger('MainForm', 'ArchFormat', NativeInt(FArchFormat));
        // ���k���x��
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
// �A�[�J�C�u������

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
// �A�[�J�C�u���J��

procedure TArcBoxForm.OpenArchive(const AFileName: string);
begin
  if FileExists2(AFileName) then
    ConvArchive(AFileName)
  else if DirectoryExists2(AFileName) then
    SaveArchive(AFileName);
end;

// -----------------------------------------------------------------------------
// �A�[�J�C�u��ۑ�

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
// �A�[�J�[�u��ϊ�

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
// �t�@�C���ɕۑ�

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
// �A�[�J�C�u�p�X���[�h��

procedure TArcBoxForm.ArchivePassword(Sender: TObject; var APassword: string;
  var Abort: Boolean);
begin
  if not Password(Self, APassword) then
    Abort := True;
end;

// -----------------------------------------------------------------------------
// �A�[�J�C�u�i����

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
// �t�H���g�X�V

procedure TArcBoxForm.UpdateFont;
var
  LF: TLogFont;
begin
  GetObject(Font.Handle, SizeOf(TLogFont), @LF);
  LF.lfQuality := NONANTIALIASED_QUALITY;
  Font.Handle := CreateFontIndirect(LF);
end;

// -----------------------------------------------------------------------------
// �R�s�[�f�[�^��

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
// �A�[�J�C�u��

procedure TArcBoxForm.WMArchive(var Message: TMessage);
begin
  ExecArchive;
end;

end.
