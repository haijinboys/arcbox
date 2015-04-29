unit mCommon;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls,
  Winapi.ShlObj, Winapi.ActiveX, System.IniFiles;
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  StdCtrls, ShlObj, ActiveX, IniFiles;
{$IFEND}

function FileExists2(const FileName: string): Boolean;
function DirectoryExists2(const Directory: string): Boolean;
function GetFileNameFromLink(const FileName: string): string;
function GetUniqueFileName(const FileName, Ext: string): string;
function GetUniqueDirName(const DirName: string): string;
function ShortToLongFileName(const FileName: string): string;
function ExpandEnvironment(const Value: string): string;
function GetAppDataPath: string;
function GetPersonalPath: string;
function GetIniFileName(var FileName: string): Boolean;
function GetIcon(IconSize: NativeInt): HICON;
function GetFileVersion: string;
procedure MergeList(List: TStrings; const Value: string);
procedure AssignList(ComboBox: TComboBox; List: TStrings);
procedure SetChecked(ACheckBox: TCheckBox; Value: Boolean); overload;
procedure SetChecked(ARadioButton: TRadioButton; Value: Boolean); overload;
function IsDebuggerPresent: BOOL; stdcall;

var
  FIniFailed: Boolean;
  FMonitorCount: NativeInt;

var
  DebuggerPresent: Boolean;

implementation

uses
  mConsts;

// -----------------------------------------------------------------------------
// ドライブ確認

function IsDriveReady(C: Char): Boolean;
var
  W: Word;
  SR: TSearchRec;
begin
  if C = '\' then
  begin
    Result := True;
    Exit;
  end;
  C := CharUpper(PChar(string(C)))^;
  W := SetErrorMode(SEM_FAILCRITICALERRORS);
  try
    Result := DiskSize(Ord(C) - $40) <> -1;
    if Result and (GetDriveType(PChar(string(C + ':\'))) in [DRIVE_REMOTE, DRIVE_CDROM]) then
      try
        Result := FindFirst(C + ':\*.*', $3F, SR) = 0;
      finally
        FindClose(SR);
      end;
  finally
    SetErrorMode(W);
  end;
end;

// -----------------------------------------------------------------------------
// ファイル存在確認

function FileExists2(const FileName: string): Boolean;
begin
  if Length(FileName) = 0 then
    Result := False
  else
  begin
    if (ExpandFileName(FileName)[1] = '\') or
      IsDriveReady(ExpandFileName(FileName)[1]) then
      Result := FileExists(FileName)
    else
      Result := False;
  end;
end;

// -----------------------------------------------------------------------------
// ディレクトリ存在確認

function DirectoryExists2(const Directory: string): Boolean;
begin
  if Length(Directory) = 0 then
    Result := False
  else
  begin
    if (ExpandFileName(Directory)[1] = '\') or
      IsDriveReady(ExpandFileName(Directory)[1]) then
      Result := DirectoryExists(Directory)
    else
      Result := False;
  end;
end;

// -----------------------------------------------------------------------------
// リンクからファイル名取得

function GetFileNameFromLink(const FileName: string): string;
const
  IID_IPersistFile: TGUID = '{0000010B-0000-0000-C000-000000000046}';
var
  LResult: HRESULT;
  LSHellLink: IShellLink;
  LPersistFile: IPersistFile;
  LFindData: TWin32FindData;
begin
  Result := FileName;
  LResult := CoCreateInstance(CLSID_ShellLink, nil, CLSCTX_INPROC_SERVER,
    IID_IShellLinkW, LShellLink);
  if LResult = S_OK then
    if Supports(LShellLink, IID_IPersistFile, LPersistFile) then
    begin
      LResult := LPersistFile.Load(PChar(Result), STGM_READ);
      if LResult = S_OK then
      begin
        LResult := LShellLink.Resolve(0, SLR_ANY_MATCH);
        if LResult = S_OK then
        begin
          SetLength(Result, MAX_PATH);
          LResult := LSHellLink.GetPath(PChar(Result), MAX_PATH, LFindData, SLGP_UNCPRIORITY);
          if LResult = S_OK then
            Result := Trim(Result);
        end;
      end;
    end;
end;

// -----------------------------------------------------------------------------
// ユニークファイル名取得

function GetUniqueFileName(const FileName, Ext: string): string;
var
  I: NativeInt;
begin
  if not FileExists2(FileName + Ext) then
  begin
    Result := FileName + Ext;
    Exit;
  end;
  I := 1;
  while FileExists2(Format('%s (%d)%s', [FileName, I, Ext])) do
    Inc(I);
  Result := Format('%s (%d)%s', [FileName, I, Ext]);
end;

// -----------------------------------------------------------------------------
// ユニークディレクトリ名取得

function GetUniqueDirName(const DirName: string): string;
var
  I: NativeInt;
begin
  if not DirectoryExists2(DirName) then
  begin
    Result := DirName;
    Exit;
  end;
  I := 1;
  while DirectoryExists2(Format('%s (%d)', [DirName, I])) do
    Inc(I);
  Result := Format('%s (%d)', [DirName, I]);
end;

// -----------------------------------------------------------------------------
// 短いファイル名から長いファイル名に変換

function ShortToLongFileName(const FileName: string): string;
var
  Len: NativeInt;
  function GetLongNameOf(const Path: string): string;
  var
    SR: TSearchRec;
  begin
    if Length(Path) = Len then
    begin
      if Len = 3 then
        Result := CharUpper(PChar(Path))
      else
        Result := Path;
    end
    else
    begin
      if FindFirst(Path, faAnyFile, SR) = 0 then
        Result := IncludeTrailingPathDelimiter(GetLongNameOf(ExtractFileDir(Path))) + SR.Name
      else
      begin
        if (Length(Path) >= 2) and (Path[1] = '\') and (Path[2] = '\') then
          Result := Path
        else
          Result := '';
      end;
      FindClose(SR);
    end;
  end;

begin
  Result := '';
  if FileExists(FileName) then
  begin
    Len := Length(ExtractFileDrive(FileName));
    if Len = 2 then
      Len := 3;
    Result := GetLongNameOf(ExpandFileName(FileName));
    if not FileExists(Result) then
      Result := FileName;
  end;
end;

// -----------------------------------------------------------------------------
// 環境変数展開

function ExpandEnvironment(const Value: string): string;
var
  C: array [0 .. MAX_PATH] of Char;
begin
  ExpandEnvironmentStrings(PChar(Value), @C, MAX_PATH);
  Result := C;
end;

// -----------------------------------------------------------------------------
// アプリケーションデータパス取得

function GetAppDataPath: string;
var
  S: array [0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);
  if SHGetFolderPath(0, CSIDL_APPDATA, 0, 0, @S) = S_OK then
    Result := IncludeTrailingBackslash(S);
end;

// -----------------------------------------------------------------------------
// パーソナルパス取得

function GetPersonalPath: string;
var
  S: array [0 .. MAX_PATH] of Char;
begin
  SetLastError(ERROR_SUCCESS);
  if SHGetFolderPath(0, CSIDL_PERSONAL, 0, 0, @S) = S_OK then
    Result := IncludeTrailingBackslash(S);
end;

// -----------------------------------------------------------------------------
// INIファイル名取得

function GetIniFileName(var FileName: string): Boolean;
begin
  Result := False;
  if not FileExists2(ParamStr(0)) then
    Exit;
  FileName := ChangeFileExt(ParamStr(0), '.ini');
  if not FileExists2(FileName) then
  begin
    FileName := GetAppDataPath + SName + '\' + ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');
    ForceDirectories(ExtractFileDir(FileName));
  end;
  if FileName <> '' then
    Result := True;
end;

// -----------------------------------------------------------------------------
// アイコン取得

function GetIcon(IconSize: NativeInt): HICON;
begin
  Result := LoadImage(HInstance, 'MAINICON', IMAGE_ICON, IconSize, IconSize, LR_SHARED);
end;

// -----------------------------------------------------------------------------
// ファイルバージョン取得

function GetFileVersion: string;
var
  FileName: string;
  InfoSize, Wnd: DWORD;
  VerBuf, P: Pointer;
  VerSize: DWORD;
begin
  Result := '';
  FileName := ParamStr(0);
  UniqueString(FileName);
  InfoSize := GetFileVersionInfoSize(PChar(FileName), Wnd);
  if InfoSize <> 0 then
  begin
    GetMem(VerBuf, InfoSize);
    try
      if GetFileVersionInfo(PChar(FileName), Wnd, InfoSize, VerBuf) then
        if VerQueryValue(VerBuf, '\StringFileInfo\041103A4\FileVersion', P, VerSize) then
          Result := PChar(P);
    finally
      FreeMem(VerBuf);
    end;
  end;
end;

// -----------------------------------------------------------------------------
// 文字列リストマージ

procedure MergeList(List: TStrings; const Value: string);
var
  I: NativeInt;
begin
  with List do
  begin
    I := IndexOf(Value);
    if I > -1 then
      Delete(I);
    Insert(0, Value);
  end;
end;

// -----------------------------------------------------------------------------
// 文字列リストアサイン

procedure AssignList(ComboBox: TComboBox; List: TStrings);
var
  I: NativeInt;
begin
  with ComboBox do
  begin
    Items.Clear;
    for I := 0 to List.Count - 1 do
      if List[I] <> '' then
        Items.Add(List[I]);
  end;
end;

// -----------------------------------------------------------------------------
// チェックボックス設定

procedure SetChecked(ACheckBox: TCheckBox; Value: Boolean);
var
  AEvent: TNotifyEvent;
begin
  with ACheckBox do
  begin
    AEvent := OnClick;
    OnClick := nil;
    Checked := Value;
    OnClick := AEvent;
  end;
end;

// -----------------------------------------------------------------------------
// ラジオボタン設定

procedure SetChecked(ARadioButton: TRadioButton; Value: Boolean);
var
  AEvent: TNotifyEvent;
begin
  with ARadioButton do
  begin
    AEvent := OnClick;
    OnClick := nil;
    Checked := Value;
    OnClick := AEvent;
  end;
end;

// -----------------------------------------------------------------------------
// デバッガ存在確認

function IsDebuggerPresent; external kernel32 name 'IsDebuggerPresent';

end.
