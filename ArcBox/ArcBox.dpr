program ArcBox;

{$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
{$WEAKLINKRTTI ON}


uses
  Windows,
  Messages,
  SysUtils,
  Forms,
  mConsts in 'mConsts.pas',
  mCommon in 'mCommon.pas',
  mArchive in 'mArchive.pas',
  mMain in 'mMain.pas' {ArcBoxForm},
  mArchLevel in 'mArchLevel.pas' {ArchLevelForm},
  mPassword in 'mPassword.pas' {PasswordForm},
  mProgress in 'mProgress.pas' {ProgressForm},
  mAbout in 'mAbout.pas' {AboutForm};

{$R *.res}


var
  Mutex, Handle: THandle;
  Struct: TCopyDataStruct;
  P: PChar;
  I: NativeInt;

const
  MutexName = 'arcbox.haijin-boys.com';

begin
  // ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'ArcBox';
  Handle := FindWindow('TArcBoxForm', nil);
  if Handle <> 0 then
  begin
    if not DebuggerPresent then
    begin
      SetForegroundWindow(Handle);
      if ParamCount > 0 then
      begin
        for I := 1 to ParamCount do
        begin
          P := StrAlloc(MAX_PATH + 1);
          StrCopy(P, PChar(ParamStr(I)));
          Struct.cbData := (MAX_PATH + 1) * 2;
          Struct.lpData := P;
          SendMessage(Handle, WM_COPYDATA, 0, LPARAM(@Struct));
          StrDispose(P);
        end;
      end
      else
      begin
        P := StrAlloc(MAX_PATH + 1);
        StrCopy(P, '');
        Struct.cbData := (MAX_PATH + 1) * 2;
        Struct.lpData := P;
        SendMessage(Handle, WM_COPYDATA, 0, LPARAM(@Struct));
        StrDispose(P);
      end;
    end;
    Exit;
  end;
  Mutex := CreateMutex(nil, False, PChar(MutexName));
  Application.CreateForm(TArcBoxForm, ArcBoxForm);
  Application.Run;
  if Mutex <> 0 then
    ReleaseMutex(Mutex);
  if Mutex <> 0 then
    CloseHandle(Mutex);

end.
