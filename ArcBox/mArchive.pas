unit mArchive;

interface

uses
{$IF CompilerVersion > 22.9}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Forms, Vcl.Controls, Vcl.Dialogs,
{$ELSE}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Forms, Controls,
  Dialogs,
{$IFEND}
  AbUnzper, AbCabExt, AbArcTyp, RAR, sevenzip;

type
  TPasswordEvent = procedure(Sender: TObject; var APassword: string;
    var Abort: Boolean) of object;
  TProgressEvent = procedure(Sender: TObject; APos, AMax: Int64;
    var Abort: Boolean) of object;

  TArchive = class
  private
    { Private éŒ¾ }
  protected
    { Protected éŒ¾ }
    FExt: TStringList;
    FOnPassword: TPasswordEvent;
    FOnProgress: TProgressEvent;
  public
    { Public éŒ¾ }
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Extract(const AFileName, APath: string); virtual;
    property OnPassword: TPasswordEvent read FOnPassword write FOnPassword;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property Ext: TStringList read FExt write FExt;
  end;

  TZipArchive = class(TArchive)
  private
    { Private éŒ¾ }
    FZip: TAbUnZipper;
    procedure ZipArchiveProgress(Sender: TObject; Progress: Byte;
      var Abort: Boolean);
    procedure ZipNeedPassword(Sender: TObject;
      var NewPassword: AnsiString);
  public
    { Public éŒ¾ }
    constructor Create; override;
    destructor Destroy; override;
    procedure Extract(const AFileName, APath: string); override;
  end;

  TCabArchive = class(TArchive)
  private
    { Private éŒ¾ }
    FCab: TAbCabExtractor;
    procedure CabArchiveProgress(Sender: TObject; Progress: Byte;
      var Abort: Boolean);
  public
    { Public éŒ¾ }
    constructor Create; override;
    destructor Destroy; override;
    procedure Extract(const AFileName, APath: string); override;
  end;

  TRarArchive = class(TArchive)
  private
    { Private éŒ¾ }
    FRar: TRAR;
    procedure RarError(Sender: TObject; const ErrorCode: Integer;
      const Operation: TRAROperation);
    procedure RarNextVolumeRequired(Sender: TObject;
      const RequiredFileName: WideString; out NewFileName: WideString;
      out Cancel: Boolean);
    procedure RarPasswordRequired(Sender: TObject;
      const HeaderPassword: Boolean; const FileName: WideString;
      out NewPassword: WideString; out Cancel: Boolean);
    procedure RarProcess(Sender: TObject; const FileName: WideString;
      const ArchiveBytesTotal, ArchiveBytesDone, FileBytesTotal,
      FileBytesDone: Int64);
    procedure RarReplace(Sender: TObject; const ExistingData,
      NewData: TRARReplaceData; out Action: TRARReplaceAction);
    procedure RarVolumeChanged(Sender: TObject;
      const NewVolumeName: WideString);
  public
    { Public éŒ¾ }
    constructor Create; override;
    destructor Destroy; override;
    procedure Extract(const AFileName, APath: string); override;
  end;

  T7zItem = class(TCollectionItem)
  private
    { Private éŒ¾ }
    FExt: TStringList;
    FArchive: I7zInArchive;
  public
    { Public éŒ¾ }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published éŒ¾ }
    property Ext: TStringList read FExt write FExt;
    property Archive: I7zInArchive read FArchive write FArchive;
  end;

  T7zItems = class;

  T7zItemsEnumerator = class
  private
    { Private éŒ¾ }
    FIndex: NativeInt;
    FCollection: T7zItems;
  public
    { Public éŒ¾ }
    constructor Create(ACollection: T7zItems);
    function GetCurrent: T7zItem;
    function MoveNext: Boolean;
    property Current: T7zItem read GetCurrent;
  end;

  T7zItems = class(TCollection)
  private
    { Private éŒ¾ }
    function GetItem(Index: NativeInt): T7zItem;
    procedure SetItem(Index: NativeInt; Value: T7zItem);
  public
    { Public éŒ¾ }
    constructor Create;
    function Add: T7zItem;
    function Find(const S: string): T7zItem;
    function GetEnumerator: T7zItemsEnumerator;
    property Items[Index: NativeInt]: T7zItem read GetItem write SetItem; default;
  end;

  T7zArchive = class(TArchive)
  private
    { Private éŒ¾ }
    F7zs: T7zItems;
  public
    { Public éŒ¾ }
    Max: Int64;
    constructor Create; override;
    destructor Destroy; override;
    procedure Extract(const AFileName, APath: string); override;
  end;

  TArchiveItem = class(TCollectionItem)
  private
    { Private éŒ¾ }
    FArchive: TArchive;
  public
    { Public éŒ¾ }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    { Published éŒ¾ }
    property Archive: TArchive read FArchive write FArchive;
  end;

  TArchiveItems = class;

  TArchiveItemsEnumerator = class
  private
    { Private éŒ¾ }
    FIndex: NativeInt;
    FCollection: TArchiveItems;
  public
    { Public éŒ¾ }
    constructor Create(ACollection: TArchiveItems);
    function GetCurrent: TArchiveItem;
    function MoveNext: Boolean;
    property Current: TArchiveItem read GetCurrent;
  end;

  TArchiveItems = class(TCollection)
  private
    { Private éŒ¾ }
    function GetItem(Index: NativeInt): TArchiveItem;
    procedure SetItem(Index: NativeInt; Value: TArchiveItem);
  public
    { Public éŒ¾ }
    constructor Create;
    function Add: TArchiveItem;
    function Find(const S: string): TArchiveItem;
    function GetEnumerator: TArchiveItemsEnumerator;
    property Items[Index: NativeInt]: TArchiveItem read GetItem write SetItem; default;
  end;

  TInArchive = class
  private
    { Private éŒ¾ }
    FArchives: TArchiveItems;
    FOnPassword: TPasswordEvent;
    FOnProgress: TProgressEvent;
  public
    { Public éŒ¾ }
    constructor Create;
    destructor Destroy; override;
    function Extract(const AFileName, APath: string): Boolean;
    property OnPassword: TPasswordEvent read FOnPassword write FOnPassword;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

  TArcFormat = (afZip, af7z, afTar, afTgz, afTbz2, afTxz, afWim);

  TOutArchive = class
  private
    { Private éŒ¾ }
    FFormat: TArcFormat;
    FLevel: NativeInt;
    FOnProgress: TProgressEvent;
  public
    { Public éŒ¾ }
    Max: Int64;
    constructor Create;
    destructor Destroy; override;
    procedure SaveToFile(const APath, AFileName: string);
    property Format: TArcFormat read FFormat write FFormat;
    property Level: NativeInt read FLevel write FLevel;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
{$IF CompilerVersion > 22.9}
  System.IOUtils,
{$ELSE}
  IOUtils,
{$IFEND}
  mConsts, mCommon;

{ TArchive }

var
  FAbort: Boolean;

constructor TArchive.Create;
begin
  FExt := TStringList.Create;
  with FExt do
  begin
    CaseSensitive := False;
    Duplicates := dupIgnore;
    Sorted := True;
  end;
end;

destructor TArchive.Destroy;
begin
  if Assigned(FExt) then
    FreeAndNil(FExt);
  inherited;
end;

procedure TArchive.Extract(const AFileName, APath: string);
begin
  //
end;

{ TZipArchive }

constructor TZipArchive.Create;
begin
  inherited;
  with FExt do
  begin
    Add('.zip');
    Add('.jar');
    Add('.xpi');
    Add('.odt');
    Add('.ods');
    Add('.docx');
    Add('.xlsx');
    Add('.bz2');
    Add('.bzip2');
    Add('.tbz2');
    Add('.tbz');
    Add('.tar');
    Add('.gz');
    Add('.gzip');
    Add('.tgz');
    Add('.tpz');
  end;
  FZip := TAbUnZipper.Create(nil);
  with FZip do
  begin
    OnArchiveProgress := ZipArchiveProgress;
    OnNeedPassword := ZipNeedPassword;
  end;
end;

destructor TZipArchive.Destroy;
begin
  if Assigned(FZip) then
    FreeAndNil(FZip);
  inherited;
end;

procedure TZipArchive.Extract(const AFileName, APath: string);
begin
  inherited;
  if not DirectoryExists2(APath) then
    ForceDirectories(APath);
  with FZip do
  begin
    BaseDirectory := APath;
    FileName := AFileName;
    ExtractOptions := [eoCreateDirs, eoRestorePath];
    ExtractFiles('*.*');
  end;
end;

procedure TZipArchive.ZipArchiveProgress(Sender: TObject; Progress: Byte;
  var Abort: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Progress, 100, FAbort);
  Abort := FAbort;
end;

procedure TZipArchive.ZipNeedPassword(Sender: TObject;
  var NewPassword: AnsiString);
var
  S: string;
begin
  S := '';
  if Assigned(FOnPassword) then
    FOnPassword(Sender, S, FAbort);
  NewPassword := AnsiString(S);
end;

{ TCabArchive }

procedure TCabArchive.CabArchiveProgress(Sender: TObject; Progress: Byte;
  var Abort: Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, Progress, 100, FAbort);
  Abort := FAbort;
end;

constructor TCabArchive.Create;
begin
  inherited;
  FExt.Add('.cab');
  FCab := TAbCabExtractor.Create(nil);
  with FCab do
  begin
    OnArchiveProgress := CabArchiveProgress;
  end;
end;

destructor TCabArchive.Destroy;
begin
  if Assigned(FCab) then
    FreeAndNil(FCab);
  inherited;
end;

procedure TCabArchive.Extract(const AFileName, APath: string);
begin
  inherited;
  if not DirectoryExists2(APath) then
    ForceDirectories(APath);
  with FCab do
  begin
    BaseDirectory := APath;
    FileName := AFileName;
    ExtractOptions := [eoCreateDirs, eoRestorePath];
    ExtractFiles('*.*');
  end;
end;

{ TRarArchive }

procedure TRarArchive.RarError(Sender: TObject; const ErrorCode: Integer;
  const Operation: TRAROperation);
begin

end;

procedure TRarArchive.RarNextVolumeRequired(Sender: TObject;
  const RequiredFileName: WideString; out NewFileName: WideString;
  out Cancel: Boolean);
begin
  with TOpenDialog.Create(nil) do
    try
      Filter := SAllFiles;
      FileName := ExtractFileName(RequiredFileName);
      InitialDir := ExtractFilePath(RequiredFileName);
      Options := [ofHideReadOnly, ofNoChangeDir, ofFileMustExist, ofEnableSizing];
      if Execute(Application.Handle) then
        NewFileName := FileName
      else
        Cancel := True;
    finally
      Free;
    end;
end;

procedure TRarArchive.RarPasswordRequired(Sender: TObject;
  const HeaderPassword: Boolean; const FileName: WideString;
  out NewPassword: WideString; out Cancel: Boolean);
var
  S: string;
begin
  S := '';
  if Assigned(FOnPassword) then
    FOnPassword(Sender, S, FAbort);
  NewPassword := S;
  Cancel := FAbort;
end;

procedure TRarArchive.RarProcess(Sender: TObject; const FileName: WideString;
  const ArchiveBytesTotal, ArchiveBytesDone, FileBytesTotal,
  FileBytesDone: Int64);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender, ArchiveBytesDone, ArchiveBytesTotal, FAbort);
  if FAbort then
    FRar.Abort;
end;

procedure TRarArchive.RarReplace(Sender: TObject; const ExistingData,
  NewData: TRARReplaceData; out Action: TRARReplaceAction);
begin
  //
end;

procedure TRarArchive.RarVolumeChanged(Sender: TObject;
  const NewVolumeName: WideString);
begin
  //
end;

constructor TRarArchive.Create;
begin
  inherited;
  with FExt do
  begin
    Add('.rar');
    Add('.r00');
    Add('.part1.rar');
    Add('.part01.rar');
    Add('.part001.rar');
  end;
  FRar := TRAR.Create(nil);
  with FRar do
  begin
    AutoNextVolumeOnList := False;
    OnError := RarError;
    OnNextVolumeRequired := RarNextVolumeRequired;
    OnPasswordRequired := RarPasswordRequired;
    OnProgress := RarProcess;
    OnReplace := RarReplace;
    OnVolumeChanged := RarVolumeChanged;
  end;
end;

destructor TRarArchive.Destroy;
begin
  if Assigned(FRar) then
    FreeAndNil(FRar);
  inherited;
end;

procedure TRarArchive.Extract(const AFileName, APath: string);
begin
  inherited;
  with FRar do
  begin
    if not OpenFile(AFileName) then
      raise Exception.Create(SUnknownFormat);
    if not Extract(APath, True, nil) then
      raise Exception.Create(SUnknownFormat);
  end;
end;

{ T7zItem }

constructor T7zItem.Create(Collection: TCollection);
begin
  inherited;
  FExt := TStringList.Create;
  with FExt do
  begin
    CaseSensitive := False;
    Duplicates := dupIgnore;
    Sorted := True;
  end;
  FArchive := nil;
end;

destructor T7zItem.Destroy;
begin
  FArchive := nil;
  if Assigned(FExt) then
    FreeAndNil(FExt);
  inherited;
end;

procedure T7zItem.Assign(Source: TPersistent);
begin
  if Source is T7zItem then
    with T7zItem(Source) do
    begin
      Self.FExt.Assign(Ext);
      Self.FArchive := Archive;
    end
  else
    inherited;
end;

{ T7zItemsEnumerator }

constructor T7zItemsEnumerator.Create(ACollection: T7zItems);
begin
  inherited Create;
  FIndex := -1;
  FCollection := ACollection;
end;

function T7zItemsEnumerator.GetCurrent: T7zItem;
begin
  Result := FCollection[FIndex];
end;

function T7zItemsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCollection.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ T7zItems }

function T7zItems.Add: T7zItem;
begin
  Result := T7zItem( inherited Add);
end;

constructor T7zItems.Create;
begin
  inherited Create(T7zItem);
end;

function T7zItems.Find(const S: string): T7zItem;
var
  I: NativeInt;
begin
  for I := 0 to Count - 1 do
  begin
    Result := T7zItem( inherited GetItem(I));
    with Result do
      if Ext.IndexOf(S) > -1 then
        Exit;
  end;
  Result := nil;
end;

function T7zItems.GetEnumerator: T7zItemsEnumerator;
begin
  Result := T7zItemsEnumerator.Create(Self);
end;

function T7zItems.GetItem(Index: NativeInt): T7zItem;
begin
  Result := T7zItem( inherited GetItem(Index));
end;

procedure T7zItems.SetItem(Index: NativeInt; Value: T7zItem);
begin
  inherited SetItem(Index, Value);
end;

{ T7zArchive }

constructor T7zArchive.Create;
var
  S: string;
  Item: T7zItem;
begin
  inherited;
  F7zs := T7zItems.Create;
  with F7zs do
  begin
    with Add do
    begin
      Ext.Add('.zip');
      Ext.Add('.jar');
      Ext.Add('.xpi');
      Ext.Add('.odt');
      Ext.Add('.ods');
      Ext.Add('.docx');
      Ext.Add('.xlsx');
      Archive := CreateInArchive(CLSID_CFormatZip);
    end;
    (*
     with Add do
     begin
     Ext.Add('.rar');
     Ext.Add('.r00');
     Archive := CreateInArchive(CLSID_CFormatRar);
     end;
    *)
    with Add do
    begin
      Ext.Add('.bz2');
      Ext.Add('.bzip2');
      Ext.Add('.tbz2');
      Ext.Add('.tbz');
      Archive := CreateInArchive(CLSID_CFormatBZ2);
    end;
    with Add do
    begin
      Ext.Add('.arj');
      Archive := CreateInArchive(CLSID_CFormatArj);
    end;
    with Add do
    begin
      Ext.Add('.z');
      Ext.Add('.taz');
      Archive := CreateInArchive(CLSID_CFormatZ);
    end;
    with Add do
    begin
      Ext.Add('.lzh');
      Ext.Add('.lha');
      Archive := CreateInArchive(CLSID_CFormatLzh);
    end;
    with Add do
    begin
      Ext.Add('.7z');
      Archive := CreateInArchive(CLSID_CFormat7z);
    end;
    with Add do
    begin
      Ext.Add('.cab');
      Archive := CreateInArchive(CLSID_CFormatCab);
    end;
    with Add do
      Archive := CreateInArchive(CLSID_CFormatNsis);
    with Add do
    begin
      Ext.Add('.lzma');
      Ext.Add('.lzma86');
      Archive := CreateInArchive(CLSID_CFormatLzma);
    end;
    with Add do
    begin
      Ext.Add('.xz');
      Ext.Add('.txz');
      Archive := CreateInArchive(CLSID_CFormatXz);
    end;
    with Add do
    begin
      Ext.Add('.exe');
      Ext.Add('.dll');
      Ext.Add('.sys');
      Archive := CreateInArchive(CLSID_CFormatPe);
    end;
    with Add do
      Archive := CreateInArchive(CLSID_CFormatElf);
    with Add do
      Archive := CreateInArchive(CLSID_CFormatMacho);
    with Add do
    begin
      Ext.Add('.iso');
      Ext.Add('.img');
      Archive := CreateInArchive(CLSID_CFormatUdf);
    end;
    with Add do
    begin
      Ext.Add('.xar');
      Archive := CreateInArchive(CLSID_CFormatXar);
    end;
    with Add do
      Archive := CreateInArchive(CLSID_CFormatMub);
    with Add do
    begin
      Ext.Add('.hfs');
      Archive := CreateInArchive(CLSID_CFormatHfs);
    end;
    with Add do
    begin
      Ext.Add('.dmg');
      Archive := CreateInArchive(CLSID_CFormatDmg);
    end;
    with Add do
    begin
      Ext.Add('.msi');
      Ext.Add('.msp');
      Ext.Add('.doc');
      Ext.Add('.xls');
      Ext.Add('.ppt');
      Archive := CreateInArchive(CLSID_CFormatCompound);
    end;
    with Add do
    begin
      Ext.Add('.wim');
      Ext.Add('.swm');
      Archive := CreateInArchive(CLSID_CFormatWim);
    end;
    with Add do
    begin
      Ext.Add('.iso');
      Ext.Add('.img');
      Archive := CreateInArchive(CLSID_CFormatIso);
    end;
    with Add do
      Archive := CreateInArchive(CLSID_CFormatBkf);
    with Add do
    begin
      Ext.Add('.chm');
      Ext.Add('.chi');
      Ext.Add('.chq');
      Ext.Add('.chw');
      Ext.Add('.hxs');
      Ext.Add('.hxi');
      Ext.Add('.hxr');
      Ext.Add('.hxq');
      Ext.Add('.hxw');
      Ext.Add('.lit');
      Archive := CreateInArchive(CLSID_CFormatChm);
    end;
    with Add do
    begin
      Ext.Add('.001');
      Archive := CreateInArchive(CLSID_CFormatSplit);
    end;
    with Add do
    begin
      Ext.Add('.rpm');
      Archive := CreateInArchive(CLSID_CFormatRpm);
    end;
    with Add do
    begin
      Ext.Add('.deb');
      Archive := CreateInArchive(CLSID_CFormatDeb);
    end;
    with Add do
    begin
      Ext.Add('.cpio');
      Archive := CreateInArchive(CLSID_CFormatCpio);
    end;
    with Add do
    begin
      Ext.Add('.tar');
      Archive := CreateInArchive(CLSID_CFormatTar);
    end;
    with Add do
    begin
      Ext.Add('.gz');
      Ext.Add('.gzip');
      Ext.Add('.tgz');
      Ext.Add('.tpz');
      Archive := CreateInArchive(CLSID_CFormatGZip);
    end;
  end;
  for Item in F7zs do
    with Item do
      for S in Ext do
        Self.FExt.Add(S);
end;

destructor T7zArchive.Destroy;
begin
  if Assigned(F7zs) then
    FreeAndNil(F7zs);
  inherited;
end;

function PasswordCallback(Sender: Pointer;
  var Password: string): HRESULT; stdcall;
begin
  with T7zArchive(Sender) do
  begin
    if Assigned(OnPassword) then
      OnPassword(Sender, Password, FAbort);
  end;
  if not FAbort then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function ProgressCallback(Sender: Pointer; Total: Boolean;
  Value: Int64): HRESULT; stdcall;
begin
  with T7zArchive(Sender) do
  begin
    if Total then
      Max := Value
    else
    begin
      if Assigned(OnProgress) then
        OnProgress(Sender, Value, Max, FAbort);
    end;
  end;
  if not FAbort then
    Result := S_OK
  else
    Result := S_FALSE;
end;

procedure T7zArchive.Extract(const AFileName, APath: string);
var
  S: string;
  I: NativeInt;
  P: Pointer;
  List: TList;
  Item: T7zItem;
  Rslt: HRESULT;
begin
  inherited;
  Rslt := S_FALSE;
  S := LowerCase(ExtractFileExt(AFileName));
  List := TList.Create;
  try
    for Item in F7zs do
      if Item.Ext.IndexOf(S) > -1 then
        List.Insert(0, Item)
      else
        List.Add(Item);
    for P in List do
    begin
      with T7zItem(P), Archive do
      begin
        try
          OpenFile(AFileName);
        except
          Continue;
        end;
        for I := 0 to NumberOfItems - 1 do
          if ItemIsFolder[I] then
            ForceDirectories(IncludeTrailingPathDelimiter(APath) + ItemPath[I]);
        SetPasswordCallback(Pointer(Self), PasswordCallback);
        SetProgressCallback(Pointer(Self), ProgressCallback);
        try
          ExtractTo(APath);
        finally
          Close;
        end;
        Rslt := S_OK;
        Exit;
      end;
    end;
  finally
    FreeAndNil(List);
  end;
  if Rslt <> S_OK then
    raise Exception.Create(SUnknownFormat);
end;

{ TArchiveItem }

constructor TArchiveItem.Create(Collection: TCollection);
begin
  inherited;
  FArchive := nil;
end;

destructor TArchiveItem.Destroy;
begin
  if Assigned(FArchive) then
    FreeAndNil(FArchive);
  inherited;
end;

procedure TArchiveItem.Assign(Source: TPersistent);
begin
  if Source is TArchiveItem then
    with TArchiveItem(Source) do
    begin
      Self.FArchive := Archive;
    end
  else
    inherited;
end;

{ TArchiveItemsEnumerator }

constructor TArchiveItemsEnumerator.Create(ACollection: TArchiveItems);
begin
  inherited Create;
  FIndex := -1;
  FCollection := ACollection;
end;

function TArchiveItemsEnumerator.GetCurrent: TArchiveItem;
begin
  Result := FCollection[FIndex];
end;

function TArchiveItemsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCollection.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TArchiveItems }

function TArchiveItems.Add: TArchiveItem;
begin
  Result := TArchiveItem( inherited Add);
end;

constructor TArchiveItems.Create;
begin
  inherited Create(TArchiveItem);
end;

function TArchiveItems.Find(const S: string): TArchiveItem;
var
  I: NativeInt;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TArchiveItem( inherited GetItem(I));
    with Result do
      if Archive.Ext.IndexOf(S) > -1 then
        Exit;
  end;
  Result := nil;
end;

function TArchiveItems.GetEnumerator: TArchiveItemsEnumerator;
begin
  Result := TArchiveItemsEnumerator.Create(Self);
end;

function TArchiveItems.GetItem(Index: NativeInt): TArchiveItem;
begin
  Result := TArchiveItem( inherited GetItem(Index));
end;

procedure TArchiveItems.SetItem(Index: NativeInt; Value: TArchiveItem);
begin
  inherited SetItem(Index, Value);
end;

{ TInArchive }

constructor TInArchive.Create;
begin
  FArchives := TArchiveItems.Create;
  with FArchives do
  begin
    with Add do
      Archive := TZipArchive.Create;
    with Add do
      Archive := TCabArchive.Create;
    with Add do
      Archive := TRarArchive.Create;
    with Add do
      Archive := T7zArchive.Create;
  end;
end;

destructor TInArchive.Destroy;
begin
  if Assigned(FArchives) then
    FreeAndNil(FArchives);
  inherited;
end;

function TInArchive.Extract(const AFileName, APath: string): Boolean;
var
  S, T: string;
  P: Pointer;
  List: TList;
  Item: TArchiveItem;
  Rslt: HRESULT;
begin
  Result := True;
  FAbort := False;
  Rslt := S_FALSE;
  S := IncludeTrailingPathDelimiter(APath);
  T := LowerCase(ExtractFileExt(AFileName));
  List := TList.Create;
  try
    for Item in FArchives do
      with Item do
        if Archive.Ext.IndexOf(T) > -1 then
          List.Insert(0, Archive)
        else
          List.Add(Archive);
    for P in List do
    begin
      with TArchive(P) do
      begin
        try
          OnPassword := Self.FOnPassword;
          OnProgress := Self.FOnProgress;
          Extract(AFileName, S);
        except
          if FAbort then
          begin
            Result := False;
            Exit;
          end;
          Continue;
        end;
        Rslt := S_OK;
        Break;
      end;
    end;
  finally
    FreeAndNil(List);
  end;
  if Rslt <> S_OK then
    raise Exception.Create(SUnknownFormat);
end;

{ TOutArchive }

constructor TOutArchive.Create;
begin
  FFormat := afZip;
  FLevel := 5;
end;

destructor TOutArchive.Destroy;
begin
  //
  inherited;
end;

function ProgressCallbackEx(Sender: Pointer; Total: Boolean;
  Value: Int64): HRESULT; stdcall;
begin
  with TOutArchive(Sender) do
  begin
    if Total then
      Max := Value
    else
    begin
      if Assigned(OnProgress) then
        OnProgress(Sender, Value, Max, FAbort);
    end;
  end;
  if not FAbort then
    Result := S_OK
  else
    Result := S_FALSE;
end;

procedure TOutArchive.SaveToFile(const APath, AFileName: string);
var
  S, T: string;
  Guid: TGUID;
  Arch: I7zOutArchive;
begin
  FAbort := False;
  case FFormat of
    afZip:
      begin
        Arch := CreateOutArchive(CLSID_CFormatZip);
        with Arch do
        begin
          AddFiles(APath, '', '*.*', True);
          SetCompressionLevelAndMultiThreading(Arch, FLevel, 1);
          SetProgressCallback(Pointer(Self), ProgressCallbackEx);
          SaveToFile(AFileName);
        end;
      end;
    af7z:
      begin
        Arch := CreateOutArchive(CLSID_CFormat7z);
        with Arch do
        begin
          AddFiles(APath, '', '*.*', True);
          SetCompressionLevelAndMultiThreading(Arch, FLevel, 1);
          SetProgressCallback(Pointer(Self), ProgressCallbackEx);
          SaveToFile(AFileName);
        end;
      end;
    afTar:
      begin
        Arch := CreateOutArchive(CLSID_CFormatTar);
        with Arch do
        begin
          AddFiles(APath, '', '*.*', True);
          SetProgressCallback(Pointer(Self), ProgressCallbackEx);
          SaveToFile(AFileName);
        end;
      end;
    afTgz:
      begin
        CreateGUID(Guid);
        S := IncludeTrailingPathDelimiter(TPath.GetTempPath) + GUIDToString(Guid);
        T := IncludeTrailingPathDelimiter(S) + ChangeFileExt(ExtractFileName(AFileName), '.tar');
{$IF CompilerVersion > 22.9}
        System.SysUtils.ForceDirectories(S);
{$ELSE}
        SysUtils.ForceDirectories(S);
{$IFEND}
        Arch := CreateOutArchive(CLSID_CFormatTar);
        with Arch do
        begin
          AddFiles(APath, '', '*.*', True);
          SetProgressCallback(Pointer(Self), ProgressCallbackEx);
          SaveToFile(T);
        end;
        Arch := CreateOutArchive(CLSID_CFormatGZip);
        with Arch do
        begin
          AddFile(T, '');
          SetCompressionLevel(Arch, FLevel);
          SetProgressCallback(Pointer(Self), ProgressCallbackEx);
          SaveToFile(AFileName);
        end;
        if DirectoryExists2(S) then
          TDirectory.Delete(S, True);
      end;
    afTbz2:
      begin
        CreateGUID(Guid);
        S := IncludeTrailingPathDelimiter(TPath.GetTempPath) + GUIDToString(Guid);
        T := IncludeTrailingPathDelimiter(S) + ChangeFileExt(ExtractFileName(AFileName), '.tar');
{$IF CompilerVersion > 22.9}
        System.SysUtils.ForceDirectories(S);
{$ELSE}
        SysUtils.ForceDirectories(S);
{$IFEND}
        Arch := CreateOutArchive(CLSID_CFormatTar);
        with Arch do
        begin
          AddFiles(APath, '', '*.*', True);
          SetProgressCallback(Pointer(Self), ProgressCallbackEx);
          SaveToFile(T);
        end;
        Arch := CreateOutArchive(CLSID_CFormatBZ2);
        with Arch do
        begin
          AddFile(T, '');
          SetCompressionLevelAndMultiThreading(Arch, FLevel, 1);
          SetProgressCallback(Pointer(Self), ProgressCallbackEx);
          SaveToFile(AFileName);
        end;
        if DirectoryExists2(S) then
          TDirectory.Delete(S, True);
      end;
    afTxz:
      begin
        CreateGUID(Guid);
        S := IncludeTrailingPathDelimiter(TPath.GetTempPath) + GUIDToString(Guid);
        T := IncludeTrailingPathDelimiter(S) + ChangeFileExt(ExtractFileName(AFileName), '.tar');
{$IF CompilerVersion > 22.9}
        System.SysUtils.ForceDirectories(S);
{$ELSE}
        SysUtils.ForceDirectories(S);
{$IFEND}
        Arch := CreateOutArchive(CLSID_CFormatTar);
        with Arch do
        begin
          AddFiles(APath, '', '*.*', True);
          SetProgressCallback(Pointer(Self), ProgressCallbackEx);
          SaveToFile(T);
        end;
        Arch := CreateOutArchive(CLSID_CFormatXz);
        with Arch do
        begin
          AddFile(T, '');
          SetCompressionLevelAndMultiThreading(Arch, FLevel, 1);
          SetProgressCallback(Pointer(Self), ProgressCallbackEx);
          SaveToFile(AFileName);
        end;
        if DirectoryExists2(S) then
          TDirectory.Delete(S, True);
      end;
    afWim:
      begin
        Arch := CreateOutArchive(CLSID_CFormatWim);
        with Arch do
        begin
          AddFiles(APath, '', '*.*', True);
          SetProgressCallback(Pointer(Self), ProgressCallbackEx);
          SaveToFile(AFileName);
        end;
      end;
  end;
end;

end.
