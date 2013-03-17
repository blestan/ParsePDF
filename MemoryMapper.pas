{ $Id: MemoryMapper.pas 507 2002-04-18 19:12:19Z jamesk $}
{
  UNIT
  MemoryMapper

  SYNOPSIS
  Memory mapping for the common man.

  DESCRIPTION
  This is meant to be a platform-neutral method to easily map a file to a
  pointer.

  Memory mapping is highly OS-dependent, but a programmer normally wants a
  simple way to map a file, use the mapping, and discard the mapping. The end
  result should be the same for the programmer regardless of OS.

  The current implementation is for read-only access. Read/write access can be
  easily added to the memory-mapping call. If changes to the interface is
  needed, such as for forcing synchronization, one ought to leave the original
  read-only ImmMemoryMap interface untouched.

  USE
  1) Use the class factory CommMemoryMap.CreateInstance to obtain a valid
     instance of ImmMemoryMap.
  2) Assign the filename to be mapped to the FileName property. An exception
     may be thrown upon failure.
  3) The prudently paranoid should then check the IsValid property to ensure
     that it is True.
  4) Read the Data property to obtain the address of the start of the file. 
  5) One should check the Size property to determine the boundaries of the
     memory mapping.
  6) When the mapping is no longer needed, it may be destroyed by the usual
     interface disposal methods (e.g. explicit assignment to nil or implicitly).

  CAVEATS
  * This only handles files up to ~2GB due to the IA32 architecture address
    limitations. One could work around this by mapping to a specific location
    and remapping. (Rework of the classes required.) 

  LEGAL
  Copyright 1997-1999 James Knowles
  Copyright 1999-2001 IFM Services, LLC 

  This software is provided 'as-is', without any express or implied warranty
  except where provided for by a valid and legally binding contract with
  IFM Services, LLC. In no event will IFM Services, LLC be held liable for any
  damages arising from the use of this software.

  Persons and organizations are granted a royalty-free, non-exclusive licence to
  this software and may alter it freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not claim
     that you wrote the original software. If you use this software in a
     product, an acknowledgment in the product documentation and/or the program
     credits would be appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.

  3. This notice may not be removed or altered from any source distribution.

  SEE ALSO

  REVISION HISTORY
  2002-02-18: James Knowles
  * Removed small inefficiencies.
  * Fixed Windows bug (not flushing before unmapping)
  * Made note in documentation about the fact that this doesn't handle files
    bigger than the IA32 memory address limitations (~2GB). 
  2001-03-12: James Knowles
    Adapted for Kylix.
}

unit MemoryMapper;

interface

type
  ImmMemoryMap = interface
  ['{8A466A7C-C72C-494B-9F35-2DF2A526546B}']
    function GetData: Pointer;
    function GetIsReadOnly: Boolean;
    function GetIsValid: Boolean;
    function GetSize: Int64;
    procedure SetFileName(const Value: string);
    function GetFileName: string;
    property Data: Pointer read GetData;
    property IsReadOnly: Boolean read GetIsReadOnly;
    property IsValid: Boolean read GetIsValid;
    property Size: Int64 read GetSize;
    property FileName: string read GetFileName write SetFileName;
  end;

  CommMemoryMap = class
  public
    class function CreateInstance(const aFileName:string=''; aReadOnly:Boolean=True): ImmMemoryMap;
  end;

implementation

{$UNDEF CodeIsTested}
{$IFDEF MSWINDOWS}
  {$DEFINE CodeIsTested}
{$ENDIF}

{$IFDEF LINUX}
  {$DEFINE CodeIsTested}
{$ENDIF}

{$IFNDEF CodeIsTested}
  {$MESSAGE ERROR 'MemoryMapper not tested for this OS!'}
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
// MEMORY MAPPING IS HIGHLY OS-DEPENDENT. THIS FILE WILL BE TWO BIG SECTIONS
// FOR EACH OS, SURROUNDED BY {$IFDEF}.
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//  Linux version of memory mapping.
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{$IFDEF LINUX}

uses
  SysUtils,
  Libc;

const
  INVALID_HANDLE = -1;
  INVALID_SIZE = -1;

type
  TmmMemoryMapLinux = class(TInterfacedObject, ImmMemoryMap)
    function GetData: Pointer;
    function GetFileName: string;
    function GetIsReadOnly: Boolean;
    function GetIsValid: Boolean;
    function GetSize: Int64;
    procedure SetFileName(const Value: string);
  private
    FData: Pointer;
    FFileName: string;
    FIsReadOnly: Boolean;
    FIsValid: Boolean;
    FSize: Int64;
    fd: Integer; // Open file descriptor
    procedure Reset;
    procedure MapFile;
    procedure UnmapFile;
  public
    constructor Create(aReadOnly:Boolean=True);
    destructor Destroy; override;
    property Data: Pointer read FData;
    property FileName: string read FFileName write SetFileName;
    property IsReadOnly: Boolean read FIsReadOnly;
    property IsValid: Boolean read FIsValid;
    property Size: Int64 read FSize;
  end;

constructor TmmMemoryMapLinux.Create(aReadOnly: Boolean);
begin
  inherited Create;
  FIsReadOnly := aReadOnly;
end;

destructor TmmMemoryMapLinux.Destroy;
begin
  UnmapFile;
  inherited;
end;

function TmmMemoryMapLinux.GetData: Pointer;
begin
  Result := FData;
end;

function TmmMemoryMapLinux.GetFileName: string;
begin
  Result := FFileName;
end;

function TmmMemoryMapLinux.GetIsReadOnly: Boolean;
begin
  Result := IsReadOnly;
end;

function TmmMemoryMapLinux.GetIsValid: Boolean;
begin
  Result := FIsValid;
end;

function TmmMemoryMapLinux.GetSize: Int64;
begin
  Result := FSize;
end;

procedure TmmMemoryMapLinux.MapFile;
var
  sb :TStatBuf;

  procedure Fail(const Msg :string);
  begin
    UnmapFile;
    raise EInOutError.Create(ClassName+'.MapFile: '+Msg);
  end;

begin
  Reset;

  //  Get file descriptor
  if IsReadOnly then
    fd := open(PChar(FileName), O_RDONLY)
  else
    fd := open(PChar(FileName), O_RDWR);
  if fd=INVALID_HANDLE then Fail('Cannot open file.');
  
  //  Sanity check/get size of file
  if fstat(fd,sb)=0 then
    FSize := sb.st_size
  else
    Fail('Cannot stat file.');

  //  Create memory map of file for direct access
  if IsReadOnly then
    FData := mmap(nil, FSize, PROT_READ, MAP_SHARED, fd, 0)
  else
    FData := mmap(nil, FSize, PROT_READ or PROT_WRITE, MAP_SHARED, fd, 0);
  if FData=MAP_FAILED then Fail('Cannot create file mapping');

  //  All done
  FIsValid := True;
end;

procedure TmmMemoryMapLinux.Reset;
begin
  FData    := MAP_FAILED;
  FIsValid := False;
  FSize    := INVALID_SIZE;
  fd       := INVALID_HANDLE;
end;

procedure TmmMemoryMapLinux.SetFileName(const Value: string);
begin
  if FFileName<>Value then
  begin
    UnmapFile;
    FFileName := Value;
    MapFile;
  end;
end;

procedure TmmMemoryMapLinux.UnmapFile;
begin
  if FData<>MAP_FAILED then
  begin
    if not IsReadOnly then msync(FData, FSize, MS_SYNC);
    munmap(FData, FSize);
  end;
  if fd<>INVALID_HANDLE then __close(fd);
  Reset;
end;

////////////////////////////////////////////////////////////////////////////////
//
// CLASS FACTORY
//
////////////////////////////////////////////////////////////////////////////////

class function CommMemoryMap.CreateInstance(const aFileName:string; aReadOnly:Boolean): ImmMemoryMap;
begin
  Result := TmmMemoryMapLinux.Create(aReadOnly);
  if aFileName<>'' then Result.FileName := aFileName;
end;

{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
//
//  Win32 version of memory mapping.
//
////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{$IFDEF MSWINDOWS}

uses
  SysUtils,
  Windows;

const  
  INVALID_SIZE = -1;

type
  TmmMemoryMapWin32 = class(TInterfacedObject, ImmMemoryMap)
    function GetData: Pointer;
    function GetFileName: string;
    function GetIsReadOnly: Boolean;
    function GetIsValid: Boolean;
    function GetSize: Int64;
    procedure SetFileName(const Value: string);
  private
    FData: Pointer;
    FFileName: string;
    FIsReadOnly: Boolean;
    FIsValid: Boolean;
    FSize: Int64;
    hFile: THandle;
    hMap: THandle;
    procedure Reset;
    procedure MapFile;
    procedure UnmapFile;
  public
    constructor Create(aReadOnly:Boolean=True);
    destructor Destroy; override;
    property Data: Pointer read FData;
    property FileName: string read FFileName write SetFileName;
    property IsReadOnly: Boolean read FIsReadOnly;
    property IsValid: Boolean read FIsValid;
    property Size: Int64 read FSize;
  end;

constructor TmmMemoryMapWin32.Create(aReadOnly: Boolean);
begin
  inherited Create;
  FIsReadOnly := aReadOnly;
end;

destructor TmmMemoryMapWin32.Destroy;
begin
  UnmapFile;
  inherited;
end;

function TmmMemoryMapWin32.GetData: Pointer;
begin
  Result := FData;
end;

function TmmMemoryMapWin32.GetFileName: string;
begin
  Result := FFileName;
end;

function TmmMemoryMapWin32.GetIsReadOnly: Boolean;
begin
  Result := IsReadOnly;
end;

function TmmMemoryMapWin32.GetIsValid: Boolean;
begin
  Result := FIsValid;
end;

function TmmMemoryMapWin32.GetSize: Int64;
begin
  Result := FSize;
end;

procedure TmmMemoryMapWin32.MapFile;

  procedure Fail(const Msg :string);
  begin
    UnmapFile;
    raise EInOutError.Create(ClassName+'.MapFile: '+Msg);
  end;

begin
  Reset;

  //  Get Win32 file handle
  if IsReadOnly then
    hFile := CreateFile(PChar(FileName), GENERIC_READ, FILE_SHARE_READ, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0)
  else
    hFile := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE, 0, nil,
      OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  if hFile=INVALID_HANDLE_VALUE then Fail('Cannot open file.');

  //  Get size of file
  FSize := Windows.GetFileSize(hFile, nil);

  //  Create memory map of file for direct access
  if IsReadOnly then
  begin
    hMap := CreateFileMapping(hFile, nil, PAGE_READONLY, 0, 0, nil);
    if hMap=INVALID_HANDLE_VALUE then Fail('Cannot create file mapping');
    FData := MapViewOfFile(hMap, FILE_MAP_READ, 0, 0, 0);
    if FData=nil then Fail('Cannot map view of file');
  end
  else
  begin
    hMap := CreateFileMapping(hFile, nil, PAGE_READWRITE, 0, 0, nil);
    if hMap=INVALID_HANDLE_VALUE then Fail('Cannot create file mapping');
    FData := MapViewOfFile(hMap, FILE_MAP_READ or FILE_MAP_WRITE, 0, 0, 0);
    if FData=nil then Fail('Cannot map view of file');
  end;

  //  All done
  FIsValid := True;
end;

procedure TmmMemoryMapWin32.Reset;
begin
  FData := nil;
  FIsValid := False;
  FSize := INVALID_SIZE;
  hFile := INVALID_HANDLE_VALUE;
  hMap  := INVALID_HANDLE_VALUE;
end;

procedure TmmMemoryMapWin32.SetFileName(const Value: string);
begin
  if FFileName<>Value then
  begin
    FFileName := Value;
    UnmapFile;
    MapFile;
  end;
end;

procedure TmmMemoryMapWin32.UnmapFile;
begin
  if FData<>nil then
  begin
    if not IsReadOnly then FlushViewOfFile(FData,0);
    UnmapViewOfFile(FData);
  end;
  if hMap<>INVALID_HANDLE_VALUE then CloseHandle(hMap);
  if hFile<>INVALID_HANDLE_VALUE then CloseHandle(hFile);
  Reset;
end;

////////////////////////////////////////////////////////////////////////////////
//
// CLASS FACTORY
//
////////////////////////////////////////////////////////////////////////////////

class function CommMemoryMap.CreateInstance(const aFileName:string; aReadOnly:Boolean): ImmMemoryMap;
begin
  Result := TmmMemoryMapWin32.Create(aReadOnly);
  if aFileName<>'' then Result.FileName := aFileName;
end;

{$ENDIF}

end.

