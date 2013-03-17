unit PdfParse;
{$mode objfpc}{$H+}
interface

uses PdfObjs,pdfNotifiers;

const

  pdf_White_space_chars = [#0, #8, #9, #10, #12, #13, #32];
  pdf_Delimiter_chars = ['[', ']', '<', '>', '(', ')', '/', '%'];
  all_Delimiter_chars = [#0, #8, #9, #10, #12, #13, #32, '[', ']', '<', '>', '(', ')', '/', '%'];
  pdf_Digits = ['.', '0'..'9'];
  pdf_Numeric = ['+', '-', '.', '0'..'9'];

  LF = #10;
  CR = #13;

  CRLF = #13#10;


type

  char_set= set of char;

  pdfParser = class
  private
    FBuffer: PChar;
       FPos: PtrUInt;
   FBufSize: PtrUInt;
FProgressor: TProgress;
  public
    function EOF: Boolean;
    function BOF: Boolean;
    function Next : Boolean;
    function Prev : Boolean;
    function Current: Char;
    function PeekNext: Char;
    function PeekPrev: Char;
    procedure Jump(NewPos: PtrUInt);
    procedure Skip(Num: integer);
    function SkipBlank:Char;
    procedure SkipTo(C: char);
    procedure SkipTo(const Chars: char_set);
    procedure SkipTo(const Str: string);
    function  GetToken:String;

    function GetInteger(out num: integer): boolean;
    function GetUInt(out num: PtrUInt): boolean;
    function GetLine: string;

    function Compare(const Txt: string): boolean;

    function ParseObj : PPdfObj;
   // function ParseDictionary(const TheContainer: TPdfVar):pdfDataType;
    function ParseArray : PPdfObj;
   { function ParseHexString(const TheContainer: TPdfVar):pdfDataType;
    function ParseString(const TheContainer: TPdfVar):pdfDataType;
    function ParseName(const TheContainer: TPdfVar):pdfDataType;
    function ParseRef(const TheContainer: TPdfVar):pdfDataType;
    function ParseNumeric(const TheContainer: TPdfVar):pdfDataType;
    function ParseBoolean(const TheContainer: TPdfVar):pdfDataType; }
    constructor Create(ABuffer: Pointer; ABufSz: PtrUInt;AProgressor: TProgress = nil);
    destructor Destroy; override;


    //              function ReadObject(Offset: Integer=0): pdfObject;

    function FindStartXRef: PtrUInt;
    property Position: PtrUInt read FPos;
    property BufferSize: PtrUInt read FBufSize;
  end;


implementation

uses Classes, SysUtils;

// Parser Class
constructor PdfParser.Create(ABuffer: Pointer; ABufSz: PtrUInt;AProgressor: TProgress = nil);
begin
  inherited Create;
  FBuffer := ABuffer;
  FPos := 0;
  FBufSize := ABufSz;
  FProgressor := AProgressor;
end;

destructor PdfParser.Destroy;
begin
  FProgressor.Free;
  inherited Destroy;
end;

function pdfParser.EOF:boolean;inline;
begin
  Result:=not Position<FBufSize-1;
end;

function pdfParser.BOF:boolean;inline;
begin
  Result:=Position=0;
end;

function pdfParser.Current:Char;inline;
begin
  Result:=FBuffer[Position];
end;

function pdfParser.PeekNext: Char;inline;
begin
  if not EOF then result:=FBuffer[Position+1]
             else result:=#0
end;

function pdfParser.PeekPrev:Char;inline;
begin
 if not BOF then result:=FBuffer[Position-1]
            else result:=#0
end;


function pdfParser.Next : Boolean;inline;
begin
  Result:=Not EOF;
  if Result then Inc(FPos);
end;

function pdfParser.Prev : Boolean;inline;
begin
  Result:=not BOF;
  if Result then Dec(FPos);
end;

procedure pdfParser.Jump(NewPos: PtrUInt); inline;
begin
  FPos := NewPos;
end;

procedure pdfParser.Skip(Num: Integer); inline;
begin
  Inc(FPos, Num);
end;

function pdfParser.SkipBlank:Char;inline;
begin
  while (Current in pdf_white_space_chars) and Next do;
  Result := Current
end;


procedure pdfParser.SkipTo(C: Char);inline;
begin
  while (Current <> C) and Next do;
end;

procedure pdfParser.SkipTo(const Chars:char_set );inline;
begin
   while (not (Current in chars)) and Next do;
end;

procedure pdfParser.SkipTo(const Str: string);
begin
  while True do
  begin
    SkipTo(Str[1]);
    if Compare(Str) then Break
                    else Next
  end
end;

function pdfParser.GetToken:String;
var TokenStart: PtrUInt;
begin
 TokenStart:=Position;
 while (not (Current in all_delimiter_chars)) and Next do;
 SetString(Result,@FBuffer[TokenStart],Position-TokenStart);
end;

function pdfParser.GetLine:String;
var
  LineStart: PtrUInt;
begin
  LineStart := Position;
  SkipTo(CR);
  SetString(Result, @FBuffer[LineStart], Position - LineStart);
  if PeekNext = LF then Next;
end;

function pdfParser.Compare(const Txt: string): boolean;
var
  i, sz: integer;
begin
  i := 0;
  sz := Length(txt);
  while (Current = Txt[i + 1]) and next do
    if i < sz then inc(i) else break;

  Result := i = sz;
end;


{function pdfParser.ReadObject(Offset: Integer=0): pdfObject;
var ObjNum,ObjGen: Integer;
begin
// PdfNotify(ntInfo,'begin read object ->>',nil);
 result:=nil;
 if Offset<>0 then Jump(Offset);

 if (not GetInteger(ObjNum)) then exit;

 if not(GetInteger(ObjGen)) then exit;
                            https://dl.dropbox.com/u/65215004/Badem%20Book.zip
 SkipBlank;

 if not Compare('obj') then exit;

 SkipBlank;

 Result:=pdfObject.Create(ObjNum,ObjGen,Offset,ReadVar);

// PdfNotify(ntInfo,'<--end read.',nil);
end;
}

const ParseErrorMsg='Parsing of object failed!!! Expecting Valid TOKEN but "%s" found near "%s" at offset %d';

function pdfParser.ParseObj: PPdfObj;
begin
  Result := nil;

  case SkipBlank of

 //   '<': if PeekNext = '<' then Result := ParseDictionary;
 //                          else Result := ParseHexString(TheContainer);

    '[': Result := ParseArray;
 {   '(': Result := ParseString(TheContainer);
    '/': Result := ParseName(TheContainer);
    '.',
    '-',
    '+': Result := ParseNumeric(TheContainer);
    '0'..'9':
    begin
      Result := ParseRef(TheContainer);
      if Result = pdfInvalid then Result := ParseNumeric(TheContainer);
    end;
    'n': if Compare('null') then Result:=TheContainer.InitNull;

    'F', 'T', 'f', 't': Result := ParseBoolean(TheContainer);
    }
  end;

  if Result = nil then PdfNotify(ntWarnning, format(ParseErrorMsg,
      [@FBuffer[Position], Copy(FBuffer[Position- 8], 1, 64), Position]),nil);


  if FProgressor <> nil then FProgressor.DoSubProgress(1);
end;



function pdfParser.ParseArray: PPdfObj;
var NewItem: PPdfObj;
begin
  if Current <> '[' then exit(nil);

  Result:=TPdfObj.NewArray;

  Next;
  while (SkipBlank <> ']') do
   begin
     NewItem:=ParseObj;
     if NewItem=nil then
      begin
        TPdfObj.Release(Result);
        exit;
      end
       else Result^.AddItem(NewItem)
   end;
  Next
end;


function pdfParser.ParseDictionary(const TheContainer: TPdfVar): pdfDataType;
var
 M: TMemoryStream;
 NewItem: PPDFVar;
begin
  if not((Current= '<') and (PeekNext = '<')) then exit(pdfInvalid);
  Skip(2);
  Result:=TheContainer.InitDict;

  while (SkipBlank<> '>') do
  begin
    if Current = '/' then
       begin
         Next;
         NewItem:=TPdfVar.New;
         NewItem^.Key:=pdfNamesManager.Add(GetToken);
         Result:=ParseVar(NewItem^);
         if Result=pdfInvalid then
          begin
           NewItem^.Dispose;
           exit;
         end
          else TheContainer.AddItem(NewItem);
       end
        else PdfNotify(ntwarnning, 'Cannot Read Item Name!', nil)
   end;

  // okay we have the first > of the end of the dictionary let's check for the next >
  Next;

  if Current= '>' then next
   else PdfNotify(ntwarnning, format('Wrong Char - Expecting ">" but %s found', [FBuffer[FCursor]]),nil);

  SkipBlank;

  Result:=pdfDict;

{  if Compare('stream') then
  begin
    if (FCursor^ = #13) then
      Inc(FCursor); // skip the cr
    if (FCursor^ = #10) then
      Inc(FCursor) // skip the lf
    else
      PdfNotify(ntwarnning, 'Missing LF before Stream Content!!!', nil);

    L := Result['/Length'].AsInteger;

    M := TMemoryStream.Create;

    M.SetSize(L);

    Move(FCursor^, M.Memory^, L);

    Result.SetToStream(M);
    Skip(L);
  end;}
end;

function pdfParser.ParseString(const TheContainer: TPdfVar):pdfDataType;
var
  StrStart: PtrUInt;
begin
  if Current <> '(' then Exit(pdfInvalid);
  Next;
  StrStart := FCursor;
  while True do
  begin
    SkipTo(')');
    if PeekPrev <> '\' then Break; // check if this is an escape \)
    Inc(FCursor);
  end;
  Result:=TheContainer.Init(pdfStr,Copy(FBuffer[StrStart],0,FCursor - StrStart));
  Next;
end;

function pdfParser.ParseHexString(const TheContainer: TPdfVar):pdfDataType;
var
  StrStart: PtrUInt;
begin
  if Current <> '<' then Exit(pdfInvalid);
  Next;
  StrStart := FCursor;
  SkipTo('>');
  Result:=TheContainer.Init(pdfHexStr,Copy(FBuffer[StrStart],0,FCursor - StrStart));
  Next;
end;

function pdfParser.ParseName(const TheContainer: TPdfVar):pdfDataType;
begin
  if Current <> '/' then Exit(pdfInvalid);
  Next;
  Result:=PdfName;
  Result:=TheContainer.Init(Result,pdfNamesManager.Add(GetToken));
end;

function pdfParser.ParseBoolean(const TheContainer: TPdfVar):pdfDataType;
var
   b:boolean;
   s: String;
begin
  if (Current in ['F', 'T', 'f', 't']) then
    begin
      S:=UpperCase(GetToken);
      if (S <> 'TRUE') and (S <> 'FALSE') then  exit(pdfInvalid);
      b:=S = 'TRUE';             LConvEncoding
      Result:=TheContainer.Init(pdfBool,b);
    end
     else Result:=pdfInvalid
end;

function Char2Num(C: char): byte; inline;
begin
  Result := Ord(c) - Ord('0');
end;

function pdfParser.ParseNumeric(const TheContainer: TPdfVar):pdfDataType;
var
  NumStart: PtrUInt;
  isNeg: boolean;
  Scale: extended;
  xi: Single;
  i: Integer;
begin
  if  Current in pdf_numeric then Result:=pdfInt
                             else exit(pdfInvalid);

  isNeg := Current = '-';
  if isNeg then Next;
  if Current = '+' then Next;

  Scale := 10;
  NumStart := FCursor;
  xi := 0;
  while Current in Pdf_Digits do
  begin
    case Current of
      '.':
      begin
        if Result=pdfFloat then  exit(pdfInvalid)
                          else Result:=pdfFloat;
        Scale := 0.1;
        Inc(FCursor);
      end;
      '0'..'9':
      begin
        xi := xi * scale + Char2Num(Current);
        Inc(FCursor);
      end;
    end;
  end;

  if FCursor = NumStart then exit(pdfInvalid);

  if isNeg then  xi := -xi;

  if Result = pdfInt then
     begin
       i:=Trunc(xi);
       TheContainer.Init(Result,i)
     end
  else
   TheContainer.Init(Result,xi)
end;

function pdfParser.GetInteger(out num: integer): boolean;
var
  xi: integer;
  isNeg: boolean;
  NumStart: PtrUInt;
  digit: char;

begin
  Result := False;
  IsNeg := False;

  SkipTo(pdf_numeric);

  if Current = '-' then //check for a sign in the begining
  begin
    IsNeg := True;
    Next;
  end
  else if Current = '+' then next;

  xi := 0;
  NumStart := FCursor;

  repeat
    digit := Current;
    if not (digit in ['0'..'9']) then break;
    xi := (xi * 10);
    Dec(digit, Ord('0'));
    Inc(xi, Ord(digit));
    Inc(FCursor);
  until (False);

  if (NumStart = FCursor) then exit;
  if IsNeg then  xi := -xi;
  Num := xi;
  Result := True;
end;


function pdfParser.GetUInt(out num: PtrUInt): boolean;
var
  xi: PtrUInt;
  NumStart: PtrUInt;
  digit: char;

begin
  Result := False;

  SkipTo(['0'..'9']);

  xi := 0;
  NumStart := FCursor;

  repeat
    digit := Current;
    if not (digit in ['0'..'9']) then break;
    xi := (xi * 10);
    Dec(digit, Ord('0'));
    Inc(xi, Ord(digit));
    Inc(FCursor);
  until (False);

  if (NumStart = FCursor) then exit;

  Num := xi;
  Result := True;
end;
// if the format is not NNNN NN R then the cursor is not incremented to allow read of an numeric value on failure
function pdfParser.ParseRef(const TheContainer: TPdfVar):pdfDataType;
var
  Start: PtrUInt;
    Ref: TPdfRef;
begin
  Start := FCursor;
  if GetUInt(Ref.FNum) and GetUInt(Ref.FGen) and Compare(' R') then Result:=TheContainer.Init(pdfRef,Ref)
   else
     begin
       Result:=pdfInvalid;
       Jump(Start)
    end;
end;


function pdfParser.FindStartXRef: PtrUInt;
var i: Integer;
begin
  Result := -1;
  jump(BufferSize - 1);
  repeat
    while (Current <> 'f') do Prev;
    if StrLComp(@FBuffer[FCursor - 8], 'startxref', 9) = 0 then  break;
    Prev;
  until False;
  if GetInteger(i) then Result :=i
                   else Result :=0;
end;


end.

