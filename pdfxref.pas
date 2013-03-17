unit pdfXRef;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
interface


uses classes,pdfVars,pdfParse;

type

  // Standard - "text" representatioion of the XRef record

  PRawXRefLine=^TRawXRefLine;
  TRawXRefLine=record
              Offset: array[0..9] of char; // 10 digits object offset in the file
              Sp1: Char;                   // 1 space
              Gen: Array[0..4] of char;    // 5 digits object genetration number
              Sp2: Char;                   // 1 space
                n: Char;                   // 1 char "n" or "f"
              Eol: Array[0..1] of char;    // CR+LF
  end;




  TXRefSection= class(TObject)
     private
          FTrailer: TPDFVar;
          FNextSection: TXRefSection;
          FStart: integer;
          FOrderStart: Integer;
          FCount: integer;
          FObjects: PIndirectObject;
          function  GetXRef(ObjectNum: integer): PIndirectObject;
          procedure SetXRef(ObjectNum: Integer;  ARef: PIndirectObject);
          function IndexOf(ObjectNum: integer): integer;
          procedure InternalFree;
    public
     constructor Create(AStart,ACount: Integer);
     constructor Parse(Parser: pdfParser);
     destructor Destroy;override;
     function Contains(ObjectNum:Integer):boolean;
     property pdfObject[index: integer]:PIndirectObject read GetXRef Write SetXRef;default;
   end;

   TXRefTable=class(TObject)
             private
              FSections: TXRefSection;
              FLastSection: TXRefSection;

              FRootVar: TPDFVar;
              FObjectsCount: Integer;
              FMaxObjectNum: Integer;
              FMinObjectNum: Integer;

              FCachedSection: TXRefSection; // Last Accessed section for faster item iterration

              procedure InternalClearList;

              function GetXRefByObjectNumber(ObjectNum: integer): PIndirectObject;
              function GetXRefByOrder(Index: Integer): PIndirectObject;

             public
              constructor Create;
              destructor Destroy;override;
              procedure AddSection(Section: TXRefSection);
              procedure Parse(parser: pdfParser);
              property RootVar: TPDFVar read FRootVar;
              property Count: integer read FObjectsCount;
              property XObjects[ObjectNumber: Integer]: PIndirectObject read GetXRefByObjectNumber;default; // returns the xref of the given object number
              property Items[index: integer]: PIndirectObject read GetXRefByOrder;  // for iterration purposes only
  end;


implementation

uses sysutils,pdfNotifiers,pdfContainers;

// XRefSection Class;

Constructor TXRefSection.Create(AStart: Integer; ACount: Integer);
begin
  Inherited Create;
  FTrailer.Init(ptDictionary,pdfDictionary.Create);
  FNextSection:=nil;
  FCount:=ACount;
  FStart:=AStart;
  FOrderStart:=0;
  FObjects:=nil;
  if FCount>0  then GetMem(FObjects,FCount*SizeOf(TIndirectObject));
end;

procedure TXRefSection.InternalFree;
var i: integer;
begin
  if FObjects=nil then exit; // nothing to do
  for i := 0 to FCount - 1 do FObjects[i].Obj.Done;
  FreeMem(FObjects);
end;

Destructor TXRefSection.Destroy;
begin
  FTrailer.Done;
  InternalFree;
  Inherited Destroy;
end;

Constructor TXRefSection.Parse(parser: PdfParser);
var Raw: PRawXRefLine;
    i,xi: integer;
       j: byte;
begin
  Inherited Create;
  FTrailer.Done;
  FCount:=-1;
  FStart:=-1;
  FOrderStart:=0;
  FObjects:=nil;

 //try to get base object number
 if not parser.GetInteger(FStart) then exit;

 //try to get objects count
 if not parser.GetInteger(FCount) then exit;

 if FCount>0  then GetMem(FObjects,FCount*SizeOf(TIndirectObject))
              else exit;

 parser.SkipTo(['0'..'9']); // Find The First XRef Start

 Raw:=PRawXRefLine(parser.Cursor); //just a holder for typecast

 for i := 0 to FCount - 1 do
 with raw[i] do
  begin
   FObjects[i].number:=FStart+i;

   xi:=00;
   for j := 0 to SizeOf(Offset) - 1 do
     xi:=xi*10 + (ord(Offset[j])-ord('0'));
   FObjects[i].offset:=xi;

   xi:=00;
   for j := 0 to SizeOf(Gen) - 1 do
     xi:=xi*10 + (ord(Gen[j])-ord('0'));

   FObjects[i].Generation:=xi;

   FObjects[i].n:=n='n';

   FObjects[i].Obj.init(true);
  end;

 parser.Skip(SizeOf(RawXRefLine)*FCount); // jump at the end of xref
end;

function TXRefSection.IndexOf(ObjectNum: Integer):integer;
var i: integer;
begin

  i:=ObjectNum-FStart;

  if (i>-1) and (i<FCount) then result:=i
                           else result:=-1;
end;

function TXRefSection.Contains(ObjectNum: Integer): boolean;
begin
 result:=IndexOf(ObjectNum)<>-1;
end;

function TXRefSection.GetXRef(ObjectNum: Integer):PIndirectObject;
var i: Integer;
begin
 result:=nil;
 i:=IndexOf(ObjectNum);
 if i<>-1 then Result:=@FObjects[i];
end;

procedure TXRefSection.SetXRef(ObjectNum: Integer;  ARef: PIndirectObject);
var i: Integer;
begin
 i:=IndexOf(ObjectNum);
 if (i<>-1) and (ARef<>nil) then FObjects[i]:=ARef^;
end;

// TXRefTable;

constructor TXRefTable.Create;
begin
 inherited create;
 FSections:=nil;
 FLastSection:=nil;
 FCachedSection:=nil;
 FMaxObjectNum:=0;
 FMinObjectNum:=high(Integer);
 InternalClearList;
 FRootVar.Init(false);
end;


destructor TXRefTable.Destroy;
begin
  InternalClearList;
  inherited Destroy;
end;

procedure TXRefTable.InternalClearList;
var Section,Next: TXRefSection;
begin
 Section:= FSections;
 while Section<>nil do
  begin
    Next:=Section.FNextSection;
    Section.Free;
    Section:=Next;
  end;
 FObjectsCount:=0;
end;

function TXRefTable.GetXRefByObjectNumber(ObjectNum: integer): PIndirectObject;
var Section: TXRefSection;
begin
 Section:=FSections;
 Result:=nil;
 while Section<>nil do
  begin
   Result:=Section.GetXRef(ObjectNum);
   if Result<>nil then exit;
   Section:=Section.FNextSection;
  end;
end;

function TXRefTable.GetXRefByOrder(Index: Integer): PIndirectObject;
var Section : TXRefSection;
        Cnt : Integer;

begin
 Result:=nil;
 if (index<0) or (index>FObjectsCount-1) then exit;

 if (FCachedSection<>nil) and
    (Index>=FCachedSection.FOrderStart) then
   begin
      section:=FCachedSection;
      cnt:=Section.FOrderStart;
   end
   else
      begin
        cnt:=00;
        Section:=FSections;
      end;

 while Section<>nil do
  begin
   if Index<Cnt+Section.FCount then Break;
   inc(Cnt,Section.FCount);
   Section:=Section.FNextSection;
  end;

 if Section<>nil then Result:=@Section.FObjects[index-cnt];

 FCachedSection:=Section;

end;


procedure TXRefTable.AddSection(Section: TXRefSection);
begin
  if Section=nil  then exit;

   // is this the first entry?;
  if FSections=nil then FSections:=Section;

  // add it to the end of the chain
  if FLastSection<>nil then FLastSection.FNextSection:=Section;

  // make sure this is the end of the chain;
  FLastSection:=section;
  Section.FNextSection:=nil;

  // Set the first objnum in the section
  Section.FOrderStart:=FObjectsCount;

  // add new objects count to the global count
  inc(FObjectsCount,Section.FCount);

end;


procedure TXRefTable.Parse(Parser: PdfParser);
var
    PrevVar: PPDFVar;
    Section: TXRefSection;
    i: integer;
    SMax: Integer;
begin

 i:=Parser.FindStartXRef;

 Parser.Jump(i);

 with Parser do
  while true do
      begin
        if not compare('xref') then exit;

//        if pdf_ReadToken(Cursor)<>'xref' then exit;

        // xref found, let's read it

         // Create New XRefSection for each subsection
         while True do
          begin

           Section:=TXRefSection.Parse(Parser);

           if Section=nil then exit;

           AddSection(Section);

           with Section do
            begin
              SMax:=Section.FStart+Section.FCount-1;
              if FMaxObjectNum<SMax then FMaxObjectNum:=SMax;
              if FMinObjectNum>FStart then FMinObjectNum:=FStart;
            end;


//            pdfnotify(ntInfo,format('min obj num %d | max obj num %d',[FMinObjectNum,FMaxObjectNum]),nil);

           //check for and process further subsections ...
           if not (Current in ['0'..'9']) then break;
          end;

        // parse 'trailer dictionary' if any ...
         if not compare('trailer') then break;

         ParseVar(Section.FTrailer);

         {PrevVar:=}Section.FTrailer.Container.FItems:=0;//FindInDictionary('/Prev',Names);

   //      if FRootVar.varType=ptInvalid then FRootVar:=Section.FTrailer.Container.FindInDictionary('/Root')^; // root is an indirect reference

//         pdfNotify(ntInfo,'trailer is +'+Trailervar.asString,nil);

         if PrevVar<>nil then parser.Jump(PrevVar.AsInteger);
  end;


 end;


end.
