unit pdfDoc;
// logical objects

interface
uses pdfVars,PdfParse,XRef,contnrs;

type

       pdfDoc=class
        private
         FParser: pdfParser;

         FXTable: TXRefTable;

         FRootObject: pdfGenericVar;
         FPages: TObjectList;

         FHeaderVersion: string;
         function GetObjectsCount: integer;

         function GetRootObject: pdfGenericVar;
         function GetPagesCount: integer;

         function GetPage(index: integer): pdfGenericVar;

         function GetVersion: String;

         procedure InternalReadPages;

        public
         constructor Create;overload;
         constructor Create(AParser: pdfParser);overload;
         destructor Destroy;override;

         function Obj(ObjNum: Integer; ObjGen: Integer=0): pdfGenericVar;overload;
         function Obj(ObjRef:pdfObjectID): pdfGenericVar;overload;
         function Obj(IRefVar: pdfGenericVar): pdfGenericVar;overload;

         procedure ReadObjects;

         property ObjectsCount: integer read GetObjectsCount;
         property PagesCount: Integer read GetPagesCount;
         property Root: pdfGenericVar read GetRootObject;
         property Pages[index:integer]: pdfGenericVar read GetPage;

         property HeaderVersion:string read FHeaderVersion;
         property Version: String read GetVersion;

       end;

implementation

uses sysutils,pdfNotifiers;


// Pdf Document

constructor pdfDoc.Create;
begin
  inherited Create;
  FParser:=nil;
  FXTable:=TXRefTable.Create;
  FRootObject:=nil;
  FHeaderVersion:='';
  FPages:=TObjectList.Create(False);
end;

constructor pdfDoc.Create(AParser: pdfParser);
begin
  inherited create;
  FParser:=AParser;
  FHeaderVersion:=FParser.GetLine;
  FRootObject:=nil;
  FXTable:=TXRefTable.Create;
  FXTable.Parse(FParser);
  FPages:=TObjectList.Create(false);
  InternalReadPages;
end;

destructor pdfDoc.Destroy;
begin
 FXTable.Free;
 if FParser<>nil then FParser.Free;
 FPages.Free;
 inherited destroy;
end;

procedure pdfDoc.InternalReadPages;

procedure ReadPagesObj(K: PdfGenericVar);
var KK: pdfGenericVar;
    i: integer;
begin
  for I := 0 to K.Count - 1 do
   begin
     KK:=Obj(K.Item(i));
     if KK['/Type'].AsString='/Page' then FPages.Add(KK)
                                     else ReadPagesObj(KK['/Kids']);

   end;
end;
begin
  ReadPagesObj(Obj(Root['/Pages'])['/Kids']);
end;

function pdfDoc.GetObjectsCount:integer;
begin
 Result:=FXTable.Count;
end;

function pdfDoc.GetRootObject:pdfGenericVar;
begin
 if FRootObject=nil then FRootObject:=obj(FXTable.RootRef); // get it once for faster use
 result:=FRootObject;
end;

function pdfDoc.GetPagesCount: integer;
begin
  result:=FPages.Count;
end;

function pdfDoc.GetPage;
begin
  result:=FPages[index] as pdfGenericVar;
end;

procedure pdfDoc.ReadObjects;
var i: integer;
    Ref: TXRefRec;
begin
 try
 for i := 0 to ObjectsCount - 1 do
   begin
     Ref:=FXTable.Items[i]^;
     obj(Ref.Number);
   end;
 finally
 end;

end;

function pdfDoc.GetVersion;
var Ver: pdfGenericVar;
begin
 Ver:=Root['/Version'];
 if Ver<>nil then result:=Ver.AsString
             else Result:='';
end;

function pdfDoc.Obj(ObjNum: Integer; ObjGen: Integer = 0):pdfGenericVar;
var Ref: PXRefRec;
begin
 result:=nil;
 Ref:=FXTable[ObjNum];


 if Ref=nil  then exit; //no object with given number;


 result:=Ref.Obj; // ok! let's get the object addr stored in the xref

 if result=nil then // humm... no object at this point
 begin
  if FParser<>nil then // let's try to get it from the stream
   begin
     result:=FParser.ReadObject(Ref.Offset);
     Ref.Obj:=result // store the addr for future faster use
   end;
 end;
end;


function pdfDoc.Obj(ObjRef: pdfObjectID): pdfGenericVar;
begin
 result:=Obj(ObjRef.Number);
end;

function pdfDoc.Obj(IRefVar: pdfGenericVar): pdfGenericVar;
begin
  result:=nil;
  if IRefVar.VarType<>pdf_IRef then
   begin
    if (IRefVar.VarType=pdf_DictItem) // if this an Dict Item -> try to get Iref from it
     then result:=Obj(IRefVar.DictItemValue)
     else PdfNotify(ntWarnning,pdfTypeName(IrefVar.VarType)+' variable cannot be used as Indirect Ref',IrefVar);
    exit;
   end
    else result:=Obj(IRefVar.AsIRef.Number);

end;

end.
