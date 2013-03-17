unit lazmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form2: TForm2;

implementation

uses MemoryMapper,PdfNotifiers,PdfObjs,PdfParse,pdfXRef,pdfDoc;


procedure LogStr(const S: String);
begin
 Form2.Memo1.Lines.Add(S);
end;

function MyNotify(AType: TPdfNotyfiType;const Msg: String; Data: Pointer):boolean;
begin
  Result:=true;
  LogStr(Msg);
end;

function GetPdfPageCount(const filename: string): integer;
var
  Map :ImmMemoryMap;

  Doc: PdfDoc;

  CurrentObj: pdfGenericVar;

  Parser: PdfParser;

begin

  //on error return -1 as page count
  result := -1;
  try

    Form2.Cursor := crHourGlass;
    try

     // Obtain a read-only memory map.
    map := CommMemoryMap.CreateInstance(filename);
    if not map.IsValid then
      raise EInOutError.Create('UNEXPECTED ERROR: File mapping failed.');

     // Convert the raw mapped region to something useful (PChar)

      Parser:=pdfParser.Create(Map.Data,Map.Size);


      Doc:=pdfDoc.Create(Parser);


      LogStr(Format('%d Objects found in pdf file',[Doc.ObjectsCount]));


      if Doc.Root=nil  then exit;

      LogStr(Doc.Root.AsString);

      CurrentObj:=Doc.Obj(Doc.Root['/Pages']);

      if CurrentObj=nil then exit;

      result:=CurrentObj['/Count'].AsInteger;

      finally
      Form2.Cursor := crDefault;
      Doc.Free;



    end;
  except
    //nb: errors are flagged by returning -1
  end;
end;


{ TForm2 }

procedure TForm2.Button1Click(Sender: TObject);
begin
   if OpenDialog1.Execute then
  begin
   Memo1.Lines.Clear;
   LogSTr(Format('%d pages found in document',[(GetPdfPageCount(OpenDialog1.FileName))]));
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
   Memo1.Lines.Clear;
   SetPDFNotifyProc(@MyNotify);
end;

initialization
  {$I lazmain.lrs}

end.

