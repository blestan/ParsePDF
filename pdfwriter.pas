unit pdfWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

implementation

procedure WriteStr(const S: string; AStream: TStream); overload;
begin
  if Length(S) > 0 then
    AStream.Write(S[1], Length(S));
end;

procedure WriteStr(C: char; AStream: TStream); overload; inline;
begin
  AStream.Write(C, 1);
end;

{
procedure WritePdfVar(Item: Pointer; S: TStream);
var
  d: pdfDataType;
begin
  d := pdfDataType(byte(Item^));
  case d of
    ptNull: WriteStr(' null', S);

    ptInteger: WriteStr(' '+IntToStr(pdfScalarVar(Item^).Int), S);

    ptBoolean: if pdfScalarVar(Item^).Bool then
        WriteStr(' True', S)
      else
        WriteStr(' False', S);

    ptFloat: WriteStr(' '+FloatToStr(pdfScalarVar(Item^).Float), S);

 //  ptName: WriteStr('/'+FNames.GetKey(pdfScalarVar(Item^).Index), S);
   ptString:
    begin
      WriteStr('(', S);
      WriteStr(pdfStringVar(Item^).Value, S);
      WriteStr(')', S);
    end;
    ptHexString:
    begin
      WriteStr('<', S);
      WriteStr(pdfStringVar(Item^).Value, S);
      WriteStr('>', S);
    end;
    ptObjectRef: with pdfScalarVar(Item^) do
        WriteStr(format(' %d %d R', [Number, Generation]), S);
    ptArray,
    ptDictionary: pdfScalarVar(Item^).ObjectPtr.Write(S);
//    ptStream:
  end;
end;


procedure pdfArray.Write(S: TStream);
begin
  WriteStr('[', S);
  WriteItems(S);
  WriteStr(']', S);
end;

procedure pdfDictionary.Write(S: TStream);
begin
  WriteStr('<<', S);
  WriteItems(S);
  WriteStr('>>', S);
end;

procedure pdfDictionary.WritePdfVar(Item: Pointer; S: TStream);
begin
  Inherited WritePdfVar(Item,S);
  inc(Item,SizeOf(pdfScalarVar));
  Inherited WritePdfVar(Item,S);
end;

}
end.

