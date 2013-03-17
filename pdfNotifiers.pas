unit pdfNotifiers;

interface

type // Notifiers

     TProgress=class(TObject)
                 private
                   FProgress: Integer;
                   FSubProgress: Integer;
                 public
                   constructor Create;
                   procedure DoProgress(Steps: integer=1);virtual;
                   procedure  DoSubProgress(Steps: integer=1);virtual;
                   property Progress: Integer read FProgress;
                   property SubProgress: Integer read FSubProgress;

     end;



//  Global Procedure to publish info

TPdfNotifyType=(ntInfo,ntWarnning,ntError,ntProcInit,ntProcRelease);

TPdfNotifyProc=function(atype: TPdfNotifyType;const AMsg: String;Data: Pointer):boolean;

function SetPDFNotifyProc(AProc: TPdfNotifyProc):boolean;

function PdfNotify(AType: TPdfNotifyType;const AMsg: String='';Data: Pointer=nil):boolean;

implementation


constructor TProgress.Create;
begin
  inherited create;
  FProgress:=0;
end;

procedure TProgress.DoProgress(Steps: Integer = 1);
begin
 Inc(FProgress,Steps);
 FSubProgress:=0;
end;

procedure TProgress.DoSubProgress(Steps: Integer = 1);
begin
 Inc(FSubProgress,Steps);
end;

var pdfNotifyProc: TPdfNotifyProc=nil;


function SetPDFNotifyProc(AProc: TPdfNotifyProc):boolean;
begin
 result:=false;
 if @pdfNotifyProc<>nil then
   Result:=pdfNotifyProc(ntProcRelease,'',nil);

 pdfNotifyProc:=AProc;

 if @pdfNotifyProc<>nil  then
    result:=Result and pdfNotifyProc(ntProcInit,'',nil);

end;

function PdfNotify(atype: TPdfNotifyType;const AMsg: String='';Data: Pointer=nil):boolean;
begin
  result:=false;
  if @pdfNotifyProc<>nil then Result:=pdfNotifyProc(Atype,AMsg,Data);
end;

end.
