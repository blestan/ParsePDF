program laztest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, lazmain, pdfWriter, pdfContainers, VarArray;

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

