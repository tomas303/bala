program bala;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  uapp;

{$R *.res}

begin
  TApp.Go;
end.

