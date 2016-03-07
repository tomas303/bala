unit Sessions;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils;

type
  ISessionLinks = interface
  ['{94713CD3-434B-455B-827B-977A650C363D}']
    procedure PutToSource;
    procedure PutToInterpreter;
    procedure PutToConfiguration;
    procedure GetFromSource;
    procedure GetFromInterpreter;
    procedure GetFromConfiguration;
  end;

implementation

end.

