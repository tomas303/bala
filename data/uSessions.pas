unit uSessions;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_upersist, trl_upersiststore,
  uEnvVariables, uParameters,
  Sessions, trl_irttibroker;

type

  { TSession }

  TSession = class
  private
    fName: string;
    fInterpreter: string;
    fSource: TMemoString;
    fOutput: TMemoString;
    fEnvVariables: IPersistManyTEnvVariable;
    fParameters: IPersistManyTParameter;
    fSessionLinks: ISessionLinks;
  public
    procedure AfterConstruction; override;
  published
    property Name: string read fName write fName;
    property Interpreter: string read fInterpreter write fInterpreter;
    property Source: TMemoString read fSource write fSource;
    property Output: TMemoString read fOutput write fOutput;
    property EnvVariables: IPersistManyTEnvVariable read fEnvVariables;
    property Parameters: IPersistManyTParameter read fParameters;
  end;

implementation

{ TSession }

procedure TSession.AfterConstruction;
var
  mGuid: TGuid;
begin
  inherited AfterConstruction;
  fEnvVariables := TPersistManyTEnvVariable.Create;
  fParameters := TPersistManyTParameter.Create;
end;

end.

