unit uSessions;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_upersist, trl_upersiststore,
  uEnvVariables, uParameters,
  Sessions, trl_irttibroker, iBala, Containers;

type

  { TSession }

  TSession = class
  private
    fName: string;
    fID: string;
    fInterpreter: string;
    fSource: TMemoString;
    fSourceHighLight: THighLight;
    fOutputHighLight: THighLight;
    fEnvVariableGroups: IPersistManyRefs<TEnvVariableGroup>;
    fEnvVariables: IPersistManyTEnvVariable;
    fParameters: IPersistManyTParameter;
    fSessionLinks: ISessionLinks;
    fPrefillCurrentEnvironment: Boolean;
    fScriptLaunch: TScriptLaunch;
    fFileName: string;
  protected
    procedure SetID(const AValue: string);
  public
    procedure AfterConstruction; override;
  published
    property Name: string read fName write fName;
    property ID: string read fID write SetID;
    property Interpreter: string read fInterpreter write fInterpreter;
    property Source: TMemoString read fSource write fSource;
    property SourceHighLight: THighLight read fSourceHighLight write fSourceHighLight;
    property OutputHighLight: THighLight read fOutputHighLight write fOutputHighLight;
    property EnvVariableGroups: IPersistManyRefs<TEnvVariableGroup> read fEnvVariableGroups;
    property EnvVariables: IPersistManyTEnvVariable read fEnvVariables;
    property Parameters: IPersistManyTParameter read fParameters;
    property PrefillCurrentEnvironment: Boolean read fPrefillCurrentEnvironment write fPrefillCurrentEnvironment;
    property ScriptLaunch: TScriptLaunch read fScriptLaunch write fScriptLaunch;
    property FileName: string read fFileName write fFileName;
  end;

implementation

{ TSession }

procedure TSession.AfterConstruction;
var
  mGuid: TGuid;
begin
  inherited AfterConstruction;
  CreateGUID(mGuid);
  fID := GUIDToString(mGuid);
  fEnvVariableGroups := TPersistManyRefs<TEnvVariableGroup>.Create;
  fEnvVariables := TPersistManyTEnvVariable.Create;
  fParameters := TPersistManyTParameter.Create;
end;

procedure TSession.SetID(const AValue: string);
begin
  if AValue <> '' then
    fID := AValue;
end;

end.

