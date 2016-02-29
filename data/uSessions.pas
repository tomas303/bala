unit uSessions;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_upersist, trl_upersiststore,
  uConfigurations, uEnvVariables, uParameters;

type

  { TSession }

  TSession = class
  private
    fInterpreter: string;
    fSource: TMemoString;
    fOutput: TMemoString;
    fEnvVariableGroups: IPersistManyRefs<TEnvVariableGroup>;
    fParameterGroups: IPersistManyRefs<TParameterGroup>;
    fEnvVariables: IPersistManyTEnvVariable;
    fParameters: IPersistManyTParameter;
  public
    procedure AfterConstruction; override;
  published
    property Interpreter: string read fInterpreter write fInterpreter;
    property Source: TMemoString read fSource write fSource;
    property Output: TMemoString read fOutput write fOutput;
    property EnvVariableGroups: IPersistManyRefs<TEnvVariableGroup> read fEnvVariableGroups;
    property ParameterGroups: IPersistManyRefs<TParameterGroup> read fParameterGroups;
    property EnvVariables: IPersistManyTEnvVariable read fEnvVariables;
    property Parameters: IPersistManyTParameter read fParameters;
  end;

implementation

{ TSession }

procedure TSession.AfterConstruction;
begin
  inherited AfterConstruction;
  fEnvVariableGroups := TPersistManyRefs<TEnvVariableGroup>.Create;
  fParameterGroups := TPersistManyRefs<TParameterGroup>.Create;
  fEnvVariables := TPersistManyTEnvVariable.Create;
  fParameters := TPersistManyTParameter.Create;
end;

end.

