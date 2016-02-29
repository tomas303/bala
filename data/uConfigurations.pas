unit uConfigurations;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_upersist, trl_upersiststore,
  uSources, uInterpreters, uEnvVariables, uParameters;

type

  { TConfiguration }

  TConfiguration = class
  private
    fName: string;
    fSource: IPersistRef<TSource>;
    fInterpreter: IPersistRef<TInterpreter>;
    fEnvVariableGroups: IPersistManyRefs<TEnvVariableGroup>;
    fParameterGroups: IPersistManyRefs<TParameterGroup>;
    fEnvVariables: IPersistManyTEnvVariable;
    fParameters: IPersistManyTParameter;
  public
    procedure AfterConstruction; override;
  published
    property Name: string read fName write fName;
    property Source: IPersistRef<TSource> read fSource write fSource;
    property Interpreter: IPersistRef<TInterpreter> read fInterpreter write fInterpreter;
    property EnvVariableGroups: IPersistManyRefs<TEnvVariableGroup> read fEnvVariableGroups;
    property ParameterGroups: IPersistManyRefs<TParameterGroup> read fParameterGroups;
    property EnvVariables: IPersistManyTEnvVariable read fEnvVariables;
    property Parameters: IPersistManyTParameter read fParameters;
  end;

implementation

{ TConfiguration }

procedure TConfiguration.AfterConstruction;
begin
  inherited AfterConstruction;
  fSource := TPersistRef<TSource>.Create;
  fInterpreter := TPersistRef<TInterpreter>.Create;
  fEnvVariableGroups := TPersistManyRefs<TEnvVariableGroup>.Create;
  fParameterGroups := TPersistManyRefs<TParameterGroup>.Create;
  fEnvVariables := TPersistManyTEnvVariable.Create;
  fParameters := TPersistManyTParameter.Create;
end;

end.

