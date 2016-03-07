unit uSessions;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_upersist, trl_upersiststore,
  uConfigurations, uEnvVariables, uParameters, uSources, uInterpreters,
  Sessions, trl_irttibroker;

type

  { TSession }

  TSession = class
  protected type

    { TSessionLinks }

    TSessionLinks = class(TInterfacedObject, ISessionLinks)
    protected
      fSession: TSession;
      procedure AssignEnvVariables(const ASource, ATarget: IPersistManyTEnvVariable);
      procedure AssignParameters(const ASource, ATarget: IPersistManyTParameter);
    protected
      procedure PutToSource;
      procedure PutToInterpreter;
      procedure PutToConfiguration;
      procedure GetFromSource;
      procedure GetFromInterpreter;
      procedure GetFromConfiguration;
    public
      constructor Create(ASession: TSession);
    end;

  private
    fInterpreter: string;
    fSource: TMemoString;
    fOutput: TMemoString;
    fEnvVariableGroups: IPersistManyRefs<TEnvVariableGroup>;
    fParameterGroups: IPersistManyRefs<TParameterGroup>;
    fEnvVariables: IPersistManyTEnvVariable;
    fParameters: IPersistManyTParameter;
    fSourceLink: IPersistRef<TSource>;
    fInterpreterLink: IPersistRef<TInterpreter>;
    fConfigurationLink: IPersistRef<TConfiguration>;
    fSessionLinks: ISessionLinks;
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
    property SourceLink: IPersistRef<TSource> read fSourceLink write fSourceLink;
    property InterpreterLink: IPersistRef<TInterpreter> read fInterpreterLink write fInterpreterLink;
    property ConfigurationLink: IPersistRef<TConfiguration> read fConfigurationLink write fConfigurationLink;
    property SessionLinks: ISessionLinks read fSessionLinks;
  end;

implementation

{ TSession.TSessionLinks }

procedure TSession.TSessionLinks.AssignEnvVariables(const ASource,
  ATarget: IPersistManyTEnvVariable);
var
  i: integer;
begin
  ATarget.Count := ASource.Count;
  for i := 0 to ASource.Count - 1 do begin
    ATarget.AsPersistData[i].Assign(ASource.AsPersistData[i]);
  end;
end;

procedure TSession.TSessionLinks.AssignParameters(const ASource,
  ATarget: IPersistManyTParameter);
var
  i: integer;
begin
  ATarget.Count := ASource.Count;
  for i := 0 to ASource.Count - 1 do begin
    ATarget.AsPersistData[i].Assign(ASource.AsPersistData[i]);
  end;
end;

procedure TSession.TSessionLinks.PutToSource;
begin
  if fSession.SourceLink.Item = nil then
    Exit;
  fSession.SourceLink.Item.Source := fSession.Source;
end;

procedure TSession.TSessionLinks.PutToInterpreter;
begin
  if fSession.InterpreterLink.Item = nil then
    Exit;
  fSession.InterpreterLink.Item.Command := fSession.Interpreter;
end;

procedure TSession.TSessionLinks.PutToConfiguration;
var
  i: integer;
begin
  if fSession.ConfigurationLink.Item = nil then
    Exit;
  if fSession.ConfigurationLink.Item.Source.Item <> nil then
    fSession.ConfigurationLink.Item.Source.Item.Source := fSession.Source;
  if fSession.ConfigurationLink.Item.Interpreter.Item <> nil then
    fSession.ConfigurationLink.Item.Interpreter.Item.Command := fSession.Interpreter;
  //
  fSession.ConfigurationLink.Item.EnvVariableGroups.Count := fSession.EnvVariableGroups.Count;
  for i := 0 to fSession.EnvVariableGroups.Count - 1 do begin
    (fSession.ConfigurationLink.Item.EnvVariableGroups.AsInterface[i] as IPersistRef).SID := (fSession.EnvVariableGroups.AsInterface[i] as IPersistRef).SID;
  end;
  AssignEnvVariables(fSession.EnvVariables, fSession.ConfigurationLink.Item.EnvVariables);
  //
  fSession.ConfigurationLink.Item.ParameterGroups.Count := fSession.ParameterGroups.Count;
  for i := 0 to fSession.ConfigurationLink.Item.ParameterGroups.Count - 1 do begin
    (fSession.ConfigurationLink.Item.ParameterGroups.AsInterface[i] as IPersistRef).SID := (fSession.ParameterGroups.AsInterface[i] as IPersistRef).SID;
  end;
  AssignParameters(fSession.Parameters, fSession.ConfigurationLink.Item.Parameters);
end;

procedure TSession.TSessionLinks.GetFromSource;
begin
  if fSession.SourceLink.Item = nil then
    Exit;
  fSession.Source := fSession.SourceLink.Item.Source;
end;

procedure TSession.TSessionLinks.GetFromInterpreter;
begin
  if fSession.InterpreterLink.Item = nil then
    Exit;
  fSession.Interpreter := fSession.InterpreterLink.Item.Command;
end;

procedure TSession.TSessionLinks.GetFromConfiguration;
var
  i: integer;
begin
  if fSession.ConfigurationLink.Item = nil then
    Exit;
  if fSession.ConfigurationLink.Item.Source.Item <> nil then
    fSession.Source := fSession.ConfigurationLink.Item.Source.Item.Source;
  if fSession.ConfigurationLink.Item.Interpreter.Item <> nil then
    fSession.Interpreter := fSession.ConfigurationLink.Item.Interpreter.Item.Command;
  //
  fSession.EnvVariableGroups.Count := fSession.ConfigurationLink.Item.EnvVariableGroups.Count;
  for i := 0 to fSession.ConfigurationLink.Item.EnvVariableGroups.Count - 1 do begin
    (fSession.EnvVariableGroups.AsInterface[i] as IPersistRef).SID := fSession.ConfigurationLink.Item.EnvVariableGroups[i].SID;
  end;
  AssignEnvVariables(fSession.ConfigurationLink.Item.EnvVariables, fSession.EnvVariables);
  //
  fSession.ParameterGroups.Count := fSession.ConfigurationLink.Item.ParameterGroups.Count;
  for i := 0 to fSession.ConfigurationLink.Item.ParameterGroups.Count - 1 do begin
    (fSession.ParameterGroups.AsInterface[i] as IPersistRef).SID := fSession.ConfigurationLink.Item.ParameterGroups[i].SID;
  end;
  AssignParameters(fSession.ConfigurationLink.Item.Parameters, fSession.Parameters);
end;

constructor TSession.TSessionLinks.Create(ASession: TSession);
begin
  fSession := ASession;
end;

{ TSession }

procedure TSession.AfterConstruction;
begin
  inherited AfterConstruction;
  fEnvVariableGroups := TPersistManyRefs<TEnvVariableGroup>.Create;
  fParameterGroups := TPersistManyRefs<TParameterGroup>.Create;
  fEnvVariables := TPersistManyTEnvVariable.Create;
  fParameters := TPersistManyTParameter.Create;
  fSourceLink := TPersistRef<TSource>.Create;
  fInterpreterLink := TPersistRef<TInterpreter>.Create;
  fConfigurationLink := TPersistRef<TConfiguration>.Create;
  fSessionLinks := TSessionLinks.Create(Self);
end;

end.

