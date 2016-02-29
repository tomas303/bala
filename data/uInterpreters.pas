unit uInterpreters;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, uParameters, trl_ipersist, trl_upersiststore;

type

  { TInterpreter }

  TInterpreter = class
  private
    fName: string;
    fCommand: string;
    fParameterGroups: IPersistManyRefs<TParameterGroup>;
    fParameters: IPersistManyTParameter;
  public
    procedure AfterConstruction; override;
  published
    property Name: string read fName write fName;
    property Command: string read fCommand write fCommand;
    property ParameterGroups: IPersistManyRefs<TParameterGroup> read fParameterGroups;
    property Parameters: IPersistManyTParameter read fParameters;
  end;


implementation

{ TInterpreter }

procedure TInterpreter.AfterConstruction;
begin
  inherited AfterConstruction;
  fParameterGroups := TPersistManyRefs<TParameterGroup>.Create;
  fParameters := TPersistManyTParameter.Create;
end;

end.

