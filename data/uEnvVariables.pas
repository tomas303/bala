unit uEnvVariables;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_upersist;

type

  { TEnvVariable }

  TEnvVariable = class
  private
    fName: string;
    fValue: string;
  published
    property Name: string read fName write fName;
    property Value: string read fValue write fValue;
  end;

  { IPersistManyTEnvVariable }

  IPersistManyTEnvVariable = interface(IPersistMany<TEnvVariable>)
  ['{979097AE-63D4-4897-AD80-71388EB324C2}']
  end;

  { TPersistManyTEnvVariable }

  TPersistManyTEnvVariable = class(TPersistManyObjects<TEnvVariable>, IPersistManyTEnvVariable)
  end;

  { TEnvVariableGroup }

  TEnvVariableGroup = class
  private
    fName: string;
    fEnvVariables: IPersistManyTEnvVariable;
  public
    procedure AfterConstruction; override;
  published
    property Name: string read fName write fName;
    property EnvVariables: IPersistManyTEnvVariable read fEnvVariables;
  end;

implementation

{ TEnvVariableGroup }

procedure TEnvVariableGroup.AfterConstruction;
begin
  inherited AfterConstruction;
  fEnvVariables := TPersistManyTEnvVariable.Create;
end;

end.

