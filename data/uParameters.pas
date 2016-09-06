unit uParameters;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_upersist;

type

  { TParameter }

  TParameter = class
  private
    fValue: string;
  published
    property Value: string read fValue write fValue;
  end;

  { IPersistManyTParameter }

  IPersistManyTParameter = interface(IPersistManyItems<TParameter>)
  ['{71F10F6B-F414-4E64-84C4-D6B41BA8AA5D}']
  end;

  { TPersistManyTParameter }

  TPersistManyTParameter = class(TPersistManyObjects<TParameter>, IPersistManyTParameter)
  end;

implementation

end.

