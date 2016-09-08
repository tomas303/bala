unit uOsUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, OsUtils, trl_irttibroker, trl_ipersist;

type

  { TOsUtils }

  TOsUtils = class(TInterfacedObject, IOsUtils)
  protected
    // IOsUtils
    procedure AddSystemEnvVariables(AData: IRBData;
      const ACollection, ANamePart, AValuePart: string);
  end;


implementation

{ TOsUtils }

procedure TOsUtils.AddSystemEnvVariables(AData: IRBData; const ACollection,
  ANamePart, AValuePart: string);
var
  i: Integer;
  mCollection: IPersistMany;
  mList: TStringList;
  mShift: integer;
  mEnvVar: IRBData;
begin
  mList := TStringList.Create;
  try
    for i := 1 to GetEnvironmentVariableCount do
    begin
      mList.Add(GetEnvironmentString(i));
    end;
    mList.Sort;
    mCollection := AData.ItemByName[ACollection].AsInterface as IPersistMany;
    mShift := mCollection.Count;
    mCollection.Count := mShift + mList.Count;
    for i := 0 to mList.Count - 1 do
    begin
      mEnvVar := mCollection.AsPersistData[mShift + i];
      mEnvVar.ItemByName[ANamePart].AsString := mList.Names[i];
      mEnvVar.ItemByName[AValuePart].AsString := mList.ValueFromIndex[i];
    end;
  finally
    mList.Free;
  end;
end;

end.

