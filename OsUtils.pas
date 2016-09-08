unit OsUtils;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_irttibroker;

type
  IOsUtils = interface
  ['{64C75448-B2C5-4501-A43A-EE6956DCDD16}']
    procedure AddSystemEnvVariables(AData: IRBData;
      const ACollection, ANamePart, AValuePart: string);
  end;

implementation

end.

