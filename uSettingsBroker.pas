unit uSettingsBroker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SettingsBroker, trl_irttibroker;

type

  { TSettingsBroker }

  TSettingsBroker = class(TInterfacedObject, ISettingsBroker)
  private
    fAppSetting: IRBData;
  protected
    // ISettingsBroker
    function GetAppSetting: IRBData;
    procedure SetAppSetting(const AValue: IRBData);
    property AppSetting: IRBData read GetAppSetting write SetAppSetting;
  end;

implementation

{ TSettingsBroker }

function TSettingsBroker.GetAppSetting: IRBData;
begin
  Result := fAppSetting;
end;

procedure TSettingsBroker.SetAppSetting(const AValue: IRBData);
begin
  fAppSetting := AValue;
end;

end.

