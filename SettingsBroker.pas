unit SettingsBroker;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, trl_irttibroker;

type

  { ISettingsBroker }

  ISettingsBroker = interface
  ['{D8BD4536-8F23-410E-B0C0-F6165A1519EB}']
    function GetAppSetting: IRBData;
    procedure SetAppSetting(const AValue: IRBData);
    property AppSetting: IRBData read GetAppSetting write SetAppSetting;
  end;

implementation

end.

