unit uSettings;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_upersist, trl_irttibroker, trl_urttibroker;

type

  { TMainIDESettings }

  TMainIDESettings = class
  protected
    fLeft: integer;
    fTop: integer;
    fWidth: integer;
    fHeight: integer;
    fSplitterSessionsPos: integer;
  published
    property Left: integer read fLeft write fLeft;
    property Top: integer read fTop write fTop;
    property Width: integer read fWidth write fWidth;
    property Height: integer read fHeight write fHeight;
    property SplitterSessionsPos: integer read fSplitterSessionsPos write fSplitterSessionsPos;
  end;

  { TSessionIdeSettings }

  TSessionIdeSettings = class
  protected
    fSessionID: string;
    fSplitterEnvironmentPos: integer;
    fSplitterMainPos: integer;
    fSplitterOutputPos: integer;
  published
    property SessionID: string read fSessionID write fSessionID;
    property SplitterEnvironmentPos: integer read fSplitterEnvironmentPos write fSplitterEnvironmentPos;
    property SplitterMainPos: integer read fSplitterMainPos write fSplitterMainPos;
    property SplitterOutputPos: integer read fSplitterOutputPos write fSplitterOutputPos;
  end;

  { IPersistManyTSessionIdeSettings }

  IPersistManyTSessionIdeSettings = interface(IPersistManyItems<TSessionIdeSettings>)
  ['{0695ABA6-BAE9-4233-A793-0509C0017EF9}']
  end;

  { TPersistManTSessionIdeSettings }

  TPersistManyTSessionIdeSettings = class(TPersistManyObjects<TSessionIdeSettings>, IPersistManyTSessionIdeSettings)
  end;


  { TAppSetting }

  TAppSetting = class
  private
    fMainIDESettings: IRBData;
    fSessionIdeSettings: IPersistManyTSessionIdeSettings;
  public
    procedure AfterConstruction; override;
  published
    property MainIDESettings: IRBData read fMainIDESettings write fMainIDESettings;
    property SessionIdeSettings: IPersistManyTSessionIdeSettings read fSessionIdeSettings;
  end;

implementation

{ TAppSetting }

procedure TAppSetting.AfterConstruction;
begin
  inherited AfterConstruction;
  fMainIDESettings := TRBData.Create(TMainIDESettings.Create);
  fSessionIdeSettings := TPersistManyTSessionIdeSettings.Create;
end;

end.

