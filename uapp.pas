unit uapp;

{$mode delphi}{$H+}

interface

uses
  forms, fMain,
  tal_uapp, tal_ilauncher, tal_uguilauncher,
  trl_idifactory, trl_udifactory,
  trl_ipersist, trl_upersiststore,
  trl_dicontainer,
  trl_irttibroker, trl_urttibroker,
  trl_upersistxml,
  trl_processrunner,
  tvl_udatabinder, tvl_utallybinders,
  tvl_ibindings, tal_iedit, tvl_ubehavebinder,
  tal_SimpleListForm,
  tal_ihistorysettings,
  uParameters, uEnvVariables, fEnvVariableGroup,
  uContainers, Containers, fSession, uSessions,
  OsUtils, uOsUtils;

type

  { TAppGUILauncher }

  TAppGUILauncher = class(TGUILauncher)
  protected
    fStore: IPersistStore;
  protected
    procedure BeforeLaunch; override;
    procedure AfterLaunch; override;
  published
    property Store: IPersistStore read fStore write fStore;
  end;

  { TApp }

  TApp = class(TALApp)
  public const
    cAppID = 'APP';
    cPersistID = 'PERSIST';
    cSettingsID = 'SETTINGS';
  private
    function GetAppDIC: TDIContainer;
    function GetPersistDIC: TDIContainer;
    function GetSettingsDIC: TDIContainer;
  protected
    property AppDIC: TDIContainer read GetAppDIC;
    property PersistDIC: TDIContainer read GetPersistDIC;
    property SettingsDIC: TDIContainer read GetSettingsDIC;
  protected
    function CreateMainFormInstance: TObject;
  protected
    procedure RegisterCore;
    procedure RegisterPersist;
    procedure RegisterSettings;
    procedure RegisterGUI;
    procedure RegisterIDE;
    procedure RegisterRtl;
    procedure RegisterRunContainer;
    procedure RegisterAppServices; override;
  end;

implementation

{ TAppGUILauncher }

procedure TAppGUILauncher.BeforeLaunch;
begin
  inherited BeforeLaunch;
  Store.Open;
end;

procedure TAppGUILauncher.AfterLaunch;
begin
  Store.Close;
  inherited AfterLaunch;
end;

{ TApp }

function TApp.GetAppDIC: TDIContainer;
begin
  Result := DIC.Locate(TDIContainer, cAppID);
end;

function TApp.GetPersistDIC: TDIContainer;
begin
  Result := DIC.Locate(TDIContainer, cPersistID);
end;

function TApp.GetSettingsDIC: TDIContainer;
begin
  Result := DIC.Locate(TDIContainer, cSettingsID);
end;

function TApp.CreateMainFormInstance: TObject;
begin
  Application.CreateForm(TMainForm, Result);
end;

procedure TApp.RegisterGUI;
var
  mReg: TDIReg;
begin
  mReg := AppDIC.Add(TRBBehavioralBinder, IRBBehavioralBinder);
  //
  mReg := DIC.Add(TAppGUILauncher, ILauncher);
  mReg.InjectProp('MainForm', IMainForm, '', AppDIC);
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
end;

procedure TApp.RegisterIDE;
var
  mReg: TDIReg;
begin
  mReg := AppDIC.Add(CreateMainFormInstance, IMainForm);
  mReg.InjectProp('Containers', IContainers);
  mReg.InjectProp('SessionsBinder', IRBTallyBinder, 'sessions', PersistDIC);
  mReg.InjectProp('Factory', IPersistFactory, '', PersistDIC);
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
  mReg.InjectProp('AppFactory', IDIFactory, '', DIC);
  mReg.InjectProp('EnvVariableGroups', IListData, 'EnvVariableGroupsForm');
  mReg.InjectProp('HistorySettings', IHistorySettings, '', SettingsDIC);
  //
  mReg := AppDIC.Add(TSimpleListForm, AppDIC.Locate(TDIOwner), IListData, 'EnvVariableGroupsForm');
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
  mReg.InjectProp('Factory', IPersistFactory, '', PersistDIC);
  mReg.InjectProp('Binder', IRBTallyBinder, 'listbox', PersistDIC);
  mReg.InjectProp('Edit', IEditData, 'EnvVariableGroupForm');
  mReg.InjectProp('DataClass', TEnvVariableGroup.ClassName);
  mReg.InjectProp('Caption', 'Environment variables');
  //
  mReg := AppDIC.Add(TEnvVariableGroupForm, AppDIC.Locate(TDIOwner), IEditData, 'EnvVariableGroupForm');
  mReg.InjectProp('Binder', IRBDataBinder, '', PersistDIC);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
  mReg.InjectProp('OsUtils', IOsUtils);
end;

procedure TApp.RegisterRtl;
var
  mReg: TDIReg;
begin
  mReg := AppDIC.Add(TProcessRunner);
  //
  mReg := AppDIC.Add(TOsUtils, IOsUtils, '', ckSingle);
end;

procedure TApp.RegisterRunContainer;
var
  mReg: TDIReg;
begin
  mReg := AppDIC.Add(TContainer, IContainer);
  mReg.InjectProp('Session', IRBData, 'TSession', PersistDIC);
  mReg.InjectProp('SessionIDE', IContainerIDE);
  mReg.InjectProp('ProcessRunner', TProcessRunner);
  //
  mReg := AppDIC.Add(TSessionForm, AppDIC.Locate(TDIOwner), IContainerIDE);
  mReg.InjectProp('Binder', IRBDataBinder, '', PersistDIC);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
  mReg.InjectProp('SessionsBinder', IRBTallyBinder, 'sessions', PersistDIC);
  mReg.InjectProp('Factory', IPersistFactory, '', PersistDIC);
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
  mReg.InjectProp('OsUtils', IOsUtils);
  mReg.InjectProp('HistorySettings', IHistorySettings, '', SettingsDIC);
  //
  mReg := AppDIC.Add(TContainers, IContainers);
  mReg.InjectProp('AppFactory', IDIFactory, '', DIC);
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
end;

procedure TApp.RegisterPersist;
var
  mReg: TDIReg;
begin
  mReg := PersistDIC.Add(TRBData, IRBData);
  //
  mReg := PersistDIC.Add(TSIDList, ISIDList);
  //
  mReg := PersistDIC.Add(TPersistRef, IPersistRef);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := PersistDIC.Add(TPersistManyRefs, IPersistManyRefs);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := PersistDIC.Add(TPersistRefList, IPersistRefList);
  // persist data
  RegisterDataClass(PersistDIC, TParameter);
  RegisterDataClass(PersistDIC, TEnvVariable);
  RegisterDataClass(PersistDIC, TEnvVariableGroup);
  RegisterDataClass(PersistDIC, TSession);
  //
  mReg := PersistDIC.Add(TStoreCache);
  //
  mReg := PersistDIC.Add(TPersistStore, IPersistStore, '', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
  //
  mReg := PersistDIC.Add(TXmlStore, IPersistStoreDevice, 'xml');
  mReg.InjectProp('XMLFile', DataFile);
  mReg.InjectProp('Factory', IPersistFactory);
  // binders(conection between data and GUI)
  mReg := PersistDIC.Add(TListBoxBinder, IRBTallyBinder, 'listbox');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := PersistDIC.Add(TDrawGridBinder, IRBTallyBinder, 'drawgrid');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := PersistDIC.Add(TListBoxBinder, IRBTallyBinder, 'sessions', ckSingle);
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := PersistDIC.Add(TRBDataBinder, IRBDataBinder);
end;

procedure TApp.RegisterSettings;
var
  mReg: TDIReg;
begin
  RegisterPersistCommon(SettingsDIC);
  RegisterHistorySettings(SettingsDIC);
  //
  mReg := SettingsDIC.Add(TXmlStore, IPersistStoreDevice);
  mReg.InjectProp('XMLFile', SettingsFile);
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := SettingsDIC.Add(TPersistFactory, IPersistFactory);
  mReg.InjectProp('Container', TDIContainer, cSettingsID, DIC);
end;

procedure TApp.RegisterCore;
var
  mReg: TDIReg;
begin
  // application container
  mReg := DIC.Add(TDIContainer, cAppID, ckSingle);
  // persist container
  mReg := DIC.Add(TDIContainer, cPersistID, ckSingle);
  // settings container
  mReg := DIC.Add(TDIContainer, cSettingsID, ckSingle);
  // main DI factory
  mReg := DIC.Add(TDIFactory, IDIFactory);
  mReg.InjectProp('Container', TDIContainer, cAppID, DIC);
  // factory for persist data
  mReg := PersistDIC.Add(TPersistFactory, IPersistFactory, '', ckSingle);
  mReg.InjectProp('Container', TDIContainer, cPersistID, DIC);
  // owner of interface object without release(clear interfaces on destroy)
  mReg := AppDIC.Add(TDIOwner, '', ckSingle);
end;

procedure TApp.RegisterAppServices;
begin
  inherited RegisterAppServices;
  RegisterCore;
  RegisterRtl;
  RegisterPersist;
  RegisterSettings;
  RegisterGUI;
  RegisterIDE;
  RegisterRunContainer;
end;

end.

