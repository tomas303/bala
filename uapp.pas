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
  uParameters, uEnvVariables, fEnvVariableGroup,
  uContainers, Containers, fSession, uSessions,
  uSettings, SettingsBroker, uSettingsBroker,
  OsUtils, uOsUtils;

type

  { TAppGUILauncher }

  TAppGUILauncher = class(TGUILauncher)
  protected
    fStore: IPersistStore;
    fSettingsBroker: ISettingsBroker;
    procedure LoadSettings;
    procedure SaveSettings;
  protected
    procedure BeforeLaunch; override;
    procedure AfterLaunch; override;
  published
    property Store: IPersistStore read fStore write fStore;
    property SettingsBroker: ISettingsBroker read fSettingsBroker write fSettingsBroker;
  end;

  { TApp }

  TApp = class(TALApp)
  public const
    cAppID = 'APP';
    cPersistID = 'PERSIST';
  private
    function GetAppDIC: TDIContainer;
    function GetPersistDIC: TDIContainer;
  protected
    property AppDIC: TDIContainer read GetAppDIC;
    property PersistDIC: TDIContainer read GetPersistDIC;
  protected
    function CreateMainFormInstance: TObject;
  protected
    procedure RegisterCore;
    procedure RegisterPersist;
    procedure RegisterGUI;
    procedure RegisterIDE;
    procedure RegisterRtl;
    procedure RegisterRunContainer;
    procedure RegisterAppServices; override;
  end;

implementation

{ TAppGUILauncher }

procedure TAppGUILauncher.LoadSettings;
var
  m: IPersistRefList;
begin
  m := (Store as IPersistQuery).SelectClass('TAppSetting');
  if m.Count = 0 then begin
    SettingsBroker.AppSetting := Store.New('TAppSetting');
  end else begin
    SettingsBroker.AppSetting := m.Data[0];
  end;
end;

procedure TAppGUILauncher.SaveSettings;
begin
  Store.Save(SettingsBroker.AppSetting);
end;

procedure TAppGUILauncher.BeforeLaunch;
begin
  inherited BeforeLaunch;
  Store.Open;
  LoadSettings;
end;

procedure TAppGUILauncher.AfterLaunch;
begin
  SaveSettings;
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
  mReg.InjectProp('SettingsBroker', ISettingsBroker, '', AppDIC);
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
  mReg.InjectProp('SettingsBroker', ISettingsBroker, '', AppDIC);
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
  //
  mReg := AppDIC.Add(TSettingsBroker, ISettingsBroker, '', ckSingle);
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
  mReg.InjectProp('SettingsBroker', ISettingsBroker, '', AppDIC);
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
  RegisterDataClass(PersistDIC, TMainIDESettings);
  RegisterDataClass(PersistDIC, TSessionIdeSettings);
  RegisterDataClass(PersistDIC, TAppSetting);
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

procedure TApp.RegisterCore;
var
  mReg: TDIReg;
begin
  // application container
  mReg := DIC.Add(TDIContainer, cAppID, ckSingle);
  // persist container
  mReg := DIC.Add(TDIContainer, cPersistID, ckSingle);
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
  RegisterGUI;
  RegisterIDE;
  RegisterRunContainer;
end;

end.

