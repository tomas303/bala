unit uapp;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, tvl_uapplaunch,
  forms,
  fMain, fConfiguration,
  Controls,
  trl_idifactory, trl_udifactory,
  trl_ipersist,  trl_upersist, trl_upersiststore,
  trl_dicontainer,
  trl_irttibroker, trl_urttibroker,
  trl_upersistxml,
  trl_processrunner,
  tvl_udatabinder, tvl_udatabinders, tvl_utallybinders,
  tvl_ibindings, tvl_iedit, tvl_ubehavebinder,
  tvl_SimpleListForm,

  uParameters, uEnvVariables, uInterpreters, uSources, uConfigurations,
  fParameterGroup, fEnvVariableGroup, fInterpreter, fSource,

  uContainers, Containers, fSession, uSessions;
type

  { TKicker }

  TKicker = class(TInterfacedObject, IGUIKicker)
  private
    fMainForm: IMainForm;
    fStore: IPersistStore;
  protected
    //IGUIKicker
    procedure StartUp;
    procedure ShutDown;
    function GetMainForm: IMainForm;
  published
    property MainForm: IMainForm read fMainForm write fMainForm;
    property Store: IPersistStore read fStore write fStore;
  end;

  { TApp }

  TApp = class
  public const
    cAppID = 'APP';
    cPersistID = 'PERSIST';
  private
    fDIC: TDIContainer;
    fDataFile: string;
    function GetAppDIC: TDIContainer;
    function GetPersistDIC: TDIContainer;
  protected
    property AppDIC: TDIContainer read GetAppDIC;
    property PersistDIC: TDIContainer read GetPersistDIC;
  protected
    procedure InjectPersistRef(const AItem: IRBDataItem);
    procedure Setup;
    procedure RegisterDataClass(ADIC: TDIContainer; AClass: TClass);
    procedure RegisterCore;
    procedure RegisterPersist;
    procedure RegisterGUI;
    procedure RegisterIDE;
    procedure RegisterRtl;
    procedure RegisterRunContainer;
    procedure RegisterServices;
    procedure Launch;
  protected
    procedure DockMasterCreateControl(Sender: TObject; aName: string; var
      AControl: TControl; DoDisableAutoSizing: boolean);
  public
    constructor Create;
    destructor Destroy; override;
    class procedure Go;
  end;

implementation

{ TKicker }

procedure TKicker.StartUp;
begin
  Store.Open;
  fMainForm.StartUp;
end;

procedure TKicker.ShutDown;
begin
  fMainForm.ShutDown;
  Store.Close;
  fMainForm := nil;  //otherwisw AV - this is not automaticly freed interface object
  Store := nil;
end;

function TKicker.GetMainForm: IMainForm;
begin
  Result := fMainForm;
end;

{ TApp }

function TApp.GetAppDIC: TDIContainer;
begin
  Result := fDIC.Locate(TDIContainer, cAppID);
end;

function TApp.GetPersistDIC: TDIContainer;
begin
  Result := fDIC.Locate(TDIContainer, cPersistID);
end;

procedure TApp.InjectPersistRef(const AItem: IRBDataItem);
begin
  if AItem.IsInterface and Supports(AItem.AsInterface, IPersistRef) then
  begin
    // IPersistRef need resolve data via Store
    (AItem.AsInterface as IPersistRef).Store := PersistDIC.Locate(IPersistStore);
  end
  else
  if AItem.IsInterface and Supports(AItem.AsInterface, IPersistManyRefs) then
  begin
    // need to create IPersistRef members
    (AItem.AsInterface as IPersistManyRefs).Factory := PersistDIC.Locate(IPersistFactory, cPersistID);
  end;
end;

procedure TApp.Setup;
var
  mAppDir, mSubdir, mExt: string;
begin
  if Paramcount > 0 then
    mAppDir := ParamStr(1)
  else
  begin
    mSubdir := '.' + ExtractFileName(ParamStr(0));
    mExt := ExtractFileExt(ParamStr(0));
    mSubDir := copy(mSubDir, 1, Length(mSubdir) - Length(mExt));
    {$IFDEF UNIX}
    mAppDir := GetEnvironmentVariable('HOME') + PathDelim + mSubdir + PathDelim;
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
    mAppDir := GetEnvironmentVariable('APPDATA') + PathDelim + mSubdir + PathDelim;
    {$ENDIF WINDOWS}
  end;
  if not DirectoryExists(mAppDir) then
  begin
    if not ForceDirectories(mAppDir) then
      raise Exception.Create('Cannot create directory ' + mAppDir);
  end;
  fDataFile := mAppDir + 'data.xml';
end;

procedure TApp.RegisterDataClass(ADIC: TDIContainer; AClass: TClass);
var
  mReg: TDIReg;
begin
  // persist class
  mReg := ADIC.Add(AClass);
  mReg.InjectProp('', InjectPersistRef);
  // data envelop for persist class
  mReg := ADIC.Add(TRBData, IRBData, AClass.ClassName);
  mReg.InjectProp('UnderObject', AClass);
end;

procedure TApp.RegisterGUI;
var
  mReg: TDIReg;
begin
  mReg := AppDIC.Add(TRBBehavioralBinder, IRBBehavioralBinder);
  //
  mReg := AppDIC.Add(TGUILauncher, '', ckSingle);
  mReg.InjectProp('Kicker', IGUIKicker);
  //
  mReg := AppDIC.Add(TKicker, IGUIKicker);
  mReg.InjectProp('MainForm', IMainForm);
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
end;

procedure TApp.RegisterIDE;
var
  mReg: TDIReg;
begin
  mReg := AppDIC.Add(TMainForm, AppDIC.Locate(TDIOwner), IMainForm);
  mReg.InjectProp('Containers', IContainers);
  mReg.InjectProp('SessionsBinder', IRBTallyBinder, 'sessions', PersistDIC);
  mReg.InjectProp('Factory', IPersistFactory, cPersistID, PersistDIC);
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
  mReg.InjectProp('AppFactory', IDIFactory, '', fDIC);
  //
  mReg := AppDIC.Add(TSimpleListForm, AppDIC.Locate(TDIOwner), IListData, 'ParameterGroupsForm');
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
  mReg.InjectProp('Factory', IPersistFactory, cPersistID, PersistDIC);
  mReg.InjectProp('Binder', IRBTallyBinder, 'listbox', PersistDIC);
  mReg.InjectProp('Edit', IEditData, 'ParameterGroupForm');
  mReg.InjectProp('DataClass', TParameterGroup.ClassName);
  mReg.InjectProp('Caption', 'Parameters');
  //
  mReg := AppDIC.Add(TParameterGroupForm, AppDIC.Locate(TDIOwner), IEditData, 'ParameterGroupForm');
  mReg.InjectProp('Binder', IRBDataBinder, '', PersistDIC);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
  //
  mReg := AppDIC.Add(TSimpleListForm, AppDIC.Locate(TDIOwner), IListData, 'EnvVariableGroupsForm');
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
  mReg.InjectProp('Factory', IPersistFactory, cPersistID, PersistDIC);
  mReg.InjectProp('Binder', IRBTallyBinder, 'listbox', PersistDIC);
  mReg.InjectProp('Edit', IEditData, 'EnvVariableGroupForm');
  mReg.InjectProp('DataClass', TEnvVariableGroup.ClassName);
  mReg.InjectProp('Caption', 'Environment variables');
  //
  mReg := AppDIC.Add(TEnvVariableGroupForm, AppDIC.Locate(TDIOwner), IEditData, 'EnvVariableGroupForm');
  mReg.InjectProp('Binder', IRBDataBinder, '', PersistDIC);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
  //
  mReg := AppDIC.Add(TSimpleListForm, AppDIC.Locate(TDIOwner), IListData, 'InterpretersForm');
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
  mReg.InjectProp('Factory', IPersistFactory, cPersistID, PersistDIC);
  mReg.InjectProp('Binder', IRBTallyBinder, 'listbox', PersistDIC);
  mReg.InjectProp('Edit', IEditData, 'InterpreterForm');
  mReg.InjectProp('DataClass', TInterpreter.ClassName);
  mReg.InjectProp('Caption', 'Interpreters');
  //
  mReg := AppDIC.Add(TInterpreterForm, AppDIC.Locate(TDIOwner), IEditData, 'InterpreterForm');
  mReg.InjectProp('Binder', IRBDataBinder, '', PersistDIC);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
  //
  mReg := AppDIC.Add(TSimpleListForm, AppDIC.Locate(TDIOwner), IListData, 'SourcesForm');
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
  mReg.InjectProp('Factory', IPersistFactory, cPersistID, PersistDIC);
  mReg.InjectProp('Binder', IRBTallyBinder, 'listbox', PersistDIC);
  mReg.InjectProp('Edit', IEditData, 'SourceForm');
  mReg.InjectProp('DataClass', TSource.ClassName);
  mReg.InjectProp('Caption', 'Scripts');
  //
  mReg := AppDIC.Add(TSourceForm, AppDIC.Locate(TDIOwner), IEditData, 'SourceForm');
  mReg.InjectProp('Binder', IRBDataBinder, '', PersistDIC);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
  //
  mReg := AppDIC.Add(TSimpleListForm, AppDIC.Locate(TDIOwner), IListData, 'ConfigurationsForm');
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
  mReg.InjectProp('Factory', IPersistFactory, cPersistID, PersistDIC);
  mReg.InjectProp('Binder', IRBTallyBinder, 'listbox', PersistDIC);
  mReg.InjectProp('Edit', IEditData, 'ConfigurationForm');
  mReg.InjectProp('DataClass', TConfiguration.ClassName);
  mReg.InjectProp('Caption', 'Configurations');
  //
  mReg := AppDIC.Add(TConfigurationForm, AppDIC.Locate(TDIOwner), IEditData, 'ConfigurationForm');
  mReg.InjectProp('Binder', IRBDataBinder, '', PersistDIC);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
end;

procedure TApp.RegisterRtl;
var
  mReg: TDIReg;
begin
  mReg := AppDIC.Add(TProcessRunner);
end;

procedure TApp.RegisterRunContainer;
var
  mReg: TDIReg;
begin
  RegisterDataClass(PersistDIC, TSession);
  //
  mReg := AppDIC.Add(TContainer, IContainer);
  mReg.InjectProp('Session', IRBData, 'TSession', PersistDIC);
  mReg.InjectProp('SessionIDE', IContainerIDE);
  mReg.InjectProp('ProcessRunner', TProcessRunner);
  //
  mReg := AppDIC.Add(TSessionForm, AppDIC.Locate(TDIOwner), IContainerIDE);
  mReg.InjectProp('Binder', IRBDataBinder, '', PersistDIC);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
  mReg.InjectProp('SessionsBinder', IRBTallyBinder, 'sessions', PersistDIC);
  mReg.InjectProp('Factory', IPersistFactory, cPersistID, PersistDIC);
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
  //
  mReg := AppDIC.Add(TContainers, IContainers);
  mReg.InjectProp('AppFactory', IDIFactory, '', fDIC);
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
  RegisterDataClass(PersistDIC, TParameterGroup);
  RegisterDataClass(PersistDIC, TEnvVariable);
  RegisterDataClass(PersistDIC, TEnvVariableGroup);
  RegisterDataClass(PersistDIC, TInterpreter);
  RegisterDataClass(PersistDIC, TSource);
  RegisterDataClass(PersistDIC, TConfiguration);
  //
  mReg := PersistDIC.Add(TStoreCache);
  //
  mReg := PersistDIC.Add(TPersistStore, IPersistStore, '', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory, cPersistID);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
  //
  mReg := PersistDIC.Add(TXmlStore, IPersistStoreDevice, 'xml');
  mReg.InjectProp('XMLFile', fDataFile);
  mReg.InjectProp('Factory', IPersistFactory, cPersistID);
  // binders(conection between data and GUI)
  mReg := PersistDIC.Add(TListBoxBinder, IRBTallyBinder, 'listbox');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory, cPersistID);
  //
  mReg := PersistDIC.Add(TDrawGridBinder, IRBTallyBinder, 'drawgrid');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory, cPersistID);
  //
  mReg := PersistDIC.Add(TListBoxBinder, IRBTallyBinder, 'sessions', ckSingle);
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory, cPersistID);
  //
  mReg := PersistDIC.Add(TRBDataBinder, IRBDataBinder);
end;

procedure TApp.RegisterCore;
var
  mReg: TDIReg;
begin
  // application container
  mReg := fDIC.Add(TDIContainer, cAppID, ckSingle);
  // persist container
  mReg := fDIC.Add(TDIContainer, cPersistID, ckSingle);
  // main DI factory
  mReg := fDIC.Add(TDIFactory, IDIFactory);
  mReg.InjectProp('Container', TDIContainer, cAppID, fDIC);
  // factory for persist data
  mReg := PersistDIC.Add(TPersistFactory, IPersistFactory, cPersistID, ckSingle);
  mReg.InjectProp('Container', TDIContainer, cPersistID, fDIC);
  // owner of interface object without release(clear interfaces on destroy)
  mReg := AppDIC.Add(TDIOwner, '', ckSingle);
end;

procedure TApp.RegisterServices;
begin
  RegisterCore;
  RegisterRtl;
  RegisterPersist;
  RegisterGUI;
  RegisterIDE;
  RegisterRunContainer;
end;

procedure TApp.Launch;
var
  mGUILauncher: TGUILauncher;
begin
  mGUILauncher := AppDIC.Locate(TGUILauncher);
  mGUILauncher.Launch;
end;

procedure TApp.DockMasterCreateControl(Sender: TObject; aName: string;
  var AControl: TControl; DoDisableAutoSizing: boolean);
begin

end;

constructor TApp.Create;
begin
  fDIC := TDIContainer.Create;
end;

destructor TApp.Destroy;
begin
  FreeAndNil(fDIC);
  inherited Destroy;
end;

class procedure TApp.Go;
var
  mApp: TApp;
begin
  mApp := TApp.Create;
  try
    mApp.Setup;
    mApp.RegisterServices;
    mApp.Launch;
  finally
    mApp.Free;
  end;
end;

end.

