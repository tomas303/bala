unit fMain;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, ComCtrls, ExtCtrls, StdCtrls, tal_iedit, tvl_ibindings, trl_ipersist,
  trl_irttibroker, trl_idifactory, Containers, SettingsBroker, tal_ilauncher;

type

  { TMainForm }

  TMainForm = class(TForm, IMainForm)
    acOpenSession: TAction;
    acNewSession: TAction;
    acDeleteSession: TAction;
    acDuplicateSession: TAction;
    acOpenEnvVariableGroups: TAction;
    alMain: TActionList;
    btnAdd: TButton;
    btnEnvVariableGroups: TButton;
    btnDuplicate: TButton;
    btnDelete: TButton;
    lbSessions: TListBox;
    paSessionsNavigaton: TPanel;
    paSessions: TPanel;
    pgContainers: TPageControl;
    spSessions: TSplitter;
    procedure acDeleteSessionExecute(Sender: TObject);
    procedure acDuplicateSessionExecute(Sender: TObject);
    procedure acNewSessionExecute(Sender: TObject);
    procedure acOpenEnvVariableGroupsExecute(Sender: TObject);
    procedure acOpenSessionExecute(Sender: TObject);
    procedure lbSessionsDblClick(Sender: TObject);
  private
    fRunContainers: IContainers;
    fSessionsBinder: IRBTallyBinder;
    fFactory: IPersistFactory;
    fStore: IPersistStore;
    fAppFactory: IDIFactory;
    fEnvVariableGroups: IListData;
    fSettingsBroker: ISettingsBroker;
  protected
    //IMainForm
    procedure StartUp;
    procedure ShutDown;
    function GetMainForm: TForm;
    procedure ConnectCloseHandler(OnCloseHandler: TCloseEvent);
  protected
    function FindContainer(ASession: IRBData): IContainer;
    function GetContainer(ASession: IRBData): IContainer;
    function PinContainer(const AContainer: IContainer): integer;
    function FindContainerTab(const AContainer: IContainer): integer;
  protected
    procedure LoadSettings;
    procedure SaveSettings;
  published
    property Containers: IContainers read fRunContainers write fRunContainers;
    property SessionsBinder: IRBTallyBinder read fSessionsBinder write fSessionsBinder;
    property Factory: IPersistFactory read fFactory write fFactory;
    property Store: IPersistStore read fStore write fStore;
    property AppFactory: IDIFactory read fAppFactory write fAppFactory;
    property EnvVariableGroups: IListData read fEnvVariableGroups write fEnvVariableGroups;
    property SettingsBroker: ISettingsBroker read fSettingsBroker write fSettingsBroker;
  end;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.StartUp;
begin
  LoadSettings;
  SessionsBinder.Bind(lbSessions, 'TSession');
  Show;
end;

procedure TMainForm.ShutDown;
begin
  SessionsBinder.Unbind;
  Containers.ShutDown;
  SaveSettings;
end;

function TMainForm.GetMainForm: TForm;
begin
  Result := Self;
end;

procedure TMainForm.ConnectCloseHandler(OnCloseHandler: TCloseEvent);
begin
  AddHandlerClose(OnCloseHandler);
end;

function TMainForm.FindContainer(ASession: IRBData): IContainer;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Containers.Count - 1 do
    if Containers[i].Session = ASession then
    begin
      Result := Containers[i];
      Break;
    end;
end;

function TMainForm.GetContainer(ASession: IRBData): IContainer;
begin
  Result := FindContainer(ASession);
  if Result = nil then
  begin
    Result := Containers.Add;
    Result.Session := ASession;
  end;
end;

function TMainForm.PinContainer(const AContainer: IContainer): integer;
var
  mTab: TTabSheet;
begin
  mTab := pgContainers.AddTabSheet;
  mTab.Caption := AContainer.Session.ItemByName['Name'].AsString;
  AContainer.PinIDE(mTab);
  mTab.Tag := NativeInt(AContainer as TObject);
  Result := mTab.PageIndex;
end;

function TMainForm.FindContainerTab(const AContainer: IContainer): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to pgContainers.PageCount - 1 do
    if pgContainers.Pages[i].Tag = NativeInt(AContainer as TObject) then
    begin
      Result := i;
      Break;
    end;
end;

procedure TMainForm.LoadSettings;
var
  mMainSettings: IRBData;
begin
  mMainSettings := SettingsBroker.AppSetting.ItemByName['MainIDESettings'].AsInterface as IRBData;
  if mMainSettings.ItemByName['Left'].AsInteger > 0 then
    Left := mMainSettings.ItemByName['Left'].AsInteger;
  if mMainSettings.ItemByName['Top'].AsInteger > 0 then
    Top := mMainSettings.ItemByName['Top'].AsInteger;
  if mMainSettings.ItemByName['Width'].AsInteger > 0 then
    Width := mMainSettings.ItemByName['Width'].AsInteger;
  if mMainSettings.ItemByName['Height'].AsInteger > 0 then
    Height := mMainSettings.ItemByName['Height'].AsInteger;
  if mMainSettings.ItemByName['SplitterSessionsPos'].AsInteger > 0 then
    spSessions.SetSplitterPosition(mMainSettings.ItemByName['SplitterSessionsPos'].AsInteger);
end;

procedure TMainForm.SaveSettings;
var
  mMainSettings: IRBData;
begin
  mMainSettings := SettingsBroker.AppSetting.ItemByName['MainIDESettings'].AsInterface as IRBData;
  mMainSettings.ItemByName['Left'].AsInteger := Left;
  mMainSettings.ItemByName['Top'].AsInteger := Top;
  mMainSettings.ItemByName['Width'].AsInteger := Width;
  mMainSettings.ItemByName['Height'].AsInteger := Height;
  mMainSettings.ItemByName['SplitterSessionsPos'].AsInteger := spSessions.GetSplitterPosition;
end;

procedure TMainForm.acDeleteSessionExecute(Sender: TObject);
begin
  if SessionsBinder.CurrentData = nil then
    Exit;
  Store.Delete(SessionsBinder.CurrentData);
  Store.Flush;
  SessionsBinder.Reload;
end;

procedure TMainForm.acDuplicateSessionExecute(Sender: TObject);
var
  mContainer: IContainer;
  mTab: integer;
  mSession: IRBData;
begin
  if SessionsBinder.CurrentData = nil then
    Exit;
  mContainer := Containers.Add;
  mSession := Factory.CreateObject('TSession');
  mSession.Assign(SessionsBinder.CurrentData);
  mSession.ItemByName['Name'].AsString := 'duplicate ' + mSession.ItemByName['Name'].AsString;
  mContainer.Session := mSession;
  mTab := PinContainer(mContainer);
  pgContainers.ActivePageIndex := mTab;
end;

procedure TMainForm.acNewSessionExecute(Sender: TObject);
var
  mContainer: IContainer;
  mTab: integer;
begin
  mContainer := Containers.Add;
  mContainer.Session := Factory.CreateObject('TSession');
  mTab := PinContainer(mContainer);
  pgContainers.ActivePageIndex := mTab;
end;

procedure TMainForm.acOpenEnvVariableGroupsExecute(Sender: TObject);
begin
  EnvVariableGroups.List;
end;

procedure TMainForm.acOpenSessionExecute(Sender: TObject);
var
  mContainer: IContainer;
  mTab: integer;
begin
  if SessionsBinder.CurrentData = nil then
    Exit;
  mContainer := GetContainer(SessionsBinder.CurrentData);
  mTab := FindContainerTab(mContainer);
  if mTab = -1 then
    mTab := PinContainer(mContainer);
  pgContainers.ActivePageIndex := mTab;
end;

procedure TMainForm.lbSessionsDblClick(Sender: TObject);
begin
  acOpenSession.Execute;
end;

end.

