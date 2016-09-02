unit fMain;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
  Menus, ComCtrls, tvl_iedit, tvl_ibindings, trl_ipersist, trl_irttibroker,
  trl_idifactory, Containers;

type

  { TMainForm }

  TMainForm = class(TForm, IMainForm)
    acParameterGroups: TAction;
    acEnvVariableGroups: TAction;
    acInterpreters: TAction;
    acConfigurations: TAction;
    acSources: TAction;
    acNewRunContainer: TAction;
    acDeleteRunContainter: TAction;
    alMain: TActionList;
    miDeleteContainer: TMenuItem;
    miSettings: TMenuItem;
    miContainers: TMenuItem;
    miAddContainer: TMenuItem;
    miEnvVariableGroups: TMenuItem;
    miInterpreters: TMenuItem;
    miConfigurations: TMenuItem;
    mnMain: TMainMenu;
    miParameterGroups: TMenuItem;
    miSources: TMenuItem;
    pgContainers: TPageControl;
    procedure acDeleteRunContainterExecute(Sender: TObject);
    procedure acEnvVariableGroupsExecute(Sender: TObject);
    procedure acInterpretersExecute(Sender: TObject);
    procedure acNewRunContainerExecute(Sender: TObject);
    procedure acParameterGroupsExecute(Sender: TObject);
    procedure acConfigurationsExecute(Sender: TObject);
    procedure acSourcesExecute(Sender: TObject);
  private
    fParameterGroups: IListData;
    fEnvVariableGroups: IListData;
    fInterpreters: IListData;
    fSources: IListData;
    fConfigurations: IListData;
    fRunContainers: IContainers;
  protected
    //IMainForm
    procedure StartUp;
    procedure ShutDown;
    function GetMainForm: TForm;
    procedure ConnectCloseHandler(OnCloseHandler: TCloseEvent);
  published
    property ParameterGroups: IListData read fParameterGroups write fParameterGroups;
    property EnvVariableGroups: IListData read fEnvVariableGroups write fEnvVariableGroups;
    property Interpreters: IListData read fInterpreters write fInterpreters;
    property Sources: IListData read fSources write fSources;
    property Configurations: IListData read fConfigurations write fConfigurations;
    property Containers: IContainers read fRunContainers write fRunContainers;
  end;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.acParameterGroupsExecute(Sender: TObject);
begin
  ParameterGroups.List;
end;

procedure TMainForm.acConfigurationsExecute(Sender: TObject);
begin
  Configurations.List;
end;

procedure TMainForm.acSourcesExecute(Sender: TObject);
begin
  Sources.List;
end;

procedure TMainForm.StartUp;
var
  i: integer;
  mTab: TTabSheet;
begin
  Containers.Load;
  for i := 0 to Containers.Count - 1 do
  begin
    mTab := pgContainers.AddTabSheet;
    mTab.Caption := '---|---';
    Containers[i].PinIDE(mTab);
    mTab.Tag := i;
  end;
  Show;
end;

procedure TMainForm.ShutDown;
begin
  Containers.Save;
end;

function TMainForm.GetMainForm: TForm;
begin
  Result := Self;
end;

procedure TMainForm.ConnectCloseHandler(OnCloseHandler: TCloseEvent);
begin
  AddHandlerClose(OnCloseHandler);
end;

procedure TMainForm.acEnvVariableGroupsExecute(Sender: TObject);
begin
  EnvVariableGroups.List;
end;

procedure TMainForm.acDeleteRunContainterExecute(Sender: TObject);
begin
  Containers.Delete(pgContainers.ActivePage.Tag);
  pgContainers.ActivePage.Free;
end;

procedure TMainForm.acInterpretersExecute(Sender: TObject);
begin
  Interpreters.List;
end;

procedure TMainForm.acNewRunContainerExecute(Sender: TObject);
var
  mContainer: IContainer;
  mTab: TTabSheet;
begin
  mContainer := Containers.Add;
  mTab := pgContainers.AddTabSheet;
  mTab.Caption := '---|---';
  mContainer.PinIDE(mTab);
end;

end.

