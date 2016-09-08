unit fSession;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ExtCtrls, ActnList, ComCtrls, Grids, tvl_ibindings,
  Containers, Sessions, trl_ipersist, OsUtils;

type

  { TSessionForm }

  TSessionForm = class(TForm, IContainerIDE)
    acStart: TAction;
    acPause: TAction;
    acContinue: TAction;
    acTerminate: TAction;
    acSave: TAction;
    acAddOSVariables: TAction;
    alRun: TActionList;
    edExitCode: TEdit;
    EnvVariables_bind: TStringGrid;
    EnvVariableGroups_bind: TStringGrid;
    ilRun: TImageList;
    ilRun1: TImageList;
    Interpreter_bind: TEdit;
    miAddOSVariables: TMenuItem;
    Name_bind: TEdit;
    lblExitCode: TLabel;
    lblInterpreter: TLabel;
    lblName: TLabel;
    lblSource: TLabel;
    Output_bind: TSynEdit;
    pnOutput: TPanel;
    Parameters_bind: TStringGrid;
    pgSettings: TPageControl;
    pnOutputInfo: TPanel;
    pnRun: TPanel;
    pnSource: TPanel;
    pmEnvVariables: TPopupMenu;
    Session_bind: TPanel;
    Source_bind: TSynEdit;
    splEnvironment: TSplitter;
    splMain: TSplitter;
    splOutput: TSplitter;
    tabEnvironment: TTabSheet;
    tabParameters: TTabSheet;
    tbRun: TToolBar;
    tbStart: TToolButton;
    tbPause: TToolButton;
    tbTerminate: TToolButton;
    tbSave: TToolButton;
    procedure acAddOSVariablesExecute(Sender: TObject);
    procedure acContinueExecute(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acSaveExecute(Sender: TObject);
    procedure acStartExecute(Sender: TObject);
    procedure acTerminateExecute(Sender: TObject);
    procedure alRunUpdate(AAction: TBasicAction; var Handled: Boolean);
    procedure btnReturnConfigurationClick(Sender: TObject);
    procedure btnReturnInterpreterClick(Sender: TObject);
    procedure btnReturnSourceClick(Sender: TObject);
    procedure btnTakeConfigurationClick(Sender: TObject);
    procedure btnTakeInterpreterClick(Sender: TObject);
    procedure btnTakeSourceClick(Sender: TObject);
  private
    fBinder: IRBDataBinder;
    fBehaveBinder: IRBBehavioralBinder;
    fRunContainer: IContainer;
    fSessionsBinder: IRBTallyBinder;
    fFactory: IPersistFactory;
    fStore: IPersistStore;
    fOsUtils: IOsUtils;
  protected
    procedure PushOutput(const AData: string);
    procedure PushExitCode(const AExitCode: integer);
    procedure Pin(const AParent: TWinControl);
  protected
    procedure Bind(const AContainer: IContainer);
    procedure Flush;
  public
    property RunContainer: IContainer read fRunContainer;
  published
    property Binder: IRBDataBinder read fBinder write fBinder;
    property BehaveBinder: IRBBehavioralBinder read fBehaveBinder write fBehaveBinder;
    property SessionsBinder: IRBTallyBinder read fSessionsBinder write fSessionsBinder;
    property Factory: IPersistFactory read fFactory write fFactory;
    property Store: IPersistStore read fStore write fStore;
    property OsUtils: IOsUtils read fOsUtils write fOsUtils;
  end;

implementation

{$R *.lfm}

{ TSessionForm }

procedure TSessionForm.acStartExecute(Sender: TObject);
begin
  Output_bind.Clear;
  edExitCode.Text := '';
  edExitCode.Color := clWhite;
  tbPause.Action := acPause;
  RunContainer.Start;
end;

procedure TSessionForm.acTerminateExecute(Sender: TObject);
begin
  RunContainer.Terminate;
end;

procedure TSessionForm.alRunUpdate(AAction: TBasicAction; var Handled: Boolean);
begin
  acStart.Enabled := RunContainer.State in [rsNone];
  acPause.Enabled := RunContainer.State in [rsRun];
  acContinue.Enabled := RunContainer.State in [rsPause];
  acTerminate.Enabled := RunContainer.State in [rsRun, rsPause];
  Interpreter_bind.ReadOnly := RunContainer.State in [rsRun, rsPause];
  Source_bind.ReadOnly := RunContainer.State in [rsRun, rsPause];
  EnvVariables_bind.Enabled := RunContainer.State in [rsNone];
  Parameters_bind.Enabled := RunContainer.State in [rsNone];
  Output_bind.ReadOnly := True;
end;

procedure TSessionForm.btnReturnConfigurationClick(Sender: TObject);
var
  mLinks: ISessionLinks;
begin
  mLinks := RunContainer.Session.ItemByName['SessionLinks'].AsInterface as ISessionLinks;
  mLinks.PutToConfiguration;
  Binder.DataChange;
end;

procedure TSessionForm.btnReturnInterpreterClick(Sender: TObject);
var
  mLinks: ISessionLinks;
begin
  mLinks := RunContainer.Session.ItemByName['SessionLinks'].AsInterface as ISessionLinks;
  mLinks.PutToInterpreter;
  Binder.DataChange;
end;

procedure TSessionForm.btnReturnSourceClick(Sender: TObject);
var
  mLinks: ISessionLinks;
begin
  mLinks := RunContainer.Session.ItemByName['SessionLinks'].AsInterface as ISessionLinks;
  mLinks.PutToSource;
  Binder.DataChange;
end;

procedure TSessionForm.btnTakeConfigurationClick(Sender: TObject);
var
  mLinks: ISessionLinks;
begin
  mLinks := RunContainer.Session.ItemByName['SessionLinks'].AsInterface as ISessionLinks;
  mLinks.GetFromConfiguration;
  Binder.DataChange;
end;

procedure TSessionForm.btnTakeInterpreterClick(Sender: TObject);
var
  mLinks: ISessionLinks;
begin
  mLinks := RunContainer.Session.ItemByName['SessionLinks'].AsInterface as ISessionLinks;
  mLinks.GetFromInterpreter;
  Binder.DataChange;
end;

procedure TSessionForm.btnTakeSourceClick(Sender: TObject);
var
  mLinks: ISessionLinks;
begin
  mLinks := RunContainer.Session.ItemByName['SessionLinks'].AsInterface as ISessionLinks;
  mLinks.GetFromSource;
  Binder.DataChange;
end;

procedure TSessionForm.acPauseExecute(Sender: TObject);
begin
  RunContainer.Pause;
  tbPause.Action := acContinue;
end;

procedure TSessionForm.acSaveExecute(Sender: TObject);
begin
  if RunContainer <> nil then begin
    Store.Save(RunContainer.Session);
    Store.Flush;
    SessionsBinder.Reload;
  end;
end;

procedure TSessionForm.acContinueExecute(Sender: TObject);
begin
  RunContainer.Continue;
  tbPause.Action := acPause;
end;

procedure TSessionForm.acAddOSVariablesExecute(Sender: TObject);
begin
  OsUtils.AddSystemEnvVariables(Binder.Data, 'EnvVariables', 'Name', 'Value');
  Binder.DataChange;
end;

procedure TSessionForm.PushOutput(const AData: string);
begin
  Output_bind.Lines.Add(AData);
  Binder.Flush(Output_bind);
end;

procedure TSessionForm.PushExitCode(const AExitCode: integer);
begin
  edExitCode.Text := IntToStr(AExitCode);
  if AExitCode = 0 then
    edExitCode.Color := clLime
  else
    edExitCode.Color := clRed;
end;

procedure TSessionForm.Pin(const AParent: TWinControl);
begin
  while ControlCount > 0 do
  begin
    Controls[ControlCount - 1].Tag:=-1;
    Controls[ControlCount - 1].Parent := AParent;
  end;
end;

procedure TSessionForm.Bind(const AContainer: IContainer);
begin
  fRunContainer := AContainer;
  BehaveBinder.Bind(Self);
  Binder.Bind(Self, RunContainer.Session);
end;

procedure TSessionForm.Flush;
begin
  Output_bind.Clear;
  Binder.Flush;
end;

end.

