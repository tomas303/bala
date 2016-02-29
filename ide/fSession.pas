unit fSession;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ExtCtrls, ActnList, ComCtrls, Grids, tvl_ibindings,
  Containers;

type

  { TSessionForm }

  TSessionForm = class(TForm, IContainerIDE)
    acStart: TAction;
    acPause: TAction;
    acContinue: TAction;
    acTerminate: TAction;
    alRun: TActionList;
    edExitCode: TEdit;
    EnvVariableGroups_bind: TStringGrid;
    EnvVariables_bind: TStringGrid;
    ilRun: TImageList;
    ilRun1: TImageList;
    Interpreter_bind: TEdit;
    lblExitCode: TLabel;
    lblInterpreter: TLabel;
    lblSource: TLabel;
    Output_bind: TSynEdit;
    pnOutput: TPanel;
    ParameterGroups_bind: TStringGrid;
    Parameters_bind: TStringGrid;
    pgSettings: TPageControl;
    pnOutputInfo: TPanel;
    pnRun: TPanel;
    pnSource: TPanel;
    Session_bind: TPanel;
    Source_bind: TSynEdit;
    splEnvironment: TSplitter;
    splParameters: TSplitter;
    splMain: TSplitter;
    splOutput: TSplitter;
    tabEnvironment: TTabSheet;
    tabParameters: TTabSheet;
    tbRun: TToolBar;
    tbStart: TToolButton;
    tbPause: TToolButton;
    tbTerminate: TToolButton;
    procedure acContinueExecute(Sender: TObject);
    procedure acPauseExecute(Sender: TObject);
    procedure acStartExecute(Sender: TObject);
    procedure acTerminateExecute(Sender: TObject);
    procedure alRunUpdate(AAction: TBasicAction; var Handled: Boolean);
  private
    fBinder: IRBDataBinder;
    fBehaveBinder: IRBBehavioralBinder;
    fRunContainer: IContainer;
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
  EnvVariableGroups_bind.Enabled := RunContainer.State in [rsNone];
  EnvVariables_bind.Enabled := RunContainer.State in [rsNone];
  ParameterGroups_bind.Enabled := RunContainer.State in [rsNone];
  Parameters_bind.Enabled := RunContainer.State in [rsNone];
  Output_bind.ReadOnly := True;
end;

procedure TSessionForm.acPauseExecute(Sender: TObject);
begin
  RunContainer.Pause;
  tbPause.Action := acContinue;
end;

procedure TSessionForm.acContinueExecute(Sender: TObject);
begin
  RunContainer.Continue;
  tbPause.Action := acPause;
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
  Parent := AParent;
  Align := alClient;
  Visible := True;
end;

procedure TSessionForm.Bind(const AContainer: IContainer);
begin
  fRunContainer := AContainer;
  BehaveBinder.Bind(Self);
  Binder.Bind(Self, fRunContainer.Session);
end;

procedure TSessionForm.Flush;
begin
  Output_bind.Clear;
  Binder.Flush;
end;

end.

