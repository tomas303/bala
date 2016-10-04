unit fSession;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ExtCtrls, ActnList, ComCtrls, Grids, tvl_ibindings,
  Containers, Sessions, trl_ipersist, OsUtils, SettingsBroker, trl_irttibroker,
  iBala, uHighLight, SynEditHighlighter, typinfo, LMessages, LCLType;

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
    chkRollOutput: TCheckBox;
    lblSourceHighLight: TLabel;
    SourceHighLight_bind: TComboBox;
    EnvVariableGroups_bind: TStringGrid;
    EnvVariables_bind: TStringGrid;
    pnEnvironment: TPanel;
    Parameters_bind: TStringGrid;
    pgSettings: TPageControl;
    PrefillCurrentEnvironment_bind: TCheckBox;
    edExitCode: TEdit;
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
    fSettingsBroker: ISettingsBroker;
  protected
    procedure PushOutput(const AData: string);
    procedure PushErrOutput(const AData: string);
    procedure PushExitCode(const AExitCode: integer);
    procedure Pin(const AParent: TWinControl);
    procedure Bind(const AContainer: IContainer);
    procedure Flush;
    procedure ShutDown;
  protected
    function GetSessionSetting(const ASessionSettings: IPersistMany; const AID: string): IRBData;
    procedure LoadSettings;
    procedure SaveSettings;
  protected
    procedure ResetHighlighter(const AHighLightName: string);
    procedure SourceHighlighterDataChange(const ADataItem: IRBDataItem; AControl: TWinControl);
  protected
    procedure ScrollOutput;
  public
    property RunContainer: IContainer read fRunContainer;
  published
    property Binder: IRBDataBinder read fBinder write fBinder;
    property BehaveBinder: IRBBehavioralBinder read fBehaveBinder write fBehaveBinder;
    property SessionsBinder: IRBTallyBinder read fSessionsBinder write fSessionsBinder;
    property Factory: IPersistFactory read fFactory write fFactory;
    property Store: IPersistStore read fStore write fStore;
    property OsUtils: IOsUtils read fOsUtils write fOsUtils;
    property SettingsBroker: ISettingsBroker read fSettingsBroker write fSettingsBroker;
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
  ScrollOutput;
end;

procedure TSessionForm.PushErrOutput(const AData: string);
begin
  Output_bind.Lines.Add(AData);
  Binder.Flush(Output_bind);
  ScrollOutput;
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
  Binder.BindControl(AParent, 'Name');
  LoadSettings;
end;

procedure TSessionForm.Bind(const AContainer: IContainer);
begin
  fRunContainer := AContainer;
  BehaveBinder.Bind(Self);
  Binder.BindArea(Self, RunContainer.Session);
  Binder.RegisterChangeEvent('SourceHighLight', @SourceHighlighterDataChange);
  ResetHighlighter(Binder.Data.ItemByName['SourceHighLight'].AsString);
end;

procedure TSessionForm.Flush;
begin
  Output_bind.Clear;
  Binder.Flush;
end;

procedure TSessionForm.ShutDown;
begin
  Binder.UnregisterChangeEvent('SourceHighLight', @SourceHighlighterDataChange);
  SaveSettings;
end;

function TSessionForm.GetSessionSetting(const ASessionSettings: IPersistMany;
  const AID: string): IRBData;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to ASessionSettings.Count - 1 do begin
    if ASessionSettings.AsPersistData[i].ItemByName['SessionID'].AsString = AID then
    begin
      Result := ASessionSettings.AsPersistData[i];
      Exit;
    end;
  end;
  ASessionSettings.Count := ASessionSettings.Count + 1;
  Result := ASessionSettings.AsPersistData[ASessionSettings.Count - 1];
  Result.ItemByName['SessionID'].AsString := AID;
end;

procedure TSessionForm.LoadSettings;
var
  mSessionSettings: IPersistMany;
  mSessionSetting: IRBData;
begin
  mSessionSettings := SettingsBroker.AppSetting.ItemByName['SessionIdeSettings'].AsInterface as IPersistMany;
  mSessionSetting := GetSessionSetting(mSessionSettings, RunContainer.Session.ItemByName['ID'].AsString);
  if mSessionSetting.ItemByName['SplitterEnvironmentPos'].AsInteger > 0 then
    splEnvironment.SetSplitterPosition(mSessionSetting.ItemByName['SplitterEnvironmentPos'].AsInteger);
  if mSessionSetting.ItemByName['SplitterMainPos'].AsInteger > 0 then
    splMain.SetSplitterPosition(mSessionSetting.ItemByName['SplitterMainPos'].AsInteger);
  if mSessionSetting.ItemByName['SplitterOutputPos'].AsInteger > 0 then
    splOutput.SetSplitterPosition(mSessionSetting.ItemByName['SplitterOutputPos'].AsInteger);
  chkRollOutput.Checked := mSessionSetting.ItemByName['RollOutput'].AsBoolean;
end;

procedure TSessionForm.SaveSettings;
var
  mSessionSettings: IPersistMany;
  mSessionSetting: IRBData;
begin
  mSessionSettings := SettingsBroker.AppSetting.ItemByName['SessionIdeSettings'].AsInterface as IPersistMany;
  mSessionSetting := GetSessionSetting(mSessionSettings, RunContainer.Session.ItemByName['ID'].AsString);
  mSessionSetting.ItemByName['SplitterEnvironmentPos'].AsInteger := splEnvironment.GetSplitterPosition;
  mSessionSetting.ItemByName['SplitterMainPos'].AsInteger := splMain.GetSplitterPosition;
  mSessionSetting.ItemByName['SplitterOutputPos'].AsInteger := splOutput.GetSplitterPosition;
  mSessionSetting.ItemByName['RollOutput'].AsBoolean := chkRollOutput.Checked;
end;

procedure TSessionForm.ResetHighlighter(const AHighLightName: string);
var
  mHL: THighLight;
  mHLClass: TSynCustomHighlighterClass;
begin
  mHL := THighLight(GetEnumValue(TypeInfo(THighLight), AHighLightName));
  mHLClass := cHighLighters[mHL];
  if (mHLClass = nil) and (Source_bind.Highlighter <> nil) then
    Source_bind.Highlighter.Free
  else
  if (mHLClass <> nil) and (Source_bind.Highlighter = nil) then
    Source_bind.Highlighter := mHLClass.Create(Source_bind)
  else
  if (mHLClass <> nil) and (Source_bind.Highlighter <> nil) then
  begin
    Source_bind.Highlighter.Free;
    Source_bind.Highlighter := mHLClass.Create(Source_bind);
  end;
end;

procedure TSessionForm.SourceHighlighterDataChange(
  const ADataItem: IRBDataItem; AControl: TWinControl);
begin
  ResetHighlighter(ADataItem.AsString);
end;

procedure TSessionForm.ScrollOutput;
var
  mMsg: TLMessage;
  mMsgScroll: TLMScroll absolute mMsg;
begin
  if chkRollOutput.Checked then begin
    mMsgScroll.Msg := LM_VSCROLL;
    mMsgScroll.ScrollCode := SB_BOTTOM;
    Output_bind.WndProc(mMsg);
  end;
end;

end.

