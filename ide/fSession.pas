unit fSession;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Menus, ExtCtrls, ActnList, ComCtrls, Grids, tvl_ibindings,
  Containers, Sessions, trl_ipersist, OsUtils, trl_irttibroker,
  iBala, uHighLight, SynEditHighlighter, typinfo, LMessages, LCLType, fgl, SynEditMarkupSpecialLine,
  tal_ihistorysettings;

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
    lblFileName: TLabel;
    lblSourceHighLight: TLabel;
    lblScriptLaunch: TLabel;
    FileName_bind: TEdit;
    pnMain: TPanel;
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
    seOutput: TSynEdit;
    pnOutput: TPanel;
    pnOutputInfo: TPanel;
    pnRun: TPanel;
    pnSource: TPanel;
    pmEnvVariables: TPopupMenu;
    Session_bind: TPanel;
    ScriptLaunch_bind: TComboBox;
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
    procedure seOutputSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
  private
    fBinder: IRBDataBinder;
    fBehaveBinder: IRBBehavioralBinder;
    fRunContainer: IContainer;
    fSessionsBinder: IRBTallyBinder;
    fFactory: IPersistFactory;
    fStore: IPersistStore;
    fOsUtils: IOsUtils;
    fHistorySettings: IHistorySettings;
  private type
    TErrLines = specialize TFPGList<integer>;
  private
    fErrLines: TErrLines;
  protected
    function CorrectIDToName(AComp: TComponent; const AID: string): string;
    procedure PushOutput(const AData: string);
    procedure PushErrOutput(const AData: string);
    procedure PushExitCode(const AExitCode: integer);
    procedure Pin(const AParent: TWinControl);
    procedure Bind(const AContainer: IContainer);
    procedure Flush;
    procedure ShutDown;
  protected
    function GetSessionSetting(const ASessionSettings: IPersistMany; const AID: string): IRBData;
  protected
    procedure ResetHighlighter(const AHighLightName: string);
    procedure SourceHighlighterDataChange(const ADataItem: IRBDataItem; AControl: TWinControl);
    procedure ScriptLaunchDataChange(const ADataItem: IRBDataItem; AControl: TWinControl);
  protected
    procedure ScrollOutput;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property RunContainer: IContainer read fRunContainer;
  published
    property Binder: IRBDataBinder read fBinder write fBinder;
    property BehaveBinder: IRBBehavioralBinder read fBehaveBinder write fBehaveBinder;
    property SessionsBinder: IRBTallyBinder read fSessionsBinder write fSessionsBinder;
    property Factory: IPersistFactory read fFactory write fFactory;
    property Store: IPersistStore read fStore write fStore;
    property OsUtils: IOsUtils read fOsUtils write fOsUtils;
    property HistorySettings: IHistorySettings read fHistorySettings write fHistorySettings;
  end;

implementation

{$R *.lfm}

{ TSessionForm }

procedure TSessionForm.acStartExecute(Sender: TObject);
begin
  fErrLines.Clear;
  seOutput.Clear;
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
  seOutput.ReadOnly := True;
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

procedure TSessionForm.seOutputSpecialLineColors(Sender: TObject;
  Line: integer; var Special: boolean; var FG, BG: TColor);
begin
  if fErrLines.IndexOf(Line) > -1 then begin
    Special := True;
    BG := clRed;
  end;
end;

function TSessionForm.CorrectIDToName(AComp: TComponent; const AID: string): string;
var
  i: integer;
begin
  Result := AID;
  for i := 1 to Length(AID) do
    case AID[i] of
      '0'..'9','a'..'z', 'A'..'Z', '_':;
    else
      Result[i] := '_';
    end;
  Result := AComp.Name + '_' + Result;
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
  seOutput.Lines.Add(AData);
  Binder.Flush(seOutput);
  ScrollOutput;
end;

procedure TSessionForm.PushErrOutput(const AData: string);
begin
  seOutput.Lines.Add(AData);
  fErrLines.Add(seOutput.Lines.Count);
  Binder.Flush(seOutput);
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
  pnMain.Parent := AParent;
  Binder.BindControl(AParent, 'Name');
  HistorySettings.Load(pnOutput, Binder.Data.ItemByName['ID'].AsString);
  HistorySettings.Load(splMain, Binder.Data.ItemByName['ID'].AsString);
  HistorySettings.Load(splOutput, Binder.Data.ItemByName['ID'].AsString);
end;

procedure TSessionForm.Bind(const AContainer: IContainer);
begin
  fRunContainer := AContainer;
  BehaveBinder.Bind(Self);
  Binder.BindArea(Self, RunContainer.Session);
  Binder.RegisterChangeEvent('SourceHighLight', @SourceHighlighterDataChange);
  ResetHighlighter(Binder.Data.ItemByName['SourceHighLight'].AsString);
  Binder.RegisterChangeEvent('ScriptLaunch', @ScriptLaunchDataChange);
end;

procedure TSessionForm.Flush;
begin
  seOutput.Clear;
  Binder.Flush;
end;

procedure TSessionForm.ShutDown;
begin
  Binder.UnregisterChangeEvent('SourceHighLight', @SourceHighlighterDataChange);
  HistorySettings.Save(pnOutput, Binder.Data.ItemByName['ID'].AsString);
  HistorySettings.Save(splMain, Binder.Data.ItemByName['ID'].AsString);
  HistorySettings.Save(splOutput, Binder.Data.ItemByName['ID'].AsString);
end;

function TSessionForm.GetSessionSetting(const ASessionSettings: IPersistMany;
  const AID: string): IRBData;
var
  i: integer;
begin
  seOutput.Lines;
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

procedure TSessionForm.ScriptLaunchDataChange(const ADataItem: IRBDataItem;
  AControl: TWinControl);
var
  mLaunch: TScriptLaunch;
begin
  mLaunch := TScriptLaunch(GetEnumValue(TypeInfo(TScriptLaunch), ADataItem.AsString));
  FileName_bind.Enabled := mLaunch = slSaveToFile;
end;

procedure TSessionForm.ScrollOutput;
var
  mMsg: TLMessage;
  mMsgScroll: TLMScroll absolute mMsg;
begin
  if chkRollOutput.Checked then begin
    mMsgScroll.Msg := LM_VSCROLL;
    mMsgScroll.ScrollCode := SB_BOTTOM;
    seOutput.WndProc(mMsg);
  end;
end;

procedure TSessionForm.AfterConstruction;
begin
  inherited AfterConstruction;
  fErrLines := TErrLines.Create;
end;

procedure TSessionForm.BeforeDestruction;
begin
  FreeAndNil(fErrLines);
  inherited BeforeDestruction;
end;

end.

