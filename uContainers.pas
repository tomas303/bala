unit uContainers;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms, fgl,
  trl_ipersist, trl_idifactory, trl_irttibroker, trl_processrunner, Containers,
  uParameters, Controls, TypInfo;

type

  {$M+}

  { TContainer }

  TContainer = class(TInterfacedObject, IContainer)
  private
    fSession: IRBData;
    fSessionIDE: IContainerIDE;
    fProcessRunner: TProcessRunner;
    fBalaSource, fBalaFile: string;
    procedure SetRunIDE(AValue: IContainerIDE);
  protected
    function GetSession: IRBData;
    procedure SetSession(AValue: IRBData);
    function GetRunIDE: IContainerIDE;
    function GetState: TRunState;
    procedure OnPushOutput(const AData: string);
    procedure OnPushErrOutput(const AData: string);
    procedure OnFinished(const AExitCode: integer);
  protected
    procedure FillParameters(const AParameters: IPersistMany); overload;
    procedure FillParameters; overload;
    procedure FillEnvVariables(const AEnvVariables: IPersistMany); overload;
    procedure FillEnvVariables; overload;
    procedure AddBalaEnvVariables;
    function SaveToTempFile(const ASource, AFileName: string): string;
  public
    destructor Destroy; override;
    procedure Bind;
    procedure PinIDE(const AParent: TWinControl);
    procedure Start;
    procedure Pause;
    procedure Continue;
    procedure Terminate;
    procedure ShutDown;
  published
    property Session: IRBData read fSession write fSession;
    property SessionIDE: IContainerIDE read fSessionIDE write SetRunIDE;
    property ProcessRunner: TProcessRunner read fProcessRunner write fProcessRunner;
  end;

  {$M-}

  { TContainers }

  TContainers = class(TInterfacedObject, IContainers)
  protected type
    TContainerList = TFPGList<IContainer>;
  private
    fAppFactory: IDIFactory;
    fStore: IPersistStore;
    fList: TContainerList;
  private
    function GetCount: integer;
    function GetContainers(AIndex: integer): IContainer;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Add(const AID: string = ''): IContainer;
    procedure Delete(AIndex:  integer);
    procedure Load;
    procedure Save;
    procedure ShutDown;
    property Containers[AIndex: integer]: IContainer read GetContainers;
    property Count: integer read GetCount;
  published
    property AppFactory: IDIFactory read fAppFactory write fAppFactory;
    property Store: IPersistStore read fStore write fStore;
  end;

implementation

{ TContainer }

procedure TContainer.SetRunIDE(AValue: IContainerIDE);
begin
  fSessionIDE := AValue;
  Bind;
end;

function TContainer.GetSession: IRBData;
begin
  Result := fSession;
end;

procedure TContainer.SetSession(AValue: IRBData);
begin
  fSession := AValue;
  Bind;
end;

function TContainer.GetRunIDE: IContainerIDE;
begin
  Result := fSessionIDE;
end;

function TContainer.GetState: TRunState;
begin
  if fProcessRunner.IsRunning and not fProcessRunner.IsPaused then
    Result := rsRun
  else
  if not fProcessRunner.IsRunning and fProcessRunner.IsPaused then
    Result := rsPause
  else
  if not fProcessRunner.IsRunning and not fProcessRunner.IsPaused then
    Result := rsNone
  else
    Result := rsNone;
end;

procedure TContainer.OnPushErrOutput(const AData: string);
begin
  SessionIDE.PushErrOutput(AData);
end;

procedure TContainer.OnPushOutput(const AData: string);
begin
  SessionIDE.PushOutput(AData);
end;

procedure TContainer.OnFinished(const AExitCode: integer);
begin
  SessionIDE.PushExitCode(AExitCode);
end;

procedure TContainer.FillParameters(const AParameters: IPersistMany);
var
  i: integer;
begin
  for i := 0 to AParameters.Count - 1 do
    ProcessRunner.AddParameter(AParameters.AsPersistData[i].ItemByName['Value'].AsString);
end;

procedure TContainer.FillParameters;
var
  i: integer;
  mParams: IPersistMany;
begin
  ProcessRunner.ClearParameters;
  mParams := Session.ItemByName['Parameters'].AsInterface as IPersistMany;
  FillParameters(mParams);
end;

procedure TContainer.FillEnvVariables(const AEnvVariables: IPersistMany);
var
  i: integer;
begin
  for i := 0 to AEnvVariables.Count - 1 do
    ProcessRunner.AddEnvVariable(AEnvVariables.AsPersistData[i].ItemByName['Name'].AsString,
      AEnvVariables.AsPersistData[i].ItemByName['Value'].AsString);
end;

procedure TContainer.FillEnvVariables;
var
  i: integer;
  mEnvVars: IPersistMany;
  mEnvVarGroups: IPersistManyRefs;
begin
  ProcessRunner.ClearEnvVariables;
  mEnvVarGroups := Session.ItemByName['EnvVariableGroups'].AsInterface as IPersistManyRefs;
  for i := 0 to mEnvVarGroups.Count - 1 do begin
    mEnvVars := mEnvVarGroups[i].Data.ItemByName['EnvVariables'].AsInterface as IPersistMany;
    FillEnvVariables(mEnvVars);
  end;
  mEnvVars := Session.ItemByName['EnvVariables'].AsInterface as IPersistMany;
  FillEnvVariables(mEnvVars);
  AddBalaEnvVariables;
end;

procedure TContainer.AddBalaEnvVariables;
begin
  ProcessRunner.AddEnvVariable('BALA_SOURCE', fBalaSource);
  ProcessRunner.AddEnvVariable('BALA_FILE', fBalaFile);
end;

function TContainer.SaveToTempFile(const ASource, AFileName: string): string;
var
  mFS: TFileStream;
  mContent: UTF8String;
begin
  if AFileName <> '' then
  begin
     Result := IncludeTrailingPathDelimiter(GetTempDir) + AFileName;
     if FileExists(Result) then
       DeleteFile(Result);
  end
  else
    Result := GetTempFileName;
  mFS := TFileStream.Create(Result, fmCreate);
  try
    if ASource <> '' then begin
      mContent := ASource;
      mFS.Write(mContent[1], Length(mContent));
    end;
  finally
    mFS.Free;
  end;
end;

destructor TContainer.Destroy;
begin
  FreeAndNil(fProcessRunner);
  inherited Destroy;
end;

procedure TContainer.Bind;
begin
  if SessionIDE <> nil then
    SessionIDE.Bind(Self);
  if ProcessRunner <> nil then begin
    ProcessRunner.OnPushOutput := OnPushOutput;
    ProcessRunner.OnPushErrOutput := OnPushErrOutput;
    ProcessRunner.OnFinish := OnFinished;
  end;
end;

procedure TContainer.PinIDE(const AParent: TWinControl);
begin
  if SessionIDE <> nil then
    SessionIDE.Pin(AParent);
end;

procedure TContainer.Start;
var
  mLaunch: TScriptLaunch;
begin
  SessionIDE.Flush;
  ProcessRunner.Command := Session.ItemByName['Interpreter'].AsString;
  fBalaSource := Session.ItemByName['Source'].AsString;
  fBalaFile := '';
  ProcessRunner.Batch := ''; // this is whait is sent to standart input
  mLaunch := TScriptLaunch(GetEnumValue(TypeInfo(TScriptLaunch), Session.ItemByName['ScriptLaunch'].AsString));
  case mLaunch of
    slSentToInput:
      ProcessRunner.Batch := fBalaSource;
    slSaveToFile:
      begin
        fBalaFile := SaveToTempFile(Session.ItemByName['Source'].AsString, Session.ItemByName['FileName'].AsString);
      end;
  end;
  FillParameters;
  FillEnvVariables;
  ProcessRunner.PrefillCurentEnvironment := Session.ItemByName['PrefillCurrentEnvironment'].AsBoolean;
  ProcessRunner.Start;
end;

procedure TContainer.Pause;
begin
  ProcessRunner.Pause;
end;

procedure TContainer.Continue;
begin
  ProcessRunner.Continue;
end;

procedure TContainer.Terminate;
begin
  ProcessRunner.Terminate;
end;

procedure TContainer.ShutDown;
begin
  SessionIDE.ShutDown;
  // necessary - SessionIDE is not ref. counted(TComponent descendant), so
  // when this object will destroy, in SessionIDE can be reference to no longer
  // exists object
  SessionIDE := nil;
end;

{ TContainers }

function TContainers.GetCount: integer;
begin
  Result := fList.Count;
end;

function TContainers.GetContainers(AIndex: integer): IContainer;
begin
  Result := fList[AIndex];
end;

procedure TContainers.AfterConstruction;
begin
  inherited AfterConstruction;
  fList := TContainerList.Create;
end;

procedure TContainers.BeforeDestruction;
begin
  FreeAndNil(fList);
  inherited BeforeDestruction;
end;

function TContainers.Add(const AID: string = ''): IContainer;
begin
  Result := AppFactory.Locate(IContainer, AID);
  Result.Bind;
  fList.Add(Result);
end;

procedure TContainers.Delete(AIndex: integer);
begin
  Store.Delete(Containers[AIndex].Session);
  fList.Delete(AIndex);
end;

procedure TContainers.Load;
var
  mList: IPersistRefList;
  i: integer;
  mContainer: IContainer;
begin
  mList := (Store as IPersistQuery).SelectClass('TSession');
  for i := 0 to mList.Count - 1 do
  begin
    mContainer := Add;
    mContainer.Session := mList[i].Data;
  end;
end;

procedure TContainers.Save;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Store.Save(Containers[i].Session);
  end;
end;

procedure TContainers.ShutDown;
var
  i: integer;
begin
  for i := 0 to Count - 1 do
  begin
    Containers[i].ShutDown;
  end;
end;

end.

