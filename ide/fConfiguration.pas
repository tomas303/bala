unit fConfiguration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterBat, Forms, Controls,
  Graphics, Dialogs, ActnList, ComCtrls, StdCtrls, ExtCtrls, Buttons, Grids,
  Menus, tvl_iedit, tvl_ibindings, trl_irttibroker;

type

  { TConfigurationForm }

  TConfigurationForm = class(TForm, IEditData)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    EnvVariableGroups_bind: TStringGrid;
    EnvVariables_bind: TStringGrid;
    Interpreter_bind: TComboBox;
    lblParameters: TLabel;
    lblInterpreter: TLabel;
    lblEnvVariables: TLabel;
    lblSource: TLabel;
    pnSettings: TPanel;
    ParameterGroups_bind: TStringGrid;
    Parameters_bind: TStringGrid;
    Source_bind: TComboBox;
    lblName: TLabel;
    Name_bind: TEdit;
  private
    fBinder: IRBDataBinder;
    fBehaveBinder: IRBBehavioralBinder;
  protected
    function Edit(const AData: IRBData): Boolean;
  published
    property Binder: IRBDataBinder read fBinder write fBinder;
    property BehaveBinder: IRBBehavioralBinder read fBehaveBinder write fBehaveBinder;
  end;

implementation

{$R *.lfm}

{ TConfigurationForm }

function TConfigurationForm.Edit(const AData: IRBData): Boolean;
var
  mData: IRBData;
begin
  BehaveBinder.Bind(Self);
  try
    Binder.Bind(Self, AData);
    try
      Result := ShowModal = mrOK;
    finally
      Binder.Unbind;
    end;
  finally
    BehaveBinder.Unbind;
  end;
end;

end.

