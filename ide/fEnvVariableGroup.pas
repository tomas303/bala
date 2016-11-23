unit fEnvVariableGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Buttons, tal_iedit, tvl_ibindings, OsUtils, trl_irttibroker;

type

  { TEnvVariableGroupForm }

  TEnvVariableGroupForm = class(TForm, IEditData)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnFillActualVariables: TBitBtn;
    lblName: TLabel;
    Name_bind: TEdit;
    EnvVariables_bind: TStringGrid;
    procedure btnFillActualVariablesClick(Sender: TObject);
  private
    fBinder: IRBDataBinder;
    fBehaveBinder: IRBBehavioralBinder;
    fOsUtils: IOsUtils;
  protected
    function Edit(const AData: IRBData): Boolean;
  published
    property Binder: IRBDataBinder read fBinder write fBinder;
    property BehaveBinder: IRBBehavioralBinder read fBehaveBinder write fBehaveBinder;
    property OsUtils: IOsUtils read fOsUtils write fOsUtils;
  end;

var
  EnvVariableGroupForm: TEnvVariableGroupForm;

implementation

{$R *.lfm}

{ TEnvVariableGroupForm }

procedure TEnvVariableGroupForm.btnFillActualVariablesClick(Sender: TObject);
begin
  OsUtils.AddSystemEnvVariables(Binder.Data, 'EnvVariables', 'Name', 'Value');
  Binder.DataChange;
end;

function TEnvVariableGroupForm.Edit(const AData: IRBData): Boolean;
var
  mData: IRBData;
begin
  BehaveBinder.Bind(Self);
  try
    Binder.BindArea(Self, AData);
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

