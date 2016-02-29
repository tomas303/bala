unit fParameterGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Buttons, tvl_iedit, tvl_ibindings, trl_irttibroker;

type

  { TParameterGroupForm }

  TParameterGroupForm = class(TForm, IEditData)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    Name_bind: TEdit;
    lblName: TLabel;
    Parameters_bind: TStringGrid;
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

{ TParameterGroupForm }

function TParameterGroupForm.Edit(const AData: IRBData): Boolean;
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

