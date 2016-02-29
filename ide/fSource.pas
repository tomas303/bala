unit fSource;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterBat, Forms, Controls,
  Graphics, Dialogs, Buttons, StdCtrls, Menus, tvl_iedit, tvl_ibindings,
  trl_irttibroker;

type

  { TSourceForm }

  TSourceForm = class(TForm, IEditData)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    lblName: TLabel;
    Name_bind: TEdit;
    Source_bind: TSynEdit;
    synBAT: TSynBatSyn;
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

{ TSourceForm }

function TSourceForm.Edit(const AData: IRBData): Boolean;
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

