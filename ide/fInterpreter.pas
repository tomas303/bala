unit fInterpreter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, EditBtn, Menus, tvl_iedit, tvl_ibindings, trl_irttibroker;

type

  { TInterpreterForm }

  TInterpreterForm = class(TForm, IEditData)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    Command_bind: TFileNameEdit;
    lblName: TLabel;
    lblCommand: TLabel;
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

var
  InterpreterForm: TInterpreterForm;

implementation

{$R *.lfm}

{ TInterpreterForm }

function TInterpreterForm.Edit(const AData: IRBData): Boolean;
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

