unit Containers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, trl_irttibroker, Controls;

type

  TRunState = (rsNone, rsRun, rsPause);

  { IContainer }

  IContainer = interface
  ['{7CFF9553-6EB9-4182-8D17-01D9E2D5347D}']
    procedure Bind;
    procedure PinIDE(const AParent: TWinControl);
    function GetSession: IRBData;
    function GetState: TRunState;
    procedure SetSession(AValue: IRBData);
    property Session: IRBData read GetSession write SetSession;
    property State: TRunState read GetState;
    procedure Start;
    procedure Pause;
    procedure Continue;
    procedure Terminate;
  end;

  { IContainerIDE }

  IContainerIDE = interface
  ['{1979707A-BADE-4937-A05A-43D49A13DA0A}']
    procedure Bind(const AContainer: IContainer);
    procedure Pin(const AParent: TWinControl);
    procedure Flush;
    procedure PushOutput(const AData: string);
    procedure PushExitCode(const AExitCode: integer);
  end;

  { IContainers }

  IContainers = interface
  ['{8CEEA302-3F26-4A70-9FA5-20CECAFA5346}']
    function GetContainers(AIndex:  integer): IContainer;
    function GetCount: integer;
    function Add(const AID: string = ''): IContainer;
    procedure Load;
    procedure Save;
    property Containers[AIndex:  integer]: IContainer read GetContainers; default;
    property Count: integer read GetCount;
  end;

implementation

end.

