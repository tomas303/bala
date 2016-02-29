unit uSources;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist;

type
  TSource = class
  private
    fName: string;
    fSource: TMemoString;
  published
    property Name: string read fName write fName;
    property Source: TMemoString read fSource write fSource;
  end;

implementation

end.

