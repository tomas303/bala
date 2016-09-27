unit uHighLight;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, iBala, SynEditHighlighter,
  synhighlighterbat, synhighlightercpp, synhighlightercss, synhighlighterdiff,
  synhighlighterhtml, synhighlighterini, synhighlighterjava, synhighlighterjscript,
  synhighlighterpas, synhighlighterperl, synhighlighterphp, synhighlightersql,
  synhighlighterunixshellscript, synhighlightervb, synhighlighterxml,
  synhighlighterpython;

const
  cHighLighters: array[THighLight] of TSynCustomHighlighterClass =
    (nil, TSynBatSyn, TSynCppSyn, TSynCssSyn, TSynDiffSyn, TSynHTMLSyn, TSynIniSyn,
    TSynJavaSyn, TSynJScriptSyn, TSynPasSyn, TSynPerlSyn, TSynPHPSyn, TSynPythonSyn,
    TSynSQLSyn, TSynUNIXShellScriptSyn, TSynVBSyn, TSynXMLSyn);

implementation

end.

