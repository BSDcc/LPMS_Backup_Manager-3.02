//------------------------------------------------------------------------------
// Date.......: 02 June 2020
// System.....: LPMS Backup Manager
// Application: Form to show information such as the content of a backup
// ...........: instruction's ini file and a directory listing of the backup
// ...........: location.
// Platform...: Lazarus (Linux, macOS, Raspberry & Windows)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------
// History....: 02 June 2020 - Create first version
//------------------------------------------------------------------------------

unit ldShowInfo;

{$mode objfpc}{$H+}

interface

//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, FileUtil;

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

  { TFldShowInfo }

  TFldShowInfo = class(TForm)
    btnClose: TButton;
    memInfo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);

private { private declarations }

public  { public declarations }
    Config   : boolean;
    FileName : string;

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
  FldShowInfo: TFldShowInfo;

implementation

{$R *.lfm}

{ TFldShowInfo }

//------------------------------------------------------------------------------
// Executed before the Form is shown
//------------------------------------------------------------------------------
procedure TFldShowInfo.FormShow(Sender: TObject);
var
   idx1, idx2 : integer;
   Temp       : string;

begin

   if Config = True then
      memInfo.Lines.LoadFromFile(FileName)
   else begin

      memInfo.Lines := FindAllFiles(FileName,'*.lpb;*.zip',false);

//--- The list comes unsorted. Do a quick bubble sort on the list

      for idx1 := 0 to memInfo.Lines.Count - 1 do begin

         for idx2 := 0 to (memInfo.Lines.Count - 1) - idx1 do begin

            if (idx2 + 1) = memInfo.Lines.Count then
               continue;

            if memInfo.Lines[idx2] > memInfo.Lines[idx2 + 1] then begin

               Temp                    := memInfo.Lines[idx2];
               memInfo.Lines[idx2]     := memInfo.Lines[idx2 + 1];
               memInfo.Lines[idx2 + 1] := Temp;

            end;

         end;

      end;

      memInfo.VertScrollBar.Position := memInfo.Lines.Count - 1;

   end;

 end;

//------------------------------------------------------------------------------
// User clicked on the Close button
//------------------------------------------------------------------------------
procedure TFldShowInfo.btnCloseClick(Sender: TObject);
begin

  Close;

end;

//------------------------------------------------------------------------------
end.

