//------------------------------------------------------------------------------
// Date.......: 24 November 2015
// System.....: LPMS Backup Manager
// Platform...: Lazarus (Linux, Mac & Windows)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------
// History....: 24 November 2015 - Create first version
//------------------------------------------------------------------------------

unit ldbackupfirst;

{$mode objfpc}{$H+}

interface

//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLType;

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

  { TFLPMSBackupFirst }

  TFLPMSBackupFirst = class(TForm)
    btnGo: TButton;
    btnCancel: TButton;
    cbMultiCpy: TCheckBox;
    edtDBPrefix: TEdit;
    Label1: TLabel;
    procedure FormActivate(Sender: TObject);
    procedure btnGoClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);

  private { private declarations }

  public  { public declarations }
end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
  FLPMSBackupFirst: TFLPMSBackupFirst;

implementation

uses ldBackupApp;

{$R *.lfm}

{ TFLPMSBackupFirst }

//------------------------------------------------------------------------------
// Executed before the form is displayed
//------------------------------------------------------------------------------
procedure TFLPMSBackupFirst.FormActivate(Sender: TObject);
begin
   edtDBPrefix.Text   := '';
   cbMultiCpy.Checked := false;
   edtDBPrefix.SetFocus;
end;

//------------------------------------------------------------------------------
// User clicked on the Cancel Button
//------------------------------------------------------------------------------
procedure TFLPMSBackupFirst.btnCancelClick(Sender: TObject);
begin
   Close;
end;

//------------------------------------------------------------------------------
// User clicked on the Go Button
//------------------------------------------------------------------------------
procedure TFLPMSBackupFirst.btnGoClick(Sender: TObject);
begin
   if (Length(edtDBPrefix.Text) <> 6) then begin
      Application.MessageBox('DBPrefix appears to be invalid - please provide a valid DBPrefix','LPMS Backup Manager',(MB_OK + MB_ICONWARNING));
      edtDBPrefix.SetFocus;
      Exit;
   end;

   FLPMSBackupFirst.Hide;

   FLPMSBackup := TFLPMSBackup.Create(Application);

   FLPMSBackup.DBPrefix     := edtDBPrefix.Text;
   FLPMSBackup.MultiCompany := cbMultiCpy.Checked;
   FLPMSBackup.ShowModal;
   FLPMSBackup.Destroy;

   FLPMSBackupFirst.Show;
   Close;
end;

end.

