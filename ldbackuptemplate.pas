//------------------------------------------------------------------------------
// Date.......: 30 April 2020
// System.....: LPMS Backup Manager
// Application: Form to create and maintain a backup template
// Platform...: Lazarus (Linux, macOS & Windows)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------
// History....: 30 April 2020 - Create first version
//------------------------------------------------------------------------------

unit ldbackuptemplate;

{$mode objfpc}{$H+}

interface

//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
  Classes, SysUtils, FileUtil, IDEWindowIntf, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Spin, INIFiles, LCLType, Buttons, EditBtn;

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

  { TFLPMSBackupTemplate }

  TFLPMSBackupTemplate = class(TForm)
    btnCancel: TButton;
    btnViewer: TSpeedButton;
    btnUpdate: TButton;
    cbSMSProvider: TComboBox;
    cbxBackupType: TComboBox;
    cbxT02: TComboBox;
    cbxT03: TComboBox;
    cbxT01: TComboBox;
    cbDBSuffix: TCheckBox;
    deDirectory: TDirectoryEdit;
    dlgOpen: TOpenDialog;
    edtDBPrefix: TEdit;
    edtSMSNumber: TEdit;
    edtSMSPass: TEdit;
    edtHostName: TEdit;
    edtTemplate: TEdit;
    edtSMSUser: TEdit;
    edtViewer: TEdit;
    edtDBPass: TEdit;
    edtDBUser: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    lblModified: TLabel;
    lblL01: TLabel;
    lblL02: TLabel;
    lblL03: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblSMSPass: TLabel;
    lblSMSPass1: TLabel;
    lblSMSUser: TLabel;
    lblSMSUser1: TLabel;
    rbSuccess: TRadioButton;
    rbFailure: TRadioButton;
    rbAlways: TRadioButton;
    rbNever: TRadioButton;
    sdSelDir: TSelectDirectoryDialog;
    speBlockSize: TSpinEdit;

    procedure btnCancelClick( Sender: TObject);
    procedure btnUpdateClick( Sender: TObject);
    procedure btnViewerClick( Sender: TObject);
    procedure cbxBackupTypeChange( Sender: TObject);
    procedure deDirectoryAcceptDirectory(Sender: TObject; var Value: String);
    procedure edtHostNameChange( Sender: TObject);
    procedure FormActivate(Sender: TObject);

  private { private declarations }
    DoSave          : boolean;    // Controls whether a Save is required before Exiting

  public  { public declarations }

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
  FLPMSBackupTemplate: TFLPMSBackupTemplate;

implementation

uses ldBackupApp;

{$R *.lfm}

{ TFLPMSBackupTemplate }

//------------------------------------------------------------------------------
// Executed before the form is displayed
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate.FormActivate(Sender: TObject);
begin

   cbxBackupTypeChange(Sender);

   cbXT01.ItemIndex := FLPMSBackup.BackupTemplate.Instr_Rec.BackupT01;
   cbXT02.ItemIndex := FLPMSBackup.BackupTemplate.Instr_Rec.BackupT02;
   cbXT03.ItemIndex := FLPMSBackup.BackupTemplate.Instr_Rec.BackupT03;

   DoSave := False;

   lblModified.Caption := '-- Browse --';
   btnUpdate.Enabled   := False;

   edtHostName.SetFocus;

end;

//------------------------------------------------------------------------------
// User Clicked on the Cancel button
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate. btnCancelClick( Sender: TObject);
begin

   if DoSave = True then begin

      if (MessageDlg('Backup Manager','WARNING: There are unsaved changes!' + #10 + #10 + 'Click [Yes] to return to the Backup Manager; or' + #10 + 'Click [No] to continue editing the defaults.', mtWarning, mbYesNo, 0) =  mrYes) then
         Close
      else
         Exit;

   end else begin

     Close;

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the Update button
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate. btnUpdateClick( Sender: TObject);
var
   IniFile : TINIFile;

begin

//--- Update the Template Record

   FLPMSBackup.BackupTemplate.Instr_Rec.BackupBlock       := speBlockSize.Value;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSProvider := cbSMSProvider.ItemIndex;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupType        := cbxBackupType.ItemIndex;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupT01         := cbxT01.ItemIndex;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupT02         := cbxT02.ItemIndex;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupT03         := cbxT03.ItemIndex;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSAlways   := rbAlways.Checked;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSFailure  := rbFailure.Checked;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSNever    := rbNever.Checked;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSSuccess  := rbSuccess.Checked;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupDBPass      := edtDBPass.Text;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupDBPrefix    := edtDBPrefix.Text;

   if cbDBSuffix.Checked = True then
      FLPMSBackup.BackupTemplate.Instr_Rec.BackupDBSuffix := '_LPMS'
   else
      FLPMSBackup.BackupTemplate.Instr_Rec.BackupDBSuffix := '';

   FLPMSBackup.BackupTemplate.Instr_Rec.BackupDBUser      := edtDBUser.Text;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupHostName    := edtHostName.Text;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupLocation    := deDirectory.Text;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSNumber   := edtSMSNumber.Text;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSPass     := edtSMSPass.Text;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSUser     := edtSMSUser.Text;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupTemplate    := edtTemplate.Text;
   FLPMSBackup.BackupTemplate.Instr_Rec.BackupViewer      := edtViewer.Text;
   FLPMSBackup.BackupTemplate.LastBackup                  := '';

//--- Update the 'Registry'

   IniFile := TINIFile.Create(FLPMSBackup.CfgFile);

   IniFile.WriteInteger('Template','BackupBlock',FLPMSBackup.BackupTemplate.Instr_Rec.BackupBlock);
   IniFile.WriteInteger('Template','BackupSMSProvider',FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSProvider);
   IniFile.WriteInteger('Template','BackupType',FLPMSBackup.BackupTemplate.Instr_Rec.BackupType);
   IniFile.WriteInteger('Template','BackupT01',FLPMSBackup.BackupTemplate.Instr_Rec.BackupT01);
   IniFile.WriteInteger('Template','BackupT02',FLPMSBackup.BackupTemplate.Instr_Rec.BackupT02);
   IniFile.WriteInteger('Template','BackupT03',FLPMSBackup.BackupTemplate.Instr_Rec.BackupT03);
   IniFile.WriteBool('Template','BackupSMSAlways',FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSAlways);
   IniFile.WriteBool('Template','BackupSMSFailure',FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSFailure);
   IniFile.WriteBool('Template','BackupSMSNever',FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSNever);
   IniFile.WriteBool('Template','BackupSMSSuccess',FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSSuccess);
   IniFile.WriteString('Template','BackupDBPass',FLPMSBackup.Encode(FLPMSBackup.BackupTemplate.Instr_Rec.BackupDBPass));
   IniFile.WriteString('Template','BackupDBPrefix',FLPMSBackup.BackupTemplate.Instr_Rec.BackupDBPrefix);
   IniFile.WriteString('Template','BackupDBSuffix',FLPMSBackup.BackupTemplate.Instr_Rec.BackupDBSuffix);
   IniFile.WriteString('Template','BackupDBUser',FLPMSBackup.BackupTemplate.Instr_Rec.BackupDBUser);
   IniFile.WriteString('Template','BackupHostName',FLPMSBackup.BackupTemplate.Instr_Rec.BackupHostName);
   IniFile.WriteString('Template','BackupLocation',FLPMSBackup.BackupTemplate.Instr_Rec.BackupLocation);
   IniFile.WriteString('Template','BackupSMSNumber',FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSNumber);
   IniFile.WriteString('Template','BackupSMSPass',FLPMSBackup.Encode(FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSPass));
   IniFile.WriteString('Template','BackupSMSUser',FLPMSBackup.BackupTemplate.Instr_Rec.BackupSMSUser);
   IniFile.WriteString('Template','BackupTemplate',FLPMSBackup.BackupTemplate.Instr_Rec.BackupTemplate);
   IniFile.WriteString('Template','BackupViewer',FLPMSBackup.BackupTemplate.Instr_Rec.BackupViewer);
   IniFile.WriteString('Template','BackupLast',FLPMSBackup.BackupTemplate.LastBackup);

   IniFile.Destroy;

   btnUpdate.Enabled   := False;
   DoSave              := False;
   lblModified.Caption := '-- Browse --';

end;

//------------------------------------------------------------------------------
// User clicked on the button to select a backup file viewer
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate. btnViewerClick( Sender: TObject);
begin

   if Trim(edtViewer.Text) <> '' then
      dlgOpen.InitialDir := ExtractFilePath(edtViewer.Text);

{$ifdef WINDOWS}
   dlgOpen.DefaultExt := '.exe';
   dlgOpen.Filter     := 'Application Files (*.exe)|*.exe|All Files (*.*)|*.*';
{$endif}

{$ifdef LINUX}
   dlgOpen.DefaultExt := '';
   dlgOpen.Filter     := 'All Files (*.*)|*.*';
{$endif}

{$ifdef DARWIN}
   dlgOpen.DefaultExt := '';
   dlgOpen.Filter     := 'All Files (*.*)|*.*';
{$endif}

   if (dlgOpen.Execute = true) then
      edtViewer.Text := dlgOpen.FileName;

end;

//------------------------------------------------------------------------------
// Backup Type Changed
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate. cbxBackupTypeChange( Sender: TObject);
var
   idx : integer;

begin

//--- Set the correct display fields based on the Backup Type

   lblL01.Visible := true;
   lblL02.Visible := true;
   lblL03.Visible := false;
   cbxT01.Visible := true;
   cbxT02.Visible := true;
   cbxT03.Visible := false;

   if (cbxBackupType.Text = 'Hourly') then begin

      lblL01.Caption := 'Minute Mark:';
      cbxT01.Clear;

      for idx := 1 to 12 do
         cbxT01.Items.Add(FLPMSBackup.MinArray[idx]);

      lblL02.Visible := false;
      cbxT02.Visible := false;

   end else if (cbxBackupType.Text = 'Daily') then begin

      lblL01.Caption := 'Select Hour:';
      cbxT01.Clear;

      for idx := 1 to 24 do
         cbxT01.Items.Add(FLPMSBackup.HourArray[idx]);

      lblL02.Caption := 'Select Minute:';
      cbxT02.Clear;

      for idx := 1 to 12 do
         cbxT02.Items.Add(FLPMSBackup.MinArray[idx]);

      lblL02.Visible := true;
      cbxT02.Visible := true;

   end else if (cbxBackupType.Text = 'Weekly') then begin

      lblL01.Caption := 'Select Day:';
      cbxT01.Clear;

      for idx := 1 to 7 do
         cbxT01.Items.Add(FLPMSBackup.WeekArray[idx]);

      lblL02.Caption := 'Select Hour:';
      cbxT02.Clear;

      for idx := 1 to 24 do
         cbxT02.Items.Add(FLPMSBackup.HourArray[idx]);

      lblL03.Caption := 'Select Minute:';
      cbxT03.Clear;

      for idx := 1 to 12 do
         cbxT03.Items.Add(FLPMSBackup.MinArray[idx]);

      lblL02.Visible := true;
      cbxT02.Visible := true;
      lblL03.Visible := true;
      cbxT03.Visible := true;

   end;

   cbxT01.ItemIndex := 0;
   cbxT02.ItemIndex := 0;
   cbxT03.ItemIndex := 0;

   edtHostNameChange(Sender);

end;

//------------------------------------------------------------------------------
// User selected a directory
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate.deDirectoryAcceptDirectory(Sender: TObject; var Value: String);
var
   ThisDir : string;

begin

   ThisDir := Value;

//--- We need to add a final OS dependant delimiter (OSDelim) to the path.
//--- If we are running on Winblows then the path must be at least 4 chars in
//--- length (e.g 'C:\A') before we can add the backslash

{$IFDEF WINDOWS}

   if (Length(ThisDir) > 3) then
      ThisDir := ThisDir + FLPMSBackup.OSDelim;

{$ELSE}

   ThisDir := ThisDir + FLPMSBackup.OSDelim;

{$ENDIF}

   Value := ThisDir;

end;

//------------------------------------------------------------------------------
// An editable field changed
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate. edtHostNameChange( Sender: TObject);
begin

   lblModified.Caption := '** Modified **';
   btnUpdate.Enabled   := True;

   DoSave := True;

end;

//------------------------------------------------------------------------------
end.

