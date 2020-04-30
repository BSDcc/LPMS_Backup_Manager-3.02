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
  Dialogs, StdCtrls, Spin, INIFiles, LCLType, Buttons, EditBtn, HTTPSend;

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

  { TFLPMSBackupTemplate }

  TFLPMSBackupTemplate = class(TForm)
    btnCancel: TButton;
    btnViewer: TSpeedButton;
    btnSMSTest: TButton;
    btnUpdate: TButton;
    cbSMSProvider: TComboBox;
    cbBackupType: TComboBox;
    cbT02: TComboBox;
    cbT03: TComboBox;
    cbT01: TComboBox;
    deDirectory: TDirectoryEdit;
    dlgOpen: TOpenDialog;
    edtDBPrefix: TEdit;
    edtDBSuffix: TEdit;
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
    lblT01: TLabel;
    lblT02: TLabel;
    lblT03: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
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

    procedure FormActivate(Sender: TObject);
{
    procedure btnCancelClick(Sender: TObject);
    procedure btnSMSTestClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure btnViewerClick(Sender: TObject);
    procedure cbSMSProviderChange(Sender: TObject);
    procedure edtDBPrefixChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
}
  private { private declarations }
{
    DoSave          : boolean;    // Controls whether a Save is required before Exiting
    DBPrefixChanged : boolean;    // Indicates  change in the value of DBPrefix
    KeepDBPrefix    : string;     // Retain value of DBPrefix for later use
    RegString       : string;     // Points to the correct INI file taking MultiCompany into account
}
  public  { public declarations }
{
    SMSProvider     : integer;    // Index for the SMS Provider dropdown list
    BackupBlock     : integer;    // Number of records to read in one operation
    SMSUser         : string;     // UserID for SMS Provider login
    SMSPassword     : string;     // Password for SMS Provider login
    DBPrefix        : string;     // DBPRefix value passed from calling program
    BackupViewer    : string;     // App to view the last backup
    MultiCompany    : boolean;    // Multi Company value passed from calling program
}

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
  FLPMSBackupTemplate: TFLPMSBackupTemplate;

{
  IniFile     : TINIFile;    // IniFile holding defaults
  DebugOn     : boolean;     // Debug switch
}
implementation

uses ldBackupApp;

{$R *.lfm}

{ TFLPMSBackupTemplate }

//------------------------------------------------------------------------------
// Executed before the form is displayed
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate.FormActivate(Sender: TObject);
var
   HostName  : string;

begin

{

   FLPMSBackupTemplate.Caption := 'LPMS Backup Manager';
   DebugOn := false;

   if (MultiCompany = false) then
      RegString := 'LPMS.ini'
   else
      RegString := 'LPMS_' + DBPrefix + '.ini';

//--- Get the value of Hostname

   IniFile := TINIFile.Create(RegString);
   HostName := IniFile.ReadString('Parameters','BackupHostName','www.bluecrane.cc');
   IniFile.Destroy;

   edtHostName.Text := HostName;
   edtDBPrefix.Text := DBPrefix;
   KeepDBPrefix     := DBPrefix;

   cbMultiCpy.Checked      := MultiCompany;
   cbSMSProvider.ItemIndex := SMSProvider;
   edtDBUser.Text         := SMSUser;
   edtDBPass.Text         := SMSPassword;
   speBlockSize.Value      := BackupBlock;
   edtViewer.Text          := BackupViewer;
   cbSMSProvider.SetFocus;

   if ((cbSMSProvider.ItemIndex > 0) and (edtDBUser.Text <> '') and (edtDBPass.Text <> '')) then
      btnSMSTest.Enabled := true
   else
      btnSMSTest.Enabled := false;

   btnSMSTest.Default := true;
   DoSave             := false;
   DBPrefixChanged    := false;
   btnCancel.Enabled  := false;
   btnUpdate.Caption  := 'Return';
}
end;

{
//------------------------------------------------------------------------------
// Executed before the form is finally closed
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
   if (DoSave = true) then begin
      if (Application.MessageBox('WARNING: There are unsaved changes. Click [Yes] to ignore the changes or [No] to return to the changes.','LPMS Backup Manager - Configuration', (MB_YESNO + MB_ICONWARNING )) = IDNO) then begin
         CloseAction := caNone;
         Exit;
      end;
   end;

//--- Write the updated values to the Registry taking the value of MultiCompany
//--- into account

   if (cbMultiCpy.Checked = false) then
      RegString := 'LPMS.ini'
   else
      RegString := 'LPMS_' + edtDBPrefix.Text + '.ini';

   IniFile := TINIFile.Create(RegString);

   IniFile.WriteInteger('Parameters','BackupSMSProvider',cbSMSProvider.ItemIndex);
   IniFile.WriteInteger('Parameters','BackupBlock',speBlockSize.Value);
   IniFile.WriteString('Parameters','BackupSMSUser',edtDBUser.Text);
   IniFile.WriteString('Parameters','BackupSMSPass',edtDBPass.Text);
   IniFile.WriteString('Parameters','BackupHostName',edtHostName.Text);
   IniFile.WriteString('Parameters','BackupViewer',edtViewer.Text);

   IniFile.Destroy;
end;

//------------------------------------------------------------------------------
// User changed the DBPrefix
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate.edtDBPrefixChange(Sender: TObject);
begin
   DBPrefixChanged := true;
   cbSMSProviderChange(Sender);
end;

//------------------------------------------------------------------------------
// A field on the screen has changed
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate.cbSMSProviderChange(Sender: TObject);
begin
   DoSave             := true;
   btnUpdate.Caption  := 'Update';
   btnCancel.Enabled  := true;
   btnSMSTest.Enabled := false;
end;

//------------------------------------------------------------------------------
// User clicked on the Test button
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate.btnSMSTestClick(Sender: TObject);
var
   PosStart   : integer;
   PosEnd     : integer;
   URL        : string;
   Params     : string;
   Answer     : string;
   Credits    : string;
   Req_Result : string;
   Response   : TMemoryStream;

begin
   Response := TMemoryStream.Create;

//--- Construct the RPC based on the selected interface

   case cbSMSProvider.ItemIndex of
      1: begin
         URL := 'http://www.mymobileapi.com/api5/http5.aspx';
         Params   := 'Type=credits&username=' + edtDBUser.Text + '&password=' + edtDBPass.Text;
      end;

      2: begin
         URL    := 'http://bulksms.2way.co.za/eapi/user/get_credits/1/1.1';
         Params := 'username=' + edtDBUser.Text + '&password=' + edtDBPass.Text;
      end;

      3: begin
         URL    := 'http://www.winsms.co.za/api/credits.ASP';
         Params := 'User=' + edtDBUser.Text + '&Password=' + edtDBPass.Text;
      end;
   end;

//--- Do the HTTP Post/Get operation

   try
      SMSDone := HTTPpostURL(URL, Params, Response);
   finally
   end;

//--- Extract the result from the HTTP Post/Get operation

   SetString(Answer, PAnsiChar(Response.Memory), Response.Size);
   Response.Destroy;

   case cbSMSProvider.ItemIndex of
      1: begin
         PosStart := AnsiPos('<result>',Answer);
         PosEnd   := AnsiPos('</result>',Answer);
         Req_Result := Copy(Answer,PosStart + 8,PosEnd - (PosStart + 8));

         if (lowercase(Req_Result) = 'true') then begin
            PosStart := AnsiPos('<credits>',Answer);
            PosEnd   := AnsiPos('</credits>',Answer);
            Credits  := Copy(Answer,PosStart + 9,PosEnd - (PosStart + 9));
            Application.MessageBox(PChar('Test successful - Number of Credits Remaining: ' + Credits),'LPMS Backup Manager - Configuration',(MB_OK + MB_ICONINFORMATION));
         end else
            Application.MessageBox('Test unsuccessful - Check "User name" and/or "Password".','LPMS Backup Manager - Configuration',(MB_OK + MB_ICONWARNING));
      end;

      2: begin
         Req_Result := Copy(Answer,1,1);

         if (Req_Result = '0') then begin
            PosStart := AnsiPos('.', Answer);
            Credits  := Copy(Answer,3,PosStart);
            Application.MessageBox(PChar('Test successful - Number of Credits Remaining: ' + Credits),'LPMS Backup Manager - Configuration',(MB_OK + MB_ICONINFORMATION));
         end else
            Application.MessageBox('Test unsuccessful - Check "User name" and/or "Password".','LPMS Backup Manager - Configuration',(MB_OK + MB_ICONWARNING));
      end;

      3: begin
         Req_Result := Copy(Answer,9,999999);

         if (lowercase(Req_Result) = 'fail') then
            Application.MessageBox('Test unsuccessful - Check "User name" and/or "Password".','LPMS Backup Manager - Configuration',(MB_OK + MB_ICONWARNING))
         else begin
            Credits  := Req_Result;
            Application.MessageBox(PChar('Test successful - Number of Credits Remaining: ' + Credits),'LPMS Backup Manager - Configuration',(MB_OK + MB_ICONINFORMATION));
         end;
      end;
   end;
end;

//------------------------------------------------------------------------------
// User clicked on the Update button
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate.btnUpdateClick(Sender: TObject);
begin
   if (btnUpdate.Caption = 'Return') then begin
      Close;
      Exit;
   end;

   if (DBPrefixChanged = true) then begin
      if (Length(edtDBPrefix.Text) <> 6) then begin
         Application.MessageBox('DBPrefix appears to be invalid - please provide a valid DBPrefix','LPMS Backup Manager - Configuration', (MB_OK + MB_ICONWARNING));
         edtDBPrefix.SetFocus;
         Exit;
      end;

      if (Application.MessageBox('WARNING: Changing "DBPrefix" to an invalid value may prevent LPMS Backup Manager from functioning properly!' + #10 + #10 + 'Click [Yes] to continue or [No] to reset the change and return.','LPMS Backup Manager - Configuration', (MB_YESNO + MB_ICONWARNING )) = IDNO) then begin
         edtDBPrefix.Text := KeepDBPrefix;
         edtDBPrefix.SetFocus;
         Exit;
      end;
   end;

   if ((cbSMSProvider.ItemIndex > 0) and (edtDBUser.Text = '')) then begin
      Application.MessageBox('"User name" is a required field and cannot be blank.','LPMS Backup Manager - Configuration', (MB_OK + MB_ICONWARNING ));
      edtDBUser.SetFocus;
      Exit;
   end;

   if ((cbSMSProvider.ItemIndex > 0) and (edtDBPass.Text = '')) then begin
      Application.MessageBox('"Password" is a required field and cannot be blank.','LPMS Backup Manager - Configuration', (MB_OK + MB_ICONWARNING ));
      edtDBPass.SetFocus;
      Exit;
   end;

//--- Update the values in the calling program

   FLPMSBackup.SMSUserID       := edtDBUser.Text;
   FLPMSBackup.SMSPassword     := edtDBPass.Text;
   FLPMSBackup.HostName        := edtHostName.Text;
   FLPMSBackup.DBPrefix        := edtDBPrefix.Text;
   FLPMSBackup.KeepSMSProvider := cbSMSProvider.ItemIndex;
   FLPMSBackup.KeepBackupBlock := speBlockSize.Value;
   FLPMSBackup.DoSave          := true;
   FLPMSBackup.MultiCompany    := cbMultiCpy.Checked;
   FLPMSBackup.BackupViewer    := edtViewer.Text;
//   FLPMSBackup.SMSProviderName := cbSMSProvider.Text;

   if ((cbSMSProvider.ItemIndex > 0) and (edtDBUser.Text <> '') and (edtDBPass.Text <> '')) then
      btnSMSTest.Enabled := true
   else
      btnSMSTest.Enabled := false;

   DoSave := false;
   btnUpdate.Caption := 'Return';
   btnCancel.Enabled := false;

   KeepDBPrefix := edtDBPrefix.Text;
   DBPrefixChanged := false;
end;

//------------------------------------------------------------------------------
// User clicked on the Cancel button
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate.btnCancelClick(Sender: TObject);
begin
   if (DoSave = true) then begin
      if (Application.MessageBox('WARNING: There are unsaved changes. Click [Yes] to ignore the changes or [No] to return to the changes.','LPMS Backup Manager - Configuration', (MB_YESNO + MB_ICONWARNING )) = IDNO) then begin
         Exit;
      end;
   end;

   cbSMSProvider.ItemIndex := FLPMSBackup.KeepSMSProvider;
   edtDBUser.Text         := FLPMSBackup.SMSUserID;
   edtDBPass.Text         := FLPMSBackup.SMSPassword;
   edtViewer.Text          := FLPMSBackup.BackupViewer;

   if ((cbSMSProvider.ItemIndex > 0) and (edtDBUser.Text <> '') and (edtDBPass.Text <> '')) then
      btnSMSTest.Enabled := true
   else
      btnSMSTest.Enabled := false;

   btnUpdate.Caption := 'Return';
   btnCancel.Enabled := false;
   DoSave := false;

   edtDBPrefix.Text := KeepDBPrefix;
   DBPrefixChanged  := false;
end;

//------------------------------------------------------------------------------
// User clicked on the button to select a backup file viewer
//------------------------------------------------------------------------------
procedure TFLPMSBackupTemplate.btnViewerClick(Sender: TObject);
begin
   {$IFDEF WINDOWS}
      dlgOpen.DefaultExt := '.exe';
      dlgOpen.Filter     := 'Application Files (*.exe)|*.exe|All Files (*.*)|*.*';
   {$ELSE}
      dlgOpen.DefaultExt := '';
      dlgOpen.Filter     := 'All Files (*.*)|*.*';
   {$ENDIF}

   if (dlgOpen.Execute = true) then
      edtViewer.Text := dlgOpen.FileName;
end;
}
//------------------------------------------------------------------------------
end.

