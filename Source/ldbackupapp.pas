//------------------------------------------------------------------------------
// Date.......: 09 October 2012
// System.....: LPMS Backup Manager
// Platform...: Lazarus (Linux, Mac & Windows)
// Author.....: Francois De Bruin Meyer (BlueCrane Software Development CC)
//------------------------------------------------------------------------------
// History....: 09 October 2012 - Create first version
//------------------------------------------------------------------------------
// Hisyory....: 19 November 2015 - Changed to align with LPMS 3.0.2 version.
// ...........: Updated to Version 2.0.1
// ...........: 03 April 2020 - Added ability to manage more than one Backup
// ...........: instruction
//------------------------------------------------------------------------------

unit ldBackupApp;

{$mode objfpc}{$H+}

interface

//------------------------------------------------------------------------------
// Uses clause
//------------------------------------------------------------------------------
uses
  Classes, SysUtils, sqldb, mysql50conn, mysql57conn, mysql56conn, LCLType,
  FileUtil, Forms, Controls, Graphics, Dialogs, ActnList, Menus, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, EditBtn, strutils, INIFiles, HTTPSend, Synacode,
  DateUtils;

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

  { TFLPMSBackup }

  TFLPMSBackup = class(TForm)
    Bevel1: TBevel;
    btnOpenLB: TSpeedButton;
    edtLocation: TDirectoryEdit;
    edtLastBackup: TEdit;
    imgSmall: TImageList;
    Label5: TLabel;
    lvLogSel: TListView;
    lvLogAll: TListView;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    pnlP00b: TPanel;
    pnlP00: TPanel;
    pnlP00a: TPanel;
    pnlP02: TPanel;
    pnlP00a3: TPanel;
    rbSMSAlways: TRadioButton;
    jvBrowse: TSelectDirectoryDialog;
    splSplit01: TSplitter;
    ToolsMinimise: TAction;
    ToolsRestore: TAction;
    btnCancel: TButton;
    btnClose: TButton;
    btnRunNow: TButton;
    btnMinimise: TButton;
    btnSMSConfig: TButton;
    btnUpdate: TButton;
    cbxT01: TComboBox;
    cbxT02: TComboBox;
    cbxT03: TComboBox;
    cbxType: TComboBox;
    edtSMSNumber: TEdit;
    edtTemplate: TEdit;
    ToolsExit: TAction;
    actList: TActionList;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    lblL01: TLabel;
    lblL02: TLabel;
    lblL03: TLabel;
    lblL04: TLabel;
    lblL05: TLabel;
    dlgOpen: TOpenDialog;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    pnlP03: TPanel;
    pnlP00a2: TPanel;
    pnlP00a1: TPanel;
    pnlP01: TPanel;
    pumTray: TPopupMenu;
    rbSMSFailure: TRadioButton;
    rbSMSNever: TRadioButton;
    rbSMSSuccess: TRadioButton;
    sqlQry1: TSQLQuery;
    sqlTran: TSQLTransaction;
    StatusBar1: TStatusBar;
    timTimer2: TTimer;
    timTimer: TTimer;
    TrayIcon: TTrayIcon;
    tvInstructions: TTreeView;

    procedure btnMinimiseClick(Sender: TObject);
    procedure btnOpenLBClick(Sender: TObject);
    procedure btnRunNowClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnSMSConfigClick(Sender: TObject);
    procedure btnUpdateClick(Sender: TObject);
    procedure cbxTypeChange(Sender: TObject);
    procedure edtLocationAcceptDirectory(Sender: TObject; var Value: String);
    procedure edtLocationButtonClick(Sender: TObject);
    procedure edtDBPrefixChange(Sender: TObject);
    procedure edtTemplateChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure timTimer2Timer(Sender: TObject);
    procedure timTimerTimer(Sender: TObject);
    procedure ToolsRestoreExecute(Sender: TObject);
    procedure TrayIconMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure tvInstructionsClick(Sender: TObject);

type
   HINT_OPTIONS = (HINT_WAITING, HINT_RUNNING);
   STAT_OPTIONS = (STAT_RUNNOW, STAT_WAITING, STAT_RUNNING, STAT_INACTIVE);

   REC_IniRecord = record
      BackupBlock       : integer;
      BackupSMSProvider : integer;
      BackupType        : integer;
      BackupT01         : integer;
      BackupT02         : integer;
      BackupT03         : integer;
      BackupSMSAlways   : boolean;
      BackupSMSFailure  : boolean;
      BackupSMSNever    : boolean;
      BackupSMSSuccess  : boolean;
      BackupDBPass      : string;
      BackupDBPrefix    : string;
      BackupDBSuffix    : string;
      BackupDBUser      : string;
      BackupHostName    : string;
      BackupLocation    : string;
      BackupSMSNumber   : string;
      BackupSMSPass     : string;
      BackupSMSUser     : string;
      BackupTemplate    : string;
      BackupViewer      : string;
   end;

   REC_Instructions = record
      Active            : boolean;
      Instruction       : string;
      Ini_File          : string;
      NextDate          : string;
      NextTime          : string;
      SymCpy            : string;
      SymVersion        : string;
      Instr_Rec         : Rec_IniRecord;
      Status            : integer;
   end;

   Array_Instructions = array of Rec_Instructions;

private { private declarations }
   LimitActive     : boolean;     // Indicates whether LIMIT is used when reading SQL records
   CanUpdate       : boolean;     // Used to override setting of DoSave
   DebugOn         : boolean;     // Debug switch
   SMSSuccess      : boolean;     // Send SMS on Success only
   SMSFailure      : boolean;     // Send SMS on Failure only
   SMSNever        : boolean;     // Don't send SMS ever
   SMSAlways       : boolean;     // Always send and SMS egardless of success or failure
   RecTotal        : integer;     // Holds unformatted rec count after backup
   BackupType      : integer;     // Type of backup to be taken
   BackupT01       : integer;     // Value for ComboBox 01
   BackupT02       : integer;     // Value for ComboBox 02
   BackupT03       : integer;     // Value for ComboBox 03
   NumInstr        : integer;     // Number of backup instructions in the Configuration File
   RecTotalF       : string;      // Holds formated rec count after backup
   ThisMsgF        : string;      // Holds formatted message containing outcome of current backup
   OutFile         : string;      // Name of the Backup file that will be created
   RegString       : string;      // Holds the name of the ini file
   BackupLocation  : string;      // Directory where backups are stored
   BackupTemplate  : string;      // Mask for creating the backup file name
   BackupLogFile   : string;      // Log file name
   SMSNumber       : string;      // Number to send SMS messages to
   SymCpy          : string;      // Symbolic Variable containing the Company name
   SymHost         : string;      // Symbolic Variable containing the Host name
   KeepVersion     : string;      // Holds the current DB version
   LastMsg         : string;      // Last SQL error message
   BackupMsg       : string;      // Holds nex backup date and time
   OSDelim         : string;      // Holds '/' or '\' depending on the OS
   CfgFile         : string;      // Name of the default Configuration File
   SMSResult       : string;      // Holds result returned by the SMS Provider
   StartTime       : TDateTime;   // Start time of the current Backup
   EndTime         : TDateTime;   // End time of the current Backup
   IniFile         : TINIFile;    // IniFile holding defaults
   LogList         : TStringList; // Holds the Disassembled Log Message
   InstrTokens     : TStrings;    // Holds the List of Backup Instruction names
   Instr_List      : Array_Instructions; // Array of in-memory Backup instructions

   function  GetInstruction() : integer;
   procedure ShowInstruction();
   procedure DBConnect(DBHost: string; DBName: string; DBUser: string; DBPass: string);
   procedure DispLogMsg(ThisDate: string; ThisTime: string; ThisInstr: string; ThisMsg: string);
   procedure DispLogMsg(RecCount: integer);
   procedure DispLogMsg(ThisMsg: string);
   procedure OpenCfg(FileName: string);
   procedure SaveCfg(FileName: string);
   procedure SaveLog(FileName: string);
   procedure OpenLog(FileName: string);
   procedure SendSMS(ThisMsg: string);
   procedure Set_Hint(ThisType: integer);
   function  Get_Send_XML(SMS: string) : string;
   function  DoBackup() : boolean;
   function  GetFieldType(ThisType: string) : integer;
   function  ReadDB(FileName: string; ThisType: integer) : boolean;
   procedure GetNextSlot(ThisInstr: integer);
   function  GetBasicInfo() : boolean;
   function  ReadTable(Table: string; LimitStart: integer; LimitEnd: integer) : boolean;

public  { public declarations }
   DoSave          : boolean;    // Tracks whether a Save is requried
   MultiCompany    : boolean;    // Indicates Multi Company or not
   KeepSMSProvider : integer;    // Holds SMS provider index across calls to Config module
   KeepBackupBlock : integer;    // Holds blocksize to read from MySQL across calls to Config module
   SMSUSerID       : string;     // SMS Subscription UserID
   SMSPassword     : string;     // SMS Subscription Password
   HostName        : string;     // Current Host Name
   DBPrefix        : string;     // Current Database Prefix
   SMSProviderName : string;     // NAme of SMS service provider
   BackupViewer    : string;     // Full path to the viewer that will be invoked to view the last successful backup
end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
   FLPMSBackup: TFLPMSBackup;

   ActiveInstr : integer;         // Set by the Timer to indicate which backup instruction popped
   InstrSel    : string;          // Contains the Text of the selected TreeView item

   NextDate    : TDateTime = 0;   // Next backup time
   TimeBackup  : TTime;           // Time (in minutes) of next backup

   SMSDone     : boolean = false; // True if SMS send was successful

{$IFDEF WINDOWS}
   sqlCon : TMySQL56Connection;       // Running on Windows
{$ELSE}
   {$IFDEF LINUX}
      sqlCon : TMySQL57Connection;    // Running on Linux
   {$ELSE}
      {$IFDEF LCLCARBON}
         sqlCon : TMySQL57Connection; // Running on macOS 10.*
      {$ENDIF}
   {$ENDIF}
{$ENDIF}


implementation

uses ldBackupSMSConfig;

{$R *.lfm}

{ TFLPMSBackup }

//------------------------------------------------------------------------------
// Executed before the form is displayed
//------------------------------------------------------------------------------
procedure TFLPMSBackup.FormActivate(Sender: TObject);
begin

//--- Determine the Platform on which we are running and set the defaults to be
//--- Platform specific

{$IFDEF WINDOWS}
   OSDelim := '\';
   sqlCon  := TMySQL56Connection.Create(nil);
{$ELSE}
   {$IFDEF LINUX}
      OSDelim := '/';
      sqlCon  := TMySQL57Connection.Create(nil);
   {$ELSE}
      {$IFDEF LCLCARBON}
         OSDelim := '/';
         sqlCon  := TMySQL57Connection.Create(nil);
      {$ENDIF}
   {$ENDIF}
{$ENDIF}

   sqlTran.DataBase    := sqlCon;
   sqlQry1.Transaction := sqlTran;

//--- Set up

   FLPMSBackup.Caption := 'LPMS Backup Manager';
   DebugOn             := True;
   CfgFile             := 'LPMS Backup Manager.cfg';

   tvInstructions.Items.Clear;
   pnlP00b.Visible := True;
   pnlP00a.Visible := False;

   BackupLogFile := 'LPMS Backup Logfile.txt';

//--- Open the Config file then build the in-memory list and the Treeview

   OpenCfg(CfgFile);
   tvInstructions.Items.Item[0].Selected := True;
   InstrSel := '@Backup';
   tvInstructions.AutoExpand := True;
   tvInstructions.FullExpand;

//--- Open and load the contents of the Log File

   LogList := TStringList.Create;
   OpenLog(BackupLogFile);
   DispLogMsg('Starting LPMS Backup Manager');

//--- Start the Scheduler's timer

   timTimer.Enabled  := false;
   timTimer.Interval := 5000;
   timTimer.Enabled  := true;

end;

//------------------------------------------------------------------------------
// Executed before the form is closed
//------------------------------------------------------------------------------
procedure TFLPMSBackup.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
   idx1     : integer;
   CfgStr   : string;
   CfgInstr : TStringList;
   ThisNode : TTreeNode;

begin

   if (DoSave = true) then begin

      if (MessageDlg('LPMS Backup Manager','WARNING: There are unsaved changes!' + #10 + #10 + 'Click [Yes] to Terminate anyway or [No] to return.', mtWarning, mbYesNo, 0) =  mrNo) then begin
        CloseAction := caNone;
        Exit
     end;

   end;

   if (MessageDlg('LPMS Backup Manager','WARNING: No backups will be taken while Backup Manager is inactive!' + #10 + #10 + 'Click [Yes] to Terminate anyway or [No] to return.', mtWarning, mbYesNo, 0) =  mrNo) then begin

      CloseAction := caNone;
      Exit

   end;

//--- Transfer the in-memory information to the 'Registry' and build the
//--- contents of the Config File

   CfgStr := '';

   ThisNode := tvInstructions.TopItem;
   ThisNode := ThisNode.GetFirstChild;

   while ThisNode <> nil do begin

      for idx1 := 0 to NumInstr -1 do begin

         if Instr_List[idx1].Instruction = ThisNode.Text then begin

//***
//--- TO DO - Add a switch to indicate whether inactive instructions should be
//--- left out from the Config file
//***

            CfgStr := CfgStr + ThisNode.Text + '|';

//--- Don't write to the 'Registry' for inactive instructions

            if Instr_List[idx1].Active = False then
               break;

            IniFile := TINIFile.Create(Instr_List[idx1].Ini_File);

            IniFile.WriteInteger('Parameters','BackupBlock',Instr_List[idx1].Instr_Rec.BackupBlock);
            IniFile.WriteInteger('Parameters','BackupSMSProvider',Instr_List[idx1].Instr_Rec.BackupSMSProvider);
            IniFile.WriteInteger('Parameters','BackupType',Instr_List[idx1].Instr_Rec.BackupType);
            IniFile.WriteInteger('Parameters','BackupT01',Instr_List[idx1].Instr_Rec.BackupT01);
            IniFile.WriteInteger('Parameters','BackupT02',Instr_List[idx1].Instr_Rec.BackupT02);
            IniFile.WriteInteger('Parameters','BackupT03',Instr_List[idx1].Instr_Rec.BackupT03);
            IniFile.WriteBool('Parameters','BackupSMSAlways',Instr_List[idx1].Instr_Rec.BackupSMSAlways);
            IniFile.WriteBool('Parameters','BackupSMSFailure',Instr_List[idx1].Instr_Rec.BackupSMSFailure);
            IniFile.WriteBool('Parameters','BackupSMSNever',Instr_List[idx1].Instr_Rec.BackupSMSNever);
            IniFile.WriteBool('Parameters','BackupSMSSuccess',Instr_List[idx1].Instr_Rec.BackupSMSSuccess);
            IniFile.WriteString('Parameters','BackupDBPass',Instr_List[idx1].Instr_Rec.BackupDBPass);
            IniFile.WriteString('Parameters','BackupDBPrefix',Instr_List[idx1].Instr_Rec.BackupDBPrefix);
            IniFile.WriteString('Parameters','BackupDBSuffix',Instr_List[idx1].Instr_Rec.BackupDBSuffix);
            IniFile.WriteString('Parameters','BackupDBUser',Instr_List[idx1].Instr_Rec.BackupDBUser);
            IniFile.WriteString('Parameters','BackupHostName',Instr_List[idx1].Instr_Rec.BackupHostName);
            IniFile.WriteString('Parameters','BackupLocation',Instr_List[idx1].Instr_Rec.BackupLocation);
            IniFile.WriteString('Parameters','BackupSMSNumber',Instr_List[idx1].Instr_Rec.BackupSMSNumber);
            IniFile.WriteString('Parameters','BackupSMSPass',Instr_List[idx1].Instr_Rec.BackupSMSPass);
            IniFile.WriteString('Parameters','BackupSMSUser',Instr_List[idx1].Instr_Rec.BackupSMSUser);
            IniFile.WriteString('Parameters','BackupTemplate',Instr_List[idx1].Instr_Rec.BackupTemplate);
            IniFile.WriteString('Parameters','BackupViewer',Instr_List[idx1].Instr_Rec.BackupViewer);

            IniFile.Destroy;

            break;
         end;

      end;

      ThisNode := ThisNode.GetNextSibling;
   end;

//--- Save the Configuration File

   if CfgStr <> '' then begin

      CfgInstr := TStringList.Create;
      CfgInstr.Add(CfgStr);

      CfgInstr.SaveToFile(CfgFile);

   end;

//--- Close the data base

   sqlQry1.Close;

   DispLogMsg('***Stopping LPMS Backup Manager');
   SaveLog(BackupLogFile);
   LogList.Free;
   timTimer.Enabled := false;
   TrayIcon.Visible := false;
end;

//------------------------------------------------------------------------------
// Function to return the in-memory list index of the visible instruction
//------------------------------------------------------------------------------
function TFLPMSBackup.GetInstruction() : integer;
var
   idx1, ListNum : integer;
   ThisInstr     : string;

begin

   ListNum := -1;

//--- Get the Instruction name so that we can find it in the in-memory list.

   ThisInstr := tvInstructions.Selected.Text;
   InstrSel  := ThisInstr;

   for idx1 := 0 to NumInstr -1 do begin

      if (Instr_List[idx1].Instruction = ThisInstr) then begin

         ListNum := idx1;
         break;

      end;
   end;

   Result := ListNum;

end;

//------------------------------------------------------------------------------
// User clicked on a node in the Instructions Treeview
//------------------------------------------------------------------------------
procedure TFLPMSBackup.tvInstructionsClick(Sender: TObject);
begin

   if (tvInstructions.Selected.Level = 0) then begin
      pnlP00b.Visible := True;
      pnlP00a.Visible := False;
   end else begin
      pnlP00b.Visible := False;
      pnlP00a.Visible := True;

      ShowInstruction();
   end;
end;

//------------------------------------------------------------------------------
// Load instruction information from the Registry
//------------------------------------------------------------------------------
procedure TFLPMSBackup.ShowInstruction();
var
   ListNum : integer;

begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ',';

   ListNum := GetInstruction();

//--- Check whether the selected instruction is active (a config file exists)

   if Instr_List[ListNum].Active = False then begin

      if (MessageDlg('LPMS Backup Manager','WARNING: The selected Backup Instruction is not configured!' + #10 + #10 + 'Click [Yes] to configure or [No] to return.', mtWarning, mbYesNo, 0) =  mrNo) then begin

         tvInstructions.Items.Item[0].Selected := True;
         tvInstructionsClick(Application);

         Exit;

      end;

//***
//*** TO DO - Code to do a Config
//***

   end;

//--- Initialize the date/time dropdown boxes and the input fields

   cbxType.ItemIndex := Instr_List[ListNum].Instr_Rec.BackupType;
   cbxTypeChange(Application);

   CanUpdate := false;

   edtTemplate.Text     := Instr_List[ListNum].Instr_Rec.BackupTemplate;
   edtLocation.Text     := Instr_List[ListNum].Instr_Rec.BackupLocation;
   cbxT01.ItemIndex     := Instr_List[ListNum].Instr_Rec.BackupT01;
   cbxT02.ItemIndex     := Instr_List[ListNum].Instr_Rec.BackupT02;
   cbxT03.ItemIndex     := Instr_List[ListNum].Instr_Rec.BackupT03;
   edtSMSNumber.Text    := Instr_List[ListNum].Instr_Rec.BackupSMSNumber;
   rbSMSSuccess.Checked := Instr_List[ListNum].Instr_Rec.BackupSMSSuccess;
   rbSMSFailure.Checked := Instr_List[ListNum].Instr_Rec.BackupSMSFailure;
   rbSMSNever.Checked   := Instr_List[ListNum].Instr_Rec.BackupSMSNever;
   rbSMSAlways.Checked  := Instr_List[ListNum].Instr_Rec.BackupSMSAlways;

   SMSProviderName := 'the selected SMS service provider';

//--- Connect to the database and get some basic information

   DBConnect(Instr_List[ListNum].Instr_Rec.BackupHostName, Instr_List[ListNum].Instr_Rec.BackupDBPrefix + Instr_List[ListNum].Instr_Rec.BackupDBSuffix, Instr_List[ListNum].Instr_Rec.BackupDBUser, Instr_List[ListNum].Instr_Rec.BackupDBPass);

   StatusBar1.Panels.Items[0].Text := ' LPMS Backup Manager © 2008-' + FormatDateTime('YYYY',Now()) + ' BlueCrane Software Development CC';
   StatusBar1.Panels.Items[1].Text := ' Version 3.02';
   StatusBar1.Panels.Items[2].Text := ' Waiting...';
   StatusBar1.Panels.Items[3].Text := ' ' + Instr_List[ListNum].Instr_Rec.BackupHostName + '[' + Instr_List[ListNum].Instr_Rec.BackupDBPrefix + Instr_List[ListNum].Instr_Rec.BackupDBSuffix + ']';
   StatusBar1.Panels.Items[4].Text := ' ' + IntToStr(Instr_List[ListNum].Instr_Rec.BackupBlock);
   lblL04.Caption := BackupMsg;

   DoSave            := false;
   CanUpdate         := true;
   btnCancel.Enabled := false;
   btnUpdate.Enabled := false;
   btnRunNow.Enabled := true;
   btnOpenLB.Enabled := false;

   //--- Shutdown the Database for now - we will open it again when a Backup starts

   sqlQry1.Close;
   sqlCon.Close;

{
DBPrefix := tvInstructions.Selected.Text;
   RegString := 'LPMS_' + DBPrefix + '.ini';

//--- Set the global format settings for this session

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ',';

//--- Extract the information contained in the 'registry'

   if (FileExists(RegString) = false) then begin
      Application.MessageBox('Initialisation file not found - using defaults','LPMS Backup Manager - Login', (MB_ICONWARNING + MB_OK));

      HostName        := 'www.bluecrane.cc';
      MultiCompany    := false;
      BackupLocation  := '~';
      BackupTemplate  := '&Date@&Time - &BackupType Backup (&CpyName on &HostName)';
      BackupType      := 0;
      BackupT01       := 0;
      BackupT02       := 0;
      BackupT03       := 0;
      BackupViewer    := '';
      SMSNumber       := '';
      SMSUserID       := '';
      SMSPassword     := '';
      SMSSuccess      := false;
      SMSFailure      := false;
      SMSNever        := true;
      SMSAlways       := false;
      KeepSMSProvider := 0;
      KeepBackupBlock := 5000;

   end else begin

      IniFile := TINIFile.Create(RegString);

      SMSSuccess      := IniFile.ReadBool('Parameters','BackupSMSSuccess',false);
      SMSFailure      := IniFile.ReadBool('Parameters','BackupSMSFailure',false);
      SMSNever        := IniFile.ReadBool('Parameters','BackupSMSNever',true);
      SMSAlways       := IniFile.ReadBool('Parameters','BackupSMSAlways',false);
      HostName        := IniFile.ReadString('Parameters','BackupHostName','www.bluecrane.cc');
      BackupLocation  := IniFile.ReadString('Parameters','BackupLocation','');
      BackupTemplate  := IniFile.ReadString('Parameters','BackupTemplate','&Date@&Time - &BackupType Backup (&CpyName on &HostName)');
      BackupViewer    := IniFile.ReadString('Parameters','BackupViewer','');
      SMSNumber       := IniFile.ReadString('Parameters','BackupSMSNumber','');
      SMSUserID       := IniFile.ReadString('Parameters','BackupSMSUser','');
      SMSPassword     := IniFile.ReadString('Parameters','BackupSMSPass','');
      BackupType      := IniFile.ReadInteger('Parameters','BackupType',0);
      BackupT01       := IniFile.ReadInteger('Parameters','BackupT01',0);
      BackupT02       := IniFile.ReadInteger('Parameters','BackupT02',0);
      BackupT03       := IniFile.ReadInteger('Parameters','BackupT03',0);
      KeepSMSProvider := IniFile.ReadInteger('Parameters','BackupSMSProvider',0);
      KeepBackupBlock := IniFile.ReadInteger('Parameters','BackupBlock',5000);

      IniFile.Destroy;
   end;

//--- If the Backup Type and the Time is not set then set it for Daily backups
//--- and to do a backup on the next quarter

   if ((BackupType = 0) and (BackupT01 = 0) and (BackupT02 = 0) and (BackupT03 = 0)) then begin
      BackupType := 1;
      BackupT01  := StrToInt(FormatDateTime('HH',Now()));
      BackupT02  := StrToInt(FormatDateTime('mm',Now()));

      if (BackupT02 < 15) then
         BackupT02 := 2
      else if (BackupT02 < 30) then
         BackupT02 := 3
      else if (BackupT02 < 45) then
         BackupT02 := 4
      else begin
         BackupT02 := 1;
         inc(BackupT01);
      end;
   end;

//--- Initialize the date/time dropdown boxes and the input fields

   cbxType.ItemIndex := BackupType;
   cbxTypeChange(Application);

   CanUpdate := false;

   edtTemplate.Text     := BackupTemplate;
   edtLocation.Text     := BackupLocation;
   cbxT01.ItemIndex     := BackupT01;
   cbxT02.ItemIndex     := BackupT02;
   cbxT03.ItemIndex     := BackupT03;
   edtSMSNumber.Text    := SMSNumber;
   rbSMSSuccess.Checked := SMSSuccess;
   rbSMSFailure.Checked := SMSFailure;
   rbSMSNever.Checked   := SMSNever;
   rbSMSAlways.Checked  := SMSAlways;

   SMSProviderName := 'the selected SMS service provider';

//--- Connect to the database and get some basic information

   DBConnect();

//--- Start the backup timer

   Interval := GetNextSlot;

   timTimer.Interval := Interval;
   timTimer.Enabled  := true;

   DispLogMsg('Backup timer set to do next automatic backup on ' + Copy(BackupMsg,16,20));
   TrayIcon.Visible := true;

   if (DebugOn = true) then
      DispLogMsg('### Next backup in ' + IntToStr(Interval div 60000) + ' minutes');

   StatusBar1.Panels.Items[0].Text := ' LPMS_Backup © 2008-' + FormatDateTime('YYYY',Now()) + ' BlueCrane Software Development CC';
   StatusBar1.Panels.Items[1].Text := ' Version 2.01';
   StatusBar1.Panels.Items[2].Text := ' Waiting...';
   StatusBar1.Panels.Items[3].Text := ' ' + HostName + '[' + DBPrefix + ']';
   StatusBar1.Panels.Items[4].Text := ' ' + IntToStr(KeepBackupBlock);
   lblL04.Caption := BackupMsg;

   DoSave            := false;
   CanUpdate         := true;
   btnCancel.Enabled := false;
   btnUpdate.Enabled := false;
   btnRunNow.Enabled := true;
   btnOpenLB.Enabled := false;

//--- Shutdown the Database for now - we will open it again when a Backup starts

   sqlQry1.Close;
   sqlCon_.Close;

}
end;

//------------------------------------------------------------------------------
// User selected a directory
//------------------------------------------------------------------------------
procedure TFLPMSBackup.edtLocationAcceptDirectory(Sender: TObject; var Value: String);
var
   ThisDir : string;

begin
   ThisDir := Value;

//--- We need to add a final OS dependant delimiter (OSDelim) to the path.
//--- If we are running on Winblows then the path must be at least 4 chars in
//--- length (e.g "C:\A") before we can add the backslash

   {$IFDEF WINDOWS}
      if (Length(ThisDir) > 3) then
         ThisDir := ThisDir + OSDelim;
   {$ELSE}
      ThisDir := ThisDir + OSDelim;
   {$ENDIF}

   Value := ThisDir;
   Instr_List[GetInstruction()].Instr_Rec.BackupLocation := ThisDir;
end;

//------------------------------------------------------------------------------
// User clicked on the button to select a directory
//------------------------------------------------------------------------------
procedure TFLPMSBackup.edtLocationButtonClick(Sender: TObject);
begin
   edtLocation.RootDir := Instr_List[GetInstruction()].Instr_Rec.BackupLocation;
end;

//---------------------------------------------------------------------------
// Function to open the last successfull backup file
//---------------------------------------------------------------------------
procedure TFLPMSBackup.btnOpenLBClick(Sender: TObject);
begin
   ExecuteProcess(BackupViewer,PChar('"' + edtLastBackup.Text + '"'),[]);
end;

//------------------------------------------------------------------------------
// Procedure to connect to the Database and to get some basic information
//------------------------------------------------------------------------------
procedure TFLPMSBackup.DBConnect(DBHost: string; DBName: string; DBUser: string; DBPass: string);
var
   idx1 : integer;

begin
   sqlQry1.Close;
   sqlCon.Close;

   sqlCon.HostName     := DBHost;
   sqlCon.UserName     := DBUser;
   sqlCon.Password     := DBPass;
   sqlCon.DatabaseName := DBName;
   sqlQry1.DataBase    := sqlCon;

   try
      sqlCon.Connected := true;
   except on E : Exception do
      begin
         LastMsg := E.Message;
         Application.MessageBox(Pchar('FATAL: Unexpected database error: ''' + LastMsg + '''. Please try connecting again'),'LPMS - Backup Manager',(MB_OK + MB_ICONSTOP));
         Exit;
      end;
   end;

//--- Get the currently selected Instruction's reference ID

   idx1 := GetInstruction();

//--- Treat LPMS databases as a special case by getting basic information from
//--- the database

   if Instr_List[idx1].Instr_Rec.BackupDBSuffix = '_LPMS' then begin

      if (GetBasicInfo() = false) then begin
         Application.MessageBox(Pchar('FATAL: Unexpected database error: ''' + LastMsg + '''. LPMS Backup Manager will now terminate'),'LPMS - Backup Manager',(MB_OK + MB_ICONSTOP));
         Application.Terminate;
      end;

      Instr_List[idx1].SymCpy     := sqlQry1.FieldByName('CpyName').AsString;
      Instr_List[idx1].SymVersion := sqlQry1.FieldByName('Version').AsString;

   end else begin

      Instr_List[idx1].SymCpy     := '';
      Instr_List[idx1].SymVersion := '';

   end;

//--- Set up the res tof the information

   if (Instr_List[idx1].Instr_Rec.BackupSMSProvider = 0) then begin
      edtSMSNumber.Enabled := false;
      rbSMSSuccess.Enabled := false;
      rbSMSFailure.Enabled := false;
      rbSMSNever.Enabled   := false;
      rbSMSAlways.Enabled  := false;
      DispLogMsg('SMS Messaging for ' + Instr_List[idx1].Instruction + ' is inactive');
   end else begin
      edtSMSNumber.Enabled := true;
      rbSMSSuccess.Enabled := true;
      rbSMSFailure.Enabled := true;
      rbSMSNever.Enabled   := true;
      rbSMSAlways.Enabled  := true;

      if (edtSMSNumber.Text <> '') then begin
         if (rbSMSAlways.Checked = true) then
            DispLogMsg('SMS will always be sent to "' + edtSMSNumber.Text + '" (Success or Failure) using "' + SMSProviderName + '"')
         else if (rbSMSSuccess.Checked = true) then
            DispLogMsg('SMS Message will be sent to "' + edtSMSNumber.Text + '" after a successful backup using "' + SMSProviderName + '"')
         else if (rbSMSFailure.Checked = true) then
            DispLogMsg('SMS Message will be sent to "' + edtSMSNumber.Text + '" if a backup fails using "' + SMSProviderName + '"')
         else
            DispLogMsg('SMS Messages will never be sent');
      end else
         DispLogMsg('SMS Messaging is inactive because "SMS Number: is not specified');
   end;

//--- Treat LPMS databases as a special case by displaying information about
//--- the database version

   if Instr_List[idx1].Instr_Rec.BackupDBSuffix = '_LPMS' then begin
      DispLogMsg('Backups will be taken for "' + Instr_List[idx1].SymCpy + '" on "' + DBHost + '[' + Instr_List[idx1].Instr_Rec.BackupDBPrefix + Instr_List[idx1].Instr_Rec.BackupDBSuffix + ']"');
      DispLogMsg('Database version is "' + Instr_List[idx1].SymVersion + '"');
   end else
      DispLogMsg('Backups will be taken for "' + DBHost + '[' + Instr_List[idx1].Instr_Rec.BackupDBPrefix + Instr_List[idx1].Instr_Rec.BackupDBSuffix + ']"');

   StatusBar1.Panels.Items[2].Text := ' Waiting...';
   StatusBar1.Panels.Items[3].Text := ' ' + DBHost + '[' + Instr_List[idx1].Instr_Rec.BackupDBPrefix + Instr_List[idx1].Instr_Rec.BackupDBSuffix + ']';
   StatusBar1.Panels.Items[4].Text := ' ' + IntToStr(Instr_List[idx1].Instr_Rec.BackupBlock);
end;

//------------------------------------------------------------------------------
// User clicked on the Close button
//------------------------------------------------------------------------------
procedure TFLPMSBackup.btnCloseClick(Sender: TObject);
begin
   Close;
end;

//------------------------------------------------------------------------------
// User clicked on the Configure button
//------------------------------------------------------------------------------
procedure TFLPMSBackup.btnSMSConfigClick(Sender: TObject);
begin
   FLPMSBackup.Hide;

   FLPMSBackupSMSConfig := TFLPMSBackupSMSConfig.Create(Application);

//--- Set the values to be used in the config utility

   FLPMSBackupSMSConfig.SMSProvider  := KeepSMSProvider;
   FLPMSBackupSMSConfig.BackupBlock  := KeepBackupBlock;
   FLPMSBackupSMSConfig.SMSUser      := SMSUserID;
   FLPMSBackupSMSConfig.SMSPassword  := SMSPassword;
   FLPMSBackupSMSConfig.DBPrefix     := DBPrefix;
   FLPMSBackupSMSConfig.MultiCompany := MultiCompany;
   FLPMSBackupSMSConfig.BackupViewer := BackupViewer;

   FLPMSBackupSMSConfig.ShowModal;
   FLPMSBackupSMSConfig.Destroy;

   FLPMSBackup.Show;

   if (DoSave = true) then begin

      btnUpdateClick(Sender);

      sqlQry1.Close;
      sqlCon.Close;

      sqlCon.HostName := HostName;
      sqlCon.UserName := DBPrefix + '_LD';
      sqlCon.Password := 'LD01';
      sqlCon.DatabaseName := DBPrefix + '_LPMS';
      sqlQry1.DataBase := sqlCon;

      try
         sqlCon.Connected := true;
      except on E : Exception do
         begin
            LastMsg := E.Message;
            Application.MessageBox(Pchar('FATAL: Unexpected database error: "' + LastMsg + '". LPMS Backup Manager will now terminate'),'LPMS - Backup Manager',(MB_OK + MB_ICONSTOP));
            Application.Terminate;
            Exit;
         end;
      end;

      if (GetBasicInfo() = false) then begin
         Application.MessageBox(Pchar('FATAL: Unexpected database error: "' + LastMsg + '". LPMS Backup Manager will now terminate'),'LPMS - Backup Manager',(MB_OK + MB_ICONSTOP));
         Application.Terminate;
         Exit;
      end;

      SymCpy      := sqlQry1.FieldByName('CpyName').AsString;
      SymHost     := HostName;
      KeepVersion := sqlQry1.FieldByName('Version').AsString;

      if (KeepSMSProvider = 0) then begin
         edtSMSNumber.Enabled := false;
         rbSMSSuccess.Enabled := false;
         rbSMSFailure.Enabled := false;
         rbSMSNever.Enabled   := false;
         rbSMSAlways.Enabled  := false;
         DispLogMsg('SMS Messaging is inactive');
      end else begin
         edtSMSNumber.Enabled := true;
         rbSMSSuccess.Enabled := true;
         rbSMSFailure.Enabled := true;
         rbSMSNever.Enabled   := true;
         rbSMSAlways.Enabled  := true;

         if (edtSMSNumber.Text <> '') then begin
            if (rbSMSAlways.Checked = true) then
               DispLogMsg('SMS Message will always be sent to "' + edtSMSNumber.Text + '" (Success or Failure)')
            else if (rbSMSSuccess.Checked = true) then
               DispLogMsg('SMS Message will be sent to "' + edtSMSNumber.Text + '" after a successful backup')
            else if (rbSMSFailure.Checked = true) then
               DispLogMsg('SMS Message will be sent to "' + edtSMSNumber.Text + '" if a backup fails')
            else
               DispLogMsg('SMS Messages will never be sent');
         end else
            DispLogMsg('SMS Messaging is inactive because "SMS Number: is not specified');
      end;

      DispLogMsg('Backups will be taken for "' + SymCpy + '" on "' + HostName + '[' + DBPrefix + ']"');
      DispLogMsg('Database version is "' + KeepVersion + '"');

      StatusBar1.Panels.Items[2].Text := ' Waiting...';
      StatusBar1.Panels.Items[3].Text := ' ' + HostName + '[' + DBPrefix + ']';
      StatusBar1.Panels.Items[4].Text := ' ' + IntToStr(KeepBackupBlock);
   end;
end;

//------------------------------------------------------------------------------
// User clicked on the Cancel button
//------------------------------------------------------------------------------
procedure TFLPMSBackup.btnCancelClick(Sender: TObject);
begin
   if (DoSave = true) then begin
      if (MessageDlg('LPMS Backup Manager','WARNING: There are unsaved changes - click [Yes] to ignore the changes or [No] to return', mtWarning, mbYesNo, 0) =  mrNo) then begin
         Exit
      end;
   end;

   CanUpdate := false;

   edtLocation.Text     := BackupLocation;
   edtTemplate.Text     := BackupTemplate;
   cbxType.ItemIndex    := BackupType;
   cbxT01.ItemIndex     := BackupT01;
   cbxT02.ItemIndex     := BackupT02;
   cbxT03.ItemIndex     := BackupT03;
   edtSMSNumber.Text    := SMSNumber;
   rbSMSSuccess.Checked := SMSSuccess;
   rbSMSFailure.Checked := SMSFailure;
   rbSMSNever.Checked   := SMSNever;
   rbSMSAlways.Checked  := SMSAlways;

   btnUpdate.Enabled := false;
   btnCancel.Enabled := false;

   DoSave := false;
   CanUpdate := true;
   StatusBar1.Panels.Items[2].Text := ' Waiting...';
end;

//------------------------------------------------------------------------------
// User clicked on the Run Now button
//------------------------------------------------------------------------------
procedure TFLPMSBackup.btnRunNowClick(Sender: TObject);
var
   ListNum : integer;

begin

   ListNum := GetInstruction();

   DispLogMsg('Received an instruction to immediately run the next backup');
   Set_Hint(ord(HINT_RUNNING));

//--- Mark the instruction as RunNow

   Instr_List[ListNum].Status := ord(STAT_RUNNOW);

end;

//------------------------------------------------------------------------------
// User clicked on the Minimise button
//------------------------------------------------------------------------------
procedure TFLPMSBackup.btnMinimiseClick(Sender: TObject);
begin
      FLPMSBackup.Hide;
end;

//------------------------------------------------------------------------------
// User clicked on the Update button
//------------------------------------------------------------------------------
procedure TFLPMSBackup.btnUpdateClick(Sender: TObject);
var
   Interval  : integer;

begin

//--- Check whether the required fields are valid

   if (edtTemplate.Text = '') then begin
      Application.MessageBox('Template is a mandatory field - please provide','LPMS Backup Manager', MB_ICONHAND + MB_OK);
      edtTemplate.SetFocus;
      Exit;
   end;

   if (edtLocation.Text = '') then begin
      Application.MessageBox('Location is a mandatory field - please select or provide a valid Folder','LPMS Backup Manager', MB_ICONHAND + MB_OK);
      edtLocation.SetFocus;
      Exit;
   end;

   BackupLocation := edtLocation.Text;
   BackupTemplate := edtTemplate.Text;
   BackupType     := cbxType.ItemIndex;
   BackupT01      := cbxT01.ItemIndex;
   BackupT02      := cbxT02.ItemIndex;
   BackupT03      := cbxT03.ItemIndex;
   SMSNumber      := edtSMSNumber.Text;
   SMSSuccess     := rbSMSSuccess.Checked;
   SMSFailure     := rbSMSFailure.Checked;
   SMSNever       := rbSMSNever.Checked;
   SMSAlways      := rbSMSAlways.Checked;

   if (MultiCompany = true) then
      RegString := 'LPMS_' + DBPrefix + '.ini'
   else
      RegString := 'LPMS.ini';

   IniFile := TINIFile.Create(RegString);

   IniFile.WriteBool('Parameters','BackupSMSSuccess',SMSSuccess);
   IniFile.WriteBool('Parameters','BackupSMSFailure',SMSFailure);
   IniFile.WriteBool('Parameters','BackupSMSNever',SMSNever);
   IniFile.WriteBool('Parameters','BackupSMSAlways',SMSAlways);
   IniFile.WriteString('Parameters','BackupDBPrefix',DBPrefix);
   IniFile.WriteString('Parameters','BackupHostName',HostName);
   IniFile.WriteString('Parameters','BackupLocation',BackupLocation);
   IniFile.WriteString('Parameters','BackupTemplate',BackupTemplate);
   IniFile.WriteString('Parameters','BackupLogFile',BackupLogFile);
   IniFile.WriteString('Parameters','BackupSMSNumber',SMSNumber);
   IniFile.WriteString('Parameters','BackupSMSUser',SMSUserID);
   IniFile.WriteString('Parameters','BackupSMSPass',SMSPassword);
   IniFile.WriteString('Parameters','BackupViewer',BackupViewer);
   IniFile.WriteInteger('Parameters','BackupType',BackupType);
   IniFile.WriteInteger('Parameters','BackupT01',BackupT01);
   IniFile.WriteInteger('Parameters','BackupT02',BackupT02);
   IniFile.WriteInteger('Parameters','BackupT03',BackupT03);
   IniFile.WriteInteger('Parameters','BackupSMSProvider',KeepSMSProvider);
   IniFile.WriteInteger('Parameters','BackupBlock',KeepBackupBlock);

   IniFile.Destroy;

   btnUpdate.Enabled := false;
   btnCancel.Enabled := false;
   btnRunNow.Enabled := true;

//--- Start the backup timer

   DispLogMsg('Parameters changed - resetting backup timer');
   timTimer.Enabled := false;
//   Interval := GetNextSlot;
   timTimer.Interval := Interval;
   timTimer.Enabled  := true;

   DispLogMsg('Backup timer set to do next automatic backup on ' + Copy(BackupMsg,16,20));
   if (DebugOn = true) then
      DispLogMsg('### Next backup in ' + IntToStr(Interval div 60000) + ' minutes');

   StatusBar1.Panels.Items[2].Text := ' ';
   lblL04.Caption := BackupMsg;

   DoSave := false;
end;

//------------------------------------------------------------------------------
// User selected a backup frequency from the dropdown list
//------------------------------------------------------------------------------
procedure TFLPMSBackup.cbxTypeChange(Sender: TObject);
var
   idx : integer;

const

   WeekArray : array[1..7]  of string = ('Sunday', 'Monday', 'Tuesday',
               'Wednesday', 'Thursday', 'Friday', 'Saturday');
   HourArray : array[1..32] of string = ('00', '01', '02', '03', '04', '05',
               '06', '07', '08', '09', '10', '11', '12', '13', '14', '15',
               '16', '17', '18', '19', '20', '21', '22', '23', '24', '25',
               '26', '27', '28', '29', '30', '31');
   MinArray  : array[1..4]  of string = ('00', '15', '30', '45');

begin
   lblL01.Visible := true;
   lblL02.Visible := true;
   lblL03.Visible := false;
   cbxT01.Visible := true;
   cbxT02.Visible := true;
   cbxT03.Visible := false;

   if (cbxType.Text = 'Hourly') then begin
      lblL01.Caption := 'Select Time:';
      cbxT01.Clear;

      for idx := 1 to 4 do
         cbxT01.Items.Add(MinArray[idx]);

      lblL02.Visible := false;
      cbxT02.Visible := false;
   end else if (cbxType.Text = 'Daily') then begin
      lblL01.Caption := 'Select Hour:';
      cbxT01.Clear;

      for idx := 1 to 24 do
         cbxT01.Items.Add(HourArray[idx]);

      lblL02.Caption := 'Select Minute:';
      cbxT02.Clear;

      for idx := 1 to 4 do
         cbxT02.Items.Add(MinArray[idx]);

      lblL02.Visible := true;
      cbxT02.Visible := true;
   end else if (cbxType.Text = 'Weekly') then begin
      lblL01.Caption := 'Select Day:';
      cbxT01.Clear;

      for idx := 1 to 7 do
         cbxT01.Items.Add(WeekArray[idx]);

      lblL02.Caption := 'Select Hour:';
      cbxT02.Clear;

      for idx := 1 to 24 do
         cbxT02.Items.Add(HourArray[idx]);

      lblL03.Caption := 'Select Minute:';
      cbxT03.Clear;

      for idx := 1 to 4 do
         cbxT03.Items.Add(MinArray[idx]);

      lblL02.Visible := true;
      cbxT02.Visible := true;
      lblL03.Visible := true;
      cbxT03.Visible := true;
   end else if (cbxType.Text = 'Monthly') then begin
      lblL01.Caption := 'Select Day:';
      cbxT01.Clear;

      for idx := 2 to 32 do
         cbxT01.Items.Add(HourArray[idx]);

      lblL02.Caption := 'Select Hour:';
      cbxT02.Clear;

      for idx := 1 to 24 do
         cbxT02.Items.Add(HourArray[idx]);

      lblL03.Caption := 'Select Minute:';
      cbxT03.Clear;

      for idx := 1 to 4 do
         cbxT03.Items.Add(MinArray[idx]);

      lblL02.Visible := true;
      cbxT02.Visible := true;
      lblL03.Visible := true;
      cbxT03.Visible := true;
   end;

   CanUpdate := false;
   cbxT01.ItemIndex := 0;
   cbxT02.ItemIndex := 0;
   cbxT03.ItemIndex := 0;
   CanUpdate := true;

   edtTemplateChange(Sender);
end;

//------------------------------------------------------------------------------
// A field on the screen changed
//------------------------------------------------------------------------------
procedure TFLPMSBackup.edtTemplateChange(Sender: TObject);
begin
   if (CanUpdate = false) then
      Exit;

   DoSave := true;
   btnUpdate.Enabled := true;
   btnCancel.Enabled := true;
   btnRunNow.Enabled := false;

   StatusBar1.Panels.Items[2].Text := ' Modified';
end;

//------------------------------------------------------------------------------
// The DBPrefix or Hostname changed
//------------------------------------------------------------------------------
procedure TFLPMSBackup.edtDBPrefixChange(Sender: TObject);
begin
   if (CanUpdate = false) then
      Exit;

   StatusBar1.Panels.Items[2].Text := ' Modified';
   edtTemplateChange(Sender);
end;

//------------------------------------------------------------------------------
// The Timer popped
//------------------------------------------------------------------------------
procedure TFLPMSBackup.timTimerTimer(Sender: TObject);
var
   idx1{, ThisInterval}                 : integer;
   RunBackup                          : boolean;
   SMSMessage, DispMessage, ThisInstr : string;

begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ',';

//--- Prevent the Scheduler from popping while a Backup is active and disable
//--- the [Run Now] button

   timTimer.Enabled  := False;
   btnRunNow.Enabled := False;
   RunBackup         := False;

//--- Run through the instructions and check whether there are any backups that
//--- must run now

   for idx1 := 0 to NumInstr -1 do begin

      if Instr_List[idx1].Active = True then begin

         ThisInstr := Instr_List[idx1].Instruction;

         case Instr_List[idx1].Status of

            ord(STAT_RUNNOW) : begin
               RunBackup   := True;
               ActiveInstr := idx1;

               DispLogMsg('*** Backup scheduled for ' + Instr_List[idx1].NextDate + ' at ' + Instr_List[idx1].NextTime + ' started on ' + FormatDateTime('yyyy/MM/dd',Now()) + ' at ' + FormatDateTime('hh:nn',Now()) + ' - [Run Now]')
            end;

            ord(STAT_WAITING) : begin
               // Check for instructions that are due

//               RunBackup   := True;
//               ActiveInstr := idx1;

//               DispLogMsg('*** Backup scheduled for ' + Instr_List[idx1].NextDate + ' at ' + Instr_List[idx1].NextTime + ' started - [Scheduler]');
            end;

            ord(STAT_INACTIVE) : begin
               //
            end;

         end;

         if RunBackup = True then begin

            RunBackup := False;

            if (DoBackup() = false) then begin

               DispLogMsg('*** Backup failed with error message: "' + LastMsg + '"');

               if (((rbSMSFailure.Checked = true) or (rbSMSAlways.Checked = true)) and (Instr_List[idx1].Instr_Rec.BackupSMSProvider <> 0)) then begin

                  DispMessage := FormatDateTime('yyyy/MM/dd@hh:nn:ss',Now) + ' ' + cbxType.Text + ' Backup FAILED (Time: ' + FormatDateTime('hh:nn:ss.zzz',Now - StartTime) + ', Records: ' + FloatToStrF(RecTotal,ffNumber,10,0) + '). Check Log for errors. ' + SymHost + '(' + DBPrefix + ')';
                  SMSMessage  := Get_Send_XML(DispMessage);
                  SendSMS(SMSMessage);

                  if (SMSDone = true) then
                     DispLogMsg('SMS indicating FAILURE with message "' + DispMessage + '" sent to "' + edtSMSNumber.Text + '"')
                  else
                     DispLogMsg('Attempt to send SMS indicating FAILURE failed');

               end;

            end;

         end;

      end;

   end;

//--- Restart the Scheduler

   timTimer.Enabled  := True;
   btnRunNow.Enabled := True;

{
if (RunNow = true) then
      DispLogMsg('*** Backup scheduled for ' + Copy(BackupMsg,16,20) + ' started on ' + FormatDateTime('yyyy/MM/dd',Now()) + ' at ' + FormatDateTime('hh:nn',Now()))
   else
      DispLogMsg('*** Backup scheduled for ' + Copy(BackupMsg,16,20) + ' started');

   StatusBar1.Panels.Items[2].Text := ' Running...';
   StatusBar1.Refresh;

   if (DoBackup() = false) then begin
      DispLogMsg('*** Backup failed with error message: "' + LastMsg + '"');
      if (((rbSMSFailure.Checked = true) or (rbSMSAlways.Checked = true)) and (KeepSMSProvider <> 0)) then begin
         DispMessage := FormatDateTime('yyyy/MM/dd@hh:nn:ss',Now) + ' ' + cbxType.Text + ' Backup FAILED (Time: ' + FormatDateTime('hh:nn:ss.zzz',Now - StartTime) + ', Records: ' + FloatToStrF(RecTotal,ffNumber,10,0) + '). Check Log for errors. ' + SymHost + '(' + DBPrefix + ')';
         SMSMessage  := Get_Send_XML(DispMessage);
         SendSMS(SMSMessage);

         if (SMSDone = true) then
            DispLogMsg('SMS indicating FAILURE with message "' + DispMessage + '" sent to "' + edtSMSNumber.Text + '"')
         else
            DispLogMsg('Attempt to send SMS indicating FAILURE failed');
      end;
   end;

//   ThisInterval := GetNextSlot();
   timTimer.Interval := ThisInterval;

   DispLogMsg('Backup timer set to do next automatic backup on ' + Copy(BackupMsg,16,20));
   if (DebugOn = true) then
      DispLogMsg('### Next backup in ' + IntToStr(ThisInterval div 60000) + ' minutes');

   StatusBar1.Panels.Items[2].Text := ' Waiting...';
   timTimer.Enabled := true;
//   RunNow := false;
   btnRunNow.Enabled := true;
   lblL04.Caption := BackupMsg;
}
end;

//------------------------------------------------------------------------------
// User selected 'Restore' from the popup menu in the Tray
//------------------------------------------------------------------------------
procedure TFLPMSBackup.ToolsRestoreExecute(Sender: TObject);
begin
      FLPMSBackup.Show;
end;

//------------------------------------------------------------------------------
// The User hovered the mouse over the tray icon - display some information
//------------------------------------------------------------------------------
procedure TFLPMSBackup.TrayIconMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   Set_Hint(ord(HINT_WAITING));
end;

//------------------------------------------------------------------------------
// Set the Hint to be displayed when the Mouse pointer is hovered over the tray icon
//------------------------------------------------------------------------------
procedure TFLPMSBackup.Set_Hint(ThisType: integer);
var
   MsgPart : string;

begin
   if (ThisType = ord(HINT_WAITING)) then
      MsgPart := lblL05.Caption
   else
      MsgPart := 'Currently running...';

   TrayIcon.Hint := 'LPMS - Backup Manager (' + DBPrefix + '): Right click for options' + #10 + 'Double click to restore LPMS - Backup Manager' + #10 + MsgPart;
end;

//------------------------------------------------------------------------------
// Procedure to do the backup
//------------------------------------------------------------------------------
function TFLPMSBackup.DoBackup() : boolean;
var
//   STime, ETime : string;
   ThisTime     : string;
   SMSMessage   : string;
   DispMessage  : string;


begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ',';

//   STime    := FormatDateTime('hh:nn',Now());
   ThisTime := FormatDateTime('hh',Now()) + 'h' + FormatDateTime('nn',Now());

   OutFile := edtLocation.Text + edtTemplate.Text + '.lpb';

   OutFile := AnsiReplaceStr(OutFile,'&Date',FormatDateTime('yyyyMMdd',Now()));
   OutFile := AnsiReplaceStr(OutFile,'&Year',FormatDateTime('yyyy',Now()));
   OutFile := AnsiReplaceStr(OutFile,'&Month',FormatDateTime('MM',Now()));
   OutFile := AnsiReplaceStr(OutFile,'&Day',FormatDateTime('dd',Now()));
   OutFile := AnsiReplaceStr(OutFile,'&Time',ThisTime);
   OutFile := AnsiReplaceStr(OutFile,'&Hour',FormatDateTime('hh',Now()));
   OutFile := AnsiReplaceStr(OutFile,'&Minute',FormatDateTime('nn',Now()));
   OutFile := AnsiReplaceStr(OutFile,'&CpyName',Instr_List[ActiveInstr].SymCpy);
   OutFile := AnsiReplaceStr(OutFile,'&HostName',Instr_List[ActiveInstr].Instr_Rec.BackupHostName);
   OutFile := AnsiReplaceStr(OutFile,'&BackupType',cbxType.Text);
   OutFile := AnsiReplaceStr(OutFile,'&DBPrefix',Instr_List[ActiveInstr].Instr_Rec.BackupDBPrefix + Instr_List[ActiveInstr].Instr_Rec.BackupDBSuffix);

   DispLogMsg('Backup will be saved to "' + OutFile + '"');
   StartTime := Now;
   DispLogMsg('Backup attempt started on ' + FormatDateTime('yyyy/MM/dd',Now()) + ' at ' + FormatDateTime('hh:nn:ss.zzz',StartTime));
   lblL04.Caption := cbxType.Text + ' Backup started on ' + FormatDateTime('yyyy/MM/dd',Now()) + ' at ' + FormatDateTime('hh:nn:ss',StartTime);

//--- Temporarily stop the count down display

   timTimer2.Enabled := false;
   lblL05.Caption := '';
   lblL05.Repaint;

//--- Create the Backup File

   if (ReadDB(OutFile,1) = false) then begin
      Result := false;
      Exit;
   end;

//--- Indicate the success of the backup and send an SMS if necessary

   DispLogMsg('Backup succesfully completed');
   DispLogMsg(-1);

   EndTime := Now;

   edtLastBackup.Text := OutFile;
   btnOpenLB.Enabled  := true;

   if (((rbSMSSuccess.Checked = true) or (rbSMSAlways.Checked = true)) and (Instr_List[ActiveInstr].Instr_Rec.BackupSMSProvider <> 0)) then begin
      DispMessage := FormatDateTime('yyyy/MM/dd@hh:nn:ss',Now) + ' ' + cbxType.Text + ' Backup Successful (Time: ' + FormatDateTime('hh:nn:ss.zzz',EndTime - StartTime) + ', Records: ' + RecTotalF + ', Size: ' + ThisMsgF + '). ' + Instr_List[ActiveInstr].Instr_Rec.BackupHostName + '(' + Instr_List[ActiveInstr].Instr_Rec.BackupDBPrefix + Instr_List[ActiveInstr].Instr_Rec.BackupDBSuffix + ')';
      SMSMessage  := Get_Send_XML(DispMessage);

      SendSMS(SMSMessage);

      if (SMSDone = true) then
         DispLogMsg('SMS indicating SUCCESS with message "' + DispMessage + '" sent to "' + edtSMSNumber.Text + '"')
      else
         DispLogMsg('Attempt to send SMS indicating SUCCESS failed with message "' + SMSResult + '"');
   end;

   DispLogMsg('Backup attempt completed on ' + FormatDateTime('yyyy/MM/dd',Now()) + ' at ' + FormatDateTime('hh:nn:ss.zzz',EndTime) + ', Time taken: ' + FormatDateTime('hh:nn:ss.zzz',EndTime - StartTime));

//--- Get the next time slot for this backup instruction

   GetNextSlot(ActiveInstr);

{
//--- Prevent a backup from being done more than once if it is completed
//--- in less than a minute

   ETime := FormatDateTime('hh:nn',Now());
   if (ETime = STime) then
      Sleep(60000);
}

   Result := true;

end;

//------------------------------------------------------------------------------
// Function to dynamically backup all the tables in the open Database
//------------------------------------------------------------------------------
function TFLPMSBackup.ReadDB(FileName: string; ThisType: integer) : boolean;
type
   Fields_Struct = record
      Field      : string;
      ThisType   : string;
      Null       : string;
      Key        : string;
      Def        : string;
      Extra      : string;
      AlphaNum   : integer;
   end;

var
   idx1, idx2, RecCount, LimitStart, LimitEnd{, ListNum}    : integer;
   P1, P2, P3, P4, P5, P6, P7, Res, ThisLine, ThisTable   : string;
   ThisTitle, ThisDate, ThisTime, TypeBackup, ThisVersion : string;
   ThisTables, ThisMode{, ThisInstr}                        : string;
   BackupFile                                             : TextFile;
   TableNames                                             : TStringList;
   FieldNames                                             : TList;
   Fields_Rec                                             : ^Fields_Struct;

begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ',';

//--- Create the in memory lists

   TableNames := TStringList.Create;
   FieldNames := TList.Create;

//--- Update the Message fields

   lblL05.Caption := 'Preparing backup...';
   lblL05.Refresh;

//--- Open a connection to the Database

   sqlQry1.Close;
   sqlCon.Close;

   sqlCon.HostName     := Instr_List[ActiveInstr].Instr_Rec.BackupHostName;
   sqlCon.UserName     := Instr_List[ActiveInstr].Instr_Rec.BackupDBUser;
   sqlCon.Password     := Instr_List[ActiveInstr].Instr_Rec.BackupDBPass;
   sqlCon.DatabaseName := Instr_List[ActiveInstr].Instr_Rec.BackupDBPrefix + Instr_List[ActiveInstr].Instr_Rec.BackupDBSuffix;
   sqlQry1.DataBase    := sqlCon;

//--- Get the Table Names for the open Database

   sqlCon.GetTableNames(TableNames,false);

//--- Set the Flags

   ThisTitle   := 'LPMS_Backup';
   ThisDate    := FormatDateTime('yyyy/mm/dd',Now);
   ThisTime    := FormatDateTime('hh:nn:ss',Now);
   TypeBackup  := 'Full';
   ThisVersion := KeepVersion;

   if (ThisType = 1) then
      ThisMode := 'Standalone'
   else
      ThisMode := 'Managed';

   P7         := '';
   ThisTables := '';

   for idx1 := 0 to TableNames.Count - 1 do begin
      ThisTables := ThisTables + P7 + TableNames.Strings[idx1];
      P7 := ',';
   end;

//--- Create and Open the Backup File then write the heading

   AssignFile(BackupFile,FileName);
   ReWrite(BackupFile);

   Res := 'Title=' + ThisTitle + '|Date=' + ThisDate + '|Time=' + ThisTime +
          '|Version=' + ThisVersion + '|Type=' + TypeBackup + '|Mode=' +
          ThisMode + '|Tables=' + ThisTables;

   ThisLine := '#' + Res;
   WriteLn(BackupFile,ThisLine);

//--- Step through the tables and create the necessary SQL statements for each

   for idx1 := 0 to TableNames.Count - 1 do begin
      ThisTable := TableNames.Strings[idx1];

//--- Update the log display

      DispLogMsg('   Backing up "' + ThisTable + '" table');

//--- Insert the Table name if in Managed mode

      if (ThisType = 2) then begin
         ThisLine := '0' + ThisTable;
         WriteLn(BackupFile,ThisLine);
      end;

//--- Create and write the DROP statement for the current table

      Res := 'DROP TABLE IF EXISTS ' + ThisTable;
      if (ThisType = 1) then
         ThisLine := Res + ';'
      else
         ThisLine := '1' + Res;

      WriteLn(BackupFile,ThisLine);

//--- Get the column names for the current table

      try
         sqlQry1.Close;
         sqlQry1.SQL.Text := 'SHOW COLUMNS IN ' + ThisTable;
         sqlQry1.Open;
      except on E : Exception do
         begin
            LastMsg := E.Message;
            Result := false;
            Exit;
         end;
      end;

//--- Extract the returned Column names

      FieldNames.Clear;
      sqlQry1.First;

      while (sqlQry1.EOF = false) do begin
         New(Fields_Rec);

         Fields_Rec^.Field    := sqlQry1.FieldByName('Field').AsString;
         Fields_Rec^.ThisType := sqlQry1.FieldByName('Type').AsString;
         Fields_Rec^.Null     := sqlQry1.FieldByName('Null').AsString;
         Fields_Rec^.Key      := sqlQry1.FieldByName('Key').AsString;
         Fields_Rec^.Def      := sqlQry1.FieldByName('Default').AsString;
         Fields_Rec^.Extra    := sqlQry1.FieldByName('Extra').AsString;
         Fields_Rec^.AlphaNum := GetFieldType(Fields_Rec^.ThisType);

         FieldNames.Add(Fields_Rec);
         sqlQry1.Next;
      end;

//--- Build and write the Create statement for this Table

      P1 := '';
      P2 := '';
      P3 := '';
      P4 := '';
      P5 := '';
      P6 := '';
      Res := 'CREATE TABLE ' + ThisTable + ' (';

      New(Fields_Rec);

      for idx2 := 0 to FieldNames.Count - 1 do begin
         Fields_Rec := FieldNames.Items[idx2];

         P2 := Fields_Rec^.Field + ' ' + Fields_Rec^.ThisType;

         if (Fields_Rec^.Null = 'NO') then
            P3 := ' NOT NULL';

         if (Fields_Rec^.Key = 'PRI') then
            P4 := ', PRIMARY KEY (' + Fields_Rec^.Field + ')) ENGINE=MyISAM DEFAULT CHARSET=latin1 ROW_FORMAT=DYNAMIC';

         if (Fields_Rec^.Extra = 'auto_increment') then begin
            P3 := ' NOT NULL';
            P6 := ' AUTO_INCREMENT';
         end else begin
            if (Fields_Rec^.Def = '') then
               P5 := ''
            else begin
               if (Fields_Rec^.AlphaNum = 2) then
                  P5 := ' DEFAULT ' + Fields_Rec^.Def
               else
                  P5 := ' DEFAULT ''' + Fields_Rec^.Def + '''';
            end;
         end;

         Res := Res + P1 + P2 + P3 + P5 + P6;
         P1  := ', ';
         P2  := '';
         P3  := '';
         P5  := '';
         P6  := '';
      end;

      if (P4 = '') then
         P4 := ') ENGINE=MyISAM DEFAULT CHARSET=latin1 ROW_FORMAT=DYNAMIC';

      Res := Res + P4;

      if (ThisType = 1) then
         ThisLine := Res + ';'
      else
         ThisLine := '2' + Res + ';';

      WriteLn(BackupFile,ThisLine);

//--- Update the Message fields

      lblL05.Caption := 'Backing up Table "' + ThisTable + '", Reading...';
      lblL05.Refresh;

//--- Create and write the Insert statements for this Table in blocks

      LimitEnd    := Instr_List[ActiveInstr].Instr_Rec.BackupBlock;
      LimitActive := true;
      LimitStart  := 0;
      RecCount    := 0;

      if (ReadTable(ThisTable,LimitStart,LimitEnd) = false) then begin
         Result := false;
         Exit;
      end;

      while (LimitActive = true) do begin
         sqlQry1.First;

         while (sqlQry1.Eof = false) do begin

//--- Build the part of the INSERT statement that contain the field names

            Res := 'INSERT INTO ' + ThisTable + ' (';
            P1  := '';

            for idx2 := 0 to FieldNames.Count -1 do begin
               Fields_Rec := FieldNames.Items[idx2];

//--- We don't write the AutoIncrement field to the backup file

               if (Fields_Rec^.Extra = 'auto_increment') then
                  continue;

               Res := Res + P1 + Fields_Rec^.Field;
               P1  := ', ';
            end;

//--- Build the part of the INSERT statement that contain the field values

            Res := Res + ') Values (';
            P1  := '';

            for idx2 := 0 to FieldNames.Count -1 do begin
               Fields_Rec := FieldNames.Items[idx2];

//--- We don't write the value of the AutoIncrement field to the backup file

               if (Fields_Rec^.Extra = 'auto_increment') then
                  continue;

//--- Determine whether the field should be quoted or not

               if (Fields_Rec^.AlphaNum = 1) then
                  P2 := ''''
               else
                  P2 := '';

               Res := Res + P1 + P2 + sqlQry1.FieldByName(Fields_Rec^.Field).AsString + P2;
               P1  := ', ';
            end;

            Res := Res + ')';

            if (ThisType = 1) then
               ThisLine := Res + ';'
            else
               ThisLine := '3' + Res + ';';

            WriteLn(BackupFile,ThisLine);

            Inc(RecCount);
            if (RecCount mod 10 = 0) then begin
               lblL05.Caption := 'Backing up Table "' + ThisTable + '", Writing record no.: ' + IntToStr(RecCount);
               lblL05.Refresh;
            end;

            sqlQry1.Next;
         end;

         LimitStart := LimitStart + Instr_List[ActiveInstr].Instr_Rec.BackupBlock;
         lblL05.Caption := 'Backing up Table "' + ThisTable + '", Reading...';
         lblL05.Refresh;
         if (ReadTable(ThisTable,LimitStart,LimitEnd) = false) then begin
            Result := false;
            Exit;
         end;
      end;

      DispLogMsg(RecCount);
      Dispose(Fields_Rec);

   end;

//--- Write the final record then close the Backup file

   Res := '##END##';

   if (ThisType = 1) then
      ThisLine := Res
   else
      ThisLine := '0' + Res;

   WriteLn(BackupFile,ThisLine);
   CloseFile(BackupFile);

//--- Clear and delete the Lists that were used

   TableNames.Destroy;
   FieldNames.Destroy;

//--- Close the connection to the Database

   sqlQry1.Close;
   sqlCon.Close;

   Result := true;
end;

//------------------------------------------------------------------------------
// Function to determine whether a field is numeric or alphabetic
//------------------------------------------------------------------------------
function TFLPMSBackup.GetFieldType(ThisType: string) : integer;
begin
   if ((Copy(ThisType,1,4) = LowerCase('char')) or (Copy(ThisType,1,7) = LowerCase('varchar')) or (Copy(ThisType,1,7) = LowerCase('tinytext')) or (Copy(ThisType,1,7) = LowerCase('text')) or (Copy(ThisType,1,7) = LowerCase('mediumtext')) or (Copy(ThisType,1,7) = LowerCase('longtext')) or (Copy(ThisType,1,4) = LowerCase('blob'))) then
      Result := 1
   else
      Result := 2;
end;

//------------------------------------------------------------------------------
// The timer to display the remaining time before the next backup popped.
// Remaining time is displayed only if less than 24 hours remain
//------------------------------------------------------------------------------
procedure TFLPMSBackup.timTimer2Timer(Sender: TObject);
begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ',';

   if (cbxType.Text = 'Weekly') then begin
      lblL05.Caption := '';
      Exit;
   end;

   lblL05.Caption := 'Next Backup in: ' + FormatDateTime('hh:nn:ss',TimeBackup - Time());
end;

//------------------------------------------------------------------------------
// Function to determine the next backup date and time
//------------------------------------------------------------------------------
procedure TFLPMSBackup.GetNextSlot(ThisInstr: integer);
var
   idx1, val1, mins, hours, days, ThisDay, BackupDay   : integer;
   ThisWeekDay, DispDate, DispTime, ThisDate, ThisTime : string;
   BackupTime                                          : string;
   TempDate                                            : TDateTime;

const
   WeekDays : array[1..7]  of string = ('Sun','Mon','Tue','Wed','Thu','Fri','Sat');

   WeekArray : array[1..7]  of string = ('Sunday', 'Monday', 'Tuesday',
               'Wednesday', 'Thursday', 'Friday', 'Saturday');
   HourArray : array[1..32] of string = ('00', '01', '02', '03', '04', '05',
               '06', '07', '08', '09', '10', '11', '12', '13', '14', '15',
               '16', '17', '18', '19', '20', '21', '22', '23', '24', '25',
               '26', '27', '28', '29', '30', '31');
   MinArray  : array[1..4]  of string = ('00', '15', '30', '45');

begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ',';

//--- Set date and time format for this session

   ThisDate    := FormatDateTime('yyyyMMdd',Now());
   ThisTime    := FormatDateTime('hhnn',Now());
   ThisWeekDay := FormatDateTime('DDD',Now());

//--- Work out the next time slot and the internval to the next time slot

   case Instr_List[ThisInstr].Instr_Rec.BackupType of

      0: begin                         // Hourly

         BackupTime := HourArray[Instr_List[ThisInstr].Instr_Rec.BackupT01]; { cbxT01.Text }

         if (BackupTime <= Copy(ThisTime,3,2)) then begin
            mins := ((StrToInt(BackupTime) - StrToInt(Copy(ThisTime,3,2))) + 60);

            val1 := StrToInt(Copy(ThisTime,1,2)) + 1;
            DispTime := IntToStr(val1) + ':' + HourArray[Instr_List[ThisInstr].Instr_Rec.BackupT01]; { cbxT01.Text }
         end else begin
            mins := StrToInt(Copy(BackupTime,1,2)) - StrToInt(Copy(ThisTime,3,2));

            DispTime   := Copy(ThisTime,1,2) + ':' + Copy(BackupTime,1,2);
         end;

         DispDate := Copy(ThisDate,1,4) + '/' + Copy(ThisDate,5,2) + '/' + Copy(ThisDate,7,2);

      end;

      1: begin                         // Daily

         BackupTime := HourArray[Instr_List[ThisInstr].Instr_Rec.BackupT01] + MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT02]; { cbxT01.Text + cbxT02.Text }

         if (BackupTime <= ThisTime) then begin

            hours := ((StrToInt(Copy(BackupTime,1,2)) - StrToInt(Copy(ThisTime,1,2))) + 23);
            mins  := (hours * 60) - ((StrToInt(Copy(ThisTime,3,2)) - StrToInt(MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT02])) - 60);

            TempDate := StrToDate(Copy(ThisDate,1,4) + '/' + Copy(ThisDate,5,2) + '/' + Copy(ThisDate,7,2));
            TempDate := IncDay(TempDate,1);
            ThisDate := FormatDateTime('yyyyMMdd',TempDate);

            DispDate := Copy(ThisDate,1,4) + '/' + Copy(ThisDate,5,2) + '/' + Copy(ThisDate,7,2);
            DispTime := HourArray[Instr_List[ThisInstr].Instr_Rec.BackupT01] + ':' + MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT02];

         end else begin

            hours := (StrToInt(Copy(ThisTime,1,2)) - StrToInt(Copy(BackupTime,1,2))) * -1;
            mins  := (hours * 60) + StrToInt(MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT02]) - StrToInt(Copy(ThisTime,3,2));

            DispDate   := Copy(ThisDate,1,4) + '/' + Copy(ThisDate,5,2) + '/' + Copy(ThisDate,7,2);
            DispTime   := Copy(BackupTime,1,2) + ':' + Copy(BackupTime,3,2);

         end;

      end;

      2: begin                         // Weekly

         BackupDay := Instr_List[ThisInstr].Instr_Rec.BackupT01 + 1; { cbxT01.ItemIndex + 1 }

         for idx1 := 1 to 7 do begin

            if (ThisWeekDay = WeekDays[idx1]) then begin
               ThisDay := idx1;
               Break;
            end;

         end;

         if (BackupDay < ThisDay) then
            days := (BackupDay - ThisDay + 7)
         else
            days := (BackupDay - ThisDay);

         BackupTime := HourArray[Instr_List[ThisInstr].Instr_Rec.BackupT02] + MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT03]; { cbxT02.Text + cbxT03.Text }

         if (BackupTime <= ThisTime) then begin

            hours := (days * 24) + ((StrToInt(Copy(BackupTime,1,2)) - StrToInt(Copy(ThisTime,1,2))) + 23);
            mins  := (hours * 60) - ((StrToInt(Copy(ThisTime,3,2)) - StrToInt(MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT03])) - 60);

            DispDate := WeekArray[Instr_List[ThisInstr].Instr_Rec.BackupT01]; { cbxT01.Text }
            DispTime := HourArray[Instr_List[ThisInstr].Instr_Rec.BackupT02] + ':' + MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT03];

         end else begin

            hours := (days *24 ) + (StrToInt(Copy(ThisTime,1,2)) - StrToInt(Copy(BackupTime,1,2))) * -1;
            mins  := (hours * 60) + StrToInt(MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT03]) - StrToInt(Copy(ThisTime,3,2));

            DispDate := WeekArray[Instr_List[ThisInstr].Instr_Rec.BackupT01]; { cbxT01.Text };
            DispTime := Copy(BackupTime,1,2) + ':' + Copy(BackupTime,3,2);

         end;

      end;

   end;

//--- Update the instruction's data in the in-memory list

   Instr_List[ThisInstr].NextDate := DispDate;
   Instr_List[ThisInstr].NextTime := DispTime;
   Instr_List[ThisInstr].Status   := ord(STAT_WAITING);

   BackupMsg  := 'Next Backup on ' + DispDate + ' at ' + DispTime;

//--- Set the amount of time before the next backup and restart Timer 2

   TimeBackup := IncMinute(Time(),mins);
   timTimer2.Enabled := true;
//   Result := Interval;

end;

//---------------------------------------------------------------------------
// Function to construct the XML for the SMS to be sent
//---------------------------------------------------------------------------
function TFLPMSBackup.Get_Send_XML(SMS: string) : string;
var
   Res      : string;
   send_xml : TStringList;

begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ',';

   send_xml := TStringList.Create;

//--- Ensue the message is correctly encoded

   SMS := EncodeURLElement(SMS);

   case Instr_List[ActiveInstr].Instr_Rec.BackupSMSProvider of
      1: begin
         send_xml.Add('<senddata>');
         send_xml.Add('<settings>');
         send_xml.Add('<live>True</live>');
         send_xml.Add('<return_entries_success_status>True</return_entries_success_status>');
         send_xml.Add('<return_credits>True</return_credits>');
         send_xml.Add('<default_date>' + FormatDateTime('DD/MMM/YYYY',Now) + '</default_date>');
         send_xml.Add('<default_time>' + FormatDateTime('hh:mm',Now) + '</default_time>');
         send_xml.Add('<default_data1>' + SMS + '</default_data1>');
         send_xml.Add('<default_flash>False</default_flash>');
         send_xml.Add('<default_type>SMS</default_type>');
         send_xml.Add('<default_senderid/>');
         send_xml.Add('</settings>');
         send_xml.Add('<entries>');
         send_xml.Add('<numto>' + Instr_List[ActiveInstr].Instr_Rec.BackupSMSNumber + '</numto>');
         send_xml.Add('<customerid>LPMS Backup Manager</customerid>');
         send_xml.Add('</entries>');
         send_xml.Add('</senddata>');
      end;

      2: send_xml.Add(SMS + '&msisdn=' + '27' + Copy(SMSNumber,2,99));
      3: send_xml.Add(SMS + '&numbers=' + '27' + Copy(SMSNumber,2,99));
   end;

   Res := send_xml.Text;
   send_xml.Destroy;
   Result := Res;
end;

//---------------------------------------------------------------------------
// Procedure to send an SMS
//---------------------------------------------------------------------------
procedure TFLPMSBackup.SendSMS(ThisMsg: string);
var
   PosStart   : integer;
   PosEnd     : integer;
   URL        : string;
   Params     : string;
   Answer     : string;
   Req_Result : string;
   Response   : TMemoryStream;

begin
   Response := TMemoryStream.Create;

   case Instr_List[ActiveInstr].Instr_Rec.BackupSMSProvider of
      0: Exit;
      1: begin
         URL := 'http://www.mymobileapi.com/api5/http5.aspx';
         Params   := 'type=send&username=' + Instr_List[ActiveInstr].Instr_Rec.BackupSMSUser + '&password=' + Instr_List[ActiveInstr].Instr_Rec.BackupSMSPass + '&XMLData=' + ThisMsg;
      end;

      2: begin
         URL    := 'http://bulksms.2way.co.za/eapi/submission/send_sms/2/2.0';
         Params := 'username=' + Instr_List[ActiveInstr].Instr_Rec.BackupSMSUser + '&password=' + Instr_List[ActiveInstr].Instr_Rec.BackupSMSPass + '&message=' + ThisMsg;
      end;

      3: begin
         URL    := 'http://www.winsms.co.za/api/batchmessage.asp';
         Params := 'user=' + Instr_List[ActiveInstr].Instr_Rec.BackupSMSUser + '&password=' + Instr_List[ActiveInstr].Instr_Rec.BackupSMSPass + '&message=' + ThisMsg;
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

   SMSDone := false;

   case Instr_List[ActiveInstr].Instr_Rec.BackupSMSProvider of
      1: begin
         PosStart := AnsiPos('<result>',Answer);
         PosEnd   := AnsiPos('</result>',Answer);
         Req_Result := Copy(Answer,PosStart + 8,PosEnd - (PosStart + 8));

         if (lowercase(Req_Result) = 'true') then
            SMSDone := true
         else begin
            PosStart := AnsiPos('<error>',Answer);
            PosEnd   := AnsiPos('</error>',Answer);
            SMSResult := Copy(Answer,PosStart + 7,PosEnd - (PosStart + 7));
         end;


      end;

      2: begin
         Req_Result := Copy(Answer,1,1);

         if (Req_Result = '0') then SMSDone := true;
      end;

      3: begin
         PosStart := AnsiPos('=',Answer);
         Req_Result := Copy(Answer,PosStart + 1,1);
         if ((Req_Result >= '1') and (Req_Result <= '9')) then SMSDone := true;
      end;
   end;
end;

//---------------------------------------------------------------------------
// Display a message in the Log listview
//---------------------------------------------------------------------------
procedure TFLPMSBackup.DispLogMsg(ThisMsg: string);
var
   ThisDate, ThisTime  : string;
   ThisItem            : TListItem;

begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ',';

   ThisDate := FormatDateTime('yyyy/mm/dd',Now());
   ThisTime := FormatDateTime('hh:mm:ss.zzz',Now());

//--- Display the message in the main log

   ThisItem := lvLogAll.Items.Add;

   ThisItem.Caption := ThisDate;
   ThisItem.SubItems.Add(Thistime);

   if tvInstructions.Selected.Level = 0 then
      ThisItem.SubItems.Add('Backup')
   else
      ThisItem.SubItems.Add(tvInstructions.Selected.Text);

   ThisItem.SubItems.Add(ThisMsg);
   ThisItem.MakeVisible(false);
   ThisItem.Selected := true;

   lvLogAll.Repaint;

   if tvInstructions.Selected.Level > 0 then begin

      ThisItem := lvLogSel.Items.Add;

      ThisItem.Caption := ThisDate;
      ThisItem.SubItems.Add(Thistime);
      ThisItem.SubItems.Add(tvInstructions.Selected.Text);
      ThisItem.SubItems.Add(ThisMsg);
      ThisItem.MakeVisible(false);
      ThisItem.Selected := true;

      lvLogSel.Repaint;

   end;

end;

//---------------------------------------------------------------------------
// Add Record Count to the last displayed message in the listview
//---------------------------------------------------------------------------
procedure TFLPMSBackup.DispLogMsg(RecCount : integer);
var
   ThisNum                    : double;
   ThisMsg, ThisPart, RecPart : string;
   ThisItem                   : TListItem;
   SearchRec                  : TSearchRec;

begin

   ThisPart := ' MB';

//--- Get the line to be modified from the overall log

   ThisItem := lvLogAll.Selected;
   ThisMsg  := ThisItem.SubItems.Strings[2];

   if RecCount = -1 then begin

      if (FindFirst(ExpandFileName(OutFile),faAnyFile,SearchRec) = 0) then
         ThisNum := SearchRec.Size
      else
         ThisNum := 0;

      FindClose(SearchRec);

      if ThisNum <= 1048576 then begin

         ThisNum  := ThisNum / 1024;
         ThisPart := ' KB';

      end else
         ThisNum := ThisNum / 1048576;

      RecTotalF := FloatToStrF(RecTotal,ffNumber,10,0);
      ThisMsgF  := FloatToStrF(ThisNum,ffNumber,10,2) + ThisPart;

      ThisMsg  := ThisMsg + ', Total Records: ' + RecTotalF + ', File Size: ' + ThisMsgF;
      RecTotal := 0;

   end else begin

      if RecCount = 1 then
         RecPart := ' Record'
      else
         RecPart := ' Records';

      ThisMsg  := ThisMsg + ' (' + FloatToStrF(RecCount,ffNumber,10,0) + RecPart + ')';
      RecTotal := RecTotal + RecCount;

   end;

//--- Update both the overall log and the instruction specific log if it is
//--- visible

   ThisItem := lvLogAll.Selected;
   ThisItem.SubItems.Strings[2] := ThisMsg;
   lvLogAll.Refresh;

   if tvInstructions.Selected.Level > 0 then begin

      ThisItem := lvLogSel.Selected;
      ThisItem.SubItems.Strings[2] := ThisMsg;
      lvLogSel.Refresh;

   end;

end;

//---------------------------------------------------------------------------
// Display a message in the Log listview with date and time supplied
//---------------------------------------------------------------------------
procedure TFLPMSBackup.DispLogMsg(ThisDate: string; ThisTime: string; ThisInstr: string; ThisMsg: string);
var
   ThisItem  : TListItem;

begin

//--- Display the message int he overall log

   ThisItem := lvLogAll.Items.Add;
   ThisItem.Caption := ThisDate;
   ThisItem.SubItems.Add(ThisTime);
   ThisItem.SubItems.Add(ThisInstr);
   ThisItem.SubItems.Add(ThisMsg);
   ThisItem.MakeVisible(false);

   lvLogAll.Repaint;

//--- If an Instruction is selected then also display the message in the
//--- Instruction's log

   if tvInstructions.Selected.Level > 0 then begin

      ThisItem := lvLogSel.Items.Add;
      ThisItem.Caption := ThisDate;
      ThisItem.SubItems.Add(ThisTime);
      ThisItem.SubItems.Add(ThisInstr);
      ThisItem.SubItems.Add(ThisMsg);
      ThisItem.MakeVisible(false);

      lvLogSel.Repaint;

   end;
end;

//---------------------------------------------------------------------------
// Procedure to Open and Read the Configuration File and to build the tree
// view and the in-memory instrruction list
//---------------------------------------------------------------------------
procedure TFLPMSBackup.OpenCfg(FileName: string);
var
   idx1       : integer;
   FirstChild : boolean;
   ThisLine   : string;
   CfgInstr   : TStringList;
   ThisNode   : TTreeNode;

begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ',';

   NumInstr := 0;

   if (FileExists(FileName) = true) then begin

//--- Create the String Lists

      CfgInstr    := TStringList.Create;
      InstrTokens := TStringList.Create;

//--- Load the Configuration file into memory and then extract the 1st Line

      CfgInstr.LoadFromFile(FileName);
      ThisLine := CfgInstr.Strings[0];

      ExtractStrings(['|'], [], PChar(ThisLine), InstrTokens);

//--- Save the number of instructions that were listed in the Configuration file

      NumInstr := InstrTokens.Count;

   end;

//--- Build the Treeview and the in-memory array of instructions

   FirstChild := True;

//--- Insert the root node

   ThisNode := tvInstructions.Items.Add(nil,'Backup Instructions');
   ThisNode.ImageIndex    := 2;
   ThisNode.SelectedIndex := 3;

//--- Process the Instructions and build the in-memory list

   SetLength(Instr_List,NumInstr);

   for idx1 := 0 to NumInstr -1 do begin

      if (FirstChild = True) then begin
         ThisNode   := tvInstructions.Items.AddChildFirst(ThisNode,InstrTokens.Strings[idx1]);
         FirstChild := False;
      end else
         ThisNode   := tvInstructions.Items.Add(ThisNode,InstrTokens.Strings[idx1]);

      Instr_List[idx1].Ini_File    := 'LPMS_Backup_' + InstrTokens.Strings[idx1] + '.ini';
      Instr_List[idx1].Instruction := InstrTokens.Strings[idx1];
      Instr_List[idx1].NextDate    := '2099/12/31';
      Instr_List[idx1].NextTime    := '23:59:59';

//--- Load the contents of the ini File

      RegString := Instr_List[idx1].Ini_File;

//--- Extract the information contained in the 'registry'

      if (FileExists(RegString) = false) then begin

         Instr_List[idx1].Instr_Rec.BackupBlock       := 5000;
         Instr_List[idx1].Instr_Rec.BackupSMSProvider := 0;
         Instr_List[idx1].Instr_Rec.BackupType        := 0;
         Instr_List[idx1].Instr_Rec.BackupT01         := 0;
         Instr_List[idx1].Instr_Rec.BackupT02         := 0;
         Instr_List[idx1].Instr_Rec.BackupT03         := 0;
         Instr_List[idx1].Instr_Rec.BackupSMSAlways   := False;
         Instr_List[idx1].Instr_Rec.BackupSMSFailure  := False;
         Instr_List[idx1].Instr_Rec.BackupSMSNever    := True;
         Instr_List[idx1].Instr_Rec.BackupSMSSuccess  := False;
         Instr_List[idx1].Instr_Rec.BackupDBPass      := '';
         Instr_List[idx1].Instr_Rec.BackupDBPrefix    := InstrTokens[idx1];
         Instr_List[idx1].Instr_Rec.BackupDBSuffix    := '';
         Instr_List[idx1].Instr_Rec.BackupDBUser      := '';
         Instr_List[idx1].Instr_Rec.BackupHostName    := '127.0.0.1';
         Instr_List[idx1].Instr_Rec.BackupLocation    := '~';
         Instr_List[idx1].Instr_Rec.BackupSMSNumber   := '';
         Instr_List[idx1].Instr_Rec.BackupSMSPass     := '';
         Instr_List[idx1].Instr_Rec.BackupSMSUser     := '';
         Instr_List[idx1].Instr_Rec.BackupTemplate    := '&Date@&Time - &BackupType Backup (&CpyName on &HostName)';
         Instr_List[idx1].Instr_Rec.BackupViewer      := '';

         Instr_List[idx1].Active := False;
         Instr_List[idx1].Status := ord(STAT_INACTIVE);
         ThisNode.ImageIndex     := 0;

      end else begin

         IniFile := TINIFile.Create(RegString);

         Instr_List[idx1].Instr_Rec.BackupBlock       := IniFile.ReadInteger('Parameters','BackupBlock',5000);
         Instr_List[idx1].Instr_Rec.BackupSMSProvider := IniFile.ReadInteger('Parameters','BackupSMSProvider',0);
         Instr_List[idx1].Instr_Rec.BackupType        := IniFile.ReadInteger('Parameters','BackupType',0);
         Instr_List[idx1].Instr_Rec.BackupT01         := IniFile.ReadInteger('Parameters','BackupT01',0);
         Instr_List[idx1].Instr_Rec.BackupT02         := IniFile.ReadInteger('Parameters','BackupT02',0);
         Instr_List[idx1].Instr_Rec.BackupT03         := IniFile.ReadInteger('Parameters','BackupT03',0);
         Instr_List[idx1].Instr_Rec.BackupSMSAlways   := IniFile.ReadBool('Parameters','BackupSMSAlways',False);
         Instr_List[idx1].Instr_Rec.BackupSMSFailure  := IniFile.ReadBool('Parameters','BackupSMSFailure',False);
         Instr_List[idx1].Instr_Rec.BackupSMSNever    := IniFile.ReadBool('Parameters','BackupSMSNever',True);
         Instr_List[idx1].Instr_Rec.BackupSMSSuccess  := IniFile.ReadBool('Parameters','BackupSMSSuccess',False);
         Instr_List[idx1].Instr_Rec.BackupDBPass      := IniFile.ReadString('Parameters','BackupDBPass','');
         Instr_List[idx1].Instr_Rec.BackupDBPrefix    := IniFile.ReadString('Parameters','BackupDBPrefix','');
         Instr_List[idx1].Instr_Rec.BackupDBSuffix    := IniFile.ReadString('Parameters','BackupDBSuffix','');
         Instr_List[idx1].Instr_Rec.BackupDBUser      := IniFile.ReadString('Parameters','BackupDBUser','');
         Instr_List[idx1].Instr_Rec.BackupHostName    := IniFile.ReadString('Parameters','BackupHostName','www.bluecrane.cc');
         Instr_List[idx1].Instr_Rec.BackupLocation    := IniFile.ReadString('Parameters','BackupLocation','~');
         Instr_List[idx1].Instr_Rec.BackupSMSNumber   := IniFile.ReadString('Parameters','BackupSMSNumber','');
         Instr_List[idx1].Instr_Rec.BackupSMSPass     := IniFile.ReadString('Parameters','BackupSMSPass','');
         Instr_List[idx1].Instr_Rec.BackupSMSUser     := IniFile.ReadString('Parameters','BackupSMSUser','');
         Instr_List[idx1].Instr_Rec.BackupTemplate    := IniFile.ReadString('Parameters','BackupTemplate','&Date@&Time - &BackupType Backup (&CpyName on &HostName)');
         Instr_List[idx1].Instr_Rec.BackupViewer      := IniFile.ReadString('Parameters','BackupViewer','');

         IniFile.Destroy;

//--- Update the Active and Status indicators to indciate that this instruction
//--- is valid and in Waiting state

         Instr_List[idx1].Active := True;
         Instr_List[idx1].Status := ord(STAT_WAITING);
         ThisNode.ImageIndex     := 1;

      end;

//--- If the Backup Type and the Time is not set then set it for Daily backups
//--- and to do a backup on the next quarter

      if ((Instr_List[idx1].Instr_Rec.BackupType = 0) and (Instr_List[idx1].Instr_Rec.BackupT01 = 0) and (Instr_List[idx1].Instr_Rec.BackupT02 = 0) and (Instr_List[idx1].Instr_Rec.BackupT03 = 0)) then begin
         Instr_List[idx1].Instr_Rec.BackupType := 1;
         Instr_List[idx1].Instr_Rec.BackupT01  := StrToInt(FormatDateTime('HH',Now()));
         Instr_List[idx1].Instr_Rec.BackupT02  := StrToInt(FormatDateTime('mm',Now()));

         if (Instr_List[idx1].Instr_Rec.BackupT02 < 15) then
            Instr_List[idx1].Instr_Rec.BackupT02 := 2
         else if (Instr_List[idx1].Instr_Rec.BackupT02 < 30) then
            Instr_List[idx1].Instr_Rec.BackupT02 := 3
         else if (Instr_List[idx1].Instr_Rec.BackupT02 < 45) then
            Instr_List[idx1].Instr_Rec.BackupT02 := 4
         else begin
            Instr_List[idx1].Instr_Rec.BackupT02 := 1;
            inc(Instr_List[idx1].Instr_Rec.BackupT01);
         end;
      end;

      ThisNode.SelectedIndex := 3;

      if (Instr_List[idx1].Active = True) then
         GetNextSlot(idx1);

   end;

   CfgInstr.Destroy;
   InstrTokens.Destroy;

end;

//---------------------------------------------------------------------------
// Procedure to Save the Configuration File
//---------------------------------------------------------------------------
procedure TFLPMSBackup.SaveCfg(FileName: string);
var
   idx1      : integer;
   ThisLine  : string;
   SaveList  : TStringList;
//   ThisLV    : TListView;

const
   Delim     : char = '|';

begin
//   if (tvInstructions.Selected.Index = 0) then
//      ThisLV := lvLogAll
//   else
//      ThisLV := lvLogSel;

   SaveList := TStringList.Create;

   for idx1 := 0 to lvLogAll.Items.Count - 1 do begin
      ThisLine := lvLogAll.Items.Item[idx1].Caption + Delim +
                  lvLogAll.Items.Item[idx1].SubItems.Strings[0] + Delim +
                  lvLogAll.Items.Item[idx1].SubItems.Strings[1] + Delim +
                  lvLogAll.Items.Item[idx1].SubItems.Strings[2] + Delim;

      SaveList.Add(ThisLine);
   end;

   SaveList.SaveToFile(FileName);
   SaveList.Free;
end;

//---------------------------------------------------------------------------
// Procedure to Open and read a Log File from disk
//---------------------------------------------------------------------------
procedure TFLPMSBackup.OpenLog(FileName: string);
var
   idx1      : integer;
   NumLines  : integer = 0;
   ThisLine  : string;
   LogLines  : TStringList;
   LogTokens : TStrings;

//***
//*** TO DO - Check for invalid log file content/format and give user options
//***

begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ',';

   lvLogAll.Clear();

   if FileExists(FileName) = true then begin

      LogLines  := TStringList.Create;

      LogLines.LoadFromFile(FileName);

      for idx1 := 0 to LogLines.Count -1 do begin

         LogTokens := TStringList.Create;
         ThisLine  := LogLines.Strings[idx1];

         ExtractStrings(['|'], [], PChar(ThisLine), LogTokens);

         DispLogMsg(LogTokens[0], LogTokens[1], LogTokens[2], LogTokens[3]);
         inc(NumLines);

         LogTokens.Destroy;

      end;

      LogLines.Destroy;

   end;

   if (NumLines = 0) then begin
      DispLogMsg(FormatDateTime('yyyy/MM/dd',Now()),FormatDateTime('hh:nn:ss.zzz',Now()),'##New Log File Created', '');
   end;
end;

//---------------------------------------------------------------------------
// Procedure to Save a Log File to disk
//---------------------------------------------------------------------------
procedure TFLPMSBackup.SaveLog(FileName: string);
var
   idx1      : integer;
   ThisLine  : string;
   SaveList  : TStringList;

const
   Delim     : char = '|';

begin

   SaveList := TStringList.Create;

   for idx1 := 0 to lvLogAll.Items.Count - 1 do begin

      ThisLine := lvLogAll.Items.Item[idx1].Caption + Delim +
                  lvLogAll.Items.Item[idx1].SubItems.Strings[0] + Delim +
                  lvLogAll.Items.Item[idx1].SubItems.Strings[1] + Delim +
                  lvLogAll.Items.Item[idx1].SubItems.Strings[2] + Delim;

      SaveList.Add(ThisLine);

   end;

   SaveList.SaveToFile(FileName);
   SaveList.Destroy;

end;

//---------------------------------------------------------------------------
// Function to get Required information from the DB
//---------------------------------------------------------------------------
function TFLPMSBackup.GetBasicInfo() : boolean;
var
   S1   : string;

begin
   S1 := 'SELECT Version, CpyName FROM lpms';

   try
      sqlQry1.Close;
      sqlQry1.SQL.Text := S1;
      sqlQry1.Open;
   except on E : Exception do
      begin
         LastMsg := E.Message;
         Result := false;
         Exit;
      end;
   end;
   Result := true;
end;

//---------------------------------------------------------------------------
// Function to read a record from the current Table
//---------------------------------------------------------------------------
function TFLPMSBackup.ReadTable(Table: string; LimitStart: integer; LimitEnd: integer) : boolean;
var
   S1 : string;

begin
//   lblL05.Caption := 'Backing up Table "' + Table + '", Reading up to ' + IntToStr(KeepBackupBlock) + ' records';
   lblL05.Caption := 'Backing up Table "' + Table + '", Reading...';
   lblL05.Refresh;

   S1 := 'SELECT * FROM ' + Table + ' LIMIT ' + IntToStr(LimitStart) + ',' + IntToStr(LimitEnd);

   try
      sqlQry1.Close;
      sqlQry1.SQL.Text := S1;
      sqlQry1.Open;
   except on E : Exception do
      begin
         LastMsg := E.Message;
         Result  := false;
         Exit;
      end;
   end;

   if (sqlQry1.RecordCount = 0) then
      LimitActive := false;

   Result := true;
end;

//------------------------------------------------------------------------------
end.

