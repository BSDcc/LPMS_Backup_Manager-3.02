//------------------------------------------------------------------------------
// Date.......: 09 October 2012
// System.....: LPMS Backup Manager
// Platform...: Lazarus (Linux, macOS & Windows)
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

{$ifdef DARWIN}                                    // Target is macOS
  macOSAll, Folders, FileUtil, mysql57conn,
{$endif}

{$ifdef LINUX}                                     // Target is Linux
  FileUtil, mysql57conn,
{$endif}

{$ifdef WINDOWS}                                   // Target is Winblows
  FileUtil, mysql56conn,
{$endif}

  Classes, SysUtils, sqldb, LCLType, Forms, Controls, Graphics, Dialogs,
  ActnList, Menus, ComCtrls, StdCtrls, Buttons, ExtCtrls, EditBtn, Spin,
  usplashabout, strutils, INIFiles, HTTPSend, Synacode, DateUtils,
  LazFileUtils, Zipper;

//------------------------------------------------------------------------------
// Declarations
//------------------------------------------------------------------------------
type

  { TFLPMSBackup }

   TFLPMSBackup = class(TForm)
   ActionsRestore: TAction;
   Bevel4: TBevel;
   Bevel5: TBevel;
   Bevel6: TBevel;
   btnDBTest: TButton;
   btnTemplate: TButton;
   cbxCompress: TCheckBox;
   cbxDoSort: TCheckBox;
   cbxDelete: TCheckBox;
   FileDelete: TAction;
   FileNew: TAction;
   btnSMSTest: TButton;
   btnViewer: TSpeedButton;
   cbSMSProviderC: TComboBox;
   cbxDBSuffixC: TCheckBox;
   edtDBUserC: TEdit;
   edtDBPassC: TEdit;
   edtDBPrefixC: TEdit;
   edtInstrNameC: TEdit;
   edtHostNameC: TEdit;
   edtLocationC: TDirectoryEdit;
   edtSMSPassC: TEdit;
   edtSMSUserC: TEdit;
   edtTemplateC: TEdit;
   edtViewerC: TEdit;
   dlgFind: TFindDialog;
   HelpAbout: TAction;
   ActionsRunNow: TAction;
   ActionsNext: TAction;
   ActionsPrevious: TAction;
   ActionsLast: TAction;
   ActionsFirst: TAction;
   imgSmall: TImageList;
   imgLargeD: TImageList;
   imgLargeH: TImageList;
   Label10: TLabel;
   Label11: TLabel;
   Label12: TLabel;
   Label13: TLabel;
   Label14: TLabel;
   Label15: TLabel;
   Label16: TLabel;
   Label17: TLabel;
   Label18: TLabel;
   Label19: TLabel;
   Label2: TLabel;
   Label3: TLabel;
   lblSMSPass: TLabel;
   lblSMSUser: TLabel;
   MenuItem21: TMenuItem;
   MenuItem22: TMenuItem;
   MenuItem23: TMenuItem;
   MenuItem6: TMenuItem;
   N4: TMenuItem;
   N3: TMenuItem;
   N1: TMenuItem;
   pnl00b1b2: TPanel;
   pnl00b1b1: TPanel;
   pnl00b1b: TPanel;
   pnl00b1a: TPanel;
   SearchFind: TAction;
   EditUpdate: TAction;
   EditCancel: TAction;
   Bevel1: TBevel;
   Bevel3: TBevel;
   edtConfigFile: TEdit;
   edtHostName: TEdit;
   edtNextBackup: TEdit;
   edtLocation: TEdit;
   imgLargeN: TImageList;
   lblDBVersion: TLabel;
   Label7: TLabel;
   Label8: TLabel;
   Label9: TLabel;
   MenuItem10: TMenuItem;
   MenuItem11: TMenuItem;
   MenuItem12: TMenuItem;
   MenuItem13: TMenuItem;
   MenuItem14: TMenuItem;
   MenuItem16: TMenuItem;
   MenuItem17: TMenuItem;
   MenuItem18: TMenuItem;
   MenuItem8: TMenuItem;
   N2: TMenuItem;
   MenuItem9: TMenuItem;
   mnuMain: TMainMenu;
   FileFile: TMenuItem;
   HelpHelp: TMenuItem;
   MenuItem5: TMenuItem;
   pcInstructions: TPageControl;
   Panel1: TPanel;
   Panel2: TPanel;
   Panel3: TPanel;
   pnl00b2: TPanel;
   pnl00b1: TPanel;
   speBlockSizeC: TSpinEdit;
   saAbout: TSplashAbout;
   Splitter1: TSplitter;
   ToolButton17: TToolButton;
   ToolButton18: TToolButton;
   ToolButton19: TToolButton;
   tsInstruction: TTabSheet;
   tsConfiguration: TTabSheet;
   ToolBar1: TToolBar;
   ToolButton1: TToolButton;
   ToolButton10: TToolButton;
   ToolButton11: TToolButton;
   ToolButton13: TToolButton;
   ToolButton14: TToolButton;
   ToolButton15: TToolButton;
   ToolButton16: TToolButton;
   ToolButton2: TToolButton;
   ToolButton3: TToolButton;
   ToolButton4: TToolButton;
   btnFirst: TToolButton;
   btnNext: TToolButton;
   btnLast: TToolButton;
   btnPrev: TToolButton;
   ActionsMinimise: TAction;
   Bevel2: TBevel;
   btnOpenLB: TSpeedButton;
   edtLastBackup: TEdit;
   imgTree: TImageList;
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
   btnCancel: TButton;
   btnClose: TButton;
   btnRunNow: TButton;
   btnMinimise: TButton;
   btnUpdate: TButton;
   cbxT01: TComboBox;
   cbxT02: TComboBox;
   cbxT03: TComboBox;
   cbxType: TComboBox;
   edtSMSNumber: TEdit;
   FileClose: TAction;
   actList: TActionList;
   Image1: TImage;
   Label1: TLabel;
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
   pnlP01: TPanel;
   pumTray: TPopupMenu;
   rbSMSFailure: TRadioButton;
   rbSMSNever: TRadioButton;
   rbSMSSuccess: TRadioButton;
   sqlQry1: TSQLQuery;
   sqlTran: TSQLTransaction;
   sbStatus: TStatusBar;
   timTimer2: TTimer;
   timTimer1: TTimer;
   TrayIcon: TTrayIcon;
   tvSmall: TTreeView;
   tvInstructions: TTreeView;

    procedure dlgFindFind( Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FileCloseExecute(Sender: TObject);
    procedure FileDeleteExecute(Sender: TObject);
    procedure FileNewExecute(Sender: TObject);
    procedure EditCancelExecute(Sender: TObject);
    procedure EditUpdateExecute(Sender: TObject);
    procedure FormCreate( Sender: TObject);
    procedure SearchFindExecute(Sender: TObject);
    procedure ActionsFirstExecute(Sender: TObject);
    procedure ActionsLastExecute(Sender: TObject);
    procedure ActionsNextExecute(Sender: TObject);
    procedure ActionsPreviousExecute(Sender: TObject);
    procedure ActionsMinimiseExecute(Sender: TObject);
    procedure ActionsRestoreExecute(Sender: TObject);
    procedure ActionsRunNowExecute(Sender: TObject);
    procedure HelpAboutExecute(Sender: TObject);
    procedure tvInstructionsClick(Sender: TObject);
    procedure edtLocationAcceptDirectory(Sender: TObject; var Value: String);
    procedure edtLocationButtonClick(Sender: TObject);
    procedure btnOpenLBClick(Sender: TObject);
    procedure btnDBTestClick(Sender: TObject);
    procedure btnSMSTestClick(Sender: TObject);
    procedure btnTemplateClick(Sender: TObject);
    procedure btnViewerClick(Sender: TObject);
    procedure cbxDoSortChange(Sender: TObject);
    procedure btnMinimiseClick(Sender: TObject);
    procedure cbxTypeChange(Sender: TObject);
    procedure edtInstrNameCChange(Sender: TObject);
    procedure pcInstructionsChange(Sender: TObject);
    procedure timTimer2Timer(Sender: TObject);
    procedure timTimer1Timer(Sender: TObject);
    procedure TrayIconMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure tvInstructionsEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
    procedure tvSmallClick( Sender: TObject);
    procedure tvSmallEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);

type
   TYPE_DISPLAY = (TYPE_LOGALL,         // Information message is displayed in the Overall Log only
                   TYPE_LOGSEL,         // Information message is displayed in the Instruction Log only
                   TYPE_BOTH);          // Information message is displaye din both Logs

   HINT_OPTIONS = (HINT_WAITING,        // Hint shown in Winblows when the Application is minimised to the System Tray
                   HINT_RUNNING);       // Hint shown in Winblows when the Applciation is minimised to the System Tray

   STAT_OPTIONS = (STAT_INACTIVE,       // Current Instruction will not be considered by the Scheduler
                   STAT_WAITING,        // Current Instruction is waiting to be scheduled on the designated time
                   STAT_RUNNOW,         // Current Instruction wil be scheduled immediately
                   STAT_SCHEDULED);     // Currint Instruction was scheduled and is waiting to go complete or go back into the queue

   BTN_STATE    = (BTN_INITIAL,         // App startup or USer clicked on Root of the TreeView
                   BTN_INSTRUCTION,     // User clicked on an Instruction in the TreeView or a [Run Now] has completed
                   BTN_RUNNOW,          // User clicked on [Run Now]
                   BTN_UPDATE);         // A User Editable field was changed

   TVIEW_REQ    = (TV_DUPLICATE,        // Request to check for a duplicate
                   TV_REPLACE,          // Request to replace/rename an existing Backup Instruction in the TreeView
                   TV_DELETE);          // Request to delete an existing Backup Instruction

   REC_InstrRecord = record
      InstrNum     : string;
      InstrName    : string;
      InstrNewName : string;
      Request      : integer;
   end;

   REC_IniRecord = record
      BackupBlock           : integer;
      BackupSMSProvider     : integer;
      BackupType            : integer;
      BackupT01             : integer;
      BackupT02             : integer;
      BackupT03             : integer;
      BackupSMSAlways       : boolean;
      BackupSMSFailure      : boolean;
      BackupSMSNever        : boolean;
      BackupSMSSuccess      : boolean;
      BackupDBPass          : string;
      BackupDBPrefix        : string;
      BackupDBSuffix        : string;
      BackupDBUser          : string;
      BackupHostName        : string;
      BackupLocation        : string;
      BackupMsg             : string;
      BackupSMSNumber       : string;
      BackupSMSPass         : string;
      BackupSMSUser         : string;
      BackupTemplate        : string;
      BackupViewer          : string;
      BackupSMSProviderName : string;
   end;

   REC_Instructions = record
      Active                : boolean;
      Instruction           : string;
      Ini_File              : string;
      NextDate              : string;
      NextTime              : string;
      SymCpy                : string;
      SymVersion            : string;
      Instr_Rec             : Rec_IniRecord;
      Status                : integer;
   end;

   Array_Instructions = array of REC_Instructions;

private { private declarations }

   LimitActive      : boolean;     // Indicates whether LIMIT is used when reading SQL records
   CanUpdate        : boolean;     // Used to override setting of DoSave
   SMSDone          : boolean;     // True if SMS send was successful
   DoSave           : boolean;     // Tracks whether a Save is requried
   DoNotConnect     : boolean;     // Prevents a DB connection when a previously inactive Backup Instruction is made active
   BackupSuccess    : boolean;     // Indicates whether the backup was successful or not
   FirstRun         : boolean;     // Prevents FormActivate from running more than once
   AtFirst          : boolean;     // Indicates a Search has hit the first line of the Log
   AtLast           : boolean;     // Indciates a Search has reached the last line of the Log
   RecTotal         : integer;     // Holds unformatted rec count after backup
   NumInstr         : integer;     // Number of backup instructions in the Configuration File
   SaveInstr        : integer;     // Holds instruction index when an Update started - used in case of a Cancel
   ActiveInstr      : integer;     // Set by the Timer to indicate which backup instruction popped
   LogInstrType     : integer;     // Determines in which logs the message is displayed
   LastPos          : integer;     // Used by Search to keep track of the lof entry line that will be searched next
   RecTotalF        : string;      // Holds formated rec count after backup
   ThisMsgF         : string;      // Holds formatted message containing outcome of current backup
   OutFile          : string;      // Name of the Backup file that will be created
   RegString        : string;      // Holds the name of the ini file
   BackupLogFile    : string;      // Log file name
   KeepVersion      : string;      // Holds the current DB version if this is a LPMS DB
   LastMsg          : string;      // Last SQL error message
   OSName           : string;      // Holds the name of the Platform we are running on
   SMSResult        : string;      // Holds result returned by the SMS Provider
   SaveName         : string;      // Holds instruction name when an Update started - used in case of a Cancel
   InstrSel         : string;      // Contains the Text of the selected TreeView item
   ActiveName       : string;      // Name of the Instruction scheduled by the Scheduler
   LocalPath        : string;      // Dir where Log, Config File and Back Instructions File are stored
   StartTime        : TDateTime;   // Start time of the current Backup
   EndTime          : TDateTime;   // End time of the current Backup
   IniFile          : TINIFile;    // IniFile holding defaults
   InstrTokens      : TStrings;    // Holds the List of Backup Instruction names
   VisibleLog       : TListView;   // Allows search in bith lvLogAll and lvLogSel
   Instr_List       : Array_Instructions; // Array of in-memory Backup instructions
   UpdateRec        : REC_InstrRecord;    // Used to insert/change/delete records from the Treeview

{$ifdef WINDOWS}
   sqlCon : TMySQL56Connection;  // Running on Winblows
{$endif}

{$ifdef LINUX}
   sqlCon : TMySQL57Connection;  // Running on Linux
{$endif}

{$ifdef DARWIN}
   sqlCon : TMySQL57Connection;  // Running on macOS
{$endif}

const

   SMSProvider : array[1..4] of string  = ('Inactive', 'SMS Portal',
                                           'BulkSMS', 'WinSMS');

   function  GetInstruction() : integer;
   procedure ShowInstruction();
   procedure Navigate();
   function  FindTextString() : integer;
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
   procedure Set_Buttons(State: integer);
   function  Get_Send_XML(SMS: string) : string;
   function  DoBackup() : boolean;
   function  GetFieldType(ThisType: string) : integer;
   function  ReadDB(FileName: string; ThisType: integer) : boolean;
   procedure DBAccessFailed(ThisMsg: string);
   procedure GetNextSlot(ThisInstr: integer);
   function  GetBasicInfo() : boolean;
   function  ReadTable(Table: string; LimitStart: integer; LimitEnd: integer) : boolean;
   function  AccessTreeView(ThisReq: REC_InstrRecord) : boolean;

public  { public declarations }

   OSDelim         : string;             // Holds '/' or '\' depending on the OS
   CfgFile         : string;             // Name of the default Configuration File
   BackupTemplate  : REC_Instructions;   // Template to be used for new/invalid backup instructions

   function  Encode(PlainStr: string) : string;
   function  Decode(CodedStr: string) : string;

const

   WeekArray   : array[1..7]  of string = ('Sunday', 'Monday', 'Tuesday',
                                           'Wednesday', 'Thursday', 'Friday',
                                           'Saturday');

   HourArray   : array[1..32] of string = ('00', '01', '02', '03', '04', '05',
                                           '06', '07', '08', '09', '10', '11',
                                           '12', '13', '14', '15', '16', '17',
                                           '18', '19', '20', '21', '22', '23',
                                           '24', '25', '26', '27', '28', '29',
                                           '30', '31');

   MinArray    : array[1..12] of string = ('00', '05', '10', '15', '20', '25',
                                           '30', '35', '40', '45', '50', '55');

end;

//------------------------------------------------------------------------------
// Global variables
//------------------------------------------------------------------------------
var
   FLPMSBackup: TFLPMSBackup;

implementation

uses ldBackupTemplate;

{$R *.lfm}

{ TFLPMSBackup }

//------------------------------------------------------------------------------
// Excuted when the Form is created
//------------------------------------------------------------------------------
procedure TFLPMSBackup. FormCreate( Sender: TObject);
begin
   FirstRun := False;
end;

//------------------------------------------------------------------------------
// Executed before the form is displayed
//------------------------------------------------------------------------------
procedure TFLPMSBackup.FormActivate(Sender: TObject);
begin

   if FirstRun = True then
      Exit;

   FirstRun := True;
   FLPMSBackup.Hide;


//--- Determine the Platform on which we are running and set the defaults to be
//--- Platform specific

{$ifdef WINDOWS}
   OSDelim := '\';
   OSName  := 'MS-Windows';
   sqlCon  := TMySQL56Connection.Create(nil);
{$endif}

{$ifdef LINUX}
   OSDelim := '/';
   OSName  := 'Linux';
   sqlCon  := TMySQL57Connection.Create(nil);
{$endif}

{$ifdef DARWIN}
   OSDelim := '/';
   OSName  := 'macOS';
   sqlCon  := TMySQL57Connection.Create(nil);
{$endif}

   sqlTran.DataBase    := sqlCon;
   sqlQry1.Transaction := sqlTran;

//--- We get the path to the user's home directory (this is platform
//--  independent). Winblows is a problem due to a lack of naming conventions
//--- across versions of Winblows. If it is not 'Documents' or 'My Documents'
//--- then we give the User a change to select the home directory.

{$IFDEF WINDOWS}

   LocalPath := AppendPathDelim(GetUserDir + 'Documents');

   if DirectoryExists(LocalPath) = False then begin

      LocalPath := AppendPathDelim(GetUserDir + 'My Documents');

      if DirectoryExists(LocalPath) = False then begin

         if (MessageDlg('Backup Manager','WARNING: Unable to locate home directory. ' + #10 + #10 + 'Click [Yes] to locate the home directory, or [No] to terminate', mtWarning, [mbYes,mbNo], '') = mrNo) then begin;

            Application.Terminate;
            Exit;

         end;


         if jvBrowse.Execute = False then begin

            Application.Terminate;
            Exit;

         end;

      end;

   end;

   LocalPath := LocalPath + 'Backup_Manager' + OSDelim;

{$ELSE}

   LocalPath := AppendPathDelim(GetUSerDir);
   LocalPath := LocalPath + '.backup_manager' + OSDelim;

{$ENDIF}

//--- We now have what passes for a home directory with the working directory
//--- Backup Manager added to it and tests whether this exists. If it does not
//--- then we ask the User whether we should create it and do so if the User
//--- agrees otherwise we terminate the Application

   if DirectoryExists(LocalPath) = False then begin

      if (MessageDlg('Backup Manager','WARNING: Backup Manager directory does not exist' + #10 + #10 + 'Click [Yes] to create the directory, or [No] to terminate', mtWarning, [mbYes,mbNo], '') = mrNo) then begin;

         Application.Terminate;
         Exit;

      end;

      if CreateDir(LocalPath) = False then begin

         MessageDlg('Backup Manager','FATAL: Unable to create Backup Manager directory.' + #10 + #10 + 'Backup Manager cannot continue and will be terminated.', mtError, [mbOk], '');
         Application.Terminate;
         Exit;

      end;

   end;

//--- Set up

   LogInstrType := ord(TYPE_LOGALL);
   SMSDone      := False;

   FLPMSBackup.Caption := 'Backup Manager';
   CfgFile             := LocalPath + 'Backup_Manager.ini';

   tvInstructions.Items.Clear;
   pnlP00b.Visible := True;
   pnlP00a.Visible := False;

   BackupLogFile := LocalPath + 'Backup_Manager_Logfile.txt';

//--- Open and load the contents of the Log File

   lvLogAll.Clear();
   lvLogSel.Clear();

   OpenLog(BackupLogFile);

//--- Open the Config file then build the in-memory list and the Treeview

   OpenCfg(CfgFile);
   tvInstructions.Items.Item[0].Selected := True;
   tvInstructions.AutoExpand := True;
   tvInstructions.FullExpand;

   FLPMSBackup.Show;

//--- Start the Time display and Scheduler timers

   timTimer1.Enabled  := False;
   timTimer1.Interval := 1000;
   timTimer1.Enabled  := True;

   timTimer2.Enabled  := False;
   timTimer2.Interval := 1000;
   timTimer2.Enabled  := True;

//--- Set up the non-instruction related information in the Status Bar

   sbStatus.Panels.Items[0].Text := ' Backup Manager © 2008-' + FormatDateTime('YYYY',Now()) + ' BlueCrane Software Development CC';
   sbStatus.Panels.Items[1].Text := ' Version 3.02';
   sbStatus.Panels.Items[2].Text := ' Waiting...';
   sbStatus.Panels.Items[5].Text := OSName;

//--- Set the state of the buttons on the main screen

   Set_Buttons(ord(BTN_INITIAL));

   ActionsLastExecute(Sender);

{$ifdef WINDOWS}
   TrayIcon.Visible := True;
{$endif}

end;

//------------------------------------------------------------------------------
// Executed before the form is closed
//------------------------------------------------------------------------------
procedure TFLPMSBackup.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
   idx1     : integer;
   CfgStr   : string;
   ThisNode : TTreeNode;

begin

   if (DoSave = true) then begin

      if (MessageDlg('Backup Manager','WARNING: There are unsaved changes!' + #10 + #10 + 'Click [Yes] to Terminate anyway or [No] to return.', mtWarning, mbYesNo, 0) =  mrNo) then begin
        CloseAction := caNone;
        Exit
     end;

   end;

   if (MessageDlg('Backup Manager','WARNING: No backups will be taken while Backup Manager is inactive!' + #10 + #10 + 'Click [Yes] to Terminate anyway or [No] to return.', mtWarning, mbYesNo, 0) =  mrNo) then begin

      CloseAction := caNone;
      Exit

   end;

//--- Stop the Scheduler and the Time display timer

   timTimer1.Enabled := False;
   timTimer2.Enabled := False;

//--- Transfer the in-memory information to the 'Registry' and build the
//--- contents of the Config File

   CfgStr := '';

   ThisNode := tvInstructions.TopItem;
   ThisNode := ThisNode.GetFirstChild;

   while ThisNode <> nil do begin

      for idx1 := 0 to NumInstr -1 do begin

         if Instr_List[idx1].Instruction = ThisNode.Text then begin

            if cbxDelete.Checked = True then begin

               if Instr_List[idx1].Active = True then
                  CfgStr := CfgStr + ThisNode.Text + '|';

            end else begin

               CfgStr := CfgStr + ThisNode.Text + '|';

            end;


//--- Don't write to the 'Registry' for inactive instructions

            if Instr_List[idx1].Active = False then
               break;

            IniFile := TINIFile.Create(LocalPath + Instr_List[idx1].Ini_File);

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
            IniFile.WriteString('Parameters','BackupDBPass',Encode(Instr_List[idx1].Instr_Rec.BackupDBPass));
            IniFile.WriteString('Parameters','BackupDBPrefix',Instr_List[idx1].Instr_Rec.BackupDBPrefix);
            IniFile.WriteString('Parameters','BackupDBSuffix',Instr_List[idx1].Instr_Rec.BackupDBSuffix);
            IniFile.WriteString('Parameters','BackupDBUser',Instr_List[idx1].Instr_Rec.BackupDBUser);
            IniFile.WriteString('Parameters','BackupHostName',Instr_List[idx1].Instr_Rec.BackupHostName);
            IniFile.WriteString('Parameters','BackupLocation',Instr_List[idx1].Instr_Rec.BackupLocation);
            IniFile.WriteString('Parameters','BackupSMSNumber',Instr_List[idx1].Instr_Rec.BackupSMSNumber);
            IniFile.WriteString('Parameters','BackupSMSPass',Encode(Instr_List[idx1].Instr_Rec.BackupSMSPass));
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

   IniFile := TINIFile.Create(CfgFile);

   IniFile.WriteString('Config','String',CfgStr);

   IniFile.WriteBool('Parameters','Sort',cbxDoSort.Checked);
   IniFile.WriteBool('Parameters','Discard',cbxDelete.Checked);
   IniFile.WriteBool('Parameters','Compress',cbxCompress.Checked);

   IniFile.WriteInteger('Template','BackupBlock',BackupTemplate.Instr_Rec.BackupBlock);
   IniFile.WriteInteger('Template','BackupSMSProvider',BackupTemplate.Instr_Rec.BackupSMSProvider);
   IniFile.WriteInteger('Template','BackupType',BackupTemplate.Instr_Rec.BackupType);
   IniFile.WriteInteger('Template','BackupT01',BackupTemplate.Instr_Rec.BackupT01);
   IniFile.WriteInteger('Template','BackupT02',BackupTemplate.Instr_Rec.BackupT02);
   IniFile.WriteInteger('Template','BackupT03',BackupTemplate.Instr_Rec.BackupT03);
   IniFile.WriteBool('Template','BackupSMSAlways',BackupTemplate.Instr_Rec.BackupSMSAlways);
   IniFile.WriteBool('Template','BackupSMSFailure',BackupTemplate.Instr_Rec.BackupSMSFailure);
   IniFile.WriteBool('Template','BackupSMSNever',BackupTemplate.Instr_Rec.BackupSMSNever);
   IniFile.WriteBool('Template','BackupSMSSuccess',BackupTemplate.Instr_Rec.BackupSMSSuccess);
   IniFile.WriteString('Template','BackupDBPass',Encode(BackupTemplate.Instr_Rec.BackupDBPass));
   IniFile.WriteString('Template','BackupDBPrefix',BackupTemplate.Instr_Rec.BackupDBPrefix);
   IniFile.WriteString('Template','BackupDBSuffix',BackupTemplate.Instr_Rec.BackupDBSuffix);
   IniFile.WriteString('Template','BackupDBUser',BackupTemplate.Instr_Rec.BackupDBUser);
   IniFile.WriteString('Template','BackupHostName',BackupTemplate.Instr_Rec.BackupHostName);
   IniFile.WriteString('Template','BackupLocation',BackupTemplate.Instr_Rec.BackupLocation);
   IniFile.WriteString('Template','BackupSMSNumber',BackupTemplate.Instr_Rec.BackupSMSNumber);
   IniFile.WriteString('Template','BackupSMSPass',Encode(BackupTemplate.Instr_Rec.BackupSMSPass));
   IniFile.WriteString('Template','BackupSMSUser',BackupTemplate.Instr_Rec.BackupSMSUser);
   IniFile.WriteString('Template','BackupTemplate',BackupTemplate.Instr_Rec.BackupTemplate);
   IniFile.WriteString('Template','BackupViewer',BackupTemplate.Instr_Rec.BackupViewer);

   IniFile.Destroy;

//--- Close the data base

   sqlQry1.Close;

   ActiveName := 'Backup';
   DispLogMsg('*** Stopping Backup Manager');
   SaveLog(BackupLogFile);

{$ifdef WINDOWS}
   TrayIcon.Visible := False;
{$endif}

end;

//------------------------------------------------------------------------------
// Action to take when the User wants to close the Application
//------------------------------------------------------------------------------
procedure TFLPMSBackup.FileCloseExecute(Sender: TObject);
begin
   Close;
end;

//------------------------------------------------------------------------------
// Action to take when the User wants to create a new Instruction
//------------------------------------------------------------------------------
procedure TFLPMSBackup.FileNewExecute(Sender: TObject);
Var
   ThisInstr : integer;
   ThisNode  : TTreeNode;

begin

//--- Insert a new entry in the main TreeView

   ThisNode := tvInstructions.Items.GetFirstNode;
   ThisNode := tvInstructions.Items.AddChild(ThisNode,'New Instruction');

   ThisNode.ImageIndex    := 1;
   ThisNode.SelectedIndex := 3;

   ThisNode.Selected := True;

//--- Insert a new entry in the Small Treeview

   ThisNode := tvSmall.Items.GetLastNode;
   ThisNode := tvSmall.Items.Add(ThisNode,'New Instruction');

   ThisNode.ImageIndex    := 1;
   ThisNode.SelectedIndex := 3;

   ThisNode.Selected := True;

   lvLogSel.Clear;

//--- Prepare the new entry with the defaults contained in the Template

   ThisInstr := Length(Instr_List);
   SetLength(Instr_List,ThisInstr + 1);
   inc(NumInstr);

   Instr_List[ThisInstr].Instruction := 'New Instruction';
   Instr_List[ThisInstr].Ini_File    := 'Backup_Manager_New Instruction.ini';
   Instr_List[ThisInstr].NextDate    := '2099/12/31';
   Instr_List[ThisInstr].NextTime    := '23:59:59';

   Instr_List[ThisInstr].Instr_Rec.BackupBlock           := BackupTemplate.Instr_Rec.BackupBlock;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSProvider     := BackupTemplate.Instr_Rec.BackupSMSProvider;
   Instr_List[ThisInstr].Instr_Rec.BackupType            := BackupTemplate.Instr_Rec.BackupType;
   Instr_List[ThisInstr].Instr_Rec.BackupT01             := BackupTemplate.Instr_Rec.BackupT01;
   Instr_List[ThisInstr].Instr_Rec.BackupT02             := BackupTemplate.Instr_Rec.BackupT02;
   Instr_List[ThisInstr].Instr_Rec.BackupT03             := BackupTemplate.Instr_Rec.BackupT03;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSAlways       := BackupTemplate.Instr_Rec.BackupSMSAlways;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSFailure      := BackupTemplate.Instr_Rec.BackupSMSFailure;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSNever        := BackupTemplate.Instr_Rec.BackupSMSNever;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSSuccess      := BackupTemplate.Instr_Rec.BackupSMSSuccess;
   Instr_List[ThisInstr].Instr_Rec.BackupDBPass          := BackupTemplate.Instr_Rec.BackupDBPass;
   Instr_List[ThisInstr].Instr_Rec.BackupDBPrefix        := BackupTemplate.Instr_Rec.BackupDBPrefix;
   Instr_List[ThisInstr].Instr_Rec.BackupDBSuffix        := BackupTemplate.Instr_Rec.BackupDBSuffix;
   Instr_List[ThisInstr].Instr_Rec.BackupDBUser          := BackupTemplate.Instr_Rec.BackupDBUser;
   Instr_List[ThisInstr].Instr_Rec.BackupHostName        := BackupTemplate.Instr_Rec.BackupHostName;
   Instr_List[ThisInstr].Instr_Rec.BackupLocation        := BackupTemplate.Instr_Rec.BackupLocation;
   Instr_List[ThisInstr].Instr_Rec.BackupMsg             := BackupTemplate.Instr_Rec.BackupMsg;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSNumber       := BackupTemplate.Instr_Rec.BackupSMSNumber;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSPass         := BackupTemplate.Instr_Rec.BackupSMSPass;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSUser         := BackupTemplate.Instr_Rec.BackupSMSUser;
   Instr_List[ThisInstr].Instr_Rec.BackupTemplate        := BackupTemplate.Instr_Rec.BackupTemplate;
   Instr_List[ThisInstr].Instr_Rec.BackupViewer          := BackupTemplate.Instr_Rec.BackupViewer;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSProviderName := BackupTemplate.Instr_Rec.BackupSMSProviderName;

   Instr_List[ThisInstr].Active := False;
   Instr_List[ThisInstr].Status := ord(STAT_INACTIVE);

   tvSmallClick(Sender);
   tvInstructionsClick(Sender);

end;

//------------------------------------------------------------------------------
// Action to take when the User wants to Delete an Instruction
//------------------------------------------------------------------------------
procedure TFLPMSBackup.FileDeleteExecute(Sender: TObject);
var
   ThisInstr : integer;

begin

//--- Stop the Scheduler

   timTimer1.Enabled := False;

   ThisInstr := GetInstruction();

   if (MessageDlg('Backup Manager','CAUTION: Your requested Backup Instruction ''' + Instr_List[ThisInstr].Instruction + ''' to be deleted. This action is irreversible and cannot be undone!' + #10 + #10 + 'Click [Yes] to proceed with the Delete request; or ' + #10 + 'Click [No] to return and keep the Backup Instruction.', mtWarning, mbYesNo, 0) =  mrNo) then begin

      timTimer1.Enabled := True;
      Exit

   end;

   Instr_List[ThisInstr].Active := False;
   Instr_List[ThisInstr].Status := ord(STAT_INACTIVE);

   UpdateRec.InstrName := tvInstructions.Selected.Text;
   UpdateRec.Request   := ord(TV_DELETE);

   AccessTreeView(UpdateRec);

   tvSmallClick(Sender);
   tvInstructionsClick(Sender);

   timTimer1.Enabled := True;

end;

//------------------------------------------------------------------------------
// Action to take when the User Cancels an Update
//------------------------------------------------------------------------------
procedure TFLPMSBackup.EditCancelExecute(Sender: TObject);
begin

   if DoSave = true then begin

      if (MessageDlg('Backup Manager','WARNING: There are unsaved changes - click [Yes] to ignore the changes or [No] to return', mtWarning, mbYesNo, 0) =  mrNo) then begin

         Exit

      end;

   end;

   CanUpdate := false;

//--- Recover from the 'Registry' as it will only be updated after the Update
//--- has been committed

   RegString := LocalPath + 'Backup_Manager_' + SaveName + '.ini';

//--- Extract the information contained in the 'registry'

   IniFile := TINIFile.Create(RegString);

   Instr_List[SaveInstr].Instr_Rec.BackupBlock       := IniFile.ReadInteger('Parameters','BackupBlock',5000);
   Instr_List[SaveInstr].Instr_Rec.BackupSMSProvider := IniFile.ReadInteger('Parameters','BackupSMSProvider',0);
   Instr_List[SaveInstr].Instr_Rec.BackupType        := IniFile.ReadInteger('Parameters','BackupType',0);
   Instr_List[SaveInstr].Instr_Rec.BackupT01         := IniFile.ReadInteger('Parameters','BackupT01',0);
   Instr_List[SaveInstr].Instr_Rec.BackupT02         := IniFile.ReadInteger('Parameters','BackupT02',0);
   Instr_List[SaveInstr].Instr_Rec.BackupT03         := IniFile.ReadInteger('Parameters','BackupT03',0);
   Instr_List[SaveInstr].Instr_Rec.BackupSMSAlways   := IniFile.ReadBool('Parameters','BackupSMSAlways',False);
   Instr_List[SaveInstr].Instr_Rec.BackupSMSFailure  := IniFile.ReadBool('Parameters','BackupSMSFailure',False);
   Instr_List[SaveInstr].Instr_Rec.BackupSMSNever    := IniFile.ReadBool('Parameters','BackupSMSNever',True);
   Instr_List[SaveInstr].Instr_Rec.BackupSMSSuccess  := IniFile.ReadBool('Parameters','BackupSMSSuccess',False);
   Instr_List[SaveInstr].Instr_Rec.BackupDBPass      := Decode(IniFile.ReadString('Parameters','BackupDBPass',''));
   Instr_List[SaveInstr].Instr_Rec.BackupDBPrefix    := IniFile.ReadString('Parameters','BackupDBPrefix','');
   Instr_List[SaveInstr].Instr_Rec.BackupDBSuffix    := IniFile.ReadString('Parameters','BackupDBSuffix','');
   Instr_List[SaveInstr].Instr_Rec.BackupDBUser      := IniFile.ReadString('Parameters','BackupDBUser','');
   Instr_List[SaveInstr].Instr_Rec.BackupHostName    := IniFile.ReadString('Parameters','BackupHostName','www.bluecrane.cc');
   Instr_List[SaveInstr].Instr_Rec.BackupLocation    := IniFile.ReadString('Parameters','BackupLocation','~');
   Instr_List[SaveInstr].Instr_Rec.BackupSMSNumber   := IniFile.ReadString('Parameters','BackupSMSNumber','');
   Instr_List[SaveInstr].Instr_Rec.BackupSMSPass     := Decode(IniFile.ReadString('Parameters','BackupSMSPass',''));
   Instr_List[SaveInstr].Instr_Rec.BackupSMSUser     := IniFile.ReadString('Parameters','BackupSMSUser','');
   Instr_List[SaveInstr].Instr_Rec.BackupTemplate    := IniFile.ReadString('Parameters','BackupTemplate','&Date@&Time - &BackupType Backup (&CpyName on &HostName)');
   Instr_List[SaveInstr].Instr_Rec.BackupViewer      := IniFile.ReadString('Parameters','BackupViewer','');

   IniFile.Destroy;

//--- If an Update has been triggered then only the Page on which the Update was
//--- initiated will be visible

   if tsInstruction.Visible = True then begin

      cbxType.ItemIndex    := Instr_List[SaveInstr].Instr_Rec.BackupType;
      cbxTypeChange(Sender);

      cbxT01.ItemIndex     := Instr_List[SaveInstr].Instr_Rec.BackupT01;
      cbxT02.ItemIndex     := Instr_List[SaveInstr].Instr_Rec.BackupT02;
      cbxT03.ItemIndex     := Instr_List[SaveInstr].Instr_Rec.BackupT03;

      edtSMSNumber.Text    := Instr_List[SaveInstr].Instr_Rec.BackupSMSNumber;
      rbSMSSuccess.Checked := Instr_List[SaveInstr].Instr_Rec.BackupSMSSuccess;
      rbSMSFailure.Checked := Instr_List[SaveInstr].Instr_Rec.BackupSMSFailure;
      rbSMSNever.Checked   := Instr_List[SaveInstr].Instr_Rec.BackupSMSNever;
      rbSMSAlways.Checked  := Instr_List[SaveInstr].Instr_Rec.BackupSMSAlways;

//--- If no SMS choice is selected then set rbSMSNever by default

      if ((rbSMSSuccess.Checked = False) and (rbSMSFailure.Checked = False) and (rbSMSAlways.Checked = False) and (rbSMSNever.Checked = False)) then
         rbSMSNever.Checked := True;

   end else begin

//--- Fix the TreeView name as this may have changed

      UpdateRec.InstrName    := Instr_List[SaveInstr].Instruction;
      UpdateRec.InstrNewName := SaveName;
      UpdateRec.Request      := ord(TV_REPLACE);

      AccessTreeView(UpdateRec);

      Instr_List[saveInstr].Instruction := SaveName;

//--- Fix the .ini file name as this may have changed

      Instr_List[SaveInstr].Ini_File := 'Backup_Manager_' + SaveName + '.ini';

//--- Restore the rest of the fields

      edtInstrNameC.Text       := Instr_List[SaveInstr].Instruction;
      speBlockSizeC.Value      := Instr_List[SaveInstr].Instr_Rec.BackupBlock;
      cbSMSProviderC.ItemIndex := Instr_List[SaveInstr].Instr_Rec.BackupSMSProvider;
      edtDBPassC.Text          := Instr_List[SaveInstr].Instr_Rec.BackupDBPass;
      edtDBPrefixC.Text        := Instr_List[SaveInstr].Instr_Rec.BackupDBPrefix;

      if Instr_List[SaveInstr].Instr_Rec.BackupDBSuffix = '_LPMS' then
         cbxDBSuffixC.Checked  := True
      else
         cbxDBSuffixC.Checked  := False;

      edtDBUserC.Text          := Instr_List[SaveInstr].Instr_Rec.BackupDBUser;
      edtHostNameC.Text        := Instr_List[SaveInstr].Instr_Rec.BackupHostName;
      edtLocationC.Text        := Instr_List[SaveInstr].Instr_Rec.BackupLocation;
      edtSMSPassC.Text         := Instr_List[SaveInstr].Instr_Rec.BackupSMSPass;
      edtSMSUserC.Text         := Instr_List[SaveInstr].Instr_Rec.BackupSMSUser;
      edtTemplateC.Text        := Instr_List[SaveInstr].Instr_Rec.BackupTemplate;
      edtViewerC.Text          := Instr_List[SaveInstr].Instr_Rec.BackupViewer;
      cbSMSProviderC.Text      := Instr_List[SaveInstr].Instr_Rec.BackupSMSProviderName;

   end;

   Set_Buttons(ord(BTN_INSTRUCTION));

//--- Make both pages visible again

   tsInstruction.TabVisible   := True;
   tsConfiguration.TabVisible := True;

//--- Reset the switches and information fields

   DoSave := false;
   CanUpdate := true;
   sbStatus.Panels.Items[2].Text := ' Waiting...';

//--- Start the Scheduler and the timer again

   timTimer1.Enabled := True;
   timTimer2.Enabled := True;

end;

//------------------------------------------------------------------------------
// Action to take when the User wants to do an Update
//------------------------------------------------------------------------------
procedure TFLPMSBackup.EditUpdateExecute(Sender: TObject);
var
   ThisInstr, UpdateReq : integer;
   UpdateTV             : boolean = False;

begin

//--- Set instruction index to the index that was saaved when the chnage was
//--- initiated

   ThisInstr := SaveInstr;

//--- If a possible Update has been triggered then only the Page on which the
//--- Update was initiated will be visible

   if tsInstruction.TabVisible = True then begin;

//--- Check whether the required fields are valid

      if Trim(edtSMSNumber.Text) = '' then begin

         if ((rbSMSSuccess.Checked = False) and (rbSMSFailure.Checked = False) and (rbSMSAlways.Checked = False) and (rbSMSNever.Checked = False)) then
            rbSMSNever.Checked := True;

      end;

   end else begin

//--- Check whether the required fields are valid

      if Trim(edtInstrNameC.Text) = '' then begin

         Application.MessageBox('''Instruction Name'' is a mandatory field - please provide','Backup Manager', MB_ICONHAND + MB_OK);
         edtInstrNameC.Text := '';
         edtInstrNameC.SetFocus;
         Exit;

      end;

      if Trim(edtDBPrefixC.Text) = '' then begin

         Application.MessageBox('''Database'' is a mandatory field - please provide','Backup Manager', MB_ICONHAND + MB_OK);
         edtDBPrefixC.Text := '';
         edtDBPrefixC.SetFocus;
         Exit;

      end;

      if Trim(edtHostNameC.Text) = '' then begin

         Application.MessageBox('''Host Name'' is a mandatory field - please provide','Backup Manager', MB_ICONHAND + MB_OK);
         edtHostNameC.Text := '';
         edtHostNameC.SetFocus;
         Exit;

      end;

      if Trim(edtDBUserC.Text) = '' then begin

         Application.MessageBox('''User ID'' is a mandatory field - please provide','Backup Manager', MB_ICONHAND + MB_OK);
         edtDBUserC.Text := '';
         edtDBUserC.SetFocus;
         Exit;

      end;

      if Trim(edtDBPassC.Text) = '' then begin

         Application.MessageBox('''Password'' is a mandatory field - please provide','Backup Manager', MB_ICONHAND + MB_OK);
         edtDBPassC.Text := '';
         edtDBPassC.SetFocus;
         Exit;

      end;

      if Trim(edtTemplateC.Text) = '' then begin

         Application.MessageBox('''Template'' is a mandatory field - please provide','Backup Manager', MB_ICONHAND + MB_OK);
         edtTemplateC.Text := '';
         edtTemplateC.SetFocus;
         Exit;

      end;

      if Trim(edtLocationC.Text) = '' then begin

         Application.MessageBox('''Backup Location'' is a mandatory field - please provide','Backup Manager', MB_ICONHAND + MB_OK);
         edtLocationC.Text := '';
         edtLocationC.SetFocus;
         Exit;

      end;

      if cbSMSProviderC.ItemIndex > 0 then begin

         if Trim(edtSMSUserC.Text) = '' then begin

            Application.MessageBox('''User ID'' is a mandatory field - please provide','Backup Manager', MB_ICONHAND + MB_OK);
            edtSMSUserC.Text := '';
            edtSMSUserC.SetFocus;
            Exit;

         end;

         if Trim(edtSMSPassC.Text) = '' then begin

            Application.MessageBox('''Password'' is a mandatory field - please provide','Backup Manager', MB_ICONHAND + MB_OK);
            edtSMSPassC.Text := '';
            edtSMSPassC.SetFocus;
            Exit;

         end;

      end else begin

         edtSMSUserC.Text := '';
         edtSMSPassC.Text := '';

      end;

//--- Check whether the Instruction Name has changed and if so then check if
//--- the new name is a duplicate

      if edtInstrNameC.Text <> Instr_List[ThisInstr].Instruction then begin

         UpdateRec.InstrName := edtInstrNameC.Text;
         UpdateRec.Request   := ord(TV_DUPLICATE);

         if AccessTreeView(UpdateRec) = False then begin

            Application.MessageBox(pchar('Updating ''' + Instr_List[ThisInstr].Instruction + ''' to ''' + edtInstrNameC.Text + ''' will create a duplicate - no duplicates allowed.' + #10 + #10 + 'Please choose another Instruction Name'),'Backup Manager', MB_ICONHAND + MB_OK);
            edtInstrNameC.SetFocus;
            Exit;

         end else begin

            UpdateTV  := True;
            UpdateReq := ord(TV_REPLACE);

         end;

      end;

   end;

//--- Update the Instruction's in-memory record

   Instr_List[ThisInstr].Instruction                     := edtInstrNameC.Text;
   Instr_List[ThisInstr].Ini_File                        := 'Backup_Manager_' + edtInstrNameC.Text + '.ini';
   Instr_List[ThisInstr].Instr_Rec.BackupBlock           := speBlockSizeC.Value;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSProvider     := cbSMSProviderC.ItemIndex;
   Instr_List[ThisInstr].Instr_Rec.BackupType            := cbxType.ItemIndex;
   Instr_List[ThisInstr].Instr_Rec.BackupT01             := cbxT01.ItemIndex;
   Instr_List[ThisInstr].Instr_Rec.BackupT02             := cbxT02.ItemIndex;
   Instr_List[ThisInstr].Instr_Rec.BackupT03             := cbxT03.ItemIndex;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSAlways       := rbSMSAlways.Checked;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSFailure      := rbSMSFailure.Checked;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSNever        := rbSMSNever.Checked;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSSuccess      := rbSMSSuccess.Checked;
   Instr_List[ThisInstr].Instr_Rec.BackupDBPass          := edtDBPassC.Text;
   Instr_List[ThisInstr].Instr_Rec.BackupDBPrefix        := edtDBPrefixC.Text;

   if cbxDBSuffixC.Checked = True then
      Instr_List[ThisInstr].Instr_Rec.BackupDBSuffix     := '_LPMS'
   else
      Instr_List[ThisInstr].Instr_Rec.BackupDBSuffix     := '';

   Instr_List[ThisInstr].Instr_Rec.BackupDBUser          := edtDBUserC.Text;
   Instr_List[ThisInstr].Instr_Rec.BackupHostName        := edtHostNameC.Text;
   Instr_List[ThisInstr].Instr_Rec.BackupLocation        := edtLocationC.Text;
   Instr_List[ThisInstr].Instr_Rec.BackupMsg             := '';
   Instr_List[ThisInstr].Instr_Rec.BackupSMSNumber       := edtSMSNumber.Text;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSPass         := edtSMSPassC.Text;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSUser         := edtSMSUserC.Text;
   Instr_List[ThisInstr].Instr_Rec.BackupTemplate        := edtTemplateC.Text;
   Instr_List[ThisInstr].Instr_Rec.BackupViewer          := edtViewerC.Text;
   Instr_List[ThisInstr].Instr_Rec.BackupSMSProviderName := cbSMSProviderC.Text;

//--- Update the TreeView if required

   if UpdateTV = True then begin

      case UpdateReq of

         ord(TV_REPLACE): begin

            UpdateRec.InstrName    := SaveName;
            UpdateRec.InstrNewName := edtInstrNameC.Text;
            UpdateRec.Request      := ord(TV_REPLACE);

            AccessTreeView(UpdateRec);

         end;

      end;

   end;

   ActiveName := Instr_List[ThisInstr].Instruction;
   DispLogMsg('Parameters changed - Resetting Scheduler for ' + Instr_List[ThisInstr].Instruction);
   GetNextSlot(ThisInstr);
   lblL04.Caption := Instr_List[ThisInstr].Instr_Rec.BackupMsg;

//--- Update the information displayed in the Small TreeView

   if tvSmall.Selected.Index <> -1 then
      tvSmallClick(Sender);

//--- Make both Tabs visible again

   CanUpdate := False;

   tsInstruction.TabVisible   := True;
   tsConfiguration.TabVisible := True;

//--- Update the SMS indciators on tsInstruction to ensure the lastest settings
//--- are always displayed

   if Instr_List[ThisInstr].Instr_Rec.BackupSMSProvider = 0 then begin

      edtSMSNumber.Enabled := False;
      rbSMSSuccess.Enabled := False;
      rbSMSFailure.Enabled := False;
      rbSMSNever.Enabled   := False;
      rbSMSAlways.Enabled  := False;

      edtSMSNumber.Text    := '';
      rbSMSSuccess.Checked := False;
      rbSMSFailure.Checked := False;
      rbSMSNever.Checked   := False;
      rbSMSAlways.Checked  := False;

   end else begin

      edtSMSNumber.Enabled := True;
      rbSMSSuccess.Enabled := True;
      rbSMSFailure.Enabled := True;
      rbSMSNever.Enabled   := True;
      rbSMSAlways.Enabled  := True;

      edtSMSNumber.Text    := Instr_List[ThisInstr].Instr_Rec.BackupSMSNumber;
      rbSMSSuccess.Checked := Instr_List[ThisInstr].Instr_Rec.BackupSMSSuccess;
      rbSMSFailure.Checked := Instr_List[ThisInstr].Instr_Rec.BackupSMSFailure;
      rbSMSNever.Checked   := Instr_List[ThisInstr].Instr_Rec.BackupSMSNever;
      rbSMSAlways.Checked  := Instr_List[ThisInstr].Instr_Rec.BackupSMSAlways;

   end;

//--- From here on changes are registered again

   CanUpdate := True;

//--- Update the 'Registry'. We do this last so that we can recover from the
//--- 'Registry' in case something goes wrong

   IniFile := TINIFile.Create(LocalPath + Instr_List[ThisInstr].Ini_File);

   IniFile.WriteInteger('Parameters','BackupBlock',Instr_List[ThisInstr].Instr_Rec.BackupBlock);
   IniFile.WriteInteger('Parameters','BackupSMSProvider',Instr_List[ThisInstr].Instr_Rec.BackupSMSProvider);
   IniFile.WriteInteger('Parameters','BackupType',Instr_List[ThisInstr].Instr_Rec.BackupType);
   IniFile.WriteInteger('Parameters','BackupT01',Instr_List[ThisInstr].Instr_Rec.BackupT01);
   IniFile.WriteInteger('Parameters','BackupT02',Instr_List[ThisInstr].Instr_Rec.BackupT02);
   IniFile.WriteInteger('Parameters','BackupT03',Instr_List[ThisInstr].Instr_Rec.BackupT03);
   IniFile.WriteBool('Parameters','BackupSMSAlways',Instr_List[ThisInstr].Instr_Rec.BackupSMSAlways);
   IniFile.WriteBool('Parameters','BackupSMSFailure',Instr_List[ThisInstr].Instr_Rec.BackupSMSFailure);
   IniFile.WriteBool('Parameters','BackupSMSNever',Instr_List[ThisInstr].Instr_Rec.BackupSMSNever);
   IniFile.WriteBool('Parameters','BackupSMSSuccess',Instr_List[ThisInstr].Instr_Rec.BackupSMSSuccess);
   IniFile.WriteString('Parameters','BackupDBPass',Encode(Instr_List[ThisInstr].Instr_Rec.BackupDBPass));
   IniFile.WriteString('Parameters','BackupDBPrefix',Instr_List[ThisInstr].Instr_Rec.BackupDBPrefix);
   IniFile.WriteString('Parameters','BackupDBSuffix',Instr_List[ThisInstr].Instr_Rec.BackupDBSuffix);
   IniFile.WriteString('Parameters','BackupDBUser',Instr_List[ThisInstr].Instr_Rec.BackupDBUser);
   IniFile.WriteString('Parameters','BackupHostName',Instr_List[ThisInstr].Instr_Rec.BackupHostName);
   IniFile.WriteString('Parameters','BackupLocation',Instr_List[ThisInstr].Instr_Rec.BackupLocation);
   IniFile.WriteString('Parameters','BackupSMSNumber',Instr_List[ThisInstr].Instr_Rec.BackupSMSNumber);
   IniFile.WriteString('Parameters','BackupSMSPass',Encode(Instr_List[ThisInstr].Instr_Rec.BackupSMSPass));
   IniFile.WriteString('Parameters','BackupSMSUser',Instr_List[ThisInstr].Instr_Rec.BackupSMSUser);
   IniFile.WriteString('Parameters','BackupTemplate',Instr_List[ThisInstr].Instr_Rec.BackupTemplate);
   IniFile.WriteString('Parameters','BackupViewer',Instr_List[ThisInstr].Instr_Rec.BackupViewer);

   IniFile.Destroy;

   Set_Buttons(ord(BTN_INSTRUCTION));

   DoSave := false;

   sbStatus.Panels.Items[2].Text := ' Waiting...';
   sbStatus.Panels.Items[3].Text := ' ' + Instr_List[ThisInstr].Instr_Rec.BackupHostName + '[' + Instr_List[ThisInstr].Instr_Rec.BackupDBPrefix + Instr_List[ThisInstr].Instr_Rec.BackupDBSuffix + ']';
   sbStatus.Panels.Items[4].Text := FloatToStrF(Instr_List[ThisInstr].Instr_Rec.BackupBlock, ffNumber, 2, 0) + ' ';

//--- Resume the Scheduler and the Timer that was stopped when an Update was
//--- initiated

   timTimer1.Enabled := True;
   timTimer2.Enabled := True;

end;

//------------------------------------------------------------------------------
// Action to start a search in a Log display
//------------------------------------------------------------------------------
procedure TFLPMSBackup.SearchFindExecute(Sender: TObject);
begin

   with dlgFind do begin

//--- We can seaech in either the overall log or the instruction log depending
//--- on what is visible

      if tvInstructions.Selected.Level = 0 then
         VisibleLog := lvLogAll
      else
         VisibleLog := lvLogSel;

      AtFirst := False;
      AtLast  := False;

//--- If frEntireScope is set then Search begins at Line 1 in the log otherwise
//--- at the selected line

     if frEntireScope in Options then begin

        if frDown in Options then
           LastPos := 0
        else
           LastPos := VisibleLog.Items.Count - 1;

     end else begin

        LastPos := VisibleLog.ItemIndex;

     end;

     Execute;

   end;

end;

//------------------------------------------------------------------------------
// User clciked on the Find button in the Find Dialog
//------------------------------------------------------------------------------
procedure TFLPMSBackup.dlgFindFind(Sender: TObject);
var
   idx1 : integer;

begin

   if AtLast = True then begin

      if (Application.MessageBox('End of log entries reached - you can:' + #10 + #10 + 'Click [Yes] and then on [Find] to restart at the beginning of the log; or' + #10 + #10 + 'Click [No] to end the search.','Backup Manager',(MB_YESNO + MB_ICONINFORMATION)) = IDYES) then
         LastPos := 0
      else
         dlgFind.CloseDialog;

      AtLast := False;
      Exit;

   end else if AtFirst = True then begin

     if (Application.MessageBox('Start of log entries reached - you can:' + #10 + #10 + 'Click [Yes] and then on [Find] to restart at the end of the log; or' + #10 + #10 + 'Click [No] to end the search.','Backup Manager',(MB_YESNO + MB_ICONINFORMATION)) = IDYES) then
        LastPos := VisibleLog.Items.Count - 1
     else
        dlgFind.CloseDialog;

       AtFirst := False;
       Exit;

   end;

   idx1 := FindTextString();

   if idx1 <> -1 then begin

      VisibleLog.ItemIndex := idx1;
      VisibleLog.Items.Item[idx1].Selected := True;
      VisibleLog.Items.Item[idx1].MakeVisible(False);

      VisibleLog.SetFocus;

   end else begin

     if frDown in dlgFind.Options then begin

        if (Application.MessageBox('End of log entries reached - you can:' + #10 + #10 + 'Click [Yes] and then on [Find] to restart at the beginning of the log; or' + #10 + #10 + 'Click [No] to end the search.','Backup Manager',(MB_YESNO + MB_ICONINFORMATION)) = IDYES) then
           LastPos := 0
        else
           dlgFind.CloseDialog;

     end else begin

        if (Application.MessageBox('Start of log entries reached - you can:' + #10 + #10 + 'Click [Yes] and then on [Find] to restart at the end of the log; or' + #10 + #10 + 'Click [No] to end the search.','Backup Manager',(MB_YESNO + MB_ICONINFORMATION)) = IDYES) then
           LastPos := VisibleLog.Items.Count - 1
        else
           dlgFind.CloseDialog;

     end;

   end;

end;

//------------------------------------------------------------------------------
// Procedure to drive the Search function
//------------------------------------------------------------------------------
function TFLPMSBackup.FindTextString() : integer;
var
   idx1      : integer;
   Found     : boolean;
   ListItem  : TlistItem;

begin

   Result := -1;

   if frDown in dlgFind.Options then begin

      for idx1 := LastPos to VisibleLog.Items.Count - 1 do begin

         ListItem := VisibleLog.Items.Item[idx1];
         Found    := False;

         if frMatchCase in dlgFind.Options then begin

            if AnsiContainsStr(ListItem.Caption,dlgFind.FindText) then
               Found := True;

            if AnsiContainsStr(ListItem.SubItems.Strings[0],dlgFind.FindText) then
               Found := True;

            if AnsiContainsStr(ListItem.SubItems.Strings[1],dlgFind.FindText) then
               Found := True;

            if AnsiContainsStr(ListItem.SubItems.Strings[2],dlgFind.FindText) then
               Found := True;

         end else begin

            if AnsiContainsText(ListItem.Caption,dlgFind.FindText) then
               Found := True;

            if AnsiContainsText(ListItem.SubItems.Strings[0],dlgFind.FindText) then
               Found := True;

            if AnsiContainsText(ListItem.SubItems.Strings[1],dlgFind.FindText) then
               Found := True;

            if AnsiContainsText(ListItem.SubItems.Strings[2],dlgFind.FindText) then
               Found := True;

         end;

         if Found = True then begin

            if idx1 < VisibleLog.Items.Count - 1 then begin

               LastPos := idx1 + 1;

            end else begin

               AtLast  := True;
               LastPos := idx1;

            end;

            Result := idx1;
            Exit

         end;

      end;

   end else begin

      for idx1 := LastPos downto 0 do begin

         ListItem := VisibleLog.Items.Item[idx1];
         Found    := false;

         if frMatchCase in dlgFind.Options then begin

            if AnsiContainsStr(ListItem.Caption,dlgFind.FindText) then
               Found := True;

            if AnsiContainsStr(ListItem.SubItems.Strings[0],dlgFind.FindText) then
               Found := True;

            if AnsiContainsStr(ListItem.SubItems.Strings[1],dlgFind.FindText) then
               Found := True;

            if AnsiContainsStr(ListItem.SubItems.Strings[2],dlgFind.FindText) then
               Found := True;


         end else begin

            if AnsiContainsText(ListItem.Caption,dlgFind.FindText) then
               Found := True;

            if AnsiContainsText(ListItem.SubItems.Strings[0],dlgFind.FindText) then
               Found := True;

            if AnsiContainsText(ListItem.SubItems.Strings[1],dlgFind.FindText) then
               Found := True;

            if AnsiContainsText(ListItem.SubItems.Strings[2],dlgFind.FindText) then
               Found := True;

         end;

         if Found = True then begin

            if idx1 > 0 then begin

               LastPos := idx1 - 1;

            end else begin

               AtFirst := True;
               LastPos := idx1;

            end;

            Result := idx1;
            Exit

         end;

      end;

   end;

end;

//------------------------------------------------------------------------------
// Action to take when the user clicks on Run Now
//------------------------------------------------------------------------------
procedure TFLPMSBackup.ActionsRunNowExecute(Sender: TObject);
var
   ListNum : integer;

begin

   ListNum := GetInstruction();

   Set_Hint(ord(HINT_RUNNING));

//--- Mark the instruction as RunNow

   Instr_List[ListNum].Status := ord(STAT_RUNNOW);

//--- Set the state of the buttons

//   Set_Buttons(ord(BTN_RUNNOW));

end;

//------------------------------------------------------------------------------
// Action to take when the user wants to go to the First Line in a log display
//------------------------------------------------------------------------------
procedure TFLPMSBackup.ActionsFirstExecute(Sender: TObject);
begin

   if tvInstructions.Selected.Level = 0 then begin

      lvLogAll.ItemIndex := 0;
      lvLogAll.Items.Item[lvLogAll.ItemIndex].Focused;

   end else begin

      lvLogSel.ItemIndex := 0;
      lvLogSel.Items.Item[lvLogSel.ItemIndex].Focused;

   end;

   Navigate();

end;

//------------------------------------------------------------------------------
// Action to take when the user wants to go to the Next Line in a log display
//------------------------------------------------------------------------------
procedure TFLPMSBackup.ActionsNextExecute(Sender: TObject);
begin

   if tvInstructions.Selected.Level = 0 then begin

      lvLogAll.ItemIndex := lvLogAll.ItemIndex + 1;
      lvLogAll.Items.Item[lvLogAll.ItemIndex].Focused;

   end else begin

      lvLogSel.ItemIndex := lvLogSel.ItemIndex + 1;
      lvLogSel.Items.Item[lvLogSel.ItemIndex].Focused;

   end;

   Navigate();

end;

//------------------------------------------------------------------------------
// Action to take when the user wants to go to the Previous Line in a log
// display
//------------------------------------------------------------------------------
procedure TFLPMSBackup.ActionsPreviousExecute(Sender: TObject);
begin

   if tvInstructions.Selected.Level = 0 then begin

      lvLogAll.ItemIndex := lvLogAll.ItemIndex - 1;
      lvLogAll.Items.Item[lvLogAll.ItemIndex].Focused;

   end else begin

      lvLogSel.ItemIndex := lvLogSel.ItemIndex - 1;
      lvLogSel.Items.Item[lvLogSel.ItemIndex].Focused;

   end;

   Navigate();

end;

//------------------------------------------------------------------------------
// Action to take when the user wants to go to the Last Line in a log display
//------------------------------------------------------------------------------
procedure TFLPMSBackup.ActionsLastExecute(Sender: TObject);
begin

   if tvInstructions.Selected.Level = 0 then begin

      lvLogAll.ItemIndex := lvLogAll.Items.Count - 1;
      lvLogAll.Items.Item[lvLogAll.ItemIndex].Focused;

   end else begin

      lvLogSel.ItemIndex := lvLogSel.Items.Count - 1;
      lvLogSel.Items.Item[lvLogSel.ItemIndex].Focused;

   end;

   Navigate();

end;

//------------------------------------------------------------------------------
// Action to take when the User selected 'Minimise'
//------------------------------------------------------------------------------
procedure TFLPMSBackup.ActionsMinimiseExecute(Sender: TObject);
begin

{$ifdef Windows}
   FLPMSBackup.Hide;
{$else}
   Application.Minimize;
{$endif}

end;

//------------------------------------------------------------------------------
// Action to take when the User selected 'Restore'
//------------------------------------------------------------------------------
procedure TFLPMSBackup.ActionsRestoreExecute(Sender: TObject);
begin
   FLPMSBackup.Show;
end;

//------------------------------------------------------------------------------
// Action to show the About dialog
//------------------------------------------------------------------------------
procedure TFLPMSBackup.HelpAboutExecute(Sender: TObject);
begin
   saAbout.ShowAbout;
end;

//------------------------------------------------------------------------------
// User clicked on a node in the Instructions Treeview
//------------------------------------------------------------------------------
procedure TFLPMSBackup.tvInstructionsClick(Sender: TObject);
begin

   if DoSave = True then
      Exit;

   if tvInstructions.Selected.Level = 0 then begin

      pnlP00b.Visible := True;
      pnlP00a.Visible := False;

      lblL04.Caption := '';
      lblL05.Caption := '';

      sbStatus.Panels.Items[3].Text := '';
      sbStatus.Panels.Items[4].Text := '';

      Set_Buttons(ord(BTN_INITIAL));

//      btnTemplate.SetFocus;

   end else begin

      pnlP00b.Visible := False;
      pnlP00a.Visible := True;

      ShowInstruction();

      if DoNotConnect = True then begin

         DoNotConnect := False;
         Set_Buttons(ord(BTN_UPDATE));

      end else begin

         Set_Buttons(ord(BTN_INSTRUCTION));

      end;

      ActionsLastExecute(Application);

      if tsInstruction.Visible = True then
         cbxType.SetFocus;

      if tsConfiguration.Visible = True then
         edtInstrNameC.SetFocus;

   end;
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
//--- length (e.g ''C:\A'') before we can add the backslash

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
   edtLocationC.RootDir := Instr_List[GetInstruction()].Instr_Rec.BackupLocation;
end;

//---------------------------------------------------------------------------
// Function to open the last successfull backup file
//---------------------------------------------------------------------------
procedure TFLPMSBackup.btnOpenLBClick(Sender: TObject);
var
   ThisNum : integer;

begin

   ThisNum := GetInstruction();

   ExecuteProcess(Instr_List[ThisNum].Instr_Rec.BackupViewer,PChar('''' + edtLastBackup.Text + ''''),[]);

end;

//------------------------------------------------------------------------------
// User changed Pages in the Instruction display
//------------------------------------------------------------------------------
procedure TFLPMSBackup.pcInstructionsChange(Sender: TObject);
begin

   Set_Buttons(ord(BTN_INSTRUCTION));

   if tsInstruction.Visible = True then
      cbxType.SetFocus;

   if tsConfiguration.Visible = True then
      edtInstrNameC.SetFocus;

end;

//------------------------------------------------------------------------------
// User clicked on the checkox to sort treeview items
//------------------------------------------------------------------------------
procedure TFLPMSBackup.cbxDoSortChange(Sender: TObject);
begin

   if cbxDoSort.Checked = True then begin

      tvInstructions.AlphaSort;
      tvSmall.AlphaSort;

   end;

end;

//------------------------------------------------------------------------------
// Block an attempt to do an in-line edit of the items in the main TreeView
//------------------------------------------------------------------------------
procedure TFLPMSBackup.tvInstructionsEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
begin
   AllowEdit := False;
end;

//------------------------------------------------------------------------------
// User clicked on an item in the small TreeView
//------------------------------------------------------------------------------
procedure TFLPMSBackup.tvSmallClick(Sender: TObject);
var
   idx1      : integer;
   ThisInstr : string;

begin

//--- The following is relevant when the User has deleted all instructions

   if tvSmall.Items.Count = 0 then begin

      edtConfigFile.Text := '';
      edtHostName.Text   := '';
      edtNextBackup.Text := '';
      edtLocation.Text   := '';

      Exit;

   end;

   ThisInstr := tvSmall.Selected.Text;

   for idx1 := 0 to NumInstr - 1 do begin

      if Instr_List[idx1].Instruction = ThisInstr then begin

         edtConfigFile.Text := LocalPath + Instr_List[idx1].Ini_File;
         edtHostName.Text   := Instr_List[idx1].Instr_Rec.BackupHostName;
         edtNextBackup.Text := 'Next Backup on ' + Instr_List[idx1].NextDate + ' at ' + Instr_List[idx1].NextTime;
         edtLocation.Text   := Instr_List[idx1].Instr_Rec.BackupLocation;

      end;

   end;

end;

//------------------------------------------------------------------------------
// Block an attempt to do an inline edit of the items in the small TreeView
//------------------------------------------------------------------------------
procedure TFLPMSBackup.tvSmallEditing(Sender: TObject; Node: TTreeNode; var AllowEdit: Boolean);
begin
   AllowEdit := False;
end;

//------------------------------------------------------------------------------
// A User customisable field changed
//------------------------------------------------------------------------------
procedure TFLPMSBackup.edtInstrNameCChange(Sender: TObject);
begin

   if CanUpdate = False then
      Exit;

//--- Get Key information about the Backup Instruction being changed

   SaveInstr := GetInstruction();
   SaveName  := Instr_List[SaveInstr].Instruction;

//--- Disable the Page that is not being updated to simplify the Update logic

   if tsInstruction.Visible = True then
      tsConfiguration.TabVisible := False
   else
      tsInstruction.TabVisible := False;

   DoSave := True;
   Set_Buttons(ord(BTN_UPDATE));
   sbStatus.Panels.Items[2].Text := ' Modified';

end;

//------------------------------------------------------------------------------
// User selected a backup frequency from the dropdown list
//------------------------------------------------------------------------------
procedure TFLPMSBackup.cbxTypeChange(Sender: TObject);
var
   idx : integer;

begin

//--- Temporarily stop the Scheduler and the Timer to update the 'Next Backup'
//--- message to avoid wrong information being passed to these

   timTimer1.Enabled := False;
   timTimer2.Enabled := False;

//--- Set the correct display fields based on the Backup Type

   lblL01.Visible := true;
   lblL02.Visible := true;
   lblL03.Visible := false;
   cbxT01.Visible := true;
   cbxT02.Visible := true;
   cbxT03.Visible := false;

   if (cbxType.Text = 'Hourly') then begin

      lblL01.Caption := 'Minute Mark:';
      cbxT01.Clear;

      for idx := 1 to 12 do
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

      for idx := 1 to 12 do
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

      for idx := 1 to 12 do
         cbxT03.Items.Add(MinArray[idx]);

      lblL02.Visible := true;
      cbxT02.Visible := true;
      lblL03.Visible := true;
      cbxT03.Visible := true;

   end;

   cbxT01.ItemIndex := 0;
   cbxT02.ItemIndex := 0;
   cbxT03.ItemIndex := 0;

//--- Necessary to call only when CanUpdate has not been flagged yet

   if CanUpdate = True then
      edtInstrNameCChange(Sender);

end;

//------------------------------------------------------------------------------
// User clicked on the button to set up a backup instruction template
//------------------------------------------------------------------------------
procedure TFLPMSBackup.btnTemplateClick(Sender: TObject);
begin

   FLPMSBackupTemplate := TFLPMSBackupTemplate.Create(Application);

//--- Populate the Fields in FLPMSBackupTemplate

   FLPMSBackupTemplate.speBlockSize.Value      := BackupTemplate.Instr_Rec.BackupBlock;
   FLPMSBackupTemplate.cbSMSProvider.ItemIndex := BackupTemplate.Instr_Rec.BackupSMSProvider;
   FLPMSBackupTemplate.cbXBackupType.ItemIndex := BackupTemplate.Instr_Rec.BackupType;
   FLPMSBackupTemplate.rbAlways.Checked        := BackupTemplate.Instr_Rec.BackupSMSAlways;
   FLPMSBackupTemplate.rbFailure.Checked       := BackupTemplate.Instr_Rec.BackupSMSFailure;
   FLPMSBackupTemplate.rbNever.Checked         := BackupTemplate.Instr_Rec.BackupSMSNever;
   FLPMSBackupTemplate.rbSuccess.Checked       := BackupTemplate.Instr_Rec.BackupSMSSuccess;
   FLPMSBackupTemplate.edtDBPass.Text          := BackupTemplate.Instr_Rec.BackupDBPass;
   FLPMSBackupTemplate.edtDBPrefix.Text        := BackupTemplate.Instr_Rec.BackupDBPrefix;

   if BackupTemplate.Instr_Rec.BackupDBSuffix = '_LPMS' then
      FLPMSBackupTemplate.cbDBSuffix.Checked   := True
   else
      FLPMSBackupTemplate.cbDBSuffix.Checked   := False;

   FLPMSBackupTemplate.edtDBUser.Text          := BackupTemplate.Instr_Rec.BackupDBUser;
   FLPMSBackupTemplate.edtHostName.Text        := BackupTemplate.Instr_Rec.BackupHostName;
   FLPMSBackupTemplate.deDirectory.Text        := BackupTemplate.Instr_Rec.BackupLocation;
   FLPMSBackupTemplate.edtSMSNumber.Text       := BackupTemplate.Instr_Rec.BackupSMSNumber;
   FLPMSBackupTemplate.edtSMSPass.Text         := BackupTemplate.Instr_Rec.BackupSMSPass;
   FLPMSBackupTemplate.edtSMSUser.Text         := BackupTemplate.Instr_Rec.BackupSMSUser;
   FLPMSBackupTemplate.edtTemplate.Text        := BackupTemplate.Instr_Rec.BackupTemplate;
   FLPMSBackupTemplate.edtViewer.Text          := BackupTemplate.Instr_Rec.BackupViewer;

   FLPMSBackup.Hide;
   FLPMSBackupTemplate.ShowModal;
   FLPMSBackupTemplate.Destroy;
   FLPMSBackup.Show;

end;

//------------------------------------------------------------------------------
// User clicked on the button to select the default viewer
//------------------------------------------------------------------------------
procedure TFLPMSBackup.btnViewerClick(Sender: TObject);
begin

   if Trim(edtViewerC.Text) <> '' then
      dlgOpen.InitialDir := ExtractFilePath(edtViewerC.Text);

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
      edtViewerC.Text := dlgOpen.FileName;

end;

//------------------------------------------------------------------------------
// User clicked on the button to test database connectivity
//------------------------------------------------------------------------------
procedure TFLPMSBackup.btnDBTestClick(Sender: TObject);
var
   Connected : boolean;

begin

//--- Temporarily stop the Scheduler

   timTimer1.Enabled := False;

//--- Try and connect to the database

   sqlQry1.Close;
   sqlCon.Close;

   sqlCon.HostName     := edtHostNameC.Text;
   sqlCon.UserName     := edtDBUserC.Text;
   sqlCon.Password     := edtDBPassC.Text;
   sqlCon.DatabaseName := edtDBPrefixC.Text;

   if cbxDBSuffixC.Checked = True then
      sqlCon.DatabaseName := sqlCon.DatabaseName + '_LPMS';

   sqlQry1.DataBase := sqlCon;

   Connected := True;

   try

      sqlCon.Connected := True;

   except

      Connected := False;

   end;

   if Connected = True then
      Application.MessageBox(PChar('Connection to ''' + sqlCon.DatabaseName + ''' @ ''' + sqlCon.HostName + ''' successfully established.'),'Backup Manager',(MB_OK + MB_ICONINFORMATION))
   else
      Application.MessageBox(PChar('Connection to ''' + sqlCon.DatabaseName + ''' @ ''' + sqlCon.HostName + ''' failed!'),'Backup Manager',(MB_OK + MB_ICONSTOP));

///--- Restart the Scheduler

   timTimer1.Enabled := True;

end;

//------------------------------------------------------------------------------
// User clicked on the SMS Test button
//------------------------------------------------------------------------------
procedure TFLPMSBackup.btnSMSTestClick(Sender: TObject);
var
   PosStart, PosEnd                         : integer;
   URL, Params, Answer, Credits, Req_Result : string;
   Response                                 : TMemoryStream;

begin

//--- Check whether enough information was given to attempt a SMS Test

   if cbSMSProviderC.ItemIndex = 0 then
      Exit;

   if Trim(edtSMSUserC.Text) = '' then
      Exit;

   if Trim(edtSMSPassC.Text) = '' then
      Exit;

   Response := TMemoryStream.Create;

//--- Construct the RPC based on the selected interface

   case cbSMSProviderC.ItemIndex of

      1: begin
         URL := 'http://www.mymobileapi.com/api5/http5.aspx';
         Params   := 'Type=credits&username=' + edtSMSUserC.Text + '&password=' + edtSMSPassC.Text;
      end;

      2: begin
         URL    := 'http://bulksms.2way.co.za/eapi/user/get_credits/1/1.1';
         Params := 'username=' + edtSMSUserC.Text + '&password=' + edtSMSPassC.Text;
      end;

      3: begin
         URL    := 'http://www.winsms.co.za/api/credits.ASP';
         Params := 'User=' + edtSMSUserC.Text + '&Password=' + edtSMSPassC.Text;
      end;

   end;

//--- Do the HTTP Post/Get operation

   try
      SMSDone := HTTPpostURL(URL, Params, Response);
   finally
   end;

//--- Extract the result from the HTTP Post/Get operation

   SetString(Answer, PAnsiChar(Response.Memory), Response.Size);

   Response.Free;

   case cbSMSProviderC.ItemIndex of

      1: begin

         PosStart   := AnsiPos('<result>',Answer);
         PosEnd     := AnsiPos('</result>',Answer);
         Req_Result := Copy(Answer,PosStart + 8,PosEnd - (PosStart + 8));

         if lowercase(Req_Result) = 'true' then begin

            PosStart := AnsiPos('<credits>',Answer);
            PosEnd   := AnsiPos('</credits>',Answer);
            Credits  := Copy(Answer,PosStart + 9,PosEnd - (PosStart + 9));
            Application.MessageBox(PChar('Test successful - Number of Credits Remaining: ' + Credits),'Backup Manager - Configuration',(MB_OK + MB_ICONINFORMATION));

         end else
            Application.MessageBox('Test unsuccessful - Check ''User name'' and/or ''Password''.','Backup Manager - Configuration',(MB_OK + MB_ICONWARNING));

      end;

      2: begin

         Req_Result := Copy(Answer,1,1);

         if (Req_Result = '0') then begin

            PosStart := AnsiPos('.', Answer);
            Credits  := Copy(Answer,3,PosStart);
            Application.MessageBox(PChar('Test successful - Number of Credits Remaining: ' + Credits),'Backup Manager - Configuration',(MB_OK + MB_ICONINFORMATION));

         end else
            Application.MessageBox('Test unsuccessful - Check ''User name'' and/or ''Password''.','Backup Manager - Configuration',(MB_OK + MB_ICONWARNING));

      end;

      3: begin

         Req_Result := Copy(Answer,9,999999);

         if (lowercase(Req_Result) = 'fail') then
            Application.MessageBox('Test unsuccessful - Check ''User name'' and/or ''Password''.','Backup Manager - Configuration',(MB_OK + MB_ICONWARNING))
         else begin

            Credits  := Req_Result;
            Application.MessageBox(PChar('Test successful - Number of Credits Remaining: ' + Credits),'Backup Manager - Configuration',(MB_OK + MB_ICONINFORMATION));

         end;

      end;

   end;

end;

//------------------------------------------------------------------------------
// User clicked on the Minimise button
//------------------------------------------------------------------------------
procedure TFLPMSBackup.btnMinimiseClick(Sender: TObject);
begin
      FLPMSBackup.Hide;
end;

//------------------------------------------------------------------------------
// The Timer to run the Scheduler popped
//------------------------------------------------------------------------------
procedure TFLPMSBackup.timTimer1Timer(Sender: TObject);
var
   idx1                               : integer;
   RunBackup, JustRan                 : boolean;
   SMSMessage, DispMessage            : string;
   ThisDate, Thistime                 : string;

begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ' ';

//--- Prevent the Scheduler from popping while a Backup is active and disable
//--- the [Run Now] button

   timTimer1.Enabled := False;
   RunBackup         := False;
   JustRan           := False;

//--- Run through the instructions and check whether there are any backups that
//--- must run now

   for idx1 := 0 to NumInstr -1 do begin

      if Instr_List[idx1].Active = True then begin

         case Instr_List[idx1].Status of

            ord(STAT_WAITING)  : begin   // Check whether the instruction is due or past due

               ThisDate := FormatDateTime('yyyy/MM/dd',Now());
               ThisTime := FormatDatetime('HH:mm',Now());

               if ((Instr_List[idx1].NextDate = ThisDate) and (Instr_List[idx1].NextTime = ThisTime)) then begin

                  RunBackup   := True;
                  JustRan     := True;
                  ActiveInstr := idx1;
                  ActiveName  := Instr_List[idx1].Instruction;

                  DispLogMsg('--- Backup scheduled for ' + Instr_List[idx1].NextDate + ' at ' + Instr_List[idx1].NextTime + ' started [Scheduler]');
                  Instr_List[idx1].Status := ord(STAT_SCHEDULED);

                  Set_Buttons(ord(BTN_RUNNOW));

               end;

//--- Check for backups that were missed due to another backup running at the
//--- same time - Status will still be STAT_WAITING

               if Instr_List[idx1].Status = ord(STAT_WAITING) then begin

                  if ((Instr_List[idx1].NextDate <= ThisDate) and (Instr_List[idx1].NextTime <= ThisTime)) then begin

                     RunBackup   := True;
                     JustRan     := True;
                     ActiveInstr := idx1;
                     ActiveName  := Instr_List[idx1].Instruction;

                     DispLogMsg('--- Backup scheduled for ' + Instr_List[idx1].NextDate + ' at ' + Instr_List[idx1].NextTime + ' started [Scheduler]');
                     Instr_List[idx1].Status := ord(STAT_SCHEDULED);

                     Set_Buttons(ord(BTN_RUNNOW));

                  end;

               end;

            end;

            ord(STAT_RUNNOW)   : begin   // User elected to run an instruction immediately

               RunBackup   := True;
               JustRan     := True;
               ActiveInstr := idx1;
               ActiveName  := Instr_List[idx1].Instruction;

               DispLogMsg('Received an instruction to immediately run the next backup');
               DispLogMsg('--- Backup scheduled for ' + Instr_List[idx1].NextDate + ' at ' + Instr_List[idx1].NextTime + ' started on ' + FormatDateTime('yyyy/MM/dd',Now()) + ' at ' + FormatDateTime('hh:nn',Now()) + ' - [Run Now]');
               Instr_List[idx1].Status := ord(STAT_SCHEDULED);

               Set_Buttons(ord(BTN_RUNNOW));

            end;

            ord(STAT_SCHEDULED) : begin   // A completed instruction will remain in this status until a minute has passed to avoid running more than once during the same minute

               ThisDate := FormatDateTime('yyyy/MM/dd',Now());
               ThisTime := FormatDatetime('HH:mm',Now());

               if ((Instr_List[idx1].NextDate <> ThisDate) and (Instr_List[idx1].NextTime <> ThisTime)) then
                  Instr_List[idx1].Status := ord(STAT_WAITING);

            end;

         end;

         if RunBackup = True then begin

            RunBackup := False;

            sbStatus.Panels.Items[2].Text := ' Running...';
            sbStatus.Refresh;

            if (DoBackup() = false) then begin

               DispLogMsg('---    Backup failed with error message: ''' + LastMsg + '''');

               if (((rbSMSFailure.Checked = true) or (rbSMSAlways.Checked = true)) and (Instr_List[idx1].Instr_Rec.BackupSMSProvider <> 0)) then begin

                  DispMessage := FormatDateTime('yyyy/MM/dd@hh:nn:ss',Now) + ' ' + cbxType.Text + ' Backup FAILED (Time: ' + FormatDateTime('hh:nn:ss.zzz',Now - StartTime) + ', Records: ' + FloatToStrF(RecTotal,ffNumber,10,0) + '). Check Log for errors. ' + Instr_List[idx1].Instr_Rec.BackupHostName + '(' + Instr_List[idx1].Instr_Rec.BackupDBPrefix + ') {' + OSName + '}';
                  SMSMessage  := Get_Send_XML(DispMessage);
                  SendSMS(SMSMessage);

                  if (SMSDone = true) then
                     DispLogMsg('---   SMS indicating FAILURE with message ''' + DispMessage + ''' sent to ''' + edtSMSNumber.Text + '''')
                  else
                     DispLogMsg('---   Attempt to send SMS indicating FAILURE failed');

               end;

            end;

            sbStatus.Panels.Items[2].Text := ' Waiting...';

         end;

      end;

   end;

//--- JustRan will be set if we received a RunNow or a Backup was scheduled

   if JustRan = True then begin

      if tvInstructions.Selected.Level = 0 then
         Set_Buttons(ord(BTN_INITIAL))
      else
         Set_Buttons(ord(BTN_INSTRUCTION));

   end;

//--- Restart the Scheduler

   timTimer1.Enabled := True;

end;

//------------------------------------------------------------------------------
// The timer to display the remaining time before the next backup popped.
// Remaining time is displayed only if less than 24 hours remain
//------------------------------------------------------------------------------
procedure TFLPMSBackup.timTimer2Timer(Sender: TObject);
var
   ThisInstr    : integer;
   BDate, BTime : string;
   BackupTime   : TDateTime;

begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ' ';

   sbStatus.Panels.Items[6].Text := FormatDateTime('HH:mm:ss',Now());
   sbStatus.Refresh;

//--- If timTimer1 is inactive then it means a backup is running and we only
//--- update the time display in the Status Bar. Similarly if we are at the
//--- TreeView root

   if (timTimer1.Enabled = False) or (tvInstructions.Selected.Level = 0) then
      Exit;

   ThisInstr := GetInstruction();
   BDate     := Instr_List[ThisInstr].NextDate;
   BTime     := Instr_List[ThisInstr].NextTime;

//--- Work out the time from now until the next backup time

   BackupTime := StrToDate(BDate) + StrToTime(BTime);

//--- If the Cycle is Hourly or Daily we simple display the time remaining.
//--- If the Cycle is Weekly we don't display the countdown message if the time
//--- remaining is more than 24 hours.

   if cbxType.Text = 'Weekly' then begin

      if DaysBetween(Now(),BackupTime) = 0 then begin

         if BTime <= FormatDateTime('HH:mm',Now()) then begin

            lblL05.Caption := 'Next Backup in: ' + FormatDateTime('hh:nn:ss',BackupTime - Time());

         end else begin

            lblL05.Caption := '';
            Exit;

         end
      end else begin

         lblL05.Caption := '';
         Exit;

      end;

   end else begin

      lblL05.Caption := 'Next Backup in: ' + FormatDateTime('hh:nn:ss',BackupTime - Time());

   end;

end;

//------------------------------------------------------------------------------
// The User hovered the mouse over the tray icon - display some information
//------------------------------------------------------------------------------
procedure TFLPMSBackup.TrayIconMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
   Set_Hint(ord(HINT_WAITING));
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

   for idx1 := 0 to NumInstr - 1 do begin

      if Instr_List[idx1].Instruction = ThisInstr then begin

         ListNum := idx1;
         break;

      end;

   end;

   Result := ListNum;

end;

//------------------------------------------------------------------------------
// Load instruction information from the Registry
//------------------------------------------------------------------------------
procedure TFLPMSBackup.ShowInstruction();
var
   ListNum      : integer;

begin

   DoNotConnect := False;
   lvLogSel.Clear;

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ' ';

   ListNum := GetInstruction();

//--- Check whether the selected instruction is active (a config file exists)

   if Instr_List[ListNum].Active = False then begin

      if MessageDlg('Backup Manager','WARNING: The selected Backup Instruction is not configured!' + #10 + #10 + 'Click [Yes] to configure or [No] to return.', mtWarning, mbYesNo, 0) =  mrNo then begin

         tvInstructions.Selected.ImageIndex := 0;
         tvInstructions.Items.Item[0].Selected := True;
         tvInstructionsClick(Application);

         Exit;

      end else begin

         Instr_List[ListNum].Active := True;
         DoNotConnect := True;

         timTimer1.Enabled := False;
         LogInstrType      := ord(TYPE_BOTH);
         ActiveName        := Instr_List[ListNum].Instruction;
         DispLogMsg('Status of Backup Instruction ''' + ActiveName + ''' changed from Inactive to Active');
         ActiveName        := '';
         timTimer1.Enabled := True;

         tvInstructions.Selected.ImageIndex := 1;

         pcInstructions.ActivePage := tsConfiguration;

      end;

   end;

//--- Initialize the date/time dropdown boxes and the input fields. This will
//--- also stop the Scheduler and the Timer that updates the Time and the
//--- 'Next Backup' time

   CanUpdate := False;

   cbxType.ItemIndex := Instr_List[ListNum].Instr_Rec.BackupType;
   cbxTypeChange(Application);

//--- Update the fields on the Instruction Page

   cbxT01.ItemIndex     := Instr_List[ListNum].Instr_Rec.BackupT01;
   cbxT02.ItemIndex     := Instr_List[ListNum].Instr_Rec.BackupT02;
   cbxT03.ItemIndex     := Instr_List[ListNum].Instr_Rec.BackupT03;

   if Instr_List[ListNum].Instr_Rec.BackupSMSProvider = 0 then begin

      edtSMSNumber.Enabled := False;
      rbSMSSuccess.Enabled := False;
      rbSMSFailure.Enabled := False;
      rbSMSNever.Enabled   := False;
      rbSMSAlways.Enabled  := False;

      edtSMSNumber.Text    := '';
      rbSMSSuccess.Checked := False;
      rbSMSFailure.Checked := False;
      rbSMSNever.Checked   := False;
      rbSMSAlways.Checked  := False;

   end else begin

      edtSMSNumber.Enabled := True;
      rbSMSSuccess.Enabled := True;
      rbSMSFailure.Enabled := True;
      rbSMSNever.Enabled   := True;
      rbSMSAlways.Enabled  := True;

      edtSMSNumber.Text    := Instr_List[ListNum].Instr_Rec.BackupSMSNumber;
      rbSMSSuccess.Checked := Instr_List[ListNum].Instr_Rec.BackupSMSSuccess;
      rbSMSFailure.Checked := Instr_List[ListNum].Instr_Rec.BackupSMSFailure;
      rbSMSNever.Checked   := Instr_List[ListNum].Instr_Rec.BackupSMSNever;
      rbSMSAlways.Checked  := Instr_List[ListNum].Instr_Rec.BackupSMSAlways;

   end;

//--- Populate the fields on the Configuration Page

   edtInstrNameC.Text       := Instr_List[ListNum].Instruction;
   edtDBPrefixC.Text        := Instr_List[ListNum].Instr_Rec.BackupDBPrefix;

   if Instr_List[ListNum].Instr_Rec.BackupDBSuffix = '_LPMS' then
      cbxDBSuffixC.Checked := True
   else
      cbxDBSuffixC.Checked := False;

   edtDBUserC.Text          := Instr_List[ListNum].Instr_Rec.BackupDBUser;
   edtDBPassC.Text          := Instr_List[ListNum].Instr_Rec.BackupDBPass;
   edtHostNameC.Text        := Instr_List[ListNum].Instr_Rec.BackupHostName;
   edtTemplateC.Text        := Instr_List[ListNum].Instr_Rec.BackupTemplate;
   edtLocationC.Text        := Instr_List[ListNum].Instr_Rec.BackupLocation;
   speBlockSizeC.Value      := Instr_List[ListNum].Instr_Rec.BackupBlock;
   edtViewerC.Text          := Instr_List[ListNum].Instr_Rec.BackupViewer;
   cbSMSProviderC.ItemIndex := Instr_List[ListNum].Instr_Rec.BackupSMSProvider;
   edtSMSUserC.Text         := Instr_List[ListNum].Instr_Rec.BackupSMSUser;
   edtSMSPassC.Text         := Instr_List[ListNum].Instr_Rec.BackupSMSPass;

//--- Connect to the database and get some basic information unless
//--- DoNotConnect = True in which case we are dealing with a previously
//--- inactive instruction and the DB connecton parameters may not exist

   if DoNotConnect = False then begin

      timTimer1.Enabled := False;
      LogInstrType      := ord(TYPE_LOGSEL);
      ActiveName        := Instr_List[ListNum].Instruction;

      DBConnect(Instr_List[ListNum].Instr_Rec.BackupHostName, Instr_List[ListNum].Instr_Rec.BackupDBPrefix + Instr_List[ListNum].Instr_Rec.BackupDBSuffix, Instr_List[ListNum].Instr_Rec.BackupDBUser, Instr_List[ListNum].Instr_Rec.BackupDBPass);

      LogInstrType      := ord(TYPE_BOTH);
      ActiveName        := '';
      timTimer1.Enabled := True;

   end;

//--- Update the Statusbar

   sbStatus.Panels.Items[3].Text := ' ' + Instr_List[ListNum].Instr_Rec.BackupHostName + '[' + Instr_List[ListNum].Instr_Rec.BackupDBPrefix + Instr_List[ListNum].Instr_Rec.BackupDBSuffix + ']';
   sbStatus.Panels.Items[4].Text := FloatToStrF(Instr_List[ListNum].Instr_Rec.BackupBlock, ffNumber, 2, 0) + ' ';

   DoSave    := False;
   CanUpdate := True;

   btnOpenLB.Enabled := False;

//--- Shutdown the Database for now - we will open it again when a Backup starts

   sqlQry1.Close;
   sqlCon.Close;

//--- If DoNotConnect = True then we are dealing with a previously inactive
//--- instruction. Set the Update flag to force the User tot review the
//--- instruction details.

   if DoNotConnect = True then
      edtInstrNameCChange(Application);

//--- Restart the Scheduler and the Timer

   timTimer1.Enabled := True;
   timTimer2.Enabled := True;

//--- Set up and display (via Timer 2) info about the next scheduled backup

   GetNextSlot(ListNum);
   lblL04.Caption := Instr_List[ListNum].Instr_Rec.BackupMsg;

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

      except on E : Exception do begin

         LastMsg := E.Message;
         Application.MessageBox(Pchar('FATAL: Unexpected database error: ' + #10 + #10 + '''' + LastMsg + ''''),'Backup Manager',(MB_OK + MB_ICONSTOP));

         LogInstrType := ord(TYPE_BOTH);
         DispLogMsg('*** Error connecting to ''' + DBName + ''' on ''' + DBHost + ''' - ''' + LastMsg + '''');
         LogInstrType := ord(TYPE_LOGSEL);

         Exit;

      end;

   end;

//--- Get the currently selected Instruction's reference ID

   idx1 := GetInstruction();

//--- Treat LPMS databases as a special case by getting basic information from
//--- the database

   if Instr_List[idx1].Instr_Rec.BackupDBSuffix = '_LPMS' then begin

      if GetBasicInfo() = false then begin

         Application.MessageBox(Pchar('FATAL: Unexpected database error: ''' + LastMsg + '''. Backup Manager will now terminate'),'Backup Manager',(MB_OK + MB_ICONSTOP));
         Application.Terminate;

      end;

      Instr_List[idx1].SymCpy     := sqlQry1.FieldByName('CpyName').AsString;
      Instr_List[idx1].SymVersion := sqlQry1.FieldByName('Version').AsString;

   end else begin

      Instr_List[idx1].SymCpy     := '';
      Instr_List[idx1].SymVersion := '';

   end;

//--- Treat LPMS databases as a special case by displaying information about
//--- the database version

   if Instr_List[idx1].Instr_Rec.BackupDBSuffix = '_LPMS' then begin

      DispLogMsg('Backups will be taken for ''' + Instr_List[idx1].SymCpy + ''' on ''' + DBHost + '[' + Instr_List[idx1].Instr_Rec.BackupDBPrefix + Instr_List[idx1].Instr_Rec.BackupDBSuffix + ']''');
      DispLogMsg('Database version is ''' + Instr_List[idx1].SymVersion + '''');

   end else

      DispLogMsg('Backups will be taken for ''' + DBHost + '[' + Instr_List[idx1].Instr_Rec.BackupDBPrefix + Instr_List[idx1].Instr_Rec.BackupDBSuffix + ']''');

//--- Set up the rest of the information

   if Instr_List[idx1].Instr_Rec.BackupSMSProvider = 0 then begin

      edtSMSNumber.Enabled := False;
      rbSMSSuccess.Enabled := False;
      rbSMSFailure.Enabled := False;
      rbSMSNever.Enabled   := False;
      rbSMSAlways.Enabled  := False;
      DispLogMsg('SMS Messaging for ' + Instr_List[idx1].Instruction + ' is inactive');

   end else begin

      edtSMSNumber.Enabled := True;
      rbSMSSuccess.Enabled := True;
      rbSMSFailure.Enabled := True;
      rbSMSNever.Enabled   := True;
      rbSMSAlways.Enabled  := True;

      if edtSMSNumber.Text <> '' then begin

         if rbSMSAlways.Checked = true then

            DispLogMsg('SMS will always be sent to ''' + edtSMSNumber.Text + ''' (Success or Failure) using ''' + Instr_List[idx1].Instr_Rec.BackupSMSProviderName + '''')

         else if rbSMSSuccess.Checked = true then

            DispLogMsg('SMS Message will be sent to ''' + edtSMSNumber.Text + ''' after a successful backup using ''' + Instr_List[idx1].Instr_Rec.BackupSMSProviderName + '''')

         else if rbSMSFailure.Checked = true then

            DispLogMsg('SMS Message will be sent to ''' + edtSMSNumber.Text + ''' if a backup fails using ''' + Instr_List[idx1].Instr_Rec.BackupSMSProviderName + '''')

         else

            DispLogMsg('SMS Messages will never be sent');

      end else

         DispLogMsg('SMS Messaging is inactive because ''SMS Number: is not specified');

   end;

   sbStatus.Panels.Items[2].Text := ' Waiting...';
   sbStatus.Panels.Items[3].Text := ' ' + DBHost + '[' + Instr_List[idx1].Instr_Rec.BackupDBPrefix + Instr_List[idx1].Instr_Rec.BackupDBSuffix + ']';
   sbStatus.Panels.Items[4].Text := FloatToStrF(Instr_List[idx1].Instr_Rec.BackupBlock, ffNumber, 2, 0) + ' ';

end;

//------------------------------------------------------------------------------
// React to the navigation buttons
//------------------------------------------------------------------------------
procedure TFLPMSBackup.Navigate();
begin


   if tvInstructions.Selected.Level = 0 then begin

      if lvLogAll.ItemIndex > 0 then begin

         btnFirst.Enabled := True;
         btnPrev.Enabled  := True;

      end else begin

         btnFirst.Enabled := False;
         btnPrev.Enabled  := False;

      end;

      if lvLogAll.ItemIndex < (lvLogAll.Items.Count - 1) then begin

         btnNext.Enabled := True;
         btnLast.Enabled := True;

      end else begin

         btnNext.Enabled := False;
         btnLast.Enabled := False;

      end;

      lvLogAll.Selected.MakeVisible(False);

   end else begin

      if pcInstructions.ActivePage = tsConfiguration then
         Exit;

      if lvLogSel.ItemIndex > 0 then begin

         btnFirst.Enabled := True;
         btnPrev.Enabled  := True;

      end else begin

         btnFirst.Enabled := False;
         btnPrev.Enabled  := False;

      end;

      if lvLogSel.ItemIndex < (lvLogSel.Items.Count - 1) then begin

         btnNext.Enabled := True;
         btnLast.Enabled := True;

      end else begin

         btnNext.Enabled := False;
         btnLast.Enabled := False;

      end;

      lvLogSel.Selected.MakeVisible(False);

   end;

end;

//------------------------------------------------------------------------------
// Set the Hint to be displayed when the Mouse pointer is hovered over the tray icon
//------------------------------------------------------------------------------
procedure TFLPMSBackup.Set_Hint(ThisType: integer);
var
   MsgPart : string;

begin

   if ThisType = ord(HINT_WAITING) then
      MsgPart := lblL05.Caption
   else
      MsgPart := 'Currently running...';

   TrayIcon.Hint := 'Backup Manager: Right click for options' + #10 + 'Double click to restore Backup Manager' + #10 + MsgPart;

end;

//------------------------------------------------------------------------------
// Procedure to set the state of all the buttons
//------------------------------------------------------------------------------
procedure TFLPMSBackup.Set_Buttons(State: integer);
begin

   FileClose.Enabled       := False;
   FileNew.Enabled         := False;
   FileDelete.Enabled      := False;
   EditCancel.Enabled      := False;
   EditUpdate.Enabled      := False;
   SearchFind.Enabled      := False;
   ActionsFirst.Enabled    := False;
   ActionsNext.Enabled     := False;
   ActionsPrevious.Enabled := False;
   ActionsLast.Enabled     := False;
   ActionsRunNow.Enabled   := False;

   ActionsMinimise.Enabled := True;
   HelpAbout.Enabled       := True;

   case State of

      ord(BTN_INITIAL): begin

         FileClose.Enabled       := True;
         FileNew.Enabled         := True;
         SearchFind.Enabled      := True;
         ActionsFirst.Enabled    := True;
         ActionsNext.Enabled     := True;
         ActionsPrevious.Enabled := True;
         ActionsLast.Enabled     := True;

      end;

      ord(BTN_INSTRUCTION): begin

         FileClose.Enabled       := True;
         FileNew.Enabled         := True;
         FileDelete.Enabled      := True;

         if pcInstructions.ActivePage = tsInstruction then begin

            SearchFind.Enabled      := True;
            ActionsFirst.Enabled    := True;
            ActionsNext.Enabled     := True;
            ActionsPrevious.Enabled := True;
            ActionsLast.Enabled     := True;

         end;

         ActionsRunNow.Enabled   := True;

      end;

      ord(BTN_RUNNOW): begin
         ActionsRunNow.Enabled   := False;
         ActionsMinimise.Enabled := False;
      end;

      ord(BTN_UPDATE): begin

         EditCancel.Enabled      := True;
         EditUpdate.Enabled      := True;
         ActionsMinimise.Enabled := False;

      end;

   end;

//--- In the Run Now state we do not switch on any of the navigation buttons

   if State <> ord(BTN_RUNNOW) then
      Navigate();

end;

//------------------------------------------------------------------------------
// Procedure to do the backup
//------------------------------------------------------------------------------
function TFLPMSBackup.DoBackup() : boolean;
var
   ThisTime     : string;
   SMSMessage   : string;
   DispMessage  : string;

begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ' ';

   ThisTime := FormatDateTime('hh',Now()) + 'h' + FormatDateTime('nn',Now());

   OutFile := Instr_List[ActiveInstr].Instr_Rec.BackupLocation + Instr_List[ActiveInstr].Instr_Rec.BackupTemplate + '.lpb';

   OutFile := AnsiReplaceStr(OutFile,'&Date',FormatDateTime('yyyyMMdd',Now()));
   OutFile := AnsiReplaceStr(OutFile,'&Year',FormatDateTime('yyyy',Now()));
   OutFile := AnsiReplaceStr(OutFile,'&Month',FormatDateTime('MM',Now()));
   OutFile := AnsiReplaceStr(OutFile,'&Day',FormatDateTime('dd',Now()));
   OutFile := AnsiReplaceStr(OutFile,'&Time',ThisTime);
   OutFile := AnsiReplaceStr(OutFile,'&Hour',FormatDateTime('hh',Now()));
   OutFile := AnsiReplaceStr(OutFile,'&Minute',FormatDateTime('nn',Now()));
   OutFile := AnsiReplaceStr(OutFile,'&CpyName',Instr_List[ActiveInstr].SymCpy);
   OutFile := AnsiReplaceStr(OutFile,'&Instruction',Instr_List[ActiveInstr].Instruction);
   OutFile := AnsiReplaceStr(OutFile,'&HostName',Instr_List[ActiveInstr].Instr_Rec.BackupHostName);
   OutFile := AnsiReplaceStr(OutFile,'&BackupType',cbxType.Text);
   OutFile := AnsiReplaceStr(OutFile,'&DBPrefix',Instr_List[ActiveInstr].Instr_Rec.BackupDBPrefix);
   OutFile := AnsiReplaceStr(OutFile,'&DBSuffix',Instr_List[ActiveInstr].Instr_Rec.BackupDBSuffix);
   OutFile := AnsiReplaceStr(OutFile,'&OSName',OSName);

   DispLogMsg('------ Backup will be saved to ''' + OutFile + '''');
   StartTime := Now;
   DispLogMsg('------ Backup attempt started on ' + FormatDateTime('yyyy/MM/dd',Now()) + ' at ' + FormatDateTime('hh:nn:ss.zzz',StartTime));

//--- Temporarily stop the count down display

   if ActiveName = InstrSel then begin

      lblL04.Caption := 'Backup started on ' + FormatDateTime('yyyy/MM/dd',Now()) + ' at ' + FormatDateTime('hh:nn:ss',StartTime);
      lblL05.Caption := '';
      lblL05.Repaint;

   end;

//--- Create the Backup File

   if ReadDB(OutFile,1) = False then begin

      Result := False;
      Exit;

   end;

//--- Indicate the success of the backup and send an SMS if necessary

   if BackupSuccess = True then
      DispLogMsg('------ Backup succesfully completed')
   else
      DispLogMsg('------ Backup FAILED');

   DispLogMsg(-1);

   EndTime := Now;

   edtLastBackup.Text := OutFile;

   if FileExists(Instr_List[ActiveInstr].Instr_Rec.BackupViewer) = True then
      btnOpenLB.Enabled := True
   else
      btnOpenLB.Enabled := False;

   if (((rbSMSSuccess.Checked = true) or (rbSMSAlways.Checked = true)) and (Instr_List[ActiveInstr].Instr_Rec.BackupSMSProvider <> 0)) then begin
      DispMessage := FormatDateTime('yyyy/MM/dd@hh:nn:ss',Now) + ' ' + cbxType.Text + ' Backup Successful (Time: ' + FormatDateTime('hh:nn:ss.zzz',EndTime - StartTime) + ', Records: ' + RecTotalF + ', Size: ' + ThisMsgF + '). ' + Instr_List[ActiveInstr].Instr_Rec.BackupHostName + '(' + Instr_List[ActiveInstr].Instr_Rec.BackupDBPrefix + Instr_List[ActiveInstr].Instr_Rec.BackupDBSuffix + ') {' + OSName + '}';
      SMSMessage  := Get_Send_XML(DispMessage);

      SendSMS(SMSMessage);

      if (SMSDone = true) then
         DispLogMsg('------ SMS indicating SUCCESS with message ''' + DispMessage + ''' sent to ''' + edtSMSNumber.Text + '''')
      else
         DispLogMsg('------ Attempt to send SMS indicating SUCCESS failed with message ''' + SMSResult + '''');
   end;

   DispLogMsg('--- Backup attempt completed on ' + FormatDateTime('yyyy/MM/dd',Now()) + ' at ' + FormatDateTime('hh:nn:ss.zzz',EndTime) + ', Time taken: ' + FormatDateTime('hh:nn:ss.zzz',EndTime - StartTime));

//--- Get the next time slot for this backup instruction then return

   GetNextSlot(ActiveInstr);

   if tvInstructions.Selected.Level = 0 then begin

      lblL04.Caption := '';
      lblL05.Caption := '';

   end else
      lblL04.Caption := Instr_List[ActiveInstr].Instr_Rec.BackupMsg;

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
   idx1, idx2, RecCount, LimitStart, LimitEnd             : integer;
   P1, P2, P3, P4, P5, P6, P7, Res, ThisLine, ThisTable   : string;
   ThisTitle, ThisDate, ThisTime, TypeBackup, ThisVersion : string;
   ThisTables, ThisMode                                   : string;
   BackupFile                                             : TextFile;
   TableNames                                             : TStringList;
   FieldNames                                             : TList;
   Fields_Rec                                             : ^Fields_Struct;
   ThisZipper                                             : TZipper;

begin

   BackupSuccess := True;

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ' ';

//--- Create the in memory lists

   TableNames := TStringList.Create;
   FieldNames := TList.Create;

//--- Update the Message fields

   if ActiveName = InstrSel then begin

      lblL05.Caption := 'Preparing backup...';
      lblL05.Refresh;

   end;

//--- Open a connection to the Database

   sqlQry1.Close;
   sqlCon.Close;

   sqlCon.HostName     := Instr_List[ActiveInstr].Instr_Rec.BackupHostName;
   sqlCon.UserName     := Instr_List[ActiveInstr].Instr_Rec.BackupDBUser;
   sqlCon.Password     := Instr_List[ActiveInstr].Instr_Rec.BackupDBPass;
   sqlCon.DatabaseName := Instr_List[ActiveInstr].Instr_Rec.BackupDBPrefix + Instr_List[ActiveInstr].Instr_Rec.BackupDBSuffix;

   sqlQry1.DataBase    := sqlCon;

//--- Get the Table Names for the open Database

   try

      sqlCon.GetTableNames(TableNames,false);

      except on E : Exception do begin

         LastMsg := '*** Error connecting to ''' + Instr_List[ActiveInstr].Instr_Rec.BackupDBPrefix + Instr_List[ActiveInstr].Instr_Rec.BackupDBSuffix + ''' on ''' + Instr_List[ActiveInstr].Instr_Rec.BackupHostName + ''' - ''' + E.Message + '''';
         DBAccessFailed(LastMsg);
         Exit;

      end;

   end;

//--- Set the Flags

   ThisTitle   := 'Backup Manager';
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

      DispLogMsg('--------- Backing up ''' + ThisTable + ''' table');

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

      sqlQry1.Close;
      sqlQry1.SQL.Text := 'SHOW COLUMNS IN ' + ThisTable;

      try

         sqlQry1.Open;

         except on E : Exception do begin

            LastMsg := '*** Error connecting to ''' + Instr_List[ActiveInstr].Instr_Rec.BackupDBPrefix + Instr_List[ActiveInstr].Instr_Rec.BackupDBSuffix + ''' on ''' + Instr_List[ActiveInstr].Instr_Rec.BackupHostName + ''' - ''' + E.Message + '''';
            DBAccessFailed(LastMsg);
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
            if (Trim(Fields_Rec^.Def) = '') then
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

      if Trim(P4) = '' then
         P4 := ') ENGINE=MyISAM DEFAULT CHARSET=latin1 ROW_FORMAT=DYNAMIC';

      Res := Res + P4;

      if (ThisType = 1) then
         ThisLine := Res + ';'
      else
         ThisLine := '2' + Res + ';';

      WriteLn(BackupFile,ThisLine);

//--- Update the Message fields

      if (ActiveName = InstrSel) then begin

//         lblL05.Caption := 'Backing up Table ''' + ThisTable + ''', Reading...';
//         lblL05.Refresh;

      end;

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

               lblL05.Caption := 'Backing up Table ''' + ThisTable + ''', Writing record no.: ' + FloatToStrF(RecCount, ffNumber, 2, 0);
               lblL05.Refresh;

            end;

            sqlQry1.Next;
         end;

         LimitStart := LimitStart + Instr_List[ActiveInstr].Instr_Rec.BackupBlock;

         if ActiveName = InstrSel then begin

            lblL05.Caption := 'Backing up Table ''' + ThisTable + ''', Reading up to ' + FloatToStrF(Instr_List[ActiveInstr].Instr_Rec.BackupBlock, ffNumber, 2, 0) + ' records';
            lblL05.Refresh;

         end;

         if ReadTable(ThisTable,LimitStart,LimitEnd) = False then begin

            Result := False;
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

//--- Compress the output file if cbxCompress is checked

   if cbxCompress.Checked = True then begin

      ThisZipper := TZipper.Create;

      ThisZipper.FileName := Instr_List[ActiveInstr].Instr_Rec.BackupLocation + ChangeFileExt(ExtractFileName(FileName),'.zip');
      ThisZipper.Entries.AddFileEntry(FileName);
      ThisZipper.ZipAllFiles;

      ThisZipper.Free;

      DeleteFile(FileName);

   end;

//--- Clear and delete the Lists that were used

   TableNames.Free;
   FieldNames.Free;

//--- Close the connection to the Database

   sqlQry1.Close;
   sqlCon.Close;

   Result := true;

end;

//------------------------------------------------------------------------------
// Procedure to log and send a message when DB Access Failed
//------------------------------------------------------------------------------
procedure TFLPMSBackup.DBAccessFailed(ThisMsg: string);
var
   SaveType   : integer;
   SMSMessage : string;

begin

   BackupSuccess := False;

   SaveType := ord(LogInstrType);
   LogInstrType := ord(TYPE_BOTH);
   DispLogMsg(LastMsg);

   if (((Instr_List[ActiveInstr].Instr_Rec.BackupSMSFailure = true) or (Instr_List[ActiveInstr].Instr_Rec.BackupSMSAlways = true)) and (Instr_List[ActiveInstr].Instr_Rec.BackupSMSProvider <> 0)) then begin
      LastMsg := FormatDateTime('yyyy/MM/dd@hh:nn:ss',Now) + ' ' + LastMsg + ' {' + OSName + '}';
      SMSMessage  := Get_Send_XML(LastMsg);

      SendSMS(SMSMessage);

      if (SMSDone = true) then
         DispLogMsg('------ SMS indicating FAILURE with message ''' + LastMsg + ''' sent to ''' + Instr_List[ActiveInstr].Instr_Rec.BackupSMSNumber + '''')
      else
         DispLogMsg('------ Attempt to send SMS indicating SUCCESS failed with message ''' + SMSResult + '''');
   end;

   LogInstrType := ord(SaveType);

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
// Function to determine the next backup date and time
//------------------------------------------------------------------------------
procedure TFLPMSBackup.GetNextSlot(ThisInstr: integer);
var
   ThisDays, BackupDay, CurrentDay        : integer;
   DispDate, DispTime, ThisDate, ThisTime : string;
   BackupTime, ThisBackupMsg              : string;

begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ' ';

//--- Set date and time format for this session

   ThisDate    := FormatDateTime('yyyy/MM/dd',Now());
   ThisTime    := FormatDateTime('hhnn',Now());

//--- Generate a message to display the date and time of the next backup and
//--- store the date and the time (HH:MM) in the in-memory list

   case Instr_List[ThisInstr].Instr_Rec.BackupType of

      0: begin                         // Hourly

         BackupTime := Copy(ThisTime,1,2) + MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT01 + 1];

         if BackupTime <= ThisTime then
            DispTime := FormatDateTime('HH',IncMinute(Now(),60)) + ':' + MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT01 + 1]
         else
            DispTime   := Copy(BackupTime,1,2) + ':' + Copy(BackupTime,3,2);

         DispDate := ThisDate;

      end;

      1: begin                         // Daily

         BackupTime := HourArray[Instr_List[ThisInstr].Instr_Rec.BackupT01 + 1] + MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT02 + 1];

         if (BackupTime <= ThisTime) then begin

            DispDate := FormatDateTime('yyyy/MM/dd',IncDay(Now(),1));
            DispTime := HourArray[Instr_List[ThisInstr].Instr_Rec.BackupT01 + 1] + ':' + MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT02 + 1];

         end else begin

            DispDate := FormatDateTime('yyyy/MM/dd',Now());
            DispTime   := Copy(BackupTime,1,2) + ':' + Copy(BackupTime,3,2);

         end;

      end;

      2: begin                         // Weekly

//--- Get the day of the week on which the backup must be taken as a number
//--- and the current day of the week as a number

         BackupDay  := Instr_List[ThisInstr].Instr_Rec.BackupT01 + 1;
         CurrentDay := DayOfWeek(Now());

//--- Check how many days to go until the backup day and set the next backup
//--- date

         if (BackupDay < CurrentDay) then
            ThisDays := (BackupDay - CurrentDay + 7)
         else
            ThisDays := (BackupDay - CurrentDay);

         DispDate   := FormatDateTime('yyyy/MM/dd',IncDay(Now(),ThisDays));

//--- If the weekly backup day is today then we need to work out whether the
//--- time for the backup today has already gone and if so then adjust the next
//--- backup date by a week

         BackupTime := HourArray[Instr_List[ThisInstr].Instr_Rec.BackupT02 + 1] + MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT03 + 1];

         if (DispDate = ThisDate) and (BackupTime <= ThisTime) then
            DispDate := FormatDateTime('yyyy/MM/dd',IncDay(StrToDate(DispDate),7));

         DispTime := HourArray[Instr_List[ThisInstr].Instr_Rec.BackupT02 + 1] + ':' + MinArray[Instr_List[ThisInstr].Instr_Rec.BackupT03 + 1];

      end;

   end;

   ThisBackupMsg := 'Next Backup on ' + DispDate + ' at ' + DispTime;

//--- Update the instruction's data in the in-memory list

   Instr_List[ThisInstr].NextDate := DispDate;
   Instr_List[ThisInstr].NextTime := DispTime;
   Instr_List[ThisInstr].Status   := ord(STAT_WAITING);
   Instr_List[ThisInstr].Instr_Rec.BackupMsg := ThisBackupMsg;

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
   FormatSettings.ThousandSeparator := ' ';

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
         send_xml.Add('<customerid>Backup Manager</customerid>');
         send_xml.Add('</entries>');
         send_xml.Add('</senddata>');
      end;

      2: send_xml.Add(SMS + '&msisdn=' + '27' + Copy(Instr_List[ActiveInstr].Instr_Rec.BackupSMSNumber,2,99));
      3: send_xml.Add(SMS + '&numbers=' + '27' + Copy(Instr_List[ActiveInstr].Instr_Rec.BackupSMSNumber,2,99));
   end;

   Res := send_xml.Text;
   send_xml.Free;
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
   Response.Free;

   SMSDone := false;

   case Instr_List[ActiveInstr].Instr_Rec.BackupSMSProvider of

      1: begin

         PosStart := AnsiPos('<result>',Answer);
         PosEnd   := AnsiPos('</result>',Answer);
         Req_Result := Copy(Answer,PosStart + 8,PosEnd - (PosStart + 8));

         if lowercase(Req_Result) = 'true' then

            SMSDone := true

         else begin

            PosStart := AnsiPos('<error>',Answer);
            PosEnd   := AnsiPos('</error>',Answer);
            SMSResult := Copy(Answer,PosStart + 7,PosEnd - (PosStart + 7));

         end;

      end;

      2: begin

         Req_Result := Copy(Answer,1,1);

         if (Req_Result = '0') then
            SMSDone := true;

      end;

      3: begin

         PosStart := AnsiPos('=',Answer);
         Req_Result := Copy(Answer,PosStart + 1,1);

         if ((Req_Result >= '1') and (Req_Result <= '9')) then
            SMSDone := true;

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
   FormatSettings.ThousandSeparator := ' ';

   ThisDate := FormatDateTime('yyyy/MM/dd',Now());
   ThisTime := FormatDateTime('hh:mm:ss.zzz',Now());

//--- Display the message in the main log

   if (LogInstrType = ord(TYPE_BOTH)) or (LogInstrType = ord(TYPE_LOGALL)) then begin

      ThisItem := lvLogAll.Items.Add;

      ThisItem.Caption := ThisDate;
      ThisItem.SubItems.Add(Thistime);

      if tvInstructions.Selected.Level = 0 then
         ThisItem.SubItems.Add('Backup')
      else
         ThisItem.SubItems.Add(ActiveName);
//         ThisItem.SubItems.Add(tvInstructions.Selected.Text);

      ThisItem.SubItems.Add(ThisMsg);
      ThisItem.MakeVisible(false);
      ThisItem.Selected := true;

      lvLogAll.Repaint;

   end;

   if (LogInstrType = ord(TYPE_BOTH)) or (LogInstrType = ord(TYPE_LOGSEL)) then begin

      if tvInstructions.Selected.Level > 0 then begin;

         if ActiveName = InstrSel then begin

            ThisItem := lvLogSel.Items.Add;

            ThisItem.Caption := ThisDate;
            ThisItem.SubItems.Add(Thistime);
            ThisItem.SubItems.Add(ActiveName);
//            ThisItem.SubItems.Add(tvInstructions.Selected.Text);
            ThisItem.SubItems.Add(ThisMsg);
            ThisItem.MakeVisible(false);
            ThisItem.Selected := true;

            lvLogSel.Repaint;

         end;

      end;

   end;

end;

//---------------------------------------------------------------------------
// Add Record Count to the last displayed message in the listview
//---------------------------------------------------------------------------
procedure TFLPMSBackup.DispLogMsg(RecCount : integer);
var
   ThisNum                              : double;
   ThisMsg, ThisPart, RecPart, ThisFile : string;
   ThisItem                             : TListItem;
   SearchRec                            : TSearchRec;

begin

   ThisPart := ' MB';

//--- Get the line to be modified from the overall log

   ThisItem := lvLogAll.Selected;
   ThisMsg  := ThisItem.SubItems.Strings[2];

   if RecCount = -1 then begin

      if cbxCompress.Checked = False then
         ThisFile := OutFile
      else
         ThisFile := ChangeFileExt(OutFile,'.zip');

      if (FindFirst(ExpandFileName(ThisFile),faAnyFile,SearchRec) = 0) then
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

//--- Update the overall log if LogInstrOnly = False and the instruction
//--- specific log if it is visible

   if (LogInstrType = ord(TYPE_BOTH)) or (LogInstrType = ord(TYPE_LOGALL)) then begin

      ThisItem := lvLogAll.Selected;
      ThisItem.SubItems.Strings[2] := ThisMsg;
      lvLogAll.Refresh;

   end;

   if (LogInstrType = ord(TYPE_BOTH)) or (LogInstrType = ord(TYPE_LOGSEL)) then begin

      if tvInstructions.Selected.Level > 0 then begin;

         if ActiveName = InstrSel then begin

            ThisItem := lvLogSel.Selected;
            ThisItem.SubItems.Strings[2] := ThisMsg;
            lvLogSel.Refresh;

         end;

      end

   end;

end;

//---------------------------------------------------------------------------
// Display a message in the Log listview with date and time supplied
//---------------------------------------------------------------------------
procedure TFLPMSBackup.DispLogMsg(ThisDate: string; ThisTime: string; ThisInstr: string; ThisMsg: string);
var
   ThisItem  : TListItem;

begin

//--- Display the message in the overall log

   if (LogInstrType = ord(TYPE_BOTH)) or (LogInstrType = ord(TYPE_LOGALL)) then begin

      ThisItem := lvLogAll.Items.Add;
      ThisItem.Caption := ThisDate;
      ThisItem.SubItems.Add(ThisTime);
      ThisItem.SubItems.Add(ThisInstr);
      ThisItem.SubItems.Add(ThisMsg);
      ThisItem.MakeVisible(false);

      lvLogAll.Repaint;

   end;

//--- If an Instruction is selected then also display the message in the
//--- Instruction's log

   if (LogInstrType = ord(TYPE_BOTH)) or (LogInstrType = ord(TYPE_LOGSEL)) then begin

      if tvInstructions.Selected.Level > 0 then begin

         if ActiveName = InstrSel then begin

            ThisItem := lvLogSel.Items.Add;
            ThisItem.Caption := ThisDate;
            ThisItem.SubItems.Add(ThisTime);
            ThisItem.SubItems.Add(ThisInstr);
            ThisItem.SubItems.Add(ThisMsg);
            ThisItem.MakeVisible(false);

            lvLogSel.Repaint;

         end;

      end;

   end;

end;

//---------------------------------------------------------------------------
// Procedure to Open and Read the Configuration File and to build the tree
// view and the in-memory instrruction list
//---------------------------------------------------------------------------
procedure TFLPMSBackup.OpenCfg(FileName: string);
var
   idx1                 : integer;
   FirstChild           : boolean;
   ThisLine, CfgStr     : string;
   CfgInstr             : TStringList;
   ThisNodeM, ThisNodeS : TTreeNode;

begin

//--- Set the Format Settings to override the system locale

   FormatSettings.ShortDateFormat   := 'yyyy/MM/dd';
   FormatSettings.DateSeparator     := '/';
   FormatSettings.ThousandSeparator := ' ';

   NumInstr    := 0;

//--- Create the String Lists

   InstrTokens := TStringList.Create;
   CfgInstr    := TStringList.Create;

   if FileExists(FileName) = True then begin

      IniFile := TINIFile.Create(FileName);

      cfgStr := IniFile.ReadString('Config','String','New Instruction|');

      cbxDoSort.Checked   := IniFile.ReadBool('Parameters','Sort',False);
      cbxDelete.Checked   := IniFile.ReadBool('Parameters','Discard',False);
      cbxCompress.Checked := IniFile.ReadBool('Parameters','Compress',True);

      BackupTemplate.Instr_Rec.BackupBlock       := IniFile.ReadInteger('Template','BackupBlock',5000);
      BackupTemplate.Instr_Rec.BackupSMSProvider := IniFile.ReadInteger('Template','BackupSMSProvider',0);
      BackupTemplate.Instr_Rec.BackupType        := IniFile.ReadInteger('Template','BackupType',1);
      BackupTemplate.Instr_Rec.BackupT01         := IniFile.ReadInteger('Template','BackupT01',7);
      BackupTemplate.Instr_Rec.BackupT02         := IniFile.ReadInteger('Template','BackupT02',0);
      BackupTemplate.Instr_Rec.BackupT03         := IniFile.ReadInteger('Template','BackupT03',0);
      BackupTemplate.Instr_Rec.BackupSMSAlways   := IniFile.ReadBool('Template','BackupSMSAlways',False);
      BackupTemplate.Instr_Rec.BackupSMSFailure  := IniFile.ReadBool('Template','BackupSMSFailure',False);
      BackupTemplate.Instr_Rec.BackupSMSNever    := IniFile.ReadBool('Template','BackupSMSNever',True);
      BackupTemplate.Instr_Rec.BackupSMSSuccess  := IniFile.ReadBool('Template','BackupSMSSuccess',False);
      BackupTemplate.Instr_Rec.BackupDBPass      := Decode(IniFile.ReadString('Template','BackupDBPass',''));
      BackupTemplate.Instr_Rec.BackupDBPrefix    := IniFile.ReadString('Template','BackupDBPrefix','');
      BackupTemplate.Instr_Rec.BackupDBSuffix    := IniFile.ReadString('Template','BackupDBSuffix','');
      BackupTemplate.Instr_Rec.BackupDBUser      := IniFile.ReadString('Template','BackupDBUser','');
      BackupTemplate.Instr_Rec.BackupHostName    := IniFile.ReadString('Template','BackupHostName','127.0.0.1');
      BackupTemplate.Instr_Rec.BackupLocation    := IniFile.ReadString('Template','BackupLocation','~');
      BackupTemplate.Instr_Rec.BackupSMSNumber   := IniFile.ReadString('Template','BackupSMSNumber','');
      BackupTemplate.Instr_Rec.BackupSMSPass     := Decode(IniFile.ReadString('Template','BackupSMSPass',''));
      BackupTemplate.Instr_Rec.BackupSMSUser     := IniFile.ReadString('Template','BackupSMSUser','');
      BackupTemplate.Instr_Rec.BackupTemplate    := IniFile.ReadString('Template','BackupTemplate','&Date@&Time - &BackupType Backup (&Instruction)');
      BackupTemplate.Instr_Rec.BackupViewer      := IniFile.ReadString('Template','BackupViewer','');

      IniFile.Destroy;

//--- Load the Configuration file into memory and then extract the 1st Line

      CfgInstr.Add(CfgStr);
      ThisLine := CfgInstr.Strings[0];

      ExtractStrings(['|'], [], PChar(ThisLine), InstrTokens);

//--- Save the number of instructions that were listed in the Configuration file

      NumInstr := InstrTokens.Count;

   end else begin

      InstrTokens.Add('New Instruction');
      NumInstr := 1;

   end;

//--- Build the Treeview and the in-memory array of instructions

   FirstChild := True;

//--- Insert the root node in the Main TreeView

   ThisNodeM := tvInstructions.Items.Add(nil,'Backup Instructions');
   ThisNodeM.ImageIndex    := 2;
   ThisNodeM.SelectedIndex := 3;

   tvInstructions.Selected := ThisNodeM;

   DispLogMsg('*** Starting Backup Manager');

//--- Process the Instructions and build the in-memory list

   SetLength(Instr_List,NumInstr);

   DispLogMsg('+++ Loading ' + IntToStr(NumInstr) + ' Backup Instructuctions');

   for idx1 := 0 to NumInstr -1 do begin

      if (FirstChild = True) then begin

         ThisNodeM := tvInstructions.Items.AddChildFirst(ThisNodeM,InstrTokens.Strings[idx1]);

         ThisNodeS := tvSmall.Items.Add(nil,InstrTokens.Strings[idx1]);
         ThisNodeS.ImageIndex    := 1;
         ThisNodeS.SelectedIndex := 3;

         FirstChild := False;

      end else begin

         ThisNodeM := tvInstructions.Items.Add(ThisNodeM,InstrTokens.Strings[idx1]);
         ThisNodeS := tvSmall.Items.Add(ThisNodeS,InstrTokens.Strings[idx1]);
         ThisNodeS.ImageIndex    := 1;
         ThisNodeS.SelectedIndex := 3;

      end;

      Instr_List[idx1].Ini_File    := 'Backup_Manager_' + InstrTokens.Strings[idx1] + '.ini';
      Instr_List[idx1].Instruction := InstrTokens.Strings[idx1];
      Instr_List[idx1].NextDate    := '2099/12/31';
      Instr_List[idx1].NextTime    := '23:59:59';

//--- Load the contents of the ini File

      RegString := LocalPath + Instr_List[idx1].Ini_File;

//--- Extract the information contained in the 'registry'

      if FileExists(RegString) = False then begin

         ThisNodeS.Delete;

         Instr_List[idx1].Instr_Rec.BackupBlock           := BackupTemplate.Instr_Rec.BackupBlock;
         Instr_List[idx1].Instr_Rec.BackupSMSProvider     := BackupTemplate.Instr_Rec.BackupSMSProvider;
         Instr_List[idx1].Instr_Rec.BackupType            := BackupTemplate.Instr_Rec.BackupType;
         Instr_List[idx1].Instr_Rec.BackupT01             := BackupTemplate.Instr_Rec.BackupT01;
         Instr_List[idx1].Instr_Rec.BackupT02             := BackupTemplate.Instr_Rec.BackupT02;
         Instr_List[idx1].Instr_Rec.BackupT03             := BackupTemplate.Instr_Rec.BackupT03;
         Instr_List[idx1].Instr_Rec.BackupSMSAlways       := BackupTemplate.Instr_Rec.BackupSMSAlways;
         Instr_List[idx1].Instr_Rec.BackupSMSFailure      := BackupTemplate.Instr_Rec.BackupSMSFailure;
         Instr_List[idx1].Instr_Rec.BackupSMSNever        := BackupTemplate.Instr_Rec.BackupSMSNever;
         Instr_List[idx1].Instr_Rec.BackupSMSSuccess      := BackupTemplate.Instr_Rec.BackupSMSSuccess;
         Instr_List[idx1].Instr_Rec.BackupDBPass          := BackupTemplate.Instr_Rec.BackupDBPass;
         Instr_List[idx1].Instr_Rec.BackupDBPrefix        := BackupTemplate.Instr_Rec.BackupDBPrefix;
         Instr_List[idx1].Instr_Rec.BackupDBSuffix        := BackupTemplate.Instr_Rec.BackupDBSuffix;
         Instr_List[idx1].Instr_Rec.BackupDBUser          := BackupTemplate.Instr_Rec.BackupDBUser;
         Instr_List[idx1].Instr_Rec.BackupHostName        := BackupTemplate.Instr_Rec.BackupHostName;
         Instr_List[idx1].Instr_Rec.BackupLocation        := BackupTemplate.Instr_Rec.BackupLocation;
         Instr_List[idx1].Instr_Rec.BackupMsg             := BackupTemplate.Instr_Rec.BackupMsg;
         Instr_List[idx1].Instr_Rec.BackupSMSNumber       := BackupTemplate.Instr_Rec.BackupSMSNumber;
         Instr_List[idx1].Instr_Rec.BackupSMSPass         := BackupTemplate.Instr_Rec.BackupSMSPass;
         Instr_List[idx1].Instr_Rec.BackupSMSUser         := BackupTemplate.Instr_Rec.BackupSMSUser;
         Instr_List[idx1].Instr_Rec.BackupTemplate        := BackupTemplate.Instr_Rec.BackupTemplate;
         Instr_List[idx1].Instr_Rec.BackupViewer          := BackupTemplate.Instr_Rec.BackupViewer;
         Instr_List[idx1].Instr_Rec.BackupSMSProviderName := BackupTemplate.Instr_Rec.BackupSMSProviderName;

         Instr_List[idx1].Active := False;
         Instr_List[idx1].Status := ord(STAT_INACTIVE);
         ThisNodeM.ImageIndex     := 0;

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
         Instr_List[idx1].Instr_Rec.BackupDBPass      := Decode(IniFile.ReadString('Parameters','BackupDBPass',''));
         Instr_List[idx1].Instr_Rec.BackupDBPrefix    := IniFile.ReadString('Parameters','BackupDBPrefix','');
         Instr_List[idx1].Instr_Rec.BackupDBSuffix    := IniFile.ReadString('Parameters','BackupDBSuffix','');
         Instr_List[idx1].Instr_Rec.BackupDBUser      := IniFile.ReadString('Parameters','BackupDBUser','');
         Instr_List[idx1].Instr_Rec.BackupHostName    := IniFile.ReadString('Parameters','BackupHostName','www.bluecrane.cc');
         Instr_List[idx1].Instr_Rec.BackupLocation    := IniFile.ReadString('Parameters','BackupLocation','~');
         Instr_List[idx1].Instr_Rec.BackupSMSNumber   := IniFile.ReadString('Parameters','BackupSMSNumber','');
         Instr_List[idx1].Instr_Rec.BackupSMSPass     := Decode(IniFile.ReadString('Parameters','BackupSMSPass',''));
         Instr_List[idx1].Instr_Rec.BackupSMSUser     := IniFile.ReadString('Parameters','BackupSMSUser','');
         Instr_List[idx1].Instr_Rec.BackupTemplate    := IniFile.ReadString('Parameters','BackupTemplate','&Date@&Time - &BackupType Backup (&CpyName on &HostName)');
         Instr_List[idx1].Instr_Rec.BackupViewer      := IniFile.ReadString('Parameters','BackupViewer','');

         IniFile.Destroy;

//--- Update the Active and Status indicators to indciate that this instruction
//--- is valid and in Waiting state

         Instr_List[idx1].Active := True;
         Instr_List[idx1].Status := ord(STAT_WAITING);
         ThisNodeM.ImageIndex     := 1;

//--- Set the name of the SMS Service Provider

         Instr_List[idx1].Instr_Rec.BackupSMSProviderName := SMSProvider[Instr_List[idx1].Instr_Rec.BackupSMSProvider + 1];

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

      ThisNodeM.SelectedIndex := 3;

      GetNextSlot(idx1);

      tvInstructions.Selected := ThisNodeM;
      ActiveName := tvInstructions.Selected.Text;

      DispLogMsg('++++++ Instruction number ' + (IntToStr(idx1 + 1)) + ' - ''' + Instr_List[idx1].Instruction + ''':');

      if Instr_List[idx1].Active = True then begin

         DispLogMsg('+++++++++ Backups will be taken for ''' + Instr_List[idx1].Instr_Rec.BackupHostName + '[' + Instr_List[idx1].Instr_Rec.BackupDBPrefix + Instr_List[idx1].Instr_Rec.BackupDBSuffix + ']''');
         DispLogMsg('+++++++++ Next Backup will be taken at ' + Instr_List[idx1].NextDate + ' on ' + Instr_List[idx1].NextTime);

         if Instr_List[idx1].Instr_Rec.BackupSMSProvider = 0 then begin

            DispLogMsg('+++++++++ SMS Messaging for ' + Instr_List[idx1].Instruction + ' is inactive');

         end else begin

            if Trim(Instr_List[idx1].Instr_Rec.BackupSMSNumber) <> '' then begin

               if Instr_List[idx1].Instr_Rec.BackupSMSAlways = True then
                  DispLogMsg('+++++++++ SMS will always be sent to ''' + Instr_List[idx1].Instr_Rec.BackupSMSNumber + ''' (Success or Failure) using ''' + Instr_List[idx1].Instr_Rec.BackupSMSProviderName + '''')
               else if Instr_List[idx1].Instr_Rec.BackupSMSSuccess = True then
                  DispLogMsg('+++++++++ SMS Message will be sent to ''' + Instr_List[idx1].Instr_Rec.BackupSMSNumber + ''' after a successful backup using ''' + Instr_List[idx1].Instr_Rec.BackupSMSProviderName + '''')
               else if Instr_List[idx1].Instr_Rec.BackupSMSFailure = True then
                  DispLogMsg('+++++++++ SMS Message will be sent to ''' + Instr_List[idx1].Instr_Rec.BackupSMSNumber + ''' if a backup fails using ''' + Instr_List[idx1].Instr_Rec.BackupSMSProviderName + '''')
               else
                  DispLogMsg('+++++++++ SMS Messages will never be sent');

            end else begin

               DispLogMsg('+++++++++ SMS Messaging is inactive because ''SMS Number: is not specified');

            end;

         end;

      end else begin

         DispLogMsg('+++++++++ No Backups will be taken for ''' + Instr_List[idx1].Instr_Rec.BackupDBPrefix + Instr_List[idx1].Instr_Rec.BackupDBSuffix + ''' - Instruction is inactive/invalid');

      end;

   end;

   if cbxDoSort.Checked = True then
      tvInstructions.AlphaSort;

   DispLogMsg('+++ End of Backup Instructuctions');

   ActiveName := '';

   try
      CfgInstr.Free;
   except
   end;

   try
      InstrTokens.Free;
   except
   end;

end;

//---------------------------------------------------------------------------
// Procedure to Save the Configuration File
//---------------------------------------------------------------------------
procedure TFLPMSBackup.SaveCfg(FileName: string);
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
   FormatSettings.ThousandSeparator := ' ';

   if FileExists(FileName) = true then begin

      LogLines  := TStringList.Create;

      LogLines.LoadFromFile(FileName);

      for idx1 := 0 to LogLines.Count -1 do begin

         LogTokens := TStringList.Create;
         ThisLine  := LogLines.Strings[idx1];

         ExtractStrings(['|'], [], PChar(ThisLine), LogTokens);

         DispLogMsg(LogTokens[0], LogTokens[1], LogTokens[2], LogTokens[3]);
         inc(NumLines);

         LogTokens.Free;

      end;

      LogLines.Free;

   end;

   if (NumLines = 0) then begin
      DispLogMsg(FormatDateTime('yyyy/MM/dd',Now()),FormatDateTime('hh:nn:ss.zzz',Now()), 'Backup', '##New Log File Created');
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
   SaveList.Free;

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

   if ActiveNAme = InstrSel then begin

      lblL05.Caption := 'Backing up Table ''' + Table + ''', Reading up to ' + FloatToStrF(Instr_List[ActiveInstr].Instr_Rec.BackupBlock, ffNumber, 2, 0) + ' records';
      lblL05.Refresh;

   end;

   S1 := 'SELECT * FROM ' + Table + ' LIMIT ' + IntToStr(LimitStart) + ',' + IntToStr(LimitEnd);

   sqlQry1.Close;
   sqlQry1.SQL.Text := S1;

   try

      sqlQry1.Open;

      except on E : Exception do begin

         LastMsg := '*** Error connecting to ''' + Instr_List[ActiveInstr].Instr_Rec.BackupDBPrefix + Instr_List[ActiveInstr].Instr_Rec.BackupDBSuffix + ''' on ''' + Instr_List[ActiveInstr].Instr_Rec.BackupHostName + ''' - ''' + E.Message + '''';
         DBAccessFailed(LastMsg);
         Exit;

      end;

   end;

   if sqlQry1.RecordCount = 0 then
      LimitActive := False;

   Result := true;

end;

//---------------------------------------------------------------------------
// Function to manipulate the TreeView entries
//---------------------------------------------------------------------------
function TFLPMSBackup.AccessTreeView(ThisReq: REC_InstrRecord) : boolean;
var
   Outcome                        : boolean;
   ThisNodeL, ThisNodeS, NextNode : TTreeNode;

begin
   Outcome := True;

   ThisNodeS := tvSmall.Items.GetFirstNode;
   ThisNodeL := tvInstructions.Items.GetFirstNode;
   ThisNodeL := ThisNodeL.GetFirstChild;

   case ThisReq.Request of

      ord(TV_DUPLICATE): begin

         while ThisNodeL <> nil do begin

            if ThisNodeL.Text = ThisReq.InstrName then begin

               Outcome := False;
               break;

            end;

            ThisNodeL := ThisNodeL.GetNextSibling;

         end;

      end;

      ord(TV_REPLACE): begin

         while ThisNodeL <> nil do begin

            if ThisNodeL.Text = ThisReq.InstrName then begin

               ThisNodeL.Text := ThisReq.InstrNewName;
               break;

            end;

            ThisNodeL := ThisNodeL.GetNextSibling;

         end;

         while ThisNodeS <> nil do begin

            if ThisNodeS.Text = ThisReq.InstrName then begin

               ThisNodeS.Text := ThisReq.InstrNewName;
               break;

            end;

            ThisNodeS := ThisNodeS.GetNextSibling;

         end;

      end;

      ord(TV_DELETE): begin

         while ThisNodeL <> nil do begin

            if ThisNodeL.Text = ThisReq.InstrName then begin

               NextNode := ThisNodeL.GetNextSibling;

               if NextNode = nil then begin

                  NextNode := ThisNodeL.GetPrevSibling;

                  if NextNode = nil then
                     NextNode := ThisNodeL.Parent;

               end;

               ThisNodeL.Delete;
               tvInstructions.Selected := NextNode;

               break;

            end;

            ThisNodeL := ThisNodeL.GetNextSibling;

         end;

         while ThisNodeS <> nil do begin

            if ThisNodeS.Text = ThisReq.InstrName then begin

               NextNode := ThisNodeS.GetNextSibling;

               if NextNode = nil then begin

                  NextNode := ThisNodeS.GetPrevSibling;

               end;

               ThisNodeS.Delete;

               if NextNode <> nil then
                  tvSmall.Selected := NextNode;

               break;

            end;

            ThisNodeS := ThisNodeS.GetNextSibling;

         end;

      end;

   end;

   Result := Outcome;

end;

//------------------------------------------------------------------------------
// Function to transform a displayable password into coded characters
//------------------------------------------------------------------------------
function TFLPMSBackup.Encode(PlainStr: string) : string;
var
   idx1   : integer;
   S2     : string;
   S1     : array[1..64] of char;
   Hi, Lo : Word;

begin

   S1   := PlainStr;
   S2   := '';
   idx1 := 1;

   while (S1[idx1] <> #0) do begin

//--- Get copies of the current character

      Hi := Word(S1[idx1]);
      Lo := Word(S1[idx1]);

//--- Move the 4 high bits to the right and mask out the four left bits

      Hi := Hi shr 4;
      Lo := Lo and %00001111;

//--- Turn the Hi and Lo parts into displayable characters

      Hi := Hi or %01000000;
      Lo := Lo or %01000000;

//--- Add them to the result string

      S2 := S2 + char(Hi) + char(Lo);

      inc(idx1);

   end;

   Result := S2;

end;

//------------------------------------------------------------------------------
// Function to transform a coded password into displayable characters
//------------------------------------------------------------------------------
function TFLPMSBackup.Decode(CodedStr: string) : string;
var
   idx1         : integer;
   S2           : string;
   S1           : array[1..64] of char;
   Hi1, Hi2, HL : Word;

begin

   S1   := CodedStr;
   S2   := '';
   idx1 := 1;

   while (S1[idx1] <> #0) do begin

//--- Get copies of the next 2 characters

      Hi1 := Word(S1[idx1]);
      Inc(idx1);
      Hi2 := Word(S1[idx1]);
      Inc(idx1);

//--- Move the 4 low bits of the first to the left and mask the 4 low bits then
//--- mask the 4 high bits of the second

      Hi1 := Hi1 shl 4;
      Hi1 := Hi1 and %11110000;
      Hi2 := Hi2 and %00001111;

//--- Merge the 2 characters

      HL := Hi1 or Hi2;

//--- Add it to the result string

      S2 := S2 + char(HL);

   end;

   Result := S2;

end;

//------------------------------------------------------------------------------
end.

