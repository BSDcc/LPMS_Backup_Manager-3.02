program LPMS_Backup;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ldBackupApp, ldbackuptemplate;

{$R *.res}

begin
   RequireDerivedFormResource := True;
  Application.Scaled:=True;
   Application. Title :='Backup Manager';
   Application.Initialize;
   Application. CreateForm( TFLPMSBackup, FLPMSBackup);
   Application.Run;
end.

