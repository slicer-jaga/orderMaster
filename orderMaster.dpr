program orderMaster;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {frmMain},
  OrderIntfs in 'OrderIntfs.pas',
  OrderFileRepository in 'OrderFileRepository.pas',
  OrderParser in 'OrderParser.pas',
  OrderComposer in 'OrderComposer.pas',
  OrderAHKCommit in 'OrderAHKCommit.pas',
  OrderFamily12MaxRepository in 'OrderFamily12MaxRepository.pas' {Family12MaxRepository: TDataModule},
  ParserThread in 'ParserThread.pas',
  OrderUtils in 'OrderUtils.pas',
  ProgressForm in 'ProgressForm.pas' {frmProgress};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
