unit ProgressForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.WinXCtrls, Vcl.StdCtrls;

type
  TfrmProgress = class(TForm)
    lblMsg: TLabel;
    ActivityIndicator: TActivityIndicator;
    btnCancel: TButton;
    dlgTeminate: TTaskDialog;
    procedure btnCancelClick(Sender: TObject);
  private
    FTerminate: Boolean;
    FOnTerminate: TNotifyEvent;
  public
    procedure StartProgress;
    procedure EndProgress;

    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

var
  frmProgress: TfrmProgress;

implementation

uses
  System.UITypes;

{$R *.dfm}
{ TfrmProgress }

procedure TfrmProgress.btnCancelClick(Sender: TObject);
begin
  if dlgTeminate.Execute and (dlgTeminate.ModalResult = mrYes) then
    Self.BeginInvoke(
      procedure
      begin
        OnTerminate(nil);
      end);
end;

procedure TfrmProgress.EndProgress;
begin
  ActivityIndicator.Animate := False;
  Hide;
end;

procedure TfrmProgress.StartProgress;
begin
  FTerminate := False;
  ActivityIndicator.Animate := True;
  Show;
end;

end.
