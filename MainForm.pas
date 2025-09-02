unit MainForm;

interface

uses
  OrderIntfs,
  OrderParser,
  OrderComposer,
  ParserThread,
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.Threading,
  Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids,
  AdvObj, BaseGrid, AdvGrid, Vcl.ExtCtrls, System.Actions, Vcl.ActnList,
  FormSize, System.ImageList, Vcl.ImgList, Vcl.ComCtrls, Vcl.ToolWin,
  Vcl.WinXCtrls, Vcl.Menus, AdvGridActns, AdvUtil;

type
  TfrmMain = class(TForm)
    mmoParser: TMemo;
    pnlParser: TPanel;
    pnlOrder: TPanel;
    splOrder: TSplitter;
    alMain: TActionList;
    actParse: TAction;
    pnlTop: TPanel;
    cbxAccount: TComboBox;
    cbxPartner: TComboBox;
    cbxCategory: TComboBox;
    lblAccount: TLabel;
    lblPartner: TLabel;
    lblCategory: TLabel;
    actCommit: TAction;
    ImageList: TImageList;
    pnlMainPage: TPanel;
    TaskDialog: TTaskDialog;
    mmoRegs: TMemo;
    splRegs: TSplitter;
    pnlRegEx: TPanel;
    lblRegEx: TLabel;
    lblOrder: TLabel;
    pnlCheckOrder: TPanel;
    pnlToolbar: TPanel;
    btnCommit: TButton;
    actListAdd: TAction;
    actListDel: TAction;
    popOrder: TPopupMenu;
    miListAdd: TMenuItem;
    miListDel: TMenuItem;
    btnCommitOne: TButton;
    actCommitOneRecord: TAction;
    sgOrder: TAdvStringGrid;
    lblList: TLabel;
    chkAutoInput: TCheckBox;
    procedure actParseExecute(Sender: TObject);
    procedure mmoParserChange(Sender: TObject);
    procedure ChangeDefault(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actCommitExecute(Sender: TObject);
    procedure sgOrderEditingDone(Sender: TObject);
    procedure mmoRegsChange(Sender: TObject);
    procedure actListAddExecute(Sender: TObject);
    procedure actListDelExecute(Sender: TObject);
    procedure DoUpdateControls(Sender: TObject);
    procedure sgOrderGetDisplText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure actCommitOneRecordExecute(Sender: TObject);
    procedure sgOrderGetEditorType(Sender: TObject; ACol, ARow: Integer;
      var AEditor: TEditorType);
    procedure sgOrderKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FRepo: IOrderRepository;
    FParser: IOrderParser;
    FComposer: IOrderComposer;
    FCommit: IOrderCommit;

    FThread: TParserThread;

    FDef: TOrderItem;
    FDefID: TOrderItem;

    FModified: Boolean;
    FNeedUpdateRegs: Boolean;
    FIniFilename: string;

    procedure LoadOptions;
    procedure SaveOptions;

    function GetItems: TOrderItems;

    procedure UpdateControls(Sender: TObject);
    procedure DoAfterParse(Sender: TObject);

    procedure InitOrder;
    procedure InitDefault(const ACombo: TComboBox; const AValue: TOrderValue);
    procedure Parse;
    procedure Commit(const AMode: TOrderCommitMode);

    procedure StartProgress(const AOnTerminate: TNotifyEvent);
    procedure EndProgress;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  System.IniFiles, System.UITypes,
  ProgressForm,
  OrderFileRepository,
  OrderFamily12MaxRepository,
  OrderAHKCommit,
  OrderUtils;

const
  C_OrderCaptions: array [TOrderValue] of string = ('№', 'Счет', 'Контрагент',
    'Название', 'Категория', 'Цена', 'Количество', 'Метки', 'Сумма');
  C_OrderWidth: array [TOrderValue] of Integer = (20, 128, 128, 610, 128, 80,
    80, 480, 80);

  { TfrmMain }

procedure TfrmMain.actCommitExecute(Sender: TObject);
begin
  Commit(ocmList);
end;

procedure TfrmMain.actCommitOneRecordExecute(Sender: TObject);
begin
  Commit(ocmRecord);
end;

procedure TfrmMain.actListAddExecute(Sender: TObject);
begin
  sgOrder.RowCount := sgOrder.RowCount + 1;
  UpdateControls(Sender);
end;

procedure TfrmMain.actListDelExecute(Sender: TObject);
begin
  sgOrder.RowCount := sgOrder.RowCount - 1;
  UpdateControls(Sender);
end;

procedure TfrmMain.actParseExecute(Sender: TObject);
begin
  Parse;
end;

procedure TfrmMain.ChangeDefault(Sender: TObject);
var
  j: Integer;
begin
  if TComboBox(Sender).ItemIndex > -1 then
  begin
    FDef[TOrderValue(TComponent(Sender).Tag)] := TComboBox(Sender)
      .Items[TComboBox(Sender).ItemIndex];
    FDefID[TOrderValue(TComponent(Sender).Tag)] :=
      NativeInt(TComboBox(Sender).Items.Objects[TComboBox(Sender).ItemIndex]
      ).ToString;
  end
  else
  begin
    FDef[TOrderValue(TComponent(Sender).Tag)] := TComboBox(Sender).Text;
    FDefID[TOrderValue(TComponent(Sender).Tag)] := '';
  end;

  for j := 1 to sgOrder.RowCount - 1 do
    if sgOrder.Cells[TComponent(Sender).Tag, j] = '' then
      sgOrder.Cells[TComponent(Sender).Tag, j] := FDef[TOrderValue(TComponent(Sender).Tag)];
end;

procedure TfrmMain.Commit(const AMode: TOrderCommitMode);
begin
  TTask.Run(
    procedure
    var
      lRes: Boolean;
      lReason: string;
      lItems: TOrderItems;
    begin
      Self.BeginInvoke(
        procedure
        begin
          StartProgress(FCommit.GetOnTerminate);
        end);

      lItems := GetItems;
      lRes := FCommit.Commit(lItems, AMode, chkAutoInput.Checked, lReason);

      Self.BeginInvoke(
        procedure
        begin
          EndProgress;

          if lRes then
          begin
            TaskDialog.MainIcon := tdiInformation;
            TaskDialog.Text := 'Внесение данных завершено.';
          end
          else
          begin
            TaskDialog.MainIcon := tdiWarning;
            TaskDialog.Text := Format('При внесении данных возникла ошибка:' +
              sLineBreak + '%s', [lReason]);
          end;

          TaskDialog.Execute;

        end);
    end);
end;

constructor TfrmMain.Create(AOwner: TComponent);
begin
  inherited;

{$IFDEF DEBUG}
  // FRepo := TOrderFileRepository.Create('..\..\hosts\test');
  FRepo := TFamily12MaxRepository.Create(Self);
  FCommit := TOrderAHKCommit.Create;
{$ELSE}
  FRepo := TFamily12MaxRepository.Create(Self);
  // FRepo := TOrderFileRepository.Create('familyMax12');
  FCommit := TOrderFamily12MaxCommit.Create;
{$ENDIF}
  FParser := TOrderParser.Create;
  FComposer := TOrderComposer.Create(FRepo);

  InitOrder;

  LoadOptions;

  FThread := TParserThread.Create(FParser, FComposer, DoAfterParse);

{$IFDEF DEBUG}
  mmoParserChange(nil);
{$ENDIF}
end;

procedure TfrmMain.DoAfterParse(Sender: TObject);
var
  i: Integer;
  lCol: TOrderValue;
  lItems: TOrderItems;
begin
  lItems := TOrderItems(Sender);

  sgOrder.RowCount := Length(lItems) + 1;

  for i := 0 to High(lItems) do
  begin
    for lCol := ovAccount to High(TOrderValue) do
      sgOrder.Cells[Integer(lCol), i + 1] := lItems[i][lCol];
    sgOrder.Cells[Integer(TOrderValue.ovNone), i + 1] := (i + 1).ToString;
  end;

  FModified := True;
  UpdateControls(nil);
end;

procedure TfrmMain.DoUpdateControls(Sender: TObject);
begin
  UpdateControls(Sender);
end;

procedure TfrmMain.StartProgress(const AOnTerminate: TNotifyEvent);
begin
  pnlMainPage.Enabled := False;
  alMain.State := asSuspended;

  if not Assigned(frmProgress) then
    frmProgress := TfrmProgress.Create(Self);

  frmProgress.OnTerminate := AOnTerminate;
  frmProgress.StartProgress;
end;

procedure TfrmMain.EndProgress;
begin
  frmProgress.EndProgress;

  pnlMainPage.Enabled := True;
  alMain.State := asNormal;
  Screen.Cursor := crDefault;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  SaveOptions;
  FThread.Terminate;
  FThread := nil;
end;

function TfrmMain.GetItems: TOrderItems;
var
  i: Integer;
  lCol: TOrderValue;
begin
  SetLength(Result, sgOrder.RowCount - 1);
  for i := 1 to sgOrder.RowCount - 1 do
    for lCol := Low(TOrderValue) to High(TOrderValue) do
    begin
      Result[i - 1][lCol] := sgOrder.Cells[Integer(lCol), i];
      if Result[i - 1][lCol] = '' then
        Result[i - 1][lCol] := FDef[lCol];
    end;
end;

procedure TfrmMain.InitDefault(const ACombo: TComboBox;
const AValue: TOrderValue);
var
  lItems: TOrderDictionaryItems;
  lItem: TOrderDictionaryItem;
begin
  ACombo.Tag := NativeInt(AValue);
  lItems := FRepo.GetItems(AValue);
  ACombo.Items.BeginUpdate;
  ACombo.Items.Add('');
  for lItem in lItems do
    ACombo.Items.AddObject(lItem[odvName], TObject(lItem[odvID].ToInteger));
  ACombo.Items.EndUpdate;
end;

procedure TfrmMain.InitOrder;
var
  lCol: TOrderValue;
begin
  FIniFilename := C_OptionsFilename;

  sgOrder.RowCount := 2;

  sgOrder.ColCount := Integer(High(TOrderValue)) + 1;
  for lCol := Low(TOrderValue) to High(TOrderValue) do
  begin
    sgOrder.ColWidths[Integer(lCol)] := C_OrderWidth[lCol];
    sgOrder.Cells[Integer(lCol), 0] := C_OrderCaptions[lCol];
  end;

  InitDefault(cbxAccount, ovAccount);
  InitDefault(cbxPartner, ovPartner);
  InitDefault(cbxCategory, ovCategory);

{$IFNDEF DEBUG}
  mmoParser.Clear;
{$ENDIF}
  UpdateControls(nil);
end;

procedure TfrmMain.LoadOptions;
var
  lIni: TMemIniFile;
  lTmp: TStringList;
  lLine: string;
  i: Integer;
begin
  lIni := TMemIniFile.Create(FIniFilename);
  lTmp := TStringList.Create;
  try
    LoadFormPlacementEx(lIni, Self, 'MainForm');

    for i := 0 to sgOrder.ColCount - 1 do
      sgOrder.ColWidths[i] := lIni.ReadInteger('Orders', 'Col' + i.ToString,
        C_OrderWidth[TOrderValue(i)]);

    mmoRegs.Lines.BeginUpdate;
    mmoRegs.Lines.Clear;
    if lIni.SectionExists('RegEx') then
    begin
      lIni.ReadSection('RegEx', lTmp);
      for lLine in lTmp do
        mmoRegs.Lines.Add(lIni.ReadString('RegEx', lLine, ''));
    end
    else
    begin
      mmoRegs.Lines.Add
        ('(?m)^(?<name>.+)\R(?<count>\d+).*$\R^(?<price>\d+).*$\R^(?<sum>\d+)|Итого\R(?<total>[\d ]+)');
      mmoRegs.Lines.Add
        ('(?m)^(?<name>.+?)\s*\R(?<count>\d+)\s*x\s*(?P<price>\d+)\s*руб\.|Итого\s(?<total>[\d ]+)');
      mmoRegs.Lines.Add('(?<name>.+)\s+(?<sum>\d+)');
    end;
    FNeedUpdateRegs := True;

    mmoRegs.Lines.EndUpdate;

    chkAutoInput.Checked := lIni.ReadBool('defaults', 'autoCommit', False);

    // cbxAccount.ItemIndex := cbxAccount.Items.IndexOf(lIni.ReadString('Defaults',
    // 'Account', ''));
    // ChangeDefault(cbxAccount);
    // cbxPartner.ItemIndex := cbxPartner.Items.IndexOf(lIni.ReadString('Defaults',
    // 'Partner', ''));
    // ChangeDefault(cbxPartner);
    // cbxCategory.ItemIndex := cbxCategory.Items.IndexOf
    // (lIni.ReadString('Defaults', 'Category', ''));
    // ChangeDefault(cbxCategory);

    pnlParser.Height := lIni.ReadInteger('BottomPanel', 'Height',
      pnlParser.Height);
    pnlRegEx.Width := lIni.ReadInteger('RegPanel', 'Width', mmoRegs.Width);
  finally
    lTmp.Free;
    lIni.Free;
  end;
end;

procedure TfrmMain.SaveOptions;
var
  lIni: TMemIniFile;
  i: Integer;
begin
  lIni := TMemIniFile.Create(FIniFilename);
  try
    SaveFormPlacement(lIni, Self, 'MainForm');

    for i := 0 to sgOrder.ColCount - 1 do
      lIni.WriteInteger('Orders', 'Col' + i.ToString, sgOrder.ColWidths[i]);

    for i := 0 to mmoRegs.Lines.Count - 1 do
      lIni.WriteString('RegEx', i.ToString, mmoRegs.Lines[i]);

    lIni.WriteString('defaults', 'Account', cbxAccount.Text);
    lIni.WriteString('defaults', 'Partner', cbxPartner.Text);
    lIni.WriteString('defaults', 'Category', cbxCategory.Text);

    lIni.WriteBool('defaults', 'autoCommit', chkAutoInput.Checked);

    lIni.WriteInteger('BottomPanel', 'Height', pnlParser.Height);
    lIni.WriteInteger('RegPanel', 'Width', pnlRegEx.Width);

    lIni.UpdateFile;
  finally
    lIni.Free;
  end;
end;

procedure TfrmMain.mmoParserChange(Sender: TObject);
begin
  Parse;
end;

procedure TfrmMain.mmoRegsChange(Sender: TObject);
begin
  FNeedUpdateRegs := True;
end;

procedure TfrmMain.Parse;
var
  lText: string;
  lRegs: TStrings;
begin
  if FNeedUpdateRegs then
  begin
    FNeedUpdateRegs := False;
    lRegs := mmoRegs.Lines;
  end
  else
    lRegs := nil;

  lText := mmoParser.Lines.Text;

  FThread.AddText(lText, FDef, FDefID, lRegs);
end;

procedure TfrmMain.sgOrderEditingDone(Sender: TObject);
begin
  UpdateControls(nil);
end;

procedure TfrmMain.sgOrderGetDisplText(Sender: TObject; ACol, ARow: Integer;
var Value: string);
begin
  if (ARow > 0) and (TOrderValue(ACol) = ovNone) then
    Value := ARow.ToString;
end;

procedure TfrmMain.sgOrderGetEditorType(Sender: TObject; ACol, ARow: Integer;
var AEditor: TEditorType);
var
  lItem: TOrderDictionaryItem;
  lItems: TOrderDictionaryItems;
begin
  case TOrderValue(ACol) of
    ovAccount, ovPartner, ovCategory:
      begin
        AEditor := edComboEdit;

        sgOrder.ClearComboString;
        sgOrder.Combobox.DropDownCount := cbxAccount.DropDownCount;
        sgOrder.Combobox.DropWidth := cbxAccount.Width;

        lItems := FRepo.GetItems(TOrderValue(ACol));
        for lItem in lItems do
          sgOrder.AddComboString(lItem[odvName]);
      end;
  end;
end;

procedure TfrmMain.sgOrderKeyDown(Sender: TObject; var Key: Word;
Shift: TShiftState);
begin
  if Shift = [ssCtrl] then
  begin
    if Key = Ord('C') then
      sgOrder.CopySelectionToClipboard;
    if Key = Ord('V') then
      sgOrder.PasteSelectionFromClipboard;
  end;

  case Key of
    VK_DELETE:
      sgOrder.ClearSelection;
    VK_INSERT:
      begin
        if ssCtrl in Shift then
          sgOrder.CopySelectionToClipboard;
        if ssShift in Shift then
          sgOrder.PasteSelectionFromClipboard;
      end;
  end;
end;

procedure TfrmMain.UpdateControls(Sender: TObject);
begin
  actCommit.Enabled := FCommit.IsSupport;
  actListDel.Enabled := sgOrder.Focused and (sgOrder.RowCount > 2);
end;

end.
