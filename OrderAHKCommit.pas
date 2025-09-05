unit OrderAHKCommit;

interface

uses
  System.Classes,
  OrderIntfs;

type
  TOrderAHKCommit = class(TInterfacedObject, IOrderCommit)
  private
    FIsSupported: Boolean;
    FProcess: NativeUInt;
    procedure DoTerminate(Sender: TObject);
  protected const
    C_TestFilename = '..\..\hosts\test\order.ahk';
    C_TerminateCode = 9;
    C_Mode: array [Boolean] of string = ('more', 'done');

  var
    FFilename: string;

    function GetItems(const AItems: TOrderItems; const AInfo: TOrderInfo;
      const AMode: TOrderCommitMode): TOrderItems;
  public
    constructor Create;

    function Commit(const AItems: TOrderItems; const AInfo: TOrderInfo;
      const AMode: TOrderCommitMode; const AAutoCommit: Boolean;
      out AReason: string): Boolean;
    function IsSupport: Boolean;
    function GetOnTerminate: TNotifyEvent;
  end;

  TOrderFamily12MaxCommit = class(TOrderAHKCommit)
  protected const
    C_Filename = 'family12Max\order.ahk';
  public
    constructor Create;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows, Winapi.ShellApi;

{ TOrderAHKCommit }

function TOrderAHKCommit.Commit(const AItems: TOrderItems;
  const AInfo: TOrderInfo; const AMode: TOrderCommitMode;
  const AAutoCommit: Boolean; out AReason: string): Boolean;
var
  lInfo: TShellExecuteInfo;
  lExitCode: Cardinal;
  i: Integer;
  lParam: string;
  lItems: TOrderItems;
begin
  Result := False;

  lExitCode := 0;

  FillChar(lInfo, SizeOf(lInfo), 0);
  lInfo.cbSize := SizeOf(lInfo);
  lInfo.lpFile := PChar(FFilename);
  lInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
  lInfo.nShow := SW_HIDE;

  lItems := GetItems(AItems, AInfo, AMode);

  for i := 0 to High(lItems) do
  begin
    lParam := Format('"%s;%s;%s;%s;%s;%s;%s" %s', [C_Mode[i = High(lItems)],
      lItems[i][ovAccount], lItems[i][ovCategory], lItems[i][ovPartner],
      lItems[i][ovSum], lItems[i][ovCaption], lItems[i][ovTags],
      BoolToStr(AAutoCommit)]);

    lInfo.lpParameters := PChar(lParam);

    Result := ShellExecuteEx(@lInfo);
    if not Result then
    begin
      AReason := SysErrorMessage(GetLastError);
      Exit;
    end;

    FProcess := lInfo.hProcess;
    WaitForSingleObject(FProcess, INFINITE);

    GetExitCodeProcess(FProcess, lExitCode);
    if lExitCode = C_TerminateCode then
      AReason := 'Процесс был прерван пользователем.';

    Result := lExitCode = 0;
    if not Result then
      Exit;
  end;
end;

constructor TOrderAHKCommit.Create;
begin
  FFilename := C_TestFilename;
  FIsSupported := FileExists(FFilename);
end;

procedure TOrderAHKCommit.DoTerminate(Sender: TObject);
begin
  TerminateProcess(FProcess, C_TerminateCode);
end;

function TOrderAHKCommit.GetItems(const AItems: TOrderItems;
  const AInfo: TOrderInfo; const AMode: TOrderCommitMode): TOrderItems;
var
  i: Integer;
begin
  case AMode of
    ocmList:
      Result := AItems;
    ocmRecord:
      begin
        SetLength(Result, 1);
        Result[0][ovCaption] := '';
        for i := 0 to High(AItems) do
        begin
          if Result[0][ovCaption] <> '' then
            Result[0][ovCaption] := Result[0][ovCaption] + ', ';
          Result[0][ovCaption] := Result[0][ovCaption] + AItems[i][ovCaption] +
            ' ' + AItems[i][ovSum];
        end;

        if AInfo.Sum <> 0 then
          Result[0][ovSum] := CurrToStr(AInfo.Sum);
      end;
  end;
end;

function TOrderAHKCommit.GetOnTerminate: TNotifyEvent;
begin
  Result := DoTerminate;
end;

function TOrderAHKCommit.IsSupport: Boolean;
begin
  Result := FIsSupported;
end;

{ TOrderFamily12MaxCommit }

constructor TOrderFamily12MaxCommit.Create;
begin
  inherited Create;
  FFilename := C_Filename;
  FIsSupported := FileExists(FFilename);
end;

end.
