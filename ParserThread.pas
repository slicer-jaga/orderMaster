unit ParserThread;

interface

uses
  OrderIntfs,
  System.Classes, System.SyncObjs;

type
  TParserThread = class(TThread)
  private
    FCR: TCriticalSection;
    FEV: TEvent;
    FStrings: TArray<string>;
    FInfos: TArray<TOrderInfo>;
    FParser: IOrderParser;
    FRegs: TStrings;
    FComposer: IOrderComposer;
    FOnUpdate: TNotifyEvent;
  protected
    procedure Execute; override;

    procedure ParsingProcess;
  public
    constructor Create(const AParser: IOrderParser;
      const AComposer: IOrderComposer; const AOnUpdate: TNotifyEvent);
    destructor Destroy; override;

    procedure AddText(const AText: string; const AInfo: TOrderInfo;
      const ARegs: TStrings);
  end;

implementation

uses
  System.SysUtils;

{ TParserThread }

procedure TParserThread.AddText(const AText: string; const AInfo: TOrderInfo;
  const ARegs: TStrings);
begin
  FCR.Enter;
  FStrings := FStrings + [AText];
  FInfos := FInfos + [AInfo];
  FRegs := ARegs;

  if Length(FStrings) = 1 then
    FEV.SetEvent;

  FCR.Leave;
end;

constructor TParserThread.Create(const AParser: IOrderParser;
  const AComposer: IOrderComposer; const AOnUpdate: TNotifyEvent);
begin
  FCR := TCriticalSection.Create;
  FEV := TEvent.Create(nil, False, False, '');

  FParser := AParser;
  FComposer := AComposer;
  FOnUpdate := AOnUpdate;

  FreeOnTerminate := True;

  inherited Create(False);
end;

destructor TParserThread.Destroy;
begin
  FreeAndNil(FEV);
  FreeAndNil(FCR);
  inherited;
end;

procedure TParserThread.Execute;
begin
  while not Terminated do
  begin
    if FEV.WaitFor(100) = wrSignaled then
      try
        ParsingProcess;
      except
      end;
  end;
end;

procedure TParserThread.ParsingProcess;
var
  lItems: TOrderItems;
  lText: string;
  lInfo: TOrderInfo;
  lRestart: Boolean;
begin
  FCR.Enter;
  lText := FStrings[High(FStrings)];
  lInfo := FInfos[High(FInfos)];
  FStrings := nil;
  FInfos := nil;
  FCR.Leave;

  if Assigned(FRegs) then
  begin
    FParser.InitRegulars(FRegs);
    FRegs := nil;
  end;

  lItems := FParser.Parse(lText);
  if Length(lItems) = 0 then
    Exit;

  FComposer.Compose(lItems, lInfo);

  FCR.Enter;
  lRestart := Length(FStrings) > 0;
  FCR.Leave;

  if lRestart then
    FEV.SetEvent
  else
    TThread.Queue(nil,
      procedure
      begin
        FOnUpdate(TObject(lItems));
      end);
end;

end.
