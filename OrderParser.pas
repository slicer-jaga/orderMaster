unit OrderParser;

interface

uses
  OrderIntfs,
  System.SysUtils, System.Classes, System.RegularExpressions;

type
  TOrderParser = class(TInterfacedObject, IOrderParser)
  private
    FRegs: TArray<TRegEx>;
    FNameReg: TRegEx;
    FGroupCounts: TArray<Integer>;
  public
    constructor Create;

    procedure InitRegulars(const ARegEx: TStrings);

    function Parse(const AText: string): TOrderItems;
  end;

implementation

{ TOrderParser }

constructor TOrderParser.Create;
begin
  FNameReg := TRegEx.Create('<[\w]+(\d+)>');
end;

procedure TOrderParser.InitRegulars(const ARegEx: TStrings);
var
  lReg: string;
  lRegEx: TRegEx;
  lMatch: TMatch;
  lMax, lVal: Integer;
begin
  FRegs := nil;
  FGroupCounts := nil;
  for lReg in ARegEx do
  begin
    try
      lRegEx := TRegEx.Create(lReg);
      FRegs := FRegs + [lRegEx];

      lMax := 0;
      for lMatch in FNameReg.Matches(lReg) do
      begin
        if lMatch.Success then
        begin
          lVal := StrToIntDef(lMatch.Groups[1].Value, 0);
          if lVal > lMax then
            lMax := lVal;
        end;
      end;

      FGroupCounts := FGroupCounts + [lMax];

    except
    end;
  end;
end;

function TOrderParser.Parse(const AText: string): TOrderItems;
var
  lMatches: TMatchCollection;
  lMatch: TMatch;
  lItem: TOrderItem;
  lVal: TOrderValue;
  lTotal: string;
  lTotalVal, lItemVal, lSum, lPrice, lCount: Currency;
  i, j: Integer;

  function _getValue(const AName: string; const AIdx: Integer): string;
  var
    lGroup: string;
  begin
    try
      if AIdx > 0 then
        lGroup := AIdx.ToString
      else
        lGroup := '';

      Result := lMatch.Groups[AName + lGroup].Value;
    except
    end;
  end;

begin
  Result := nil;
  for i := 0 to High(FRegs) do
  begin
    lMatches := FRegs[i].Matches(AText);
    lSum := 0;
    if lMatches.Count > 0 then
    begin
      for lMatch in lMatches do
      begin
        for lVal := Low(TOrderValue) to High(TOrderValue) do
          lItem[lVal] := '';

        for j := 0 to FGroupCounts[i] do
        begin
          if lItem[ovCaption] = '' then
            lItem[ovCaption] := _getValue('name', j);
          if lItem[ovCount] = '' then
            lItem[ovCount] := _getValue('count', j).Replace(' ', '', [rfReplaceAll]);
          if lItem[ovPrice] = '' then
            lItem[ovPrice] := _getValue('price', j).Replace(' ', '', [rfReplaceAll]);
          if lItem[ovSum] = '' then
            lItem[ovSum] := _getValue('sum', j).Replace(' ', '', [rfReplaceAll]);
        end;

        lItemVal := 0;
        if not TryStrToCurr(lItem[ovSum], lItemVal) then
        begin
          if (TryStrToCurr(lItem[ovPrice], lPrice) and
            TryStrToCurr(lItem[ovCount], lCount)) then
            lItemVal := lPrice * lCount;
        end;

        lTotal := _getValue('total', 0).Replace(' ', '', [rfReplaceAll]);
        if (lTotal <> '') and (lItem[ovSum] = '') and
          TryStrToCurr(lTotal, lTotalVal) then
        begin
          lItem[ovPrice] := CurrToStr(lTotalVal - lSum);
          lItem[ovSum] := CurrToStr(lTotalVal - lSum);
          if lItem[ovCaption] = '' then
          begin
            if lTotalVal - lSum > 0 then
              lItem[ovCaption] := 'Сервис'
            else
              lItem[ovCaption] := 'Скидка';
          end;
        end
        else
          lSum := lSum + lItemVal;

        Result := Result + [lItem];
      end;

      Exit;
    end;
  end;
end;

end.
