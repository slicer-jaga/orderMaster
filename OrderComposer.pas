unit OrderComposer;

interface

uses
  OrderIntfs;

type
  TOrderComposer = class(TInterfacedObject, IOrderComposer)
  private
    FRepo: IOrderRepository;
    FByName, FByPartner: TOrderItem;
    function TryGetCategory(const AItem: TOrderItem): string;
    function TryGetTags(const AItem: TOrderItem): string;
  public
    constructor Create(const ARepository: IOrderRepository);

    procedure Compose(const AItems: TOrderItems; const AInfo: TOrderInfo);
  end;

implementation

uses
  System.SysUtils;

{ TOrderComposer }

procedure TOrderComposer.Compose(const AItems: TOrderItems;
  const AInfo: TOrderInfo);
var
  i: Integer;
  lVal: TOrderValue;
begin
  for i := Low(AItems) to High(AItems) do
  begin
    for lVal := Low(TOrderValue) to High(TOrderValue) do
      if (AItems[i][lVal] = '') and (AInfo.Def[lVal] <> '') then
        AItems[i][lVal] := AInfo.Def[lVal];

    if (AItems[i][ovCategory] = '') or (AItems[i][ovTags] = '') then
    begin
      FByName := FRepo.SearchByName(AItems[i][ovCaption]);
      FByPartner := FRepo.SearchByPartner(AInfo.DefID[ovPartner]);

      if AItems[i][ovCategory] = '' then
        AItems[i][ovCategory] := TryGetCategory(AItems[i]);

      if AItems[i][ovTags] = '' then
        AItems[i][ovTags] := TryGetTags(AItems[i]);

      if (AItems[i][ovPartner] = '') and (FByName[ovPartner] <> '') then
        AItems[i][ovPartner] := FByName[ovPartner]
    end;

    if (AItems[i][ovCount] <> '') and (AItems[i][ovCount] <> '1') then
      AItems[i][ovCaption] := AItems[i][ovCaption] + ' x' + AItems[i][ovCount];
  end;
end;

constructor TOrderComposer.Create(const ARepository: IOrderRepository);
begin
  FRepo := ARepository;
end;

function TOrderComposer.TryGetCategory(const AItem: TOrderItem): string;
begin
  if FByName[ovCategory] <> '' then
    Result := FByName[ovCategory]
  else if FByPartner[ovCategory] <> '' then
    Result := FByPartner[ovCategory]
  else
    Result := '';
end;

function TOrderComposer.TryGetTags(const AItem: TOrderItem): string;
begin
  if FByName[ovTags] <> '' then
    Result := FByName[ovTags]
  else if FByPartner[ovTags] <> '' then
    Result := FByPartner[ovTags]
  else
    Result := '';
end;

end.
