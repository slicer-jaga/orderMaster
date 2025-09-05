unit OrderFileRepository;

interface

uses
  OrderIntfs;

type
  TOrderFileRepository = class(TInterfacedObject, IOrderRepository)
  private
    FDir: string;
    FItems: array [TOrderValue] of TOrderDictionaryItems;
  protected const
    C_Names: array [TOrderValue] of string = ('', 'account.txt', 'partner.txt',
      '', 'category.txt', '', '', 'tags.txt', '', '');
  public
    constructor Create(const ADirectory: string);
    function GetItems(const ADictionary: TOrderValue): TOrderDictionaryItems;
    function SearchByName(const AName: string): TOrderItem;
    function SearchByPartner(const AName: string): TOrderItem;
    function SuggestTags(const AName: string; const APartnerID: string;
      const ACategoryID: string): System.TArray<System.string>;

  end;

implementation

uses
  System.SysUtils, System.Classes;

{ TOrderFileRepository }

constructor TOrderFileRepository.Create(const ADirectory: string);
begin
  FDir := IncludeTrailingPathDelimiter(ADirectory);
end;

function TOrderFileRepository.GetItems(const ADictionary: TOrderValue)
  : TOrderDictionaryItems;
var
  lReader: TStreamReader;
  lValues: TArray<string>;
  lItem: TOrderDictionaryItem;
begin
  if Length(FItems[ADictionary]) = 0 then
  begin
    lReader := TStreamReader.Create(FDir + C_Names[ADictionary]);
    try
      while not lReader.EndOfStream do
      begin
        lValues := lReader.ReadLine.Split([';']);

        lItem[odvID] := lValues[0];
        lItem[odvName] := lValues[1];

        FItems[ADictionary] := FItems[ADictionary] + [lItem];
      end;
    finally
      lReader.Free;
    end;
  end;
  Result := FItems[ADictionary];
end;

function TOrderFileRepository.SearchByName(const AName: string): TOrderItem;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function TOrderFileRepository.SearchByPartner(const AName: string): TOrderItem;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

function TOrderFileRepository.SuggestTags(const AName, APartnerID,
  ACategoryID: string): System.TArray<System.string>;
begin
  Result := nil;
end;

end.
