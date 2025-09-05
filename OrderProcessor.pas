unit OrderProcessor;

interface

type
  TOrderProcessor = class
  public
    class function CalSummary(const APrice, ACount, ASalePerc: Currency)
      : Currency;
  end;

implementation

{ TOrderProcessor }

class function TOrderProcessor.CalSummary(const APrice, ACount,
  ASalePerc: Currency): Currency;
begin
  Result := (APrice * (1 - ASalePerc / 100)) * ACount;
end;

end.
