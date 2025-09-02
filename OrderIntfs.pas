unit OrderIntfs;

interface

uses
  System.Classes;

const
  C_OptionsFilename = 'options.ini';

type
  TOrderValue = (ovNone, ovAccount, ovPartner, ovCaption, ovCategory, ovPrice,
    ovCount, ovTags, ovSum);
  TOrderItem = array [TOrderValue] of string;
  TOrderItems = TArray<TOrderItem>;

  TOrderDictionaryValue = (odvID, odvName);
  TOrderDictionaryItem = array [TOrderDictionaryValue] of string;

  TOrderDictionaryItems = TArray<TOrderDictionaryItem>;

  TOrderCommitMode = (ocmList, ocmRecord);

  // Работа с базой
  IOrderRepository = interface
    ['{266BBAC7-A05A-41F0-8DF0-A6BD69C6E185}']
    // Справочники
    function GetItems(const ADictionary: TOrderValue): TOrderDictionaryItems;

    // Поиск подставляемых значений по имени и контрагенту
    function SearchByName(const AName: string): TOrderItem;
    function SearchByPartner(const AName: string): TOrderItem;
  end;

  // Парсер чека
  IOrderParser = interface
    ['{D6425A62-0485-4B51-B052-B48FFD2ABFFB}']
    procedure InitRegulars(const ARegEx: TStrings);
    function Parse(const AText: string): TOrderItems;
  end;

  // Ищет и подставляет значения в AItems
  IOrderComposer = interface
    ['{3589D967-AFB1-44D9-AF01-25CFFB398CE7}']
    procedure Compose(const AItems: TOrderItems; const ADef, ADefID: TOrderItem);
  end;

  // Сохранение в базу
  IOrderCommit = interface
    ['{6A0F6A68-23EA-4768-9BCF-C9FDFF339FC1}']
    function Commit(const AItems: TOrderItems; const AMode: TOrderCommitMode;
      const AAutoCommit: Boolean; out AReason: string): Boolean;
    function IsSupport: Boolean;
    function GetOnTerminate: TNotifyEvent;
  end;

implementation

end.
