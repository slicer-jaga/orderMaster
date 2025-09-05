unit OrderFamily12MaxRepository;

interface

uses
  OrderIntfs,
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.VCLUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FireDAC.Phys.IBBase, Vcl.Dialogs;

type
  TFamily12MaxRepository = class(TDataModule, IOrderRepository)
    Testfamily12maxConnection: TFDConnection;
    slAccount: TFDQuery;
    slPartner: TFDQuery;
    slTags: TFDQuery;
    slCategory: TFDQuery;
    slSearchByName: TFDQuery;
    slSearchByPartner: TFDQuery;
    FDPhysFBDriverLink: TFDPhysFBDriverLink;
    slCategoryCount: TFDQuery;
    dlgDB: TOpenDialog;
    slSuggestTags: TFDQuery;
  private
    FDBLastWrite: TDateTime;
    FSrcFilename, FDstFilename: string;
    FCatCount: Integer;

    procedure InsertCategoryToSQL;
    function GetCatCount: Integer;
    function GetCategory(const AQuery: TFDQuery; const AIndex: Integer): string;
    function GetCharCount(const AStr: string; const AChar: Char): Integer;
    function GetItemsFromSQL(const AQuery: TFDQuery): TOrderDictionaryItems;
    function GetCategoryItemsFromSQL(const AQuery: TFDQuery)
      : TOrderDictionaryItems;

    procedure CheckRepo;

    property CatCount: Integer read GetCatCount;
  protected const
    C_MinName = 6;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetItems(const ADictionary: TOrderValue): TOrderDictionaryItems;

    function SearchByName(const AName: string): TOrderItem;
    function SearchByPartner(const AName: string): TOrderItem;

    function SuggestTags(const AName, APartnerID, ACategoryID: string)
      : TArray<string>;
  end;

var
  Family12MaxRepository: TFamily12MaxRepository;

implementation

uses
  System.IniFiles, System.Generics.Collections, System.IOUtils,
  System.Generics.Defaults,
  System.RegularExpressions,
  Vcl.Forms;

{%CLASSGROUP 'Vcl.Controls.TControl'}
{$R *.dfm}
{ TdmFmaily12Max }

procedure TFamily12MaxRepository.CheckRepo;
var
  lTime: TDateTime;
begin
  // Обновляем копию, при изменении
  lTime := TFile.GetLastWriteTime(FSrcFilename);
  if lTime > FDBLastWrite then
  begin
    Testfamily12maxConnection.Connected := False;
    TFile.Copy(FSrcFilename, FDstFilename, True);
    Testfamily12maxConnection.Connected := True;
    FDBLastWrite := lTime;
  end;
end;

constructor TFamily12MaxRepository.Create(AOwner: TComponent);
{$IFNDEF DEBUG}
var
  lIni: TMemIniFile;
{$ENDIF}
begin
  inherited;

  Testfamily12maxConnection.ConnectionDefName := '';
  Testfamily12maxConnection.Params.DriverID := 'FB';
  Testfamily12maxConnection.Params.UserName := 'sysdba';
  Testfamily12maxConnection.Params.Password := 'system';

{$IFDEF DEBUG}
  FSrcFilename := '..\..\hosts\test\test.fdb';
{$ELSE}
  lIni := TMemIniFile.Create('family12Max\options.ini');
  try
    FSrcFilename := lIni.ReadString('main', 'db', '');

    if not FileExists(FSrcFilename) then
    begin
      if dlgDB.Execute then
      begin
        FSrcFilename := dlgDB.FileName;
        lIni.WriteString('main', 'db', FSrcFilename);
        lIni.UpdateFile;
      end
      else
        Application.Terminate;
    end;

  finally
    lIni.Free;
  end;
{$ENDIF}
  // На всякий случай, работаем с копией базы
  FDstFilename := TPath.GetDirectoryName(FSrcFilename) + PathDelim + '~' +
    TPath.GetFileName(FSrcFilename);
  Testfamily12maxConnection.Params.Database := FDstFilename;
end;

destructor TFamily12MaxRepository.Destroy;
begin
  if FileExists(FDstFilename) then
  begin
    Testfamily12maxConnection.Connected := False;
    TFile.Delete(FDstFilename);
  end;

  inherited;
end;

function TFamily12MaxRepository.GetCatCount: Integer;
begin
  if FCatCount <> 0 then
    Exit(FCatCount);

  CheckRepo;

  FCatCount := 1;

  slCategoryCount.Open;
  slCategoryCount.First;
  while not slCategoryCount.Eof do
  begin
    FCatCount := slCategoryCount.Fields[0].Value - 1;

    slCategoryCount.Next;
  end;

  InsertCategoryToSQL;
  Result := FCatCount;
end;

function TFamily12MaxRepository.GetCategory(const AQuery: TFDQuery;
  const AIndex: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := AIndex to AIndex + CatCount - 1 do
  begin
    if AQuery.Fields[i].Text = '' then
      Break;
    if Result <> '' then
      Result := Result + ': ';
    Result := Result + AQuery.Fields[i].Text;
  end;
end;

function TFamily12MaxRepository.GetCategoryItemsFromSQL(const AQuery: TFDQuery)
  : TOrderDictionaryItems;
var
  lItem: TOrderDictionaryItem;
begin
  GetCatCount;

  // Похоже, что в Firebird нет CONCAT_WS
  AQuery.Open;
  AQuery.First;
  while not AQuery.Eof do
  begin
    lItem[odvID] := AQuery.Fields[0].Text;
    lItem[odvName] := GetCategory(AQuery, 1);

    Result := Result + [lItem];
    AQuery.Next;
  end;
end;

function TFamily12MaxRepository.GetCharCount(const AStr: string;
  const AChar: Char): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(AStr) do
    if AStr[i] = AChar then
      Inc(Result);
end;

function TFamily12MaxRepository.GetItems(const ADictionary: TOrderValue)
  : TOrderDictionaryItems;
begin
  CheckRepo;

  case ADictionary of
    ovAccount:
      Result := GetItemsFromSQL(slAccount);
    ovPartner:
      Result := GetItemsFromSQL(slPartner);
    ovCategory:
      Result := GetCategoryItemsFromSQL(slCategory);
    ovTags:
      Result := GetItemsFromSQL(slTags);
  else
    Result := nil;
    Assert(False, 'TFamily12MaxRepository.GetItems');
  end;

  TArray.Sort<TOrderDictionaryItem>(Result,
    TComparer<TOrderDictionaryItem>.Construct(
    function(const Left, Right: TOrderDictionaryItem): Integer
    begin
      Result := AnsiCompareStr(Left[odvName], Right[odvName]);
    end));
end;

function TFamily12MaxRepository.GetItemsFromSQL(const AQuery: TFDQuery)
  : TOrderDictionaryItems;
var
  lItem: TOrderDictionaryItem;
begin
  AQuery.Open;
  AQuery.First;
  while not AQuery.Eof do
  begin
    lItem[odvID] := AQuery.Fields[0].Text;
    lItem[odvName] := AQuery.Fields[1].Text;

    Result := Result + [lItem];
    AQuery.Next;
  end;
end;

procedure TFamily12MaxRepository.InsertCategoryToSQL;
var
  lQuery: TFDQuery;
  lList: TArray<TFDQuery>;
  lTabIdx, lJoinIdx: Integer;
  i: Integer;

  function _getIdx(const AList: TStrings; const AStr: string): Integer;
  var
    i: Integer;
  begin
    for i := 0 to AList.Count - 1 do
      if AList[i].Contains(AStr) then
        Exit(i);
    Result := -1;
  end;

begin
  lList := [slCategory, slSearchByName, slSearchByPartner];
  for lQuery in lList do
  begin
    lTabIdx := _getIdx(lQuery.SQL, ',t2.cat0_name');
    lJoinIdx := _getIdx(lQuery.SQL, 'JOIN cat0 t2 ON');
    Assert((lTabIdx <> -1) and (lJoinIdx <> -1), 'InsertCategoryToSQL');

    for i := 2 to FCatCount do
    begin
      lQuery.SQL.Insert(lTabIdx + 1, Format(',t%d.cat0_name', [i + 1]));
      Inc(lTabIdx);
      Inc(lJoinIdx);
      lQuery.SQL.Insert(lJoinIdx + 1,
        Format('left JOIN cat0 t%d ON c.cat_id%d = t%d.cat0_id ',
        [i + 1, i - 1, i + 1]));
      Inc(lJoinIdx);
    end;
  end;
end;

function TFamily12MaxRepository.SearchByName(const AName: string): TOrderItem;
const
  C_CategoryIdx = 2;
var
  lCat: string;
  lCatSearched: Boolean;
  lMaxTags, lTags: string;
  lMaxTagCount, lTagCount: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  if (AName = '') or (Length(AName) < C_MinName) then
    Exit;

  CheckRepo;

  // Категория - только если все категории одинаковые
  // Теги - строка с макс. числом тегов
  // В Firebird 2x очень специфический SQL, так что пока руками

  slSearchByName.Params[0].Text := AName;
  slSearchByName.Open;

  lCat := '';
  lCatSearched := True;

  lMaxTags := '';
  lMaxTagCount := 0;

  slSearchByName.First;
  while not slSearchByName.Eof do
  begin
    if lCat = '' then
      lCat := GetCategory(slSearchByName, C_CategoryIdx)
    else if lCat <> GetCategory(slSearchByName, C_CategoryIdx) then
      lCatSearched := False;

    lTags := slSearchByName.Fields[0].Text;
    lTagCount := GetCharCount(lTags, ',');
    if lTagCount > lMaxTagCount then
    begin
      lMaxTags := lTags;
      lMaxTagCount := lTagCount;
    end;

    if slSearchByName.Fields[1].Text <> '' then
      Result[ovPartner] := slSearchByName.Fields[1].Text;

    slSearchByName.Next;
  end;
  slSearchByName.Close;

  if lCatSearched and (lCat <> '') then
    Result[ovCategory] := lCat;

  Result[ovTags] := lMaxTags;
end;

function TFamily12MaxRepository.SearchByPartner(const AName: string)
  : TOrderItem;
const
  C_CategoryIdx = 1;
var
  lCat: string;
  lCatSearched: Boolean;
  lTags: TArray<string>;
  lAllTagList: TArray<TArray<string>>;
  lAllTags: TStringList;
  i, j: Integer;
  lSkip: Boolean;
  lIdx: Integer;
begin
  FillChar(Result, SizeOf(Result), 0);
  if AName = '' then
    Exit;

  CheckRepo;

  // Категория - только если все категории одинаковые
  // Теги - строка с совпадающими тегами
  // В Firebird 2x очень специфический SQL, так что пока руками

  FillChar(Result, SizeOf(Result), 0);
  if AName = '' then
    Exit;

  slSearchByPartner.Params[0].Value := AName;
  slSearchByPartner.Open;

  lCat := '';
  lCatSearched := True;

  lAllTags := TStringList.Create;
  try
    lAllTags.Duplicates := dupIgnore;
    lAllTags.Sorted := True;

    lAllTagList := nil;

    slSearchByPartner.First;
    while not slSearchByPartner.Eof do
    begin
      if lCat = '' then
        lCat := GetCategory(slSearchByPartner, C_CategoryIdx)
      else if lCat <> GetCategory(slSearchByPartner, C_CategoryIdx) then
        lCatSearched := False;

      lTags := slSearchByPartner.Fields[0].Text.Split([', ', ',']);
      if Length(lTags) > 0 then
      begin
        TArray.Sort<string>(lTags);
        lAllTagList := lAllTagList + [lTags];
        lAllTags.AddStrings(lTags);
      end;

      slSearchByPartner.Next;
    end;

    for i := lAllTags.Count - 1 downto 0 do
    begin
      lSkip := False;
      for j := 0 to High(lAllTagList) do
        if not TArray.BinarySearch<string>(lAllTagList[j], lAllTags[i], lIdx)
        then
        begin
          lSkip := True;
          Break;
        end;
      if lSkip then
        lAllTags.Delete(i);
    end;

    if lAllTags.Count > 0 then
      Result[ovTags] := lAllTags.CommaText;

  finally
    lAllTags.Free;
  end;

  if lCatSearched and (lCat <> '') then
    Result[ovCategory] := lCat;
end;

function TFamily12MaxRepository.SuggestTags(const AName, APartnerID,
  ACategoryID: string): TArray<string>;
const
  C_EmptyQuery = '-999999';
begin
  Result := nil;
  CheckRepo;

  if AName <> '' then
    slSuggestTags.ParamByName('search').Value := AName
  else
    slSuggestTags.ParamByName('search').Value := C_EmptyQuery;
  if APartnerID <> '' then
    slSuggestTags.ParamByName('partner').Value := APartnerID
  else
    slSuggestTags.ParamByName('partner').Value := C_EmptyQuery;
  if ACategoryID <> '' then
    slSuggestTags.ParamByName('category').Value := ACategoryID
  else
    slSuggestTags.ParamByName('category').Value := C_EmptyQuery;

  slSuggestTags.Open;
  slSuggestTags.First;
  while not slSuggestTags.Eof do
  begin
    Result := Result + [slSuggestTags.Fields[0].Text];
    slSuggestTags.Next;
  end;
  slSuggestTags.Close;
end;

end.
