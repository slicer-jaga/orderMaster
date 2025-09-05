object Family12MaxRepository: TFamily12MaxRepository
  OldCreateOrder = False
  Height = 311
  Width = 672
  object Testfamily12maxConnection: TFDConnection
    Params.Strings = (
      'User_Name=sysdba'
      'ConnectionDef=testFamily12Max')
    LoginPrompt = False
    Left = 122
    Top = 49
  end
  object slAccount: TFDQuery
    Connection = Testfamily12maxConnection
    SQL.Strings = (
      'select SCH_ID, SCH_NAME from Scheta where sch_hide is null;')
    Left = 64
    Top = 120
  end
  object slPartner: TFDQuery
    Connection = Testfamily12maxConnection
    SQL.Strings = (
      'select PAYEE_ID, PAYEE_NAME from Payee where PAYEE_HIDE = 0;')
    Left = 128
    Top = 120
  end
  object slTags: TFDQuery
    Connection = Testfamily12maxConnection
    SQL.Strings = (
      'select TG_ID, TG_NAME from Tags;')
    Left = 192
    Top = 120
  end
  object slCategory: TFDQuery
    Connection = Testfamily12maxConnection
    SQL.Strings = (
      'SELECT '
      '  c.cat_id '
      '  ,t2.cat0_name'
      'FROM category c '
      'left JOIN cat0 t2 ON c.cat_id0 = t2.cat0_id '
      
        'where (c.cat_id > 0) and (c.cat_type <> 0) order by t2.cat0_name' +
        ';')
    Left = 248
    Top = 120
  end
  object slSearchByName: TFDQuery
    Connection = Testfamily12maxConnection
    SQL.Strings = (
      'SELECT '
      '    r.re_tag as tags,'
      '    p.payee_name'
      '    ,t2.cat0_name'
      'FROM reestr r'
      'JOIN category c ON r.re_cat_id = c.cat_id'
      'JOIN payee p ON r.re_paye_id = p.payee_id'
      ''
      'left JOIN cat0 t2 ON c.cat_id0 = t2.cat0_id '
      ''
      'WHERE r.re_koment CONTAINING :search;')
    Left = 64
    Top = 216
    ParamData = <
      item
        Name = 'SEARCH'
        DataType = ftString
        ParamType = ptInput
        Value = #1048#1075#1088#1072
      end>
  end
  object slSearchByPartner: TFDQuery
    Connection = Testfamily12maxConnection
    SQL.Strings = (
      'SELECT '
      '    r.re_tag as tags'
      '    ,t2.cat0_name as category'
      '    '
      'FROM reestr r'
      'JOIN category c ON r.re_cat_id = c.cat_id'
      ''
      'left JOIN cat0 t2 ON c.cat_id0 = t2.cat0_id '
      ''
      'WHERE r.re_paye_id = :partner;')
    Left = 184
    Top = 216
    ParamData = <
      item
        Name = 'PARTNER'
        DataType = ftInteger
        ParamType = ptInput
        Value = 4
      end>
  end
  object FDPhysFBDriverLink: TFDPhysFBDriverLink
    VendorLib = 'fbclientd20.dll'
    Left = 272
    Top = 48
  end
  object slCategoryCount: TFDQuery
    Connection = Testfamily12maxConnection
    SQL.Strings = (
      'SELECT COUNT(*) AS cnt'
      'FROM RDB$RELATION_FIELDS'
      'WHERE RDB$RELATION_NAME = UPPER('#39'CATEGORY'#39')'
      '  AND RDB$FIELD_NAME STARTING WITH '#39'CAT_ID'#39';')
    Left = 328
    Top = 176
  end
  object dlgDB: TOpenDialog
    DefaultExt = '.fdb'
    Filter = 'Family12Max Database (*.fdb)|*.fdb'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = #1042#1099#1073#1077#1088#1080#1090#1077' '#1092#1072#1081#1083' '#1076#1072#1085#1085#1099#1093
    Left = 552
    Top = 88
  end
  object slSuggestTags: TFDQuery
    Connection = Testfamily12maxConnection
    SQL.Strings = (
      'SELECT '
      '    r.re_tag as tags'
      'FROM reestr r'
      ''
      'WHERE r.re_koment CONTAINING :search'
      ''
      'union'
      ''
      'SELECT '
      '    r.re_tag as tags'
      'FROM reestr r'
      ''
      'WHERE r.re_paye_id = :partner'
      ''
      'union'
      ''
      'SELECT'
      '    r.re_tag as tags'
      'FROM reestr r'
      ''
      'WHERE r.re_cat_id = :category;    '
      ''
      '')
    Left = 288
    Top = 248
    ParamData = <
      item
        Name = 'SEARCH'
        DataType = ftString
        ParamType = ptInput
        Value = #1048#1075#1088#1072' 1'
      end
      item
        Name = 'PARTNER'
        DataType = ftString
        ParamType = ptInput
        Value = '4'
      end
      item
        Name = 'CATEGORY'
        DataType = ftString
        ParamType = ptInput
        Value = '21'
      end>
  end
end
