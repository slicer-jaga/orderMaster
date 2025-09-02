object frmProgress: TfrmProgress
  Left = 0
  Top = 0
  BorderIcons = [biMinimize, biMaximize]
  BorderStyle = bsDialog
  Caption = #1054#1078#1080#1076#1072#1081#1090#1077
  ClientHeight = 137
  ClientWidth = 487
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    487
    137)
  PixelsPerInch = 96
  TextHeight = 13
  object lblMsg: TLabel
    Left = 96
    Top = 16
    Width = 383
    Height = 64
    Anchors = [akLeft, akTop, akRight]
    AutoSize = False
    Caption = #1042#1085#1077#1089#1077#1085#1080#1077' '#1076#1072#1085#1085#1099#1093'...'
    EllipsisPosition = epWordEllipsis
    WordWrap = True
    ExplicitWidth = 370
  end
  object ActivityIndicator: TActivityIndicator
    Left = 16
    Top = 16
    IndicatorSize = aisXLarge
  end
  object btnCancel: TButton
    Left = 404
    Top = 104
    Width = 75
    Height = 25
    Cancel = True
    Caption = #1054#1090#1084#1077#1085#1072
    Default = True
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object dlgTeminate: TTaskDialog
    Buttons = <>
    CommonButtons = [tcbYes, tcbNo, tcbCancel]
    RadioButtons = <>
    Text = #1055#1088#1077#1088#1074#1072#1090#1100' '#1074#1085#1077#1089#1077#1085#1080#1077' '#1076#1072#1085#1085#1099#1093'?'
    Left = 336
    Top = 80
  end
end
