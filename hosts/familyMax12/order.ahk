#Requires AutoHotkey v2.0

winTitle := "Transaction"
WinWaitActive(winTitle)

; Порядок аргументов:
; 1 - Режим (more | done)
; 2 - Счет
; 3 - Категория
; 4 - Контрагент
; 5 - Сумма
; 6 - Комментарий
; 7 - Метки

autoCommit := A_Args[2]

params := StrSplit(A_Args[1], ";")

insMode := params[1]
account := params[2]
category := params[3]
counter := params[4]
amount := params[5]
comment := params[6]
tags := params[7]

; Счет
if (account != "") {
  ControlChooseString(account, "TOXPComboBox8", winTitle)
}

; Категория
if (category != "") {
  ControlChooseString(category, "TOXPComboBox9", winTitle)
}

; Контрагент
if (counter != "") {
  ControlChooseString(counter, "TOXPComboBox10", winTitle)
}

; Сумма
if (amount != "") {
  ControlSetText(amount, "TsCalcEdit1", winTitle)
}

; Комментарий
if (comment != "") {
  ControlSetText(comment, "TsEdit2", winTitle)
}

; Метки
if (tags != "") {
  ControlSetText(tags, "TsEdit1", winTitle)
}

; Ввод - Авто
if (autoCommit = "-1") {
  ControlChooseIndex(1, "TOXPComboBox11", winTitle)
}

if (insMode = "more") {   
  ; Еще
  ControlClick("TsButton1", winTitle)
} else {
  ; Запланировать
  ControlClick("TsButton3", winTitle)
}

; Таймаут для приложения
sleep(500)