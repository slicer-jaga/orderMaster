params := StrSplit(A_Args[1], ";")

for i, arg in params {
  res .= i ": " arg "`n"
}

MsgBox res