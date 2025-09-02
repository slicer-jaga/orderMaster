unit OrderUtils;

interface

uses
  System.IniFiles,
  Winapi.Windows,
  Vcl.Forms;

procedure LoadFormPlacementEx(const AIni: TCustomIniFile; const AForm: TForm;
  ASection: string);
procedure SaveFormPlacement(const AIni: TCustomIniFile; const AForm: TForm;
  ASection: string);

implementation

uses
  System.SysUtils, System.Classes,
  Winapi.Messages;

const
  C_INI_Show = 'Show';
  C_INI_Left = 'Left';
  C_INI_Top = 'Top';
  C_INI_Right = 'Right';
  C_INI_Bottom = 'Bottom';
  C_INI_Flags = 'Flags';
  C_INI_MinPos_X = 'MinPos_X';
  C_INI_MinPos_Y = 'MinPos_Y';
  C_INI_MaxPos_X = 'MaxPos_X';
  C_INI_MaxPos_Y = 'MaxPos_Y';

{$HINTS OFF}

type
  TComponentAccessProtected = class(TComponent);
{$HINTS ON}

procedure LoadFormPlacementEx(const AIni: TCustomIniFile; const AForm: TForm;
  ASection: string);
var
  WinState: TWindowState;
  lWindowPlacement, lTmp: TWindowPlacement;
  lCmd: UINT;

  procedure _ChangePosition(APosition: TPosition);
  begin
    TComponentAccessProtected(AForm).SetDesigning(True);
    try
      AForm.Position := APosition;
    finally
      TComponentAccessProtected(AForm).SetDesigning(False);
    end;
  end;

  function InRange(AValue, AMinValue, AMaxValue: Integer): Boolean;
  begin
    Result := (AValue >= AMinValue) and (AValue <= AMaxValue);
  end;

begin
  with AIni do
  begin
    if ASection = '' then
      ASection := AForm.Name;

    if ValueExists(ASection, C_INI_Show) then
    begin
      FillChar(lWindowPlacement, SizeOf(lWindowPlacement), 0);
      lWindowPlacement.Length := SizeOf(TWindowPlacement);
      GetWindowPlacement(AForm.Handle, @lWindowPlacement);
      with lWindowPlacement do
      begin
        flags := AIni.ReadInteger(ASection, C_INI_Flags,
          lWindowPlacement.flags);
        showCmd := AIni.ReadInteger(ASection, C_INI_Show, SW_SHOWNORMAL);

        ptMinPosition.X := AIni.ReadInteger(ASection, C_INI_MinPos_X, 0);
        ptMinPosition.Y := AIni.ReadInteger(ASection, C_INI_MinPos_Y, 0);
        ptMaxPosition.X := AIni.ReadInteger(ASection, C_INI_MaxPos_X, 0);
        ptMaxPosition.Y := AIni.ReadInteger(ASection, C_INI_MaxPos_Y, 0);

        rcNormalPosition.Left := AIni.ReadInteger(ASection, C_INI_Left,
          AForm.Left);
        rcNormalPosition.Top := AIni.ReadInteger(ASection, C_INI_Top,
          AForm.Top);
        rcNormalPosition.Right := AIni.ReadInteger(ASection, C_INI_Right,
          AForm.Left + AForm.Width);
        rcNormalPosition.Bottom := AIni.ReadInteger(ASection, C_INI_Bottom,
          AForm.Top + AForm.Height);
      end;

      try
        if IsRectEmpty(lWindowPlacement.rcNormalPosition) or
          not InRange(lWindowPlacement.rcNormalPosition.Left, 0,
          Screen.Width - 40) or
          not InRange(lWindowPlacement.rcNormalPosition.Top, 0,
          Screen.Height - 40) or (lWindowPlacement.rcNormalPosition.Right <=
          lWindowPlacement.rcNormalPosition.Left) then
          lWindowPlacement.rcNormalPosition := Screen.WorkAreaRect;

        if not IsRectEmpty(lWindowPlacement.rcNormalPosition) and
          InRange(lWindowPlacement.rcNormalPosition.Left, 0, Screen.Width - 40)
          and InRange(lWindowPlacement.rcNormalPosition.Top, 0,
          Screen.Height - 40) and (lWindowPlacement.rcNormalPosition.Right >
          lWindowPlacement.rcNormalPosition.Left) then
        begin
          _ChangePosition(poDesigned);

          lCmd := lWindowPlacement.showCmd;
          if lCmd = SW_SHOWMINIMIZED then
            lCmd := SW_SHOWMAXIMIZED;

          if not IsWindowVisible(AForm.Handle) then
            lWindowPlacement.showCmd := SW_HIDE
          else
          begin
            lTmp.Length := SizeOf(lTmp);
            GetWindowPlacement(AForm.Handle, @lTmp);
            lWindowPlacement.showCmd := lTmp.showCmd;
          end;
          SetWindowPlacement(AForm.Handle, @lWindowPlacement);
          lWindowPlacement.showCmd := lCmd;
        end;

        WinState := wsNormal;
        { default maximize MDI main form }
        if ((Application.MainForm = AForm) or (Application.MainForm = nil)) and
          ((AForm.FormStyle = fsMDIForm) or ((AForm.FormStyle = fsNormal) and
          (AForm.Position = poDefault))) then
          WinState := wsMaximized;
        case lWindowPlacement.showCmd of
          SW_SHOWNORMAL, SW_RESTORE, SW_SHOW:
            WinState := wsNormal;
          SW_MINIMIZE, SW_SHOWMINIMIZED, SW_SHOWMINNOACTIVE:
            WinState := wsMinimized;
          SW_MAXIMIZE:
            WinState := wsMaximized;
        end;
{$IFDEF WIN32}
        if (WinState = wsMinimized) and
          ((AForm = Application.MainForm) or (Application.MainForm = nil)) then
        begin
          TWindowState(Pointer(@AForm.WindowState)^) := wsNormal;
          PostMessage(Application.Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
          Exit;
        end;
{$ENDIF}
        if AForm.FormStyle in [fsMDIChild, fsMDIForm] then
          TWindowState(Pointer(@AForm.WindowState)^) := WinState
        else
          AForm.WindowState := WinState;
      finally
        AForm.Update;
      end;
    end;
  end;
end;

procedure SaveFormPlacement(const AIni: TCustomIniFile; const AForm: TForm;
  ASection: string);
var
  lWindowPlacement: TWindowPlacement;
begin
  with AIni do
  begin
    if ASection = '' then
      ASection := AForm.Name;

    FillChar(lWindowPlacement, SizeOf(lWindowPlacement), 0);
    lWindowPlacement.Length := SizeOf(lWindowPlacement);
    GetWindowPlacement(AForm.Handle, lWindowPlacement);

    with lWindowPlacement do
    begin
      if (AForm.FormStyle = fsMDIChild) and (AForm.WindowState = wsMinimized)
      then
        lWindowPlacement.flags := lWindowPlacement.flags or WPF_SETMINPOSITION;

      WriteInteger(ASection, C_INI_Show, showCmd);
      WriteInteger(ASection, C_INI_Left, rcNormalPosition.Left);
      WriteInteger(ASection, C_INI_Top, rcNormalPosition.Top);
      WriteInteger(ASection, C_INI_Right, rcNormalPosition.Right);
      WriteInteger(ASection, C_INI_Bottom, rcNormalPosition.Bottom);
      if ptMinPosition.X <> -1 then
      begin
        WriteInteger(ASection, C_INI_MinPos_X, ptMinPosition.X);
        WriteInteger(ASection, C_INI_MinPos_Y, ptMinPosition.Y);
      end;
      if ptMaxPosition.X <> -1 then
      begin
        WriteInteger(ASection, C_INI_MaxPos_X, ptMaxPosition.X);
        WriteInteger(ASection, C_INI_MaxPos_Y, ptMaxPosition.Y);
      end;
      WriteInteger(ASection, C_INI_Flags, flags);
    end;
  end;
end;

end.
