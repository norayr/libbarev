{
  Barev Protocol - Configuration Management
  Uses TIniFile to store user settings, contacts, and avatar paths
}

unit BarevConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, BarevTypes;

type
  { Contact information for configuration }
  TConfigContact = record
    Nick: string;
    IPv6: string;
    Port: Word;
    AvatarPath: string;  // Path to cached avatar file
  end;

  { Configuration manager }
  TBarevConfig = class
  private
    FConfigFile: string;
    FUserNick: string;
    FUserIPv6: string;
    FUserPort: Word;
    FUserAvatarPath: string;
    FContacts: TList; // List of TConfigContact^

    procedure ClearContacts;
  public
    constructor Create(const AConfigFile: string);
    destructor Destroy; override;

    { Load/Save configuration }
    function Load: Boolean;
    function Save: Boolean;
    function HasUserInfo: Boolean;

    { User settings }
    property UserNick: string read FUserNick write FUserNick;
    property UserIPv6: string read FUserIPv6 write FUserIPv6;
    property UserPort: Word read FUserPort write FUserPort;
    property UserAvatarPath: string read FUserAvatarPath write FUserAvatarPath;

    { Contact management }
    function AddContact(const Nick, IPv6: string; Port: Word; const AvatarPath: string = ''): Boolean;
    function RemoveContact(const Nick, IPv6: string): Boolean;
    function GetContactCount: Integer;
    function GetContact(Index: Integer): TConfigContact;
    function FindContact(const Nick, IPv6: string): Integer;
    procedure ClearContactList;

    property ConfigFile: string read FConfigFile;
  end;

implementation

{ TBarevConfig }

constructor TBarevConfig.Create(const AConfigFile: string);
begin
  inherited Create;
  FConfigFile := AConfigFile;
  FContacts := TList.Create;
  FUserPort := BAREV_DEFAULT_PORT;
end;

destructor TBarevConfig.Destroy;
begin
  ClearContacts;
  FContacts.Free;
  inherited;
end;

procedure TBarevConfig.ClearContacts;
var
  i: Integer;
  Contact: ^TConfigContact;
begin
  for i := 0 to FContacts.Count - 1 do
  begin
    Contact := FContacts[i];
    Dispose(Contact);
  end;
  FContacts.Clear;
end;

function TBarevConfig.Load: Boolean;
var
  Ini: TIniFile;
  Sections: TStringList;
  i: Integer;
  Section: string;
  Contact: ^TConfigContact;
begin
  Result := False;

  if not FileExists(FConfigFile) then
    Exit;

  Ini := TIniFile.Create(FConfigFile);
  Sections := TStringList.Create;
  try
    // Load user settings
    FUserNick := Ini.ReadString('User', 'Nick', '');
    FUserIPv6 := Ini.ReadString('User', 'IPv6', '');
    FUserPort := Ini.ReadInteger('User', 'Port', BAREV_DEFAULT_PORT);
    FUserAvatarPath := Ini.ReadString('User', 'AvatarPath', '');

    // Load contacts
    ClearContacts;
    Ini.ReadSections(Sections);

    for i := 0 to Sections.Count - 1 do
    begin
      Section := Sections[i];
      // Only process sections that start with "Contact-"
      if Copy(Section, 1, 8) = 'Contact-' then
      begin
        New(Contact);
        Contact^.Nick := Ini.ReadString(Section, 'Nick', '');
        Contact^.IPv6 := Ini.ReadString(Section, 'IPv6', '');
        Contact^.Port := Ini.ReadInteger(Section, 'Port', BAREV_DEFAULT_PORT);
        Contact^.AvatarPath := Ini.ReadString(Section, 'AvatarPath', '');

        if (Contact^.Nick <> '') and (Contact^.IPv6 <> '') then
          FContacts.Add(Contact)
        else
          Dispose(Contact);
      end;
    end;

    Result := True;
  finally
    Sections.Free;
    Ini.Free;
  end;
end;

function TBarevConfig.HasUserInfo: Boolean;
begin
  Result := (Trim(FUserNick) <> '') and (Trim(FUserIPv6) <> '');
end;

function TBarevConfig.Save: Boolean;
var
  Ini: TIniFile;
  i: Integer;
  Contact: ^TConfigContact;
  Section: string;
begin
  Result := False;

  Ini := TIniFile.Create(FConfigFile);
  try
    // Save user settings
    Ini.WriteString('User', 'Nick', FUserNick);
    Ini.WriteString('User', 'IPv6', FUserIPv6);
    Ini.WriteInteger('User', 'Port', FUserPort);
    Ini.WriteString('User', 'AvatarPath', FUserAvatarPath);

    // Save contacts
    // First, clear all existing Contact- sections
    // (We will recreate them from our current list)
    i := 0;
    while Ini.SectionExists('Contact-' + IntToStr(i)) do
    begin
      Ini.EraseSection('Contact-' + IntToStr(i));
      Inc(i);
    end;

    for i := 0 to FContacts.Count - 1 do
    begin
      Contact := FContacts[i];
      Section := 'Contact-' + IntToStr(i);

      Ini.WriteString(Section, 'Nick', Contact^.Nick);
      Ini.WriteString(Section, 'IPv6', Contact^.IPv6);
      Ini.WriteInteger(Section, 'Port', Contact^.Port);
      Ini.WriteString(Section, 'AvatarPath', Contact^.AvatarPath);
    end;

    Ini.UpdateFile;
    Result := True;
  finally
    Ini.Free;
  end;
end;

function TBarevConfig.AddContact(const Nick, IPv6: string; Port: Word; const AvatarPath: string): Boolean;
var
  Contact: ^TConfigContact;
begin
  Result := False;

  // Check if contact already exists
  if FindContact(Nick, IPv6) >= 0 then
    Exit;

  New(Contact);
  Contact^.Nick := Nick;
  Contact^.IPv6 := IPv6;
  Contact^.Port := Port;
  Contact^.AvatarPath := AvatarPath;

  FContacts.Add(Contact);
  Result := True;
end;

function TBarevConfig.RemoveContact(const Nick, IPv6: string): Boolean;
var
  Index: Integer;
  Contact: ^TConfigContact;
begin
  Result := False;
  Index := FindContact(Nick, IPv6);

  if Index >= 0 then
  begin
    Contact := FContacts[Index];
    Dispose(Contact);
    FContacts.Delete(Index);
    Result := True;
  end;
end;

function TBarevConfig.GetContactCount: Integer;
begin
  Result := FContacts.Count;
end;

function TBarevConfig.GetContact(Index: Integer): TConfigContact;
var
  Contact: ^TConfigContact;
begin
  if (Index >= 0) and (Index < FContacts.Count) then
  begin
    Contact := FContacts[Index];
    Result := Contact^;
  end
  else
  begin
    FillChar(Result, SizeOf(Result), 0);
  end;
end;

function TBarevConfig.FindContact(const Nick, IPv6: string): Integer;
var
  i: Integer;
  Contact: ^TConfigContact;
begin
  Result := -1;

  for i := 0 to FContacts.Count - 1 do
  begin
    Contact := FContacts[i];
    if (Contact^.Nick = Nick) and (Contact^.IPv6 = IPv6) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

procedure TBarevConfig.ClearContactList;
begin
  ClearContacts;
end;

end.