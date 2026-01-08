{
  Barev Protocol - Main Client
  High-level API for Barev messaging
}

unit Barev;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets, DateUtils,
  BarevTypes, BarevConfig, BarevAvatar, BarevChatStates, BarevXML, BarevNet, BarevFT;

type
  TypingNotificationProc =  procedure(Buddy: TBarevBuddy; IsTyping: Boolean) of object;
  { Main Barev client }
  TBarevClient = class
  private
    FNick: string;
    FMyJID: string;
    FMyIPv6: string;
    FPort: Word;
    FFileTransfer: TBarevFTManager;
    FSocketManager: TBarevSocketManager;
    FBuddies: TList; // List of TBarevBuddy
    FRunning: Boolean;
    FContactsFile: string;
    FAvatarManager: TBarevAvatarManager;
    FConfig: TBarevConfig;
    FTypingNotificationsEnabled: Boolean;
    FOnTypingNotification: procedure(Buddy: TBarevBuddy; IsTyping: Boolean) of object;

    { Event handlers }
    FOnBuddyStatus: TBuddyStatusEvent;
    FOnMessageReceived: TMessageReceivedEvent;
    FOnConnectionState: TConnectionStateEvent;
    FOnLog: TLogEvent;

    procedure Log(const Level, Message: string);

    function StripLeadingXMLDecl(const S: string): string;
    function StripXMLDeclForStanza(const S: string): string;

    function GetMyJID_Internal: string;
    function GetMyIPv6_Internal: string;
    procedure SendRawToBuddy(Buddy: TBarevBuddy; const Data: string);
    function SendToBuddy(Buddy: TBarevBuddy; const Stanza: string): Boolean;
    procedure HandleIncomingConnection;
    procedure HandleBuddyConnection(Buddy: TBarevBuddy);
    procedure ProcessReceivedData(Buddy: TBarevBuddy; const Data: string);
    procedure HandleStreamStart(Buddy: TBarevBuddy; const XML: string);
    procedure HandlePresence(Buddy: TBarevBuddy; const XML: string);
    procedure HandleMessage(Buddy: TBarevBuddy; const XML: string);
    procedure HandlePing(Buddy: TBarevBuddy; const XML: string);
    procedure HandlePong(Buddy: TBarevBuddy; const XML: string);
    procedure HandleIQ(Buddy: TBarevBuddy; const XML: string);
    procedure TriggerBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
    procedure TriggerMessageReceived(Buddy: TBarevBuddy; const MessageText: string);
    procedure TriggerConnectionState(Buddy: TBarevBuddy; State: TConnectionState);
  public
    constructor Create(const ANick, AMyIPv6: string; APort: Word = BAREV_DEFAULT_PORT);
    destructor Destroy; override;

    { Client control }
    function Start: Boolean;
    procedure Stop;
    procedure Process; // Call this regularly in your main loop

    { Buddy management }
    function AddBuddy(const BuddyNick, BuddyIPv6: string; BuddyPort: Word = BAREV_DEFAULT_PORT): TBarevBuddy;
    function RemoveBuddy(const BuddyJID: string): Boolean;
    function GetBuddy(const BuddyJID: string): TBarevBuddy;
    function GetBuddyCount: Integer;
    function GetBuddyByIndex(Index: Integer): TBarevBuddy;
    function FindBuddyByJID(const JID: string): TBarevBuddy;
    function FindBuddyByIP(const IP: string): TBarevBuddy;
    // This one apparently is not needed because source port is chosen by OS randomly
    function FindBuddyByIPAndPort(const IP: string; Port: Word): TBarevBuddy;

    { Config file }
    function LoadContactsFromFile(const FileName: string): Boolean;
    function SaveContactsToFile(const FileName: string): Boolean;
    function LoadConfig(const ConfigFile: string): Boolean;
    function SaveConfig: Boolean;

    { Avatar }
    function LoadMyAvatar(const FilePath: string): Boolean;
    procedure ClearMyAvatar;
    function GetMyAvatarHash: string;
    function RequestBuddyAvatar(const BuddyJid: string): Boolean;

    { Typing notifications }
    function SendTyping(const BuddyJID: string): Boolean;
    function SendPaused(const BuddyJID: string): Boolean;

    { Communication }
    function ConnectToBuddy(const BuddyJID: string): Boolean;
    function SendMessage(const BuddyJID, MessageText: string): Boolean;
    function SendPresence(Status: TBuddyStatus = bsAvailable; const StatusMessage: string = ''): Boolean;
    function SendPresenceToBuddy(const BuddyJID: string; Status: TBuddyStatus = bsAvailable; const StatusMessage: string = ''): Boolean;

    { Properties }
    property Nick: string read FNick;
    property MyJID: string read FMyJID;
    property MyIPv6: string read FMyIPv6;
    property Port: Word read FPort;
    property Running: Boolean read FRunning;
    property ContactsFile: string read FContactsFile write FContactsFile;
    property AvatarManager: TBarevAvatarManager read FAvatarManager;
    property TypingNotificationsEnabled: Boolean read FTypingNotificationsEnabled write FTypingNotificationsEnabled;

    { Event handlers }
    property OnBuddyStatus: TBuddyStatusEvent read FOnBuddyStatus write FOnBuddyStatus;
    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
    property OnConnectionState: TConnectionStateEvent read FOnConnectionState write FOnConnectionState;
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property FileTransfer: TBarevFTManager read FFileTransfer;
    property OnTypingNotification: TypingNotificationProc read FOnTypingNotification write FOnTypingNotification;
  end;

implementation

uses
  StrUtils;

{ Helper function to normalize JID for comparison }
{ Uses NormalizeIPv6 from BarevTypes unit }
function NormalizeJID(const JID: string): string;
var
  AtPos: Integer;
  Nick, IPv6: string;
begin
  AtPos := Pos('@', JID);
  if AtPos = 0 then
  begin
    Result := LowerCase(JID);
    Exit;
  end;
  
  Nick := Copy(JID, 1, AtPos - 1);
  IPv6 := Copy(JID, AtPos + 1, Length(JID) - AtPos);
  
  // Normalize the IPv6 part using barevtypes.NormalizeIPv6
  IPv6 := NormalizeIPv6(IPv6);
  
  Result := LowerCase(Nick) + '@' + IPv6;
end;

{ TBarevClient }

constructor TBarevClient.Create(const ANick, AMyIPv6: string; APort: Word);
begin
  inherited Create;

  FAvatarManager := TBarevAvatarManager.Create;
  FConfig := nil; // Will be created when LoadConfig is called
  FOnTypingNotification := nil;
  FTypingNotificationsEnabled := True;

  FNick := ANick;
  FMyIPv6 := AMyIPv6;
  FPort := APort;
  FMyJID := FNick + '@' + FMyIPv6;

  FSocketManager := TBarevSocketManager.Create(APort);
  FSocketManager.OnLog := @Log;

  FFileTransfer := TBarevFTManager.Create(
    @SendRawToBuddy,
    @Log,
    @GetMyJID_Internal,
    @GetMyIPv6_Internal
  );


  FBuddies := TList.Create;
  FRunning := False;
  FContactsFile := '';

  Randomize;
end;

destructor TBarevClient.Destroy;
var
  i: Integer;
begin
  Stop;

  // Free all buddies
  for i := 0 to FBuddies.Count - 1 do
    TBarevBuddy(FBuddies[i]).Free;
  FBuddies.Free;

  FreeAndNil(FFileTransfer);
  FSocketManager.Free;

  FreeAndNil(FAvatarManager);
  FreeAndNil(FConfig);

  inherited;
end;

// config
function TBarevClient.LoadConfig(const ConfigFile: string): Boolean;
var
  i: Integer;
  Contact: TConfigContact;
  Buddy: TBarevBuddy;
begin
  Result := False;

  if not Assigned(FConfig) then
    FConfig := TBarevConfig.Create(ConfigFile);

  if not FConfig.Load then
    Exit;

  // Load user settings
  FNick := FConfig.UserNick;
  FMyIPv6 := FConfig.UserIPv6;
  FPort := FConfig.UserPort;
  FMyJID := FNick + '@' + FMyIPv6;

  // Load avatar if configured
  if (FConfig.UserAvatarPath <> '') and FileExists(FConfig.UserAvatarPath) then
    FAvatarManager.LoadMyAvatar(FConfig.UserAvatarPath);

  // Load contacts as buddies
  for i := 0 to FConfig.GetContactCount - 1 do
  begin
    Contact := FConfig.GetContact(i);
    Buddy := AddBuddy(Contact.Nick, Contact.IPv6, Contact.Port);

    // If we have a cached avatar path, we could load it here
    // (not implemented yet - would need to parse the avatar file)
  end;

  Result := True;
end;

function TBarevClient.SaveConfig: Boolean;
var
  i: Integer;
  Buddy: TBarevBuddy;
  DefaultConfigPath: string;
begin
  Result := False;

  // If config not loaded, create one with default path
  if not Assigned(FConfig) then
  begin
    DefaultConfigPath := GetUserDir + '.barev' + PathDelim + 'barev.ini';
    if not DirectoryExists(GetUserDir + '.barev') then
      ForceDirectories(GetUserDir + '.barev');
    FConfig := TBarevConfig.Create(DefaultConfigPath);
    Log('INFO', 'Created new config file: ' + DefaultConfigPath);
  end;

  // Save user settings
  FConfig.UserNick := FNick;
  FConfig.UserIPv6 := FMyIPv6;
  FConfig.UserPort := FPort;
  FConfig.UserAvatarPath := FAvatarManager.MyAvatarPath;

  // Save contacts
  FConfig.ClearContactList;
  for i := 0 to FBuddies.Count - 1 do
  begin
    Buddy := TBarevBuddy(FBuddies[i]);
    FConfig.AddContact(Buddy.Nick, Buddy.IPv6Address, Buddy.Port);
  end;

  Result := FConfig.Save;
end;

// avatar

function TBarevClient.LoadMyAvatar(const FilePath: string): Boolean;
begin
  Result := FAvatarManager.LoadMyAvatar(FilePath);

  if Result then
  begin
    Log('INFO', 'Avatar loaded: ' + FilePath);
    Log('INFO', 'Avatar hash: ' + FAvatarManager.MyAvatarHash);

    // Update config if we have one
    if Assigned(FConfig) then
    begin
      FConfig.UserAvatarPath := FilePath;
      SaveConfig;
    end;
  end;
end;

procedure TBarevClient.ClearMyAvatar;
begin
  FAvatarManager.ClearMyAvatar;

  if Assigned(FConfig) then
  begin
    FConfig.UserAvatarPath := '';
    SaveConfig;
  end;
end;

function TBarevClient.GetMyAvatarHash: string;
begin
  Result := FAvatarManager.MyAvatarHash;
end;

function TBarevClient.RequestBuddyAvatar(const BuddyJID: string): Boolean;
var
  Buddy: TBarevBuddy;
  IQ, VCardReq: string;
  IQ_ID: string;
begin
  Result := False;

  Buddy := FindBuddyByJID(BuddyJID);
  if not Assigned(Buddy) then
    Exit;

  if not Assigned(Buddy.Connection) then
    Exit;

  // Check if connection is authenticated (csOnline might not be set in all cases)
  if not (Buddy.Connection.State in [csAuthenticated, csOnline]) then
    Exit;

  IQ_ID := GenerateID('vcard');

  IQ := '<iq type=''get'' to=''' + XMLEscape(BuddyJID) + ''' id=''' + IQ_ID + '''>' +
        '<vCard xmlns=''vcard-temp''/>' +
        '</iq>';

  Result := SendToBuddy(Buddy, IQ);

  if Result then
    Log('INFO', 'Requested avatar from ' + BuddyJID);
end;

// typing

function TBarevClient.SendTyping(const BuddyJID: string): Boolean;
var
  Buddy: TBarevBuddy;
  Stanza: string;
begin
  Result := False;
  if not FTypingNotificationsEnabled then
    Exit;

  Buddy := FindBuddyByJID(BuddyJID);
  if not Assigned(Buddy) then
    Exit;

  Stanza := TBarevChatStates.GenerateChatState(csComposing, BuddyJID);
  Result := SendToBuddy(Buddy, Stanza);
end;

function TBarevClient.SendPaused(const BuddyJID: string): Boolean;
var
  Buddy: TBarevBuddy;
  Stanza: string;
begin
  Result := False;

  if not FTypingNotificationsEnabled then
    Exit;

  Buddy := FindBuddyByJID(BuddyJID);
  if not Assigned(Buddy) then
    Exit;

  // Bonjour compatibility: send 'active' instead of 'paused'
  // Pidgin's Bonjour implementation only recognizes composing and active
  Stanza := TBarevChatStates.GenerateChatState(csActive, BuddyJID);
  Result := SendToBuddy(Buddy, Stanza);
end;



// file transfers

function TBarevClient.StripLeadingXMLDecl(const S: string): string;
var
  P: SizeInt;
begin
  Result := S;

  { Strip '<?xml ... ?>' if it is at the start }
  if Pos('<?xml', Result) = 1 then
  begin
    P := Pos('?>', Result);
    if P > 0 then
    begin
      Delete(Result, 1, P + 2); { remove through '?>' }

      { trim leading whitespace/newlines after the declaration }
      while (Length(Result) > 0) and (Result[1] in [#9, #10, #13, ' ']) do
        Delete(Result, 1, 1);
    end;
  end;
end;

function TBarevClient.StripXMLDeclForStanza(const S: string): string;
begin
  { Keep XML declaration ONLY for the opening stream header.
    Everything else (iq/message/presence/stream end) must NOT include it. }
  if (Pos('<?xml', S) = 1) and (Pos('<stream:stream', S) = 0) then
    Result := StripLeadingXMLDecl(S)
  else
    Result := S;
end;

function TBarevClient.SendToBuddy(Buddy: TBarevBuddy; const Stanza: string): Boolean;
begin
  Result := False;
  if (Buddy = nil) or (Buddy.Connection = nil) then Exit;
  Result := FSocketManager.SendData(Buddy.Connection.Socket, Stanza) > 0;
end;


procedure TBarevClient.HandleIQ(Buddy: TBarevBuddy; const XML: string);
var
  Clean: string;
  IQType, IQ_ID: string;
  VCardXML: string;
  AvatarData, MimeType, AvatarHash: string;
  SavePath: string;
begin
  Clean := StripLeadingXMLDecl(XML);
  
  Log('DEBUG', 'HandleIQ from ' + Buddy.Nick + ': ' + Clean);

  // Check for vCard requests FIRST (before file transfer)
  if (Pos('<vCard xmlns=''vcard-temp''', Clean) > 0) or 
     (Pos('<vCard xmlns="vcard-temp"', Clean) > 0) then
  begin
    Log('DEBUG', 'Detected vCard request');
    IQType := ExtractIQAttribute(Clean, 'type');
    IQ_ID := ExtractIQAttribute(Clean, 'id');
    
    Log('DEBUG', 'vCard IQType=' + IQType + ' ID=' + IQ_ID);

    if IQType = 'get' then
    begin
      // Send our vCard
      VCardXML := '<iq type=''result'' to=''' + XMLEscape(Buddy.JID) + ''' id=''' + IQ_ID + '''>' +
                  FAvatarManager.GenerateMyVCard +
                  '</iq>';
      SendToBuddy(Buddy, VCardXML);
      Log('INFO', 'Sent vCard to ' + Buddy.Nick);
    end
    else if IQType = 'result' then
    begin
      Log('DEBUG', 'Processing vCard result');
      // Parse received vCard
      if FAvatarManager.ParseVCardAvatar(Clean, AvatarData, MimeType, AvatarHash) then
      begin
        Log('DEBUG', 'ParseVCardAvatar succeeded: hash=' + AvatarHash);
        // Save avatar
        Buddy.AvatarData := AvatarData;
        Buddy.AvatarMimeType := MimeType;
        Buddy.AvatarHash := AvatarHash;

        SavePath := FAvatarManager.SaveBuddyAvatar(Buddy.Nick, Buddy.IPv6Address,
                                                     AvatarData, MimeType);
        if SavePath <> '' then
          Log('INFO', 'Saved avatar for ' + Buddy.Nick + ' to ' + SavePath)
        else
          Log('WARN', 'Failed to save avatar for ' + Buddy.Nick);
      end
      else
      begin
        Log('WARN', 'ParseVCardAvatar failed for ' + Buddy.Nick);
      end;
    end
    else
    begin
      Log('WARN', 'vCard with unhandled type: ' + IQType);
    end;
    Exit;
  end;

  // Check for ping/pong (standard XMPP IQ)
  if Pos('<ping xmlns=''urn:xmpp:ping''', Clean) > 0 then
  begin
    // This is a ping, handle it
    IQ_ID := ExtractIQAttribute(Clean, 'id');
    VCardXML := '<iq type=''result'' to=''' + XMLEscape(Buddy.JID) + ''' id=''' + IQ_ID + '''/>';
    SendToBuddy(Buddy, VCardXML);
    Log('DEBUG', 'Responded to ping from ' + Buddy.Nick);
    Exit;
  end;

  // Now check file transfer
  if Assigned(FFileTransfer) then
  begin
    FFileTransfer.HandleIQ(Buddy, Clean);
    // Don't exit - FT handler will log if unhandled
  end;
end;



function TBarevClient.GetMyJID_Internal: string;
begin
  Result := FMyJID;
end;

function TBarevClient.GetMyIPv6_Internal: string;
begin
  Result := FMyIPv6;
end;

procedure TBarevClient.SendRawToBuddy(Buddy: TBarevBuddy; const Data: string);
var
  OutData: string;
begin
  if (Buddy = nil) or (Buddy.Connection = nil) then
    Exit;

  { Prevent sending XML declarations inside an established stream }
  OutData := StripXMLDeclForStanza(Data);

  FSocketManager.SendData(Buddy.Connection.Socket, OutData);
end;


procedure TBarevClient.Log(const Level, Message: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Level, Message);
end;

function TBarevClient.Start: Boolean;
begin
  if FRunning then
  begin
    Log('WARN', 'Client already running');
    Exit(True);
  end;

  Log('INFO', 'Starting Barev client as ' + FMyJID);

  if not FSocketManager.StartListening then
  begin
    Log('ERROR', 'Failed to start listening');
    Exit(False);
  end;

  FRunning := True;
  Log('INFO', 'Barev client started successfully');
  Result := True;
end;

procedure TBarevClient.Stop;
begin
  if not FRunning then Exit;

  Log('INFO', 'Stopping Barev client');

  FSocketManager.StopListening;
  FRunning := False;

  Log('INFO', 'Barev client stopped');
end;

procedure TBarevClient.Process;
var
  i: Integer;
  Buddy: TBarevBuddy;
  CurrentTime: TDateTime;
begin
  if not FRunning then Exit;

  // Check for incoming connections
  HandleIncomingConnection;

  // Process each buddy connection
  for i := 0 to FBuddies.Count - 1 do
  begin
    Buddy := TBarevBuddy(FBuddies[i]);

    if Assigned(Buddy.Connection) then
      HandleBuddyConnection(Buddy)
    else
    begin
      // Try to auto-connect to offline buddies
      CurrentTime := Now;
      if SecondsBetween(CurrentTime, Buddy.LastActivity) > RECONNECT_INTERVAL then
      begin
        Buddy.LastActivity := CurrentTime;
        if Buddy.Status = bsOffline then
          ConnectToBuddy(Buddy.JID);
      end;
    end;
  end;
end;

procedure TBarevClient.HandleIncomingConnection;
var
  NewSocket: TSocket;
  ClientAddr: string;
  ClientPort: Word;
  Buddy: TBarevBuddy;
  Conn: TBarevConnection;
begin
  NewSocket := FSocketManager.AcceptConnection(ClientAddr, ClientPort);
  if NewSocket < 0 then Exit;

  Log('INFO', 'Incoming connection from ' + ClientAddr + ':' + IntToStr(ClientPort));

  // SECURITY: Find buddy by BOTH IP and Port
  // Buddy := FindBuddyByIPAndPort(ClientAddr, ClientPort);
  // Silly me! Above does not make sense because source port is randomly chosen by the OS.
  Buddy := FindBuddyByIP(ClientAddr);

  Log('INFO', 'Incoming connection from ' + ClientAddr + ':' + IntToStr(ClientPort) +
           ' buddy=' + BoolToStr(Assigned(Buddy), True));

  if not Assigned(Buddy) then
  begin
    Log('WARN', 'Security: Connection from unknown IP:Port ' + ClientAddr + ':' +
                IntToStr(ClientPort) + ', closing');
    CloseSocket(NewSocket);
    Exit;
  end;

  // If buddy already has a connection, close the old one
  if Assigned(Buddy.Connection) then
  begin
    Log('INFO', 'Replacing existing connection for ' + Buddy.JID);
    Conn := Buddy.Connection;
    Buddy.Connection := nil;
    Conn.Free;
  end;

  // Create new connection
  Conn := TBarevConnection.Create(Buddy, NewSocket, False);
  Buddy.Connection := Conn;
  Buddy.Status := bsAvailable;
  Buddy.LastActivity := Now;
  Buddy.PingFailures := 0;

  TriggerConnectionState(Buddy, csConnecting);
end;

procedure TBarevClient.HandleBuddyConnection(Buddy: TBarevBuddy);
var
  Data: string;
  BytesRead: Integer;
  Conn: TBarevConnection;
begin
  if not Assigned(Buddy.Connection) then Exit;

  Conn := Buddy.Connection;

  // Check if socket is readable
  if not FSocketManager.IsSocketReadable(Conn.Socket, 0) then
  begin
    // Check for ping timeout
    if (Conn.LastPingTime > 0) and
       (SecondsBetween(Now, Conn.LastPingTime) > PING_TIMEOUT) then
    begin
      Buddy.PingFailures := Buddy.PingFailures + 1;
      Conn.LastPingTime := 0;

      if Buddy.PingFailures >= MAX_PING_FAILURES then
      begin
        Log('WARN', 'Buddy ' + Buddy.JID + ' failed ping checks, disconnecting');
        Conn := Buddy.Connection;
        Buddy.Connection := nil;
        Conn.Free;
        TriggerBuddyStatus(Buddy, Buddy.Status, bsOffline);
        Buddy.Status := bsOffline;
      end;
    end;
    Exit;
  end;

  // Read data
  BytesRead := FSocketManager.ReceiveData(Conn.Socket, Data);

  if BytesRead < 0 then
  begin
    // Socket error
    Log('ERROR', 'Socket error for ' + Buddy.JID);
    Conn := Buddy.Connection;
    Buddy.Connection := nil;
    Conn.Free;
    TriggerBuddyStatus(Buddy, Buddy.Status, bsOffline);
    Buddy.Status := bsOffline;
    Exit;
  end
  else if BytesRead = 0 then
  begin
    // Connection closed
    Log('INFO', 'Connection closed by ' + Buddy.JID);
    Conn := Buddy.Connection;
    Buddy.Connection := nil;
    Conn.Free;
    TriggerBuddyStatus(Buddy, Buddy.Status, bsOffline);
    Buddy.Status := bsOffline;
    Exit;
  end;

  // Process received data
  Buddy.LastActivity := Now;
  Buddy.PingFailures := 0; // Reset on any activity
  ProcessReceivedData(Buddy, Data);
end;

procedure TBarevClient.ProcessReceivedData(Buddy: TBarevBuddy; const Data: string);
var
  Conn: TBarevConnection;
  CompleteXML: string;
  StreamStart: string;
  TempBuffer: string;
begin
  if not Assigned(Buddy.Connection) then Exit;

  Conn := Buddy.Connection;

  // Add to receive buffer
  Conn.RecvBuffer := Conn.RecvBuffer + Data;

  // Process complete XML stanzas
  TempBuffer := Conn.RecvBuffer;
  while Pos('<', TempBuffer) > 0 do
  begin
    // Check for stream start
    if not Conn.StreamStartReceived then
    begin
      if IsStreamStart(TempBuffer) then
      begin
        HandleStreamStart(Buddy, TempBuffer);
        // Remove processed part (stream header is not a complete element)
        if Pos('>', TempBuffer) > 0 then
          Delete(TempBuffer, 1, Pos('>', TempBuffer));
        Conn.RecvBuffer := TempBuffer;
        Continue;
      end;
    end;

    // Check for stream end
    if IsStreamEnd(TempBuffer) then
    begin
      Log('INFO', 'Stream end received from ' + Buddy.JID);
      Conn := Buddy.Connection;
      Buddy.Connection := nil;
      Conn.Free;
      TriggerBuddyStatus(Buddy, Buddy.Status, bsOffline);
      Buddy.Status := bsOffline;
      Exit;
    end;

    // Try to extract complete stanza
    if Pos('<presence', TempBuffer) > 0 then
    begin
      if (Pos('/>', TempBuffer) > 0) or (Pos('</presence>', TempBuffer) > 0) then
      begin
        if Pos('/>', TempBuffer) > 0 then
          CompleteXML := Copy(TempBuffer, 1, Pos('/>', TempBuffer) + 1)
        else
          CompleteXML := Copy(TempBuffer, 1, Pos('</presence>', TempBuffer) + 10);

        //HandlePresence(Buddy, CompleteXML);
        //HandleMessage(Buddy, StripLeadingXMLDecl(CompleteXML));
        HandlePresence(Buddy, StripLeadingXMLDecl(CompleteXML));


        Delete(TempBuffer, 1, Length(CompleteXML));
        Conn.RecvBuffer := TempBuffer;
        Continue;
      end;
    end;

    if Pos('<message', TempBuffer) > 0 then
    begin
      if Pos('</message>', TempBuffer) > 0 then
      begin
        CompleteXML := Copy(TempBuffer, 1, Pos('</message>', TempBuffer) + 9);
        HandleMessage(Buddy, CompleteXML);
        Delete(TempBuffer, 1, Length(CompleteXML));
        Conn.RecvBuffer := TempBuffer;
        Continue;
      end;
    end;

    if Pos('<iq', TempBuffer) > 0 then
    begin
      if (Pos('/>', TempBuffer) > 0) or (Pos('</iq>', TempBuffer) > 0) then
      begin
        if Pos('/>', TempBuffer) > 0 then
          CompleteXML := Copy(TempBuffer, 1, Pos('/>', TempBuffer) + 1)
        else
          CompleteXML := Copy(TempBuffer, 1, Pos('</iq>', TempBuffer) + 4);

        //if IsPing(CompleteXML) then
        //  HandlePing(Buddy, CompleteXML)
        //else if IsPong(CompleteXML, Conn.LastPingID) then
        //  HandlePong(Buddy, CompleteXML);
        if IsPing(CompleteXML) then
          HandlePing(Buddy, CompleteXML)
        else if IsPong(CompleteXML, Conn.LastPingID) then
          HandlePong(Buddy, CompleteXML)
        //else if Assigned(FFileTransfer) then
        //  FFileTransfer.HandleIQ(Buddy, CompleteXML);
        //else if Pos('<iq', StripLeadingXMLDecl(CompleteXML)) = 1 then
        else
          HandleIQ(Buddy, CompleteXML);


        Delete(TempBuffer, 1, Length(CompleteXML));
        Conn.RecvBuffer := TempBuffer;
        Continue;
      end;
    end;

    // If we cannot process anything, wait for more data
    Break;
  end;

  // Write back any remaining buffer
  Conn.RecvBuffer := TempBuffer;
end;

procedure TBarevClient.HandleStreamStart(Buddy: TBarevBuddy; const XML: string);
var
  Conn: TBarevConnection;
  FromJID: string;
  Response: string;
begin
  if not Assigned(Buddy.Connection) then Exit;

  Conn := Buddy.Connection;

  FromJID := ExtractAttribute(XML, 'from');
  Log('INFO', 'Stream start received from ' + FromJID);

  // SECURITY: Validate that the JID in the stream header matches expected buddy
  // Normalize both JIDs to handle IPv6 case differences and leading zeros
  if NormalizeJID(FromJID) <> NormalizeJID(Buddy.JID) then
  begin
    Log('ERROR', 'Security: JID mismatch! Expected ' + Buddy.JID + ' but got ' + FromJID);
    Log('ERROR', 'Security: Rejecting connection from ' + FromJID);
    Conn := Buddy.Connection;
    Buddy.Connection := nil;
    Conn.Free;
    Exit;
  end;

  Conn.StreamStartReceived := True;

  // If we haven not sent our stream start yet, send it now
  if not Conn.StreamStartSent then
  begin
    Response := BuildStreamHeader(FMyJID, Buddy.JID);
    FSocketManager.SendData(Conn.Socket, Response);
    Conn.StreamStartSent := True;
    Log('INFO', 'Sent stream header to ' + Buddy.JID);
  end;

  // Stream is now established
  Conn.State := csAuthenticated;
  TriggerConnectionState(Buddy, csAuthenticated);

  // Send presence
  SendPresenceToBuddy(Buddy.JID, bsAvailable, '');
end;

procedure TBarevClient.HandlePresence(Buddy: TBarevBuddy; const XML: string);
var
  PresenceType: string;
  ShowElement: string;
  StatusElement: string;
  OldStatus: TBuddyStatus;
  PhotoHash: string;
  PhotoStart, PhotoEnd: Integer;
begin
  OldStatus := Buddy.Status;

  PresenceType := ExtractAttribute(XML, 'type');

  if PresenceType = 'unavailable' then
  begin
    Log('INFO', Buddy.JID + ' is now offline');
    TriggerBuddyStatus(Buddy, OldStatus, bsOffline);
    Buddy.Status := bsOffline;
    Exit;
  end;

  // Extract show element
  ShowElement := ExtractElementContent(XML, 'show');
  StatusElement := ExtractElementContent(XML, 'status');

  if ShowElement <> '' then
    Buddy.Status := StringToStatus(ShowElement)
  else
    Buddy.Status := bsAvailable;

  Buddy.StatusMessage := StatusElement;

  Log('INFO', Buddy.JID + ' is now ' + StatusToString(Buddy.Status));
  TriggerBuddyStatus(Buddy, OldStatus, Buddy.Status);

  if Pos('<x xmlns="' + VCARD_UPDATE_NAMESPACE + '"', XML) > 0 then
  begin
    PhotoStart := Pos('<photo>', XML);
    PhotoEnd := Pos('</photo>', XML);

    if (PhotoStart > 0) and (PhotoEnd > 0) then
    begin
      PhotoHash := Copy(XML, PhotoStart + 7, PhotoEnd - PhotoStart - 7);

      // If hash changed and is not empty, request the avatar
      if (PhotoHash <> '') and (PhotoHash <> Buddy.AvatarHash) then
      begin
        Log('INFO', 'Avatar update detected for ' + Buddy.Nick + ', requesting...');
        RequestBuddyAvatar(Buddy.JID);
      end;
    end;
  end;
end;

procedure TBarevClient.HandleMessage(Buddy: TBarevBuddy; const XML: string);
var
  Body: string;
  ChatState: TChatState;
  IsTyping: Boolean;
begin
  Body := ExtractElementContent(XML, 'body');

  if Body <> '' then
  begin
    Log('INFO', 'Message from ' + Buddy.JID + ': ' + Body);
    TriggerMessageReceived(Buddy, Body);
  end;

  // Process chat state notifications if enabled
  if FTypingNotificationsEnabled then
  begin
    ChatState := TBarevChatStates.ParseChatState(XML);

    if ChatState = csComposing then
    begin
      IsTyping := True;
      if Assigned(FOnTypingNotification) then
        FOnTypingNotification(Buddy, IsTyping);
    end
    else if ChatState = csPaused then
    begin
      IsTyping := False;
      if Assigned(FOnTypingNotification) then
        FOnTypingNotification(Buddy, IsTyping);
    end;
  end;

end;

procedure TBarevClient.HandlePing(Buddy: TBarevBuddy; const XML: string);
var
  PingID: string;
  Pong: string;
begin
  PingID := ExtractAttribute(XML, 'id');
  Log('DEBUG', 'Ping received from ' + Buddy.JID + ', ID: ' + PingID);

  // Send pong
  Pong := BuildPong(Buddy.JID, PingID);
  FSocketManager.SendData(Buddy.Connection.Socket, Pong);

  Log('DEBUG', 'Sent pong to ' + Buddy.JID);
end;

procedure TBarevClient.HandlePong(Buddy: TBarevBuddy; const XML: string);
begin
  Log('DEBUG', 'Pong received from ' + Buddy.JID);
  Buddy.Connection.LastPingTime := 0; // Clear ping timeout
  Buddy.PingFailures := 0;
end;

function TBarevClient.FindBuddyByJID(const JID: string): TBarevBuddy;
var
  i: Integer;
  NormalizedSearchJID: string;
begin
  Result := nil;
  NormalizedSearchJID := NormalizeJID(JID);
  
  for i := 0 to FBuddies.Count - 1 do
  begin
    if NormalizeJID(TBarevBuddy(FBuddies[i]).JID) = NormalizedSearchJID then
    begin
      Result := TBarevBuddy(FBuddies[i]);
      Break;
    end;
  end;
end;

// This one apparently is not needed because source port is chosen by OS randomly
function TBarevClient.FindBuddyByIPAndPort(const IP: string; Port: Word): TBarevBuddy;
var
  i: Integer;
  B: TBarevBuddy;
begin
  Result := nil;
  for i := 0 to FBuddies.Count - 1 do
  begin
    B := TBarevBuddy(FBuddies[i]);
    if (NormalizeIPv6(B.IPv6Address) = NormalizeIPv6(IP)) and (B.Port = Port) then
    begin
      Result := B;
      Break;
    end;
  end;
end;

function TBarevClient.FindBuddyByIP(const IP: string): TBarevBuddy;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to FBuddies.Count - 1 do
  begin
    WriteLn('WARN', 'FBuddies[' + IntToStr(i) + '].IPv6Address is ' + TBarevBuddy(FBuddies[i]).IPv6Address);
    //WriteLn('WARN' + 'IP is ' + WordToStr(IP));
    //if TBarevBuddy(FBuddies[i]).IPv6Address = IP then
    //if SameText(TBarevBuddy(FBuddies[i]).IPv6Address, IP) then
    if NormalizeIPv6(TBarevBuddy(FBuddies[i]).IPv6Address) = NormalizeIPv6(IP) then
    begin
      Result := TBarevBuddy(FBuddies[i]);
      Break;
    end;
  end;
end;

procedure TBarevClient.TriggerBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
begin
  if Assigned(FOnBuddyStatus) then
    FOnBuddyStatus(Buddy, OldStatus, NewStatus);
end;

procedure TBarevClient.TriggerMessageReceived(Buddy: TBarevBuddy; const MessageText: string);
begin
  if Assigned(FOnMessageReceived) then
    FOnMessageReceived(Buddy, MessageText);
end;

procedure TBarevClient.TriggerConnectionState(Buddy: TBarevBuddy; State: TConnectionState);
begin
  if Assigned(FOnConnectionState) then
    FOnConnectionState(Buddy, State);
end;

function TBarevClient.AddBuddy(const BuddyNick, BuddyIPv6: string; BuddyPort: Word): TBarevBuddy;
begin
  Result := TBarevBuddy.Create(BuddyNick, BuddyIPv6, BuddyPort);
  FBuddies.Add(Result);
  Log('INFO', 'Added buddy: ' + Result.JID);
end;

function TBarevClient.RemoveBuddy(const BuddyJID: string): Boolean;
var
  Buddy: TBarevBuddy;
begin
  Result := False;
  Buddy := FindBuddyByJID(BuddyJID);
  if Assigned(Buddy) then
  begin
    FBuddies.Remove(Buddy);
    Buddy.Free;
    Log('INFO', 'Removed buddy: ' + BuddyJID);
    Result := True;
  end;
end;

function TBarevClient.GetBuddy(const BuddyJID: string): TBarevBuddy;
begin
  Result := FindBuddyByJID(BuddyJID);
end;

function TBarevClient.GetBuddyCount: Integer;
begin
  Result := FBuddies.Count;
end;

function TBarevClient.GetBuddyByIndex(Index: Integer): TBarevBuddy;
begin
  if (Index >= 0) and (Index < FBuddies.Count) then
    Result := TBarevBuddy(FBuddies[Index])
  else
    Result := nil;
end;

function TBarevClient.LoadContactsFromFile(const FileName: string): Boolean;
var
  ContactsList: TStringList;
  i, j: Integer;
  Line, BuddyNick, Address: string;
  ColonPos, PortNum: Integer;
  TempStr: string;
begin
  Result := False;

  if not FileExists(FileName) then
  begin
    Log('WARN', 'Contacts file not found: ' + FileName);
    Exit;
  end;

  ContactsList := TStringList.Create;
  try
    ContactsList.LoadFromFile(FileName);

    for i := 0 to ContactsList.Count - 1 do
    begin
      Line := Trim(ContactsList[i]);
      if (Line = '') or (Line[1] = '#') then Continue;

      // Format: nick@ipv6_address or nick@ipv6_address:port
      if ParseJID(Line, BuddyNick, Address) then
      begin
        // Check for port - find LAST colon and see if it is followed by digits
        ColonPos := 0;
        for j := Length(Address) downto 1 do
        begin
          if Address[j] = ':' then
          begin
            TempStr := Copy(Address, j + 1, Length(Address));
            PortNum := StrToIntDef(TempStr, -1);
            if PortNum > 0 then
            begin
              ColonPos := j;
              break;
            end;
          end;
        end;

        if ColonPos > 0 then
          AddBuddy(BuddyNick, Copy(Address, 1, ColonPos - 1), PortNum)
        else
          AddBuddy(BuddyNick, Address, BAREV_DEFAULT_PORT);
      end;
    end;

    FContactsFile := FileName;
    Log('INFO', 'Loaded ' + IntToStr(FBuddies.Count) + ' contacts from ' + FileName);
    Result := True;
  finally
    ContactsList.Free;
  end;
end;

function TBarevClient.SaveContactsToFile(const FileName: string): Boolean;
var
  ContactsList: TStringList;
  i: Integer;
  Buddy: TBarevBuddy;
begin
  Result := False;

  ContactsList := TStringList.Create;
  try
    ContactsList.Add('# Barev Contacts');
    ContactsList.Add('# Format: nick@ipv6_address or nick@ipv6_address:port');
    ContactsList.Add('');

    for i := 0 to FBuddies.Count - 1 do
    begin
      Buddy := TBarevBuddy(FBuddies[i]);
      if Buddy.Port = BAREV_DEFAULT_PORT then
        ContactsList.Add(Buddy.JID)
      else
        ContactsList.Add(Buddy.Nick + '@' + Buddy.IPv6Address + ':' + IntToStr(Buddy.Port));
    end;

    ContactsList.SaveToFile(FileName);
    FContactsFile := FileName;
    Log('INFO', 'Saved ' + IntToStr(FBuddies.Count) + ' contacts to ' + FileName);
    Result := True;
  finally
    ContactsList.Free;
  end;
end;

function TBarevClient.ConnectToBuddy(const BuddyJID: string): Boolean;
var
  Buddy: TBarevBuddy;
  NewSocket: TSocket;
  Conn: TBarevConnection;
  StreamHeader: string;
begin
  Result := False;

  Buddy := FindBuddyByJID(BuddyJID);
  if not Assigned(Buddy) then
  begin
    Log('ERROR', 'Buddy not found: ' + BuddyJID);
    Exit;
  end;

  // If already connected, nothing to do
  if Assigned(Buddy.Connection) then
  begin
    Log('WARN', 'Already connected to ' + BuddyJID);
    Exit(True);
  end;

  Log('INFO', 'Connecting to ' + Buddy.JID + ' at ' + Buddy.IPv6Address + ':' + IntToStr(Buddy.Port));

  NewSocket := FSocketManager.ConnectTo(Buddy.IPv6Address, Buddy.Port);
  if NewSocket < 0 then
  begin
    Log('ERROR', 'Failed to connect to ' + BuddyJID);
    Exit;
  end;

  // Create connection
  Conn := TBarevConnection.Create(Buddy, NewSocket, True);
  Buddy.Connection := Conn;
  Buddy.LastActivity := Now;

  // Send stream header
  StreamHeader := BuildStreamHeader(FMyJID, Buddy.JID);
  FSocketManager.SendData(Conn.Socket, StreamHeader);
  Conn.StreamStartSent := True;
  Conn.State := csStreamInit;

  Log('INFO', 'Sent stream header to ' + BuddyJID);
  TriggerConnectionState(Buddy, csStreamInit);

  Result := True;
end;

function TBarevClient.SendMessage(const BuddyJID, MessageText: string): Boolean;
var
  Buddy: TBarevBuddy;
  MessageXML: string;
begin
  Result := False;

  Buddy := FindBuddyByJID(BuddyJID);
  if not Assigned(Buddy) then
  begin
    Log('ERROR', 'Buddy not found: ' + BuddyJID);
    Exit;
  end;

  if not Assigned(Buddy.Connection) then
  begin
    Log('ERROR', 'Not connected to ' + BuddyJID);
    Exit;
  end;

  MessageXML := BuildMessage(FMyJID, BuddyJID, MessageText);
  Result := FSocketManager.SendData(Buddy.Connection.Socket, MessageXML) > 0;

  if Result then
    Log('INFO', 'Sent message to ' + BuddyJID + ': ' + MessageText)
  else
    Log('ERROR', 'Failed to send message to ' + BuddyJID);
end;

function TBarevClient.SendPresence(Status: TBuddyStatus; const StatusMessage: string): Boolean;
var
  i: Integer;
  Buddy: TBarevBuddy;
  AvatarUpdate: string;
begin
  Result := True;

  for i := 0 to FBuddies.Count - 1 do
  begin
    Buddy := TBarevBuddy(FBuddies[i]);
    if Assigned(Buddy.Connection) and
       Buddy.Connection.StreamStartSent and
       Buddy.Connection.StreamStartReceived then
    begin
      if not SendPresenceToBuddy(Buddy.JID, Status, StatusMessage) then
        Result := False;
    end;
  end;
end;

function TBarevClient.SendPresenceToBuddy(const BuddyJID: string;
  Status: TBuddyStatus; const StatusMessage: string): Boolean;
var
  Buddy: TBarevBuddy;
  PresenceXML: string;
  AvatarUpdate: string;
  InsertPos: Integer;
begin
  Result := False;

  Buddy := FindBuddyByJID(BuddyJID);
  if not Assigned(Buddy) or not Assigned(Buddy.Connection) then Exit;

  PresenceXML := BuildPresence('', Status, StatusMessage);
  Result := FSocketManager.SendData(Buddy.Connection.Socket, PresenceXML) > 0;

  if FAvatarManager.MyAvatarHash <> '' then
  begin
    AvatarUpdate := FAvatarManager.GenerateAvatarUpdate;
    InsertPos := Pos('</presence>', PresenceXML);
    if InsertPos > 0 then
      Insert(AvatarUpdate, PresenceXML, InsertPos);
  end;

  Result := FSocketManager.SendData(Buddy.Connection.Socket, PresenceXML) > 0;

  if Result then
    Log('INFO', 'Sent presence to ' + BuddyJID + ': ' + StatusToString(Status))
  else
    Log('ERROR', 'Failed to send presence to ' + BuddyJID);
end;

end.