{
  Barev Protocol - Basic Types and Constants
  A simplified peer-to-peer XMPP protocol for Yggdrasil networks
}

unit BarevTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets;

const
  { Protocol constants }
  BAREV_VERSION = '0.1';
  BAREV_DEFAULT_PORT = 1337;
  BAREV_DEFAULT_PORT_STR = '1337';

  { Timing constants }
  PING_INTERVAL = 30;         // Send ping every 30 seconds
  PING_TIMEOUT = 10;          // Wait 10 seconds for response
  MAX_PING_FAILURES = 3;      // Mark offline after 3 consecutive failures
  RECONNECT_INTERVAL = 5;     // Try to reconnect every 5 seconds

  { Buffer sizes }
  RECV_BUFFER_SIZE = 4096;
  SEND_BUFFER_SIZE = 4096;

  { XML constants }
  XML_HEADER = '<?xml version="1.0" encoding="UTF-8" ?>';
  STREAM_NAMESPACE = 'http://etherx.jabber.org/streams';
  JABBER_CLIENT_NS = 'jabber:client';
  PING_NAMESPACE = 'urn:xmpp:ping';
  VCARD_NAMESPACE = 'vcard-temp';
  VCARD_UPDATE_NAMESPACE = 'vcard-temp:x:update';
  CHATSTATES_NAMESPACE = 'http://jabber.org/protocol/chatstates';

type
  { Connection state }
  TConnectionState = (
    csDisconnected,
    csConnecting,
    csStreamInit,          // Waiting for stream headers
    csAuthenticated,       // Stream established
    csOnline               // Presence exchanged, ready for chat
  );

  { Buddy status }
  TBuddyStatus = (
    bsOffline,
    bsAvailable,
    bsAway,
    bsExtendedAway,
    bsDoNotDisturb
  );

  { Message type }
  TMessageType = (
    mtChat,
    mtGroupChat,
    mtHeadline,
    mtError
  );

type
  { Forward declarations }
  TBarevBuddy = class;
  TBarevConnection = class;
  //TBarevClient = class;

  { Event handlers }
  TBuddyStatusEvent = procedure(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus) of object;
  TMessageReceivedEvent = procedure(Buddy: TBarevBuddy; const MessageText: string) of object;
  TConnectionStateEvent = procedure(Buddy: TBarevBuddy; State: TConnectionState) of object;
  TLogEvent = procedure(const LogLevel, Message: string) of object;

  { Buddy information }
  TBarevBuddy = class
  private
    FNick: string;
    FIPv6Address: string;
    FPort: Word;
    FJID: string;              // Full JID: nick@ipv6_address
    FStatus: TBuddyStatus;
    FStatusMessage: string;
    FConnection: TBarevConnection;
    FLastActivity: TDateTime;
    FPingFailures: Integer;
    FAvatarHash: string;       // SHA1 hash of avatar
    FAvatarData: string;       // Base64 encoded avatar image data
    FAvatarMimeType: string;   // MIME type of avatar (e.g., image/png)
  public
    constructor Create(const ANick, AIPv6: string; APort: Word = BAREV_DEFAULT_PORT);
    destructor Destroy; override;

    property Nick: string read FNick;
    property IPv6Address: string read FIPv6Address;
    property Port: Word read FPort;
    property JID: string read FJID;
    property Status: TBuddyStatus read FStatus write FStatus;
    property StatusMessage: string read FStatusMessage write FStatusMessage;
    property Connection: TBarevConnection read FConnection write FConnection;
    property LastActivity: TDateTime read FLastActivity write FLastActivity;
    property PingFailures: Integer read FPingFailures write FPingFailures;
    property AvatarHash: string read FAvatarHash write FAvatarHash;
    property AvatarData: string read FAvatarData write FAvatarData;
    property AvatarMimeType: string read FAvatarMimeType write FAvatarMimeType;
  end;

  { Connection to a single buddy }
  TBarevConnection = class
  private
    FSocket: TSocket;
    FBuddy: TBarevBuddy;
    FState: TConnectionState;
    FRecvBuffer: string;
    FSendBuffer: string;
    FStreamStartSent: Boolean;
    FStreamStartReceived: Boolean;
    FLastPingID: string;
    FLastPingTime: TDateTime;
    FWeInitiated: Boolean;     // Did we initiate this connection?
  public
    constructor Create(ABuddy: TBarevBuddy; ASocket: TSocket; AWeInitiated: Boolean);
    destructor Destroy; override;

    property Socket: TSocket read FSocket;
    property Buddy: TBarevBuddy read FBuddy;
    property State: TConnectionState read FState write FState;
    property RecvBuffer: string read FRecvBuffer write FRecvBuffer;
    property SendBuffer: string read FSendBuffer write FSendBuffer;
    property StreamStartSent: Boolean read FStreamStartSent write FStreamStartSent;
    property StreamStartReceived: Boolean read FStreamStartReceived write FStreamStartReceived;
    property LastPingID: string read FLastPingID write FLastPingID;
    property LastPingTime: TDateTime read FLastPingTime write FLastPingTime;
    property WeInitiated: Boolean read FWeInitiated;
  end;

{ Helper functions }
function StatusToString(Status: TBuddyStatus): string;
function StringToStatus(const StatusStr: string): TBuddyStatus;
function GenerateID(const Prefix: string = ''): string;
function IsYggdrasilAddress(const Address: string): Boolean;
function ParseJID(const JID: string; out Nick, Address: string): Boolean;
function NormalizeIPv6(const S: string): string;
function ComputeSHA1Hash(const Data: string): string;

implementation

uses
  DateUtils, SHA1;

function NormalizeIPv6(const S: string): string;
var
  a: in6_addr;
begin
  // Convert text -> binary -> text using the SAME formatter as accept() uses
  a := StrToNetAddr6(S);
  Result := NetAddrToStr6(a);
end;

{ TBarevBuddy }

constructor TBarevBuddy.Create(const ANick, AIPv6: string; APort: Word);
begin
  inherited Create;
  FNick := ANick;
  FIPv6Address := NormalizeIPv6(AIPv6);
  FPort := APort;
  FJID := ANick + '@' + FIPv6Address;  // Use normalized IPv6 for JID
  FStatus := bsOffline;
  FStatusMessage := '';
  FConnection := nil;
  FLastActivity := Now;
  FPingFailures := 0;
  FAvatarHash := '';
  FAvatarData := '';
  FAvatarMimeType := '';
end;

destructor TBarevBuddy.Destroy;
begin
  if Assigned(FConnection) then
    FreeAndNil(FConnection);
  inherited;
end;

{ TBarevConnection }

constructor TBarevConnection.Create(ABuddy: TBarevBuddy; ASocket: TSocket; AWeInitiated: Boolean);
begin
  inherited Create;
  FBuddy := ABuddy;
  FSocket := ASocket;
  FWeInitiated := AWeInitiated;
  FState := csConnecting;
  FRecvBuffer := '';
  FSendBuffer := '';
  FStreamStartSent := False;
  FStreamStartReceived := False;
  FLastPingID := '';
  FLastPingTime := 0;
end;

destructor TBarevConnection.Destroy;
begin
  if FSocket <> -1 then
  begin
    CloseSocket(FSocket);
    FSocket := -1;
  end;
  inherited;
end;

{ Helper functions }

function StatusToString(Status: TBuddyStatus): string;
begin
  case Status of
    bsOffline: Result := 'offline';
    bsAvailable: Result := 'available';
    bsAway: Result := 'away';
    bsExtendedAway: Result := 'xa';
    bsDoNotDisturb: Result := 'dnd';
  else
    Result := 'available';
  end;
end;

function StringToStatus(const StatusStr: string): TBuddyStatus;
begin
  if StatusStr = 'away' then
    Result := bsAway
  else if StatusStr = 'xa' then
    Result := bsExtendedAway
  else if StatusStr = 'dnd' then
    Result := bsDoNotDisturb
  else if StatusStr = 'offline' then
    Result := bsOffline
  else
    Result := bsAvailable;
end;

function GenerateID(const Prefix: string): string;
var
  Timestamp: Int64;
begin
  Timestamp := DateTimeToUnix(Now);
  if Prefix <> '' then
    Result := Prefix + '-' + IntToStr(Timestamp) + '-' + IntToStr(Random(10000))
  else
    Result := 'id-' + IntToStr(Timestamp) + '-' + IntToStr(Random(10000));
end;

function IsYggdrasilAddress(const Address: string): Boolean;
var
  FirstHextet: string;
  HexValue: Integer;
  ColonPos: Integer;
begin
  Result := False;

  // Yggdrasil addresses start with 0x0200-0x03FF range
  // Examples: 201:..., 202:..., 300:..., 3ff:...
  if Address = '' then Exit;

  // Find the first colon
  ColonPos := Pos(':', Address);
  if ColonPos < 2 then Exit;

  FirstHextet := Copy(Address, 1, ColonPos - 1);

  // Try to parse as hex
  try
    HexValue := StrToInt('$' + FirstHextet);
    // Check if it's in Yggdrasil range (0x0200 to 0x03FF)
    Result := (HexValue >= $0200) and (HexValue <= $03FF);
  except
    Result := False;
  end;
end;

function ParseJID(const JID: string; out Nick, Address: string): Boolean;
var
  AtPos: Integer;
begin
  Result := False;
  Nick := '';
  Address := '';

  AtPos := Pos('@', JID);
  if AtPos <= 1 then Exit;  // @ not found or at beginning

  Nick := Copy(JID, 1, AtPos - 1);
  Address := Copy(JID, AtPos + 1, Length(JID));

  Result := (Nick <> '') and (Address <> '');
end;

function ComputeSHA1Hash(const Data: string): string;
var
  Digest: TSHA1Digest;
  i: Integer;
begin
  Result := '';
  if Data = '' then Exit;
  
  Digest := SHA1String(Data);
  
  // Convert to hex string
  Result := '';
  for i := 0 to 19 do
    Result := Result + LowerCase(IntToHex(Digest[i], 2));
end;

end.