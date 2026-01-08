{
  Barev Protocol - File Transfer (XEP-0096 + XEP-0065, simplified)

  Yggdrasil note:
    This unit binds/listens only on a predictable TCP port range so you can
    firewall it safely.

  Required:
    BAREV_FT_PORT_MIN = 50000
    BAREV_FT_PORT_MAX = 50049

  Interop note:
    The purple Barev plugin uses a very tolerant SOCKS5 server handshake.
    This implementation is also tolerant, so it can talk to it.
}

unit BarevFT;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets, DateUtils, Process,
  sha1,
  BarevTypes;

const
  // Fixed firewall-friendly range
  BAREV_FT_PORT_MIN = 50000;
  BAREV_FT_PORT_MAX = 50049;

  // Namespaces
  NS_SI          = 'http://jabber.org/protocol/si';
  NS_SI_FT       = 'http://jabber.org/protocol/si/profile/file-transfer';
  NS_FEATURE     = 'http://jabber.org/protocol/feature-neg';
  NS_BYTESTREAMS = 'http://jabber.org/protocol/bytestreams';

type
  TBarevFTDirection = (ftSend, ftRecv);
  TBarevFTState = (
    ftsIdle,
    ftsOfferSent,
    ftsOfferReceived,
    ftsAccepted,
    ftsStreamhostSent,
    ftsStreamhostReceived,
    ftsTransferring,
    ftsDone,
    ftsError
  );

  TOnFTOffer = procedure(Buddy: TBarevBuddy; const Sid, FileName: string; FileSize: Int64) of object;


  // Event hooks (CLI can print / auto-accept etc.)
  TFileOfferEvent = procedure(Buddy: TBarevBuddy; const Sid, FileName: string; FileSize: Int64) of object;
  TFileProgressEvent = procedure(Buddy: TBarevBuddy; const Sid: string; BytesDone, BytesTotal: Int64) of object;
  TFileCompleteEvent = procedure(Buddy: TBarevBuddy; const Sid: string; const LocalPath: string) of object;
  TFileErrorEvent = procedure(Buddy: TBarevBuddy; const Sid, ErrMsg: string) of object;

  // Callbacks into TBarevClient without creating a circular unit dependency
  TSendToBuddyEvent = procedure(Buddy: TBarevBuddy; const Data: string) of object;
  TLogEventStr = procedure(const Level, Message: string) of object;
  TGetMyJIDFunc = function: string of object;
  TGetMyIPv6Func = function: string of object;

  TBarevFileTransfer = class;

  { Worker thread to handle actual file bytes so the main loop stays responsive }
  TBarevFTWorker = class(TThread)
  private
    FXfer: TBarevFileTransfer;
    procedure ReportProgress;
    procedure ReportComplete;
    procedure ReportError(const Msg: string);
  protected
    procedure Execute; override;
  public
    constructor Create(AXfer: TBarevFileTransfer);
  end;

  TBarevFileTransfer = class
  public
    Buddy: TBarevBuddy;
    Direction: TBarevFTDirection;
    State: TBarevFTState;

    // XEP ids
    IQId: string;   // iq id that started the session (we use it as Sid)
    Sid: string;

    // Metadata
    FileName: string;
    FileSize: Int64;

    // Paths
    LocalPath: string; // for recv: destination. for send: source

    // Bytestreams
    StreamHost: string;
    StreamPort: Word;
    ListenSock: TSocket;
    DataSock: TSocket;

    // Transfer counters
    BytesDone: Int64;
    ErrorMsg: string;

    Worker: TBarevFTWorker;

    constructor Create;
    destructor Destroy; override;
  end;

  { TBarevFTManager }
  TBarevFTManager = class
  private
    FXfers: TList; // of TBarevFileTransfer

    FSendToBuddy: TSendToBuddyEvent;
    FLog: TLogEventStr;
    FGetMyJID: TGetMyJIDFunc;
    FGetMyIPv6: TGetMyIPv6Func;

    FOnFileOffer: TFileOfferEvent;
    FOnProgress: TFileProgressEvent;
    FOnComplete: TFileCompleteEvent;
    FOnError: TFileErrorEvent;

    function FindXferBySid(const Sid: string): TBarevFileTransfer;
    procedure Log(const L, M: string);

    // Minimal XML helpers
    function ExtractAttribute(const XML, Attr: string): string;
    function ExtractElementContent(const XML, Tag: string): string;
    // TODO TagHasNamespace only handles double quotes, and is not used
    // probably can be removed.
    function TagHasNamespace(const XML, Tag, NS: string): Boolean;

    // Build basic IQ wrappers
    function BuildIQ(const IQType, ID, FromJID, ToJID, Inner: string): string;

    // SI and Bytestreams builders (subset)
    function BuildSIOffer(const FromJID, ToJID, ID, Sid, FileName: string; FileSize: Int64): string;
    function BuildSIResultAccept(const FromJID, ToJID, ID: string): string;
    function BuildSIErrorReject(const FromJID, ToJID, ID: string; const Code: string = '403'): string;

    function BuildBytestreamsQuery(const FromJID, ToJID, ID, Sid, Host: string; Port: Word): string;
    function BuildBytestreamsResultStreamhostUsed(const FromJID, ToJID, ID, Sid, HostJID: string): string;
    function BuildBytestreamsError(const FromJID, ToJID, ID, Sid: string; const Code: string = '404'): string;

    // Port selection & sockets
    function ListenOnFixedRange(out LSock: TSocket; out Port: Word): Boolean;
    function AcceptOne(LSock: TSocket; out CSock: TSocket; TimeoutSec: Integer): Boolean;
    function ConnectTCPv6(const Host: string; Port: Word; out Sock: TSocket): Boolean;

    // XEP-0065 DST.ADDR = SHA1(sid + initiator + target)
    function ComputeDstAddr40(const Sid, InitiatorJID, TargetJID: string): string;

    // State machine pieces
    procedure HandleIncomingSI(Buddy: TBarevBuddy; const XML: string);

    procedure HandleIncomingSIResult(Buddy: TBarevBuddy; const XML: string);
    procedure HandleIncomingBytestreamsQuery(Buddy: TBarevBuddy; const XML: string);
    procedure HandleIncomingBytestreamsResult(Buddy: TBarevBuddy; const XML: string);

  public
    constructor Create(ASendToBuddy: TSendToBuddyEvent; ALog: TLogEventStr;
                       AGetMyJID: TGetMyJIDFunc; AGetMyIPv6: TGetMyIPv6Func);
    destructor Destroy; override;

    // Call from TBarevClient when it receives a non-ping IQ
    procedure HandleIQ(Buddy: TBarevBuddy; const XML: string);

    // API for your CLI/UI
    function OfferFile(Buddy: TBarevBuddy; const LocalPath: string): string; // returns Sid
    //function AcceptOffer(const Sid, SaveAsPath: string): Boolean;
    function AcceptOffer(const Sid, SaveAs: string): Boolean;
    function RejectOffer(const Sid: string): Boolean;

    // Events
    property OnFileOffer: TFileOfferEvent read FOnFileOffer write FOnFileOffer;
    property OnProgress: TFileProgressEvent read FOnProgress write FOnProgress;
    property OnComplete: TFileCompleteEvent read FOnComplete write FOnComplete;
    property OnError: TFileErrorEvent read FOnError write FOnError;
  end;

    // SOCKS5 (minimal/tolerant)
    function Socks5_ServerHandshake(CSock: TSocket): Boolean;
    function Socks5_ClientHandshake(Sock: TSocket; const DstAddr40: string): Boolean;

implementation
uses
{$IFDEF UNIX}
  BaseUnix,
{$ENDIF}
{$IFDEF WINDOWS}
  WinSock2,
{$ENDIF}
  StrUtils, BarevXML;


function Socks5_ServerHandshake(CSock: TSocket): Boolean;
var
  Buf: array[0..255] of byte;
  R: ssize_t;
begin
  Result := False;

  // Client greeting: VER NMETHODS METHODS...
  R := fpRead(CSock, Buf, 2);
  if R < 2 then Exit;
  if Buf[0] <> 5 then Exit;

  // read METHODS bytes (ignore)
  if Buf[1] > 0 then
  begin
    R := fpRead(CSock, Buf, Buf[1]);
    if R < Buf[1] then Exit;
  end;

  // Server choice: VER=5 METHOD=0
  Buf[0] := 5;
  Buf[1] := 0;
  if fpWrite(CSock, Buf, 2) <> 2 then Exit;

  // Client connect request (we intentionally ignore its contents, like your plugin)
  // Read “some” bytes so the client is satisfied.
  R := fpRead(CSock, Buf, 64);
  if R <= 0 then Exit;

  // Reply success (tolerant format)
  Buf[0] := 5;  // VER
  Buf[1] := 0;  // REP success
  Buf[2] := 0;  // RSV
  Buf[3] := 3;  // ATYP=DOMAIN (tolerant)
  Buf[4] := 20; // len
  FillChar(Buf[5], 20, 0);
  Buf[25] := 0; // port hi
  Buf[26] := 0; // port lo
  if fpWrite(CSock, Buf, 27) <> 27 then Exit;

  Result := True;
end;

function Socks5_ClientHandshake(Sock: TSocket; const DstAddr40: string): Boolean;
var
  Buf: array[0..512] of byte;
  R: ssize_t;
  I: Integer;
  L: Byte;
begin
  Result := False;

  // Greeting: VER=5, NMETHODS=1, METHOD=0 (no auth)
  Buf[0] := 5;
  Buf[1] := 1;
  Buf[2] := 0;
  if fpWrite(Sock, Buf, 3) <> 3 then Exit;

  R := fpRead(Sock, Buf, 2);
  if R < 2 then Exit;
  if (Buf[0] <> 5) or (Buf[1] <> 0) then Exit;

  // Connect request: VER=5 CMD=1 RSV=0 ATYP=3 DOMAINLEN=40 DOMAIN=dstaddr PORT=0
  Buf[0] := 5;
  Buf[1] := 1;
  Buf[2] := 0;
  Buf[3] := 3;
  L := 40;
  Buf[4] := L;
  for I := 1 to 40 do
    Buf[4 + I] := Ord(DstAddr40[I]);
  Buf[45] := 0;
  Buf[46] := 0;

  if fpWrite(Sock, Buf, 47) <> 47 then Exit;

  // Read reply (variable length). Read at least first 5 bytes.
  R := fpRead(Sock, Buf, 5);
  if R < 5 then Exit;
  if (Buf[0] <> 5) or (Buf[1] <> 0) then Exit;

  // Consume remaining address+port based on ATYP
  case Buf[3] of
    1: begin // IPv4
      fpRead(Sock, Buf, 4 + 2);
    end;
    3: begin // domain
      fpRead(Sock, Buf, Buf[4] + 2);
    end;
    4: begin // IPv6
      fpRead(Sock, Buf, 16 + 2);
    end;
  end;

  Result := True;
end;

{ TBarevFileTransfer }

constructor TBarevFileTransfer.Create;
begin
  inherited Create;
  Buddy := nil;
  Direction := ftRecv;
  State := ftsIdle;
  IQId := '';
  Sid := '';
  FileName := '';
  FileSize := 0;
  LocalPath := '';
  StreamHost := '';
  StreamPort := 0;
  ListenSock := -1;
  DataSock := -1;
  BytesDone := 0;
  ErrorMsg := '';
  Worker := nil;
end;

destructor TBarevFileTransfer.Destroy;
begin
  if Assigned(Worker) then
  begin
    Worker.Terminate;
    Worker.WaitFor;
    FreeAndNil(Worker);
  end;

  if DataSock <> -1 then
  begin
    CloseSocket(DataSock);
    DataSock := -1;
  end;

  if ListenSock <> -1 then
  begin
    CloseSocket(ListenSock);
    ListenSock := -1;
  end;

  inherited Destroy;
end;

{ TBarevFTWorker }

constructor TBarevFTWorker.Create(AXfer: TBarevFileTransfer);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FXfer := AXfer;
end;

procedure TBarevFTWorker.ReportProgress;
begin
  // Fired by manager (owner) via polling on FXfer.BytesDone; here is no-op
end;

procedure TBarevFTWorker.ReportComplete;
begin
  // no-op
end;

procedure TBarevFTWorker.ReportError(const Msg: string);
begin
  // no-op
end;

procedure TBarevFTWorker.Execute;
var
  Buf: array[0..8191] of byte;
  R: ssize_t;
  FS: TFileStream;
  W: LongInt;
begin
  if FXfer = nil then Exit;

  try
    FXfer.State := ftsTransferring;

    if FXfer.Direction = ftSend then
    begin
      // Sender: accept connection, SOCKS5 server handshake, then send file bytes
      if FXfer.DataSock = -1 then
      begin
        // Wait for one incoming connection
        if not (FXfer.ListenSock <> -1) then
          raise Exception.Create('No listen socket for sending');

        // Blocking accept is okay in worker
        FXfer.DataSock := fpAccept(FXfer.ListenSock, nil, nil);
        if FXfer.DataSock = -1 then
          raise Exception.Create('accept() failed');

        if not Socks5_ServerHandshake(FXfer.DataSock) then
          ; // We can’t call instance methods here; handshake handled outside in manager
      end;

      FS := TFileStream.Create(FXfer.LocalPath, fmOpenRead or fmShareDenyNone);
      try
        while (not Terminated) do
        begin
          R := FS.Read(Buf, SizeOf(Buf));
          if R <= 0 then Break;

          W := fpWrite(FXfer.DataSock, Buf, R);
          if W <= 0 then
            raise Exception.Create('write() failed during send');

          Inc(FXfer.BytesDone, W);
        end;
      finally
        FS.Free;
      end;

      FXfer.State := ftsDone;
    end
    else
    begin
      // Receiver: after SOCKS5 client handshake, read file bytes into destination
      if FXfer.DataSock = -1 then
        raise Exception.Create('No data socket for receiving');

      ForceDirectories(ExtractFileDir(FXfer.LocalPath));
      FS := TFileStream.Create(FXfer.LocalPath, fmCreate);
      try
        while (not Terminated) and (FXfer.BytesDone < FXfer.FileSize) do
        begin
          R := fpRead(FXfer.DataSock, Buf, SizeOf(Buf));
          if R <= 0 then Break;

          FS.WriteBuffer(Buf, R);
          Inc(FXfer.BytesDone, R);
        end;
      finally
        FS.Free;
      end;

      if FXfer.BytesDone >= FXfer.FileSize then
        FXfer.State := ftsDone
      else
      begin
        FXfer.State := ftsError;
        FXfer.ErrorMsg := 'Transfer ended early';
      end;
    end;
  except
    on E: Exception do
    begin
      FXfer.State := ftsError;
      FXfer.ErrorMsg := E.Message;
    end;
  end;
end;

{ TBarevFTManager }

constructor TBarevFTManager.Create(ASendToBuddy: TSendToBuddyEvent; ALog: TLogEventStr;
  AGetMyJID: TGetMyJIDFunc; AGetMyIPv6: TGetMyIPv6Func);
begin
  inherited Create;
  FXfers := TList.Create;
  FSendToBuddy := ASendToBuddy;
  FLog := ALog;
  FGetMyJID := AGetMyJID;
  FGetMyIPv6 := AGetMyIPv6;
end;

destructor TBarevFTManager.Destroy;
var
  i: Integer;
begin
  for i := 0 to FXfers.Count - 1 do
    TObject(FXfers[i]).Free;
  FreeAndNil(FXfers);
  inherited Destroy;
end;

procedure TBarevFTManager.Log(const L, M: string);
begin
  if Assigned(FLog) then
    FLog(L, M);
end;

function TBarevFTManager.FindXferBySid(const Sid: string): TBarevFileTransfer;
var
  i: Integer;
  X: TBarevFileTransfer;
begin
  Result := nil;
  for i := 0 to FXfers.Count - 1 do
  begin
    X := TBarevFileTransfer(FXfers[i]);
    if SameText(X.Sid, Sid) then Exit(X);
  end;
end;

function TBarevFTManager.ExtractAttribute(const XML, Attr: string): string;
var
  P, Q: Integer;
  Needle: string;
begin
  Result := '';

  // Try double quotes first
  Needle := Attr + '="';
  P := Pos(Needle, XML);
  if P > 0 then
  begin
    Inc(P, Length(Needle));
    Q := P;
    while (Q <= Length(XML)) and (XML[Q] <> '"') do Inc(Q);
    if Q > P then
    begin
      Result := Copy(XML, P, Q - P);
      Exit;
    end;
  end;

  // Try single quotes
  Needle := Attr + '=''';  // Two single quotes = escaped single quote in Pascal
  P := Pos(Needle, XML);
  if P > 0 then
  begin
    Inc(P, Length(Needle));
    Q := P;
    while (Q <= Length(XML)) and (XML[Q] <> '''') do Inc(Q);  // Two single quotes
    if Q > P then
      Result := Copy(XML, P, Q - P);
  end;
end;

function TBarevFTManager.ExtractElementContent(const XML, Tag: string): string;
var
  A, B: Integer;
  OpenTag, CloseTag: string;
begin
  Result := '';
  OpenTag := '<' + Tag;
  A := Pos(OpenTag, XML);
  if A <= 0 then Exit;

  A := Pos('>', Copy(XML, A, MaxInt));
  if A <= 0 then Exit;
  // Convert to absolute:
  A := Pos(OpenTag, XML) + A;

  CloseTag := '</' + Tag + '>';
  B := Pos(CloseTag, XML);
  if (B <= 0) or (B < A) then Exit;

  Result := Copy(XML, A + 1, B - (A + 1));
end;

function TBarevFTManager.TagHasNamespace(const XML, Tag, NS: string): Boolean;
var
  P, Q: Integer;
  Snip: string;
begin
  Result := False;
  P := Pos('<' + Tag, XML);
  if P <= 0 then Exit;

  Q := Pos('>', Copy(XML, P, 300));
  if Q <= 0 then Exit;

  Snip := Copy(XML, P, Q);
  Result := (Pos('xmlns="' + NS + '"', Snip) > 0) or (Pos('xmlns:si="' + NS + '"', Snip) > 0);
end;

function TBarevFTManager.BuildIQ(const IQType, ID, FromJID, ToJID, Inner: string): string;
begin
  Result :=
    '<iq type="' + IQType + '" id="' + ID + '" from="' + FromJID + '" to="' + ToJID + '">' +
      Inner +
    '</iq>';
end;

function TBarevFTManager.BuildSIOffer(const FromJID, ToJID, ID, Sid, FileName: string; FileSize: Int64): string;
var
  Inner: string;
begin
  Inner :=
    '<si xmlns="' + NS_SI + '" id="' + Sid + '" profile="' + NS_SI_FT + '">' +
      '<file xmlns="' + NS_SI_FT + '" name="' + FileName + '" size="' + IntToStr(FileSize) + '"/>' +
      '<feature xmlns="' + NS_FEATURE + '">' +
        '<x xmlns="jabber:x:data" type="form">' +
          '<field var="stream-method" type="list-single">' +
            '<option><value>' + NS_BYTESTREAMS + '</value></option>' +
          '</field>' +
        '</x>' +
      '</feature>' +
    '</si>';
  Result := BuildIQ('set', ID, FromJID, ToJID, Inner);
end;

function TBarevFTManager.BuildSIResultAccept(const FromJID, ToJID, ID: string): string;
var
  Inner: string;
begin
  Inner :=
    '<si xmlns="' + NS_SI + '">' +
      '<feature xmlns="' + NS_FEATURE + '">' +
        '<x xmlns="jabber:x:data" type="submit">' +
          '<field var="stream-method">' +
            '<value>' + NS_BYTESTREAMS + '</value>' +
          '</field>' +
        '</x>' +
      '</feature>' +
    '</si>';
  Result := BuildIQ('result', ID, FromJID, ToJID, Inner);
end;

function TBarevFTManager.BuildSIErrorReject(const FromJID, ToJID, ID: string; const Code: string): string;
var
  Inner: string;
begin
  Inner :=
    '<error type="cancel" code="' + Code + '"><forbidden xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>';
  Result := BuildIQ('error', ID, FromJID, ToJID, Inner);
end;

function TBarevFTManager.BuildBytestreamsQuery(const FromJID, ToJID, ID, Sid, Host: string; Port: Word): string;
var
  Inner: string;
begin
  Inner :=
    '<query xmlns="' + NS_BYTESTREAMS + '" sid="' + Sid + '" mode="tcp">' +
      '<streamhost jid="' + FromJID + '" host="' + Host + '" port="' + IntToStr(Port) + '"/>' +
    '</query>';
  Result := BuildIQ('set', ID, FromJID, ToJID, Inner);
end;

function TBarevFTManager.BuildBytestreamsResultStreamhostUsed(const FromJID, ToJID, ID, Sid, HostJID: string): string;
var
  Inner: string;
begin
  Inner :=
    '<query xmlns="' + NS_BYTESTREAMS + '" sid="' + Sid + '">' +
      '<streamhost-used jid="' + HostJID + '"/>' +
    '</query>';
  Result := BuildIQ('result', ID, FromJID, ToJID, Inner);
end;

function TBarevFTManager.BuildBytestreamsError(const FromJID, ToJID, ID, Sid: string; const Code: string): string;
var
  Inner: string;
begin
  Inner :=
    '<error type="cancel" code="' + Code + '"><item-not-found xmlns="urn:ietf:params:xml:ns:xmpp-stanzas"/></error>';
  Result := BuildIQ('error', ID, FromJID, ToJID, Inner);
end;

function TBarevFTManager.ListenOnFixedRange(out LSock: TSocket; out Port: Word): Boolean;
var
  S: TSocket;
  Addr: TInetSockAddr6;
  P: Integer;
  Opt: LongInt;
begin
  Result := False;
  LSock := -1;
  Port := 0;

  S := fpSocket(AF_INET6, SOCK_STREAM, 0);
  if S = -1 then Exit;

  Opt := 1;
  fpSetSockOpt(S, SOL_SOCKET, SO_REUSEADDR, @Opt, SizeOf(Opt));

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.sin6_family := AF_INET6;
  Addr.sin6_addr := StrToNetAddr6('::'); // bind all v6

  for P := BAREV_FT_PORT_MIN to BAREV_FT_PORT_MAX do
  begin
    Addr.sin6_port := htons(P);
    if fpBind(S, @Addr, SizeOf(Addr)) = 0 then
    begin
      if fpListen(S, 1) = 0 then
      begin
        LSock := S;
        Port := P;
        Exit(True);
      end;
    end;
  end;

  CloseSocket(S);
end;

function TBarevFTManager.AcceptOne(LSock: TSocket; out CSock: TSocket; TimeoutSec: Integer): Boolean;
{$IFDEF UNIX}
var
  FDS: TFDSet;
  TV: TTimeVal;
  R: LongInt;
{$ENDIF}
begin
  Result := False;
  CSock := -1;

  {$IFDEF UNIX}
  fpFD_ZERO(FDS);
  fpFD_SET(LSock, FDS);
  TV.tv_sec := TimeoutSec;
  TV.tv_usec := 0;

  R := fpSelect(LSock + 1, @FDS, nil, nil, @TV);
  if R <= 0 then Exit(False);
  {$ELSE}
  // Windows: WinSock2 select
  FD_ZERO(FDS);
  FD_SET(LSock, FDS);
  TV.tv_sec := TimeoutSec;
  TV.tv_usec := 0;

  R := WinSock2.select(LSock + 1, @FDS, nil, nil, @TV);
  if R <= 0 then Exit(False);
  {$ENDIF}

  CSock := fpAccept(LSock, nil, nil);
  Result := (CSock <> -1);
end;

function TBarevFTManager.ConnectTCPv6(const Host: string; Port: Word; out Sock: TSocket): Boolean;
var
  S: TSocket;
  LocalAddr, RemoteAddr: TInetSockAddr6;
  LocalPort: Word;
  Opt: LongInt;
  BindSuccess: Boolean;
begin
  Result := False;
  Sock := -1;

  S := fpSocket(AF_INET6, SOCK_STREAM, 0);
  if S = -1 then Exit;

  // Set SO_REUSEADDR to allow binding to ports we might have just released
  Opt := 1;
  fpSetSockOpt(S, SOL_SOCKET, SO_REUSEADDR, @Opt, SizeOf(Opt));

  // Try to bind to a port in our controlled range (50000-50049)
  BindSuccess := False;
  for LocalPort := BAREV_FT_PORT_MIN to BAREV_FT_PORT_MAX do
  begin
    FillChar(LocalAddr, SizeOf(LocalAddr), 0);
    LocalAddr.sin6_family := AF_INET6;
    LocalAddr.sin6_port := htons(LocalPort);
    // sin6_addr is zeroed by FillChar - binds to any local interface (:: in IPv6)

    if fpBind(S, @LocalAddr, SizeOf(LocalAddr)) = 0 then
    begin
      BindSuccess := True;
      Log('DEBUG', 'FT: Bound source port ' + IntToStr(LocalPort) + ' for outgoing connection');
      Break;
    end;
  end;

  if not BindSuccess then
  begin
    Log('ERROR', 'FT: Could not bind to any port in range ' +
                 IntToStr(BAREV_FT_PORT_MIN) + '-' + IntToStr(BAREV_FT_PORT_MAX));
    CloseSocket(S);
    Exit;
  end;

  // Now connect to remote host
  FillChar(RemoteAddr, SizeOf(RemoteAddr), 0);
  RemoteAddr.sin6_family := AF_INET6;
  RemoteAddr.sin6_port := htons(Port);
  RemoteAddr.sin6_addr := StrToNetAddr6(Host);

  if fpConnect(S, @RemoteAddr, SizeOf(RemoteAddr)) = 0 then
  begin
    Sock := S;
    Exit(True);
  end;

  CloseSocket(S);
end;

function TBarevFTManager.ComputeDstAddr40(const Sid, InitiatorJID, TargetJID: string): string;
begin
  // XEP-0065: dst.addr = SHA1( sid + initiator + target ), hex lowercase 40 chars
  Result := SHA1Print(SHA1String(Sid + InitiatorJID + TargetJID));
end;

procedure TBarevFTManager.HandleIQ(Buddy: TBarevBuddy; const XML: string);
var
  IQType: string;
  HasSI, HasBytestreams: Boolean;
begin
  IQType := Trim(ExtractIQAttribute(XML, 'type'));  // Use ExtractIQAttribute to get type from <iq> tag only

  Log('DEBUG', 'FT: HandleIQ type=' + IQType + ' from ' + Buddy.JID);

  // Check for SI namespace with BOTH quote styles
  HasSI := (Pos('<si ', XML) > 0) and
           ((Pos('xmlns="' + NS_SI + '"', XML) > 0) or
            (Pos('xmlns=''' + NS_SI + '''', XML) > 0));

  if HasSI then
  begin
    Log('DEBUG', 'FT: Matched SI stanza');
    if IQType = 'set' then
    begin
      Log('DEBUG', 'FT: Calling HandleIncomingSI');
      HandleIncomingSI(Buddy, XML);
    end
    else if IQType = 'result' then
    begin
      Log('DEBUG', 'FT: Calling HandleIncomingSIResult');
      HandleIncomingSIResult(Buddy, XML);
    end
    else
      Log('DEBUG', 'FT: SI stanza with unhandled type: ' + IQType);
    Exit;
  end;

  // Check for bytestreams namespace with BOTH quote styles
  HasBytestreams := (Pos('<query', XML) > 0) and
                    ((Pos('xmlns="' + NS_BYTESTREAMS + '"', XML) > 0) or
                     (Pos('xmlns=''' + NS_BYTESTREAMS + '''', XML) > 0));

  if HasBytestreams then
  begin
    Log('DEBUG', 'FT: Matched bytestreams query');
    if IQType = 'set' then
    begin
      Log('DEBUG', 'FT: Calling HandleIncomingBytestreamsQuery');
      HandleIncomingBytestreamsQuery(Buddy, XML);
    end
    else if IQType = 'result' then
    begin
      Log('DEBUG', 'FT: Calling HandleIncomingBytestreamsResult');
      HandleIncomingBytestreamsResult(Buddy, XML);
    end
    else
      Log('DEBUG', 'FT: Bytestreams query with unhandled type: ' + IQType);
    Exit;
  end;

  // If we get here, the IQ didn't match our patterns
  Log('DEBUG', 'FT: Unhandled IQ - no SI or bytestreams match');
  if Length(XML) > 300 then
    Log('DEBUG', 'FT: IQ content (first 300 chars): ' + Copy(XML, 1, 300))
  else
    Log('DEBUG', 'FT: IQ content: ' + XML);
end;

procedure TBarevFTManager.HandleIncomingSI(Buddy: TBarevBuddy; const XML: string);
var
  Id, Sid, NameStr, SizeStr: string;
  SizeVal: Int64;
  X: TBarevFileTransfer;
begin
  Id := ExtractAttribute(XML, 'id');
  Sid := ExtractAttribute(XML, 'id'); // we use iq id as sid (good enough for now)

  NameStr := ExtractAttribute(XML, 'name');
  SizeStr := ExtractAttribute(XML, 'size');
  if not TryStrToInt64(SizeStr, SizeVal) then SizeVal := 0;

  X := TBarevFileTransfer.Create;
  X.Buddy := Buddy;
  X.Direction := ftRecv;
  X.State := ftsOfferReceived;
  X.IQId := Id;
  X.Sid := Sid;
  X.FileName := NameStr;
  X.FileSize := SizeVal;

  FXfers.Add(X);

  Log('INFO', 'FT: offer from ' + Buddy.JID + ' sid=' + Sid + ' file=' + NameStr + ' size=' + IntToStr(SizeVal));
  if Assigned(FOnFileOffer) then
    FOnFileOffer(Buddy, Sid, NameStr, SizeVal);
end;

procedure TBarevFTManager.HandleIncomingSIResult(Buddy: TBarevBuddy; const XML: string);
var
  Id: string;
  X: TBarevFileTransfer;
  MyJID, MyIP: string;
  LSock: TSocket;
  Port: Word;
  Q: string;
begin
  Log('DEBUG', 'FT: HandleIncomingSIResult from ' + Buddy.JID);

  // Receiver accepted our offer, now we send streamhost list and start listening.
  Id := ExtractAttribute(XML, 'id');
  Log('DEBUG', 'FT: SI result for ID=' + Id);

  X := FindXferBySid(Id);
  if not Assigned(X) then
  begin
    Log('ERROR', 'FT: No transfer found for SID=' + Id);
    Exit;
  end;

  Log('DEBUG', 'FT: Found transfer, direction=' + IntToStr(Ord(X.Direction)));

  if X.Direction <> ftSend then
  begin
    Log('ERROR', 'FT: Transfer ' + Id + ' is not outgoing');
    Exit;
  end;

  Log('DEBUG', 'FT: Attempting to listen on fixed port range');
  if not ListenOnFixedRange(LSock, Port) then
  begin
    X.State := ftsError;
    X.ErrorMsg := 'No free FT port in fixed range';
    Log('ERROR', 'FT: Failed to find free port in range');
    if Assigned(FOnError) then FOnError(Buddy, X.Sid, X.ErrorMsg);
    Exit;
  end;

  Log('INFO', 'FT: Listening on port ' + IntToStr(Port) + ' for transfer ' + Id);

  X.ListenSock := LSock;
  X.StreamPort := Port;
  if Assigned(FGetMyJID) then MyJID := FGetMyJID() else MyJID := '';
  if Assigned(FGetMyIPv6) then MyIP := FGetMyIPv6() else MyIP := '';

  Log('DEBUG', 'FT: MyJID=' + MyJID + ' MyIP=' + MyIP);

  Q := BuildBytestreamsQuery(MyJID, Buddy.JID, GenerateID('bs'), X.Sid, MyIP, Port);

  Log('DEBUG', 'FT: Built bytestreams query, length=' + IntToStr(Length(Q)));

  if not Assigned(FSendToBuddy) then
  begin
    Log('ERROR', 'FT: FSendToBuddy callback is not assigned!');
    Exit;
  end;

  if not Assigned(Buddy.Connection) then
  begin
    Log('ERROR', 'FT: Buddy ' + Buddy.JID + ' has no active connection');
    Exit;
  end;

  Log('DEBUG', 'FT: Sending bytestreams query to ' + Buddy.JID);
  FSendToBuddy(Buddy, Q);

  X.State := ftsStreamhostSent;

  // Start worker to accept + send file
  X.Worker := TBarevFTWorker.Create(X);
  Log('INFO', 'FT: streamhost sent, listening on ' + IntToStr(Port) + ' sid=' + X.Sid);
end;

procedure TBarevFTManager.HandleIncomingBytestreamsQuery(Buddy: TBarevBuddy; const XML: string);
var
  Sid, Host, PortStr, IQId: string;
  Port: Integer;
  X: TBarevFileTransfer;
  MyJID, Reply: string;
  Sock: TSocket;
  Dst: string;
begin
  Log('DEBUG', 'FT: HandleIncomingBytestreamsQuery from ' + Buddy.JID);
  IQId := ExtractAttribute(XML, 'id');
  Sid := ExtractAttribute(XML, 'sid');
  Host := ExtractAttribute(XML, 'host');
  PortStr := ExtractAttribute(XML, 'port');
  if not TryStrToInt(PortStr, Port) then Port := 0;

  Log('DEBUG', 'FT: Bytestreams query - SID=' + Sid + ' Host=' + Host + ' Port=' + IntToStr(Port));

  X := FindXferBySid(Sid);
  if not Assigned(X) then
  begin
    Log('ERROR', 'FT: No transfer found for SID=' + Sid);
    if Assigned(FGetMyJID) then MyJID := FGetMyJID() else MyJID := '';
    Reply := BuildBytestreamsError(MyJID, Buddy.JID, IQId, Sid);
    if Assigned(FSendToBuddy) then FSendToBuddy(Buddy, Reply);
    Exit;
  end;

  X.StreamHost := Host;
  X.StreamPort := Port;
  X.State := ftsStreamhostReceived;

  // Connect to senders streamhost and do SOCKS5 client handshake.
  if not ConnectTCPv6(Host, Port, Sock) then
  begin
    X.State := ftsError;
    X.ErrorMsg := 'Could not connect to streamhost ' + Host + ':' + IntToStr(Port);
    if Assigned(FOnError) then FOnError(Buddy, X.Sid, X.ErrorMsg);
    Exit;
  end;

  // Compute DstAddr per XEP-0065
  if Assigned(FGetMyJID) then
    MyJID := FGetMyJID()
  else
    MyJID := '';

  Dst := ComputeDstAddr40(Sid, Buddy.JID, MyJID);

  if Dst = '' then
  begin
    // fallback: common ordering
    if Assigned(FGetMyJID) then
      Dst := ComputeDstAddr40(Sid, Buddy.JID, FGetMyJID());
  end;

  if not Socks5_ClientHandshake(Sock, Dst) then
  begin
    CloseSocket(Sock);
    X.State := ftsError;
    X.ErrorMsg := 'SOCKS5 handshake failed';
    if Assigned(FOnError) then FOnError(Buddy, X.Sid, X.ErrorMsg);
    Exit;
  end;

  X.DataSock := Sock;

  // Tell sender which streamhost we used
  if Assigned(FGetMyJID) then MyJID := FGetMyJID() else MyJID := '';
  Reply := BuildBytestreamsResultStreamhostUsed(MyJID, Buddy.JID, IQId, Sid, Buddy.JID);
  if Assigned(FSendToBuddy) then
    FSendToBuddy(Buddy, Reply);

  // Start receiver worker
  X.Worker := TBarevFTWorker.Create(X);
  Log('INFO', 'FT: connected to streamhost, receiving sid=' + Sid);
end;

procedure TBarevFTManager.HandleIncomingBytestreamsResult(Buddy: TBarevBuddy; const XML: string);
begin
  // For our simplified interop, we don’t need to do much here.
  // The sender-side worker is already accepting on ListenSock and will proceed.
end;

function TBarevFTManager.OfferFile(Buddy: TBarevBuddy; const LocalPath: string): string;
var
  X: TBarevFileTransfer;
  MyJID: string;
  ID: string;
  FS: TFileStream;
  FileNameOnly: string;
  Offer: string;
begin
  Result := '';
  if Buddy = nil then Exit;
  if not FileExists(LocalPath) then Exit;
  if Assigned(FGetMyJID) then MyJID := FGetMyJID() else MyJID := '';

  ID := GenerateID('si');
  FileNameOnly := ExtractFileName(LocalPath);

  FS := TFileStream.Create(LocalPath, fmOpenRead or fmShareDenyNone);
  try
    X := TBarevFileTransfer.Create;
    X.Buddy := Buddy;
    X.Direction := ftSend;
    X.State := ftsOfferSent;
    X.IQId := ID;
    X.Sid := ID;
    X.FileName := FileNameOnly;
    X.FileSize := FS.Size;
    X.LocalPath := LocalPath;
    FXfers.Add(X);
  finally
    FS.Free;
  end;

  Offer := BuildSIOffer(MyJID, Buddy.JID, ID, ID, FileNameOnly, X.FileSize);
  if Assigned(FSendToBuddy) then
    FSendToBuddy(Buddy, Offer);

  Log('INFO', 'FT: offer sent sid=' + ID + ' file=' + FileNameOnly + ' size=' + IntToStr(X.FileSize));
  Result := ID;
end;

function TBarevFTManager.AcceptOffer(const Sid, SaveAs: string): Boolean;
var
  X: TBarevFileTransfer;
  Reply: string;
  MyJID: string;
  CurrentBuddy: TBarevBuddy;
begin
  X := FindXferBySid(Sid);
  if not Assigned(X) then
  begin
    Log('ERROR', 'FT: AcceptOffer - transfer ' + Sid + ' not found');
    Exit(False);
  end;

  if X.Direction <> ftRecv then
  begin
    Log('ERROR', 'FT: AcceptOffer - transfer ' + Sid + ' is not incoming');
    Exit(False);
  end;

  // Use the buddy from the transfer, but verify it is still connected
  CurrentBuddy := X.Buddy;
  if not Assigned(CurrentBuddy) or not Assigned(CurrentBuddy.Connection) then
  begin
    Log('ERROR', 'FT: AcceptOffer - buddy ' + X.Buddy.JID + ' not connected');
    Exit(False);
  end;

  X.LocalPath := SaveAs;
  X.State := ftsAccepted;

  if Assigned(FGetMyJID) then MyJID := FGetMyJID() else MyJID := '';
  Reply := BuildSIResultAccept(MyJID, CurrentBuddy.JID, X.IQId);

  Log('DEBUG', 'FT: Sending SI accept for ' + Sid + ' to ' + CurrentBuddy.JID);

  if Assigned(FSendToBuddy) then
    FSendToBuddy(CurrentBuddy, Reply)
  else
    Log('ERROR', 'FT: FSendToBuddy not assigned!');

  Result := True;
end;

function TBarevFTManager.RejectOffer(const Sid: string): Boolean;
var
  X: TBarevFileTransfer;
  MyJID: string;
  Reply: string;
begin
  Result := False;
  X := FindXferBySid(Sid);
  if not Assigned(X) then Exit;
  if X.Direction <> ftRecv then Exit;

  if Assigned(FGetMyJID) then MyJID := FGetMyJID() else MyJID := '';
  Reply := BuildSIErrorReject(MyJID, X.Buddy.JID, X.Sid);
  if Assigned(FSendToBuddy) then
    FSendToBuddy(X.Buddy, Reply);
  X.State := ftsError;
  X.ErrorMsg := 'Rejected by user';
  Result := True;
end;

end.
