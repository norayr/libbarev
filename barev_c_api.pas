library barev_capi;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, ctypes,
  Barev, BarevTypes;

(*
  C API design:

  - Client handle is opaque (Pointer).
  - Buddy handle is opaque (Pointer), points to TBarevBuddy owned by TBarevClient.
  - All callbacks are C-style (cdecl) + userdata.
  - Strings passed into callbacks are valid ONLY during the callback.
    If the caller wants to keep them, they should copy/strdup.
  - Functions returning PChar allocate a new C string (StrNew),
    caller frees with barev_strfree().
*)

type
  // Opaque to C: actually TBarevBuddy instance pointer
  TBarevBuddyHandle = Pointer;

  // Callback types (cdecl)
  TBarevLogCB = procedure(level, msg: PChar; userdata: Pointer); cdecl;
  TBarevTypingCB = procedure(buddy: TBarevBuddyHandle; isTyping: cint; userdata: Pointer); cdecl;
  TBarevBuddyStatusCB = procedure(buddy: TBarevBuddyHandle; oldStatus, newStatus: cint; userdata: Pointer); cdecl;
  TBarevMessageCB = procedure(buddy: TBarevBuddyHandle; msg: PChar; userdata: Pointer); cdecl;
  TBarevConnStateCB = procedure(buddy: TBarevBuddyHandle; state: cint; userdata: Pointer); cdecl;

  // Bridge object converts "of object" events to C callbacks
  TEventBridge = class
  public
    UserData: Pointer;

    LogCB: TBarevLogCB;
    TypingCB: TBarevTypingCB;
    BuddyStatusCB: TBarevBuddyStatusCB;
    MessageCB: TBarevMessageCB;
    ConnStateCB: TBarevConnStateCB;

    procedure OnLog(const LogLevel, Message: string);
    procedure OnTyping(Buddy: TBarevBuddy; IsTyping: Boolean);
    procedure OnBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
    procedure OnMessageReceived(Buddy: TBarevBuddy; const MessageText: string);
    procedure OnConnectionState(Buddy: TBarevBuddy; State: TConnectionState);
  end;

  PClientHandle = ^TClientHandle;
  TClientHandle = record
    Client: TBarevClient;
    Bridge: TEventBridge;
  end;

procedure TEventBridge.OnLog(const LogLevel, Message: string);
var
  lvlA, msgA: AnsiString;
begin
  if not Assigned(LogCB) then Exit;
  lvlA := AnsiString(LogLevel);
  msgA := AnsiString(Message);
  LogCB(PChar(lvlA), PChar(msgA), UserData);
end;

procedure TEventBridge.OnTyping(Buddy: TBarevBuddy; IsTyping: Boolean);
begin
  if not Assigned(TypingCB) then Exit;
  TypingCB(Pointer(Buddy), Ord(IsTyping), UserData);
end;

procedure TEventBridge.OnBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
begin
  if not Assigned(BuddyStatusCB) then Exit;
  BuddyStatusCB(Pointer(Buddy), Ord(OldStatus), Ord(NewStatus), UserData);
end;

procedure TEventBridge.OnMessageReceived(Buddy: TBarevBuddy; const MessageText: string);
var
  msgA: AnsiString;
begin
  if not Assigned(MessageCB) then Exit;
  msgA := AnsiString(MessageText);
  MessageCB(Pointer(Buddy), PChar(msgA), UserData);
end;

procedure TEventBridge.OnConnectionState(Buddy: TBarevBuddy; State: TConnectionState);
begin
  if not Assigned(ConnStateCB) then Exit;
  ConnStateCB(Pointer(Buddy), Ord(State), UserData);
end;

function barev_strdup(const s: string): PChar; cdecl;
begin
  Result := StrNew(PChar(s));
end;

procedure barev_strfree(p: PChar); cdecl;
begin
  if p <> nil then StrDispose(p);
end;

function H(handle: Pointer): PClientHandle; inline;
begin
  Result := PClientHandle(handle);
end;

function barev_client_new(nick, myipv6: PChar; port: cuint16): Pointer; cdecl;
var
  hdl: PClientHandle;
begin
  Result := nil;
  New(hdl);
  FillChar(hdl^, SizeOf(hdl^), 0);

  try
    hdl^.Bridge := TEventBridge.Create;
    hdl^.Bridge.UserData := nil;

    hdl^.Client := TBarevClient.Create(string(nick), string(myipv6), port);

    // Install bridges (will call C callbacks if assigned)
    hdl^.Client.OnLog := @hdl^.Bridge.OnLog;
    hdl^.Client.OnTypingNotification := @hdl^.Bridge.OnTyping;
    hdl^.Client.OnBuddyStatus := @hdl^.Bridge.OnBuddyStatus;
    hdl^.Client.OnMessageReceived := @hdl^.Bridge.OnMessageReceived;
    hdl^.Client.OnConnectionState := @hdl^.Bridge.OnConnectionState;

    Result := Pointer(hdl);
  except
    if hdl <> nil then
    begin
      if hdl^.Client <> nil then hdl^.Client.Free;
      if hdl^.Bridge <> nil then hdl^.Bridge.Free;
      Dispose(hdl);
    end;
    Result := nil;
  end;
end;

procedure barev_client_free(handle: Pointer); cdecl;
begin
  if handle = nil then Exit;

  if H(handle)^.Client <> nil then
  begin
    // Detach handlers before freeing
    H(handle)^.Client.OnLog := nil;
    H(handle)^.Client.OnTypingNotification := nil;
    H(handle)^.Client.OnBuddyStatus := nil;
    H(handle)^.Client.OnMessageReceived := nil;
    H(handle)^.Client.OnConnectionState := nil;

    H(handle)^.Client.Free;
  end;

  if H(handle)^.Bridge <> nil then
    H(handle)^.Bridge.Free;

  Dispose(H(handle));
end;

function barev_client_start(handle: Pointer): cint; cdecl;
begin
  if (handle <> nil) and (H(handle)^.Client <> nil) and H(handle)^.Client.Start then
    Result := 1
  else
    Result := 0;
end;

procedure barev_client_stop(handle: Pointer); cdecl;
begin
  if (handle <> nil) and (H(handle)^.Client <> nil) then
    H(handle)^.Client.Stop;
end;

procedure barev_client_process(handle: Pointer); cdecl;
begin
  if (handle <> nil) and (H(handle)^.Client <> nil) then
    H(handle)^.Client.Process;
end;

function barev_client_myjid(handle: Pointer): PChar; cdecl;
begin
  if (handle = nil) or (H(handle)^.Client = nil) then Exit(nil);
  Result := barev_strdup(H(handle)^.Client.MyJID);
end;

function barev_client_add_buddy(handle: Pointer; buddynick, buddyipv6: PChar; port: cuint16): Pointer; cdecl;
begin
  if (handle = nil) or (H(handle)^.Client = nil) then Exit(nil);
  Result := Pointer(H(handle)^.Client.AddBuddy(string(buddynick), string(buddyipv6), port));
end;

function barev_client_remove_buddy(handle: Pointer; buddyjid: PChar): cint; cdecl;
begin
  if (handle = nil) or (H(handle)^.Client = nil) then Exit(0);
  if H(handle)^.Client.RemoveBuddy(string(buddyjid)) then Result := 1 else Result := 0;
end;

function barev_client_find_buddy(handle: Pointer; buddyjid: PChar): Pointer; cdecl;
begin
  if (handle = nil) or (H(handle)^.Client = nil) then Exit(nil);
  Result := Pointer(H(handle)^.Client.FindBuddyByJID(string(buddyjid)));
end;

function barev_client_connect(handle: Pointer; buddyjid: PChar): cint; cdecl;
begin
  if (handle = nil) or (H(handle)^.Client = nil) then Exit(0);
  if H(handle)^.Client.ConnectToBuddy(string(buddyjid)) then Result := 1 else Result := 0;
end;

function barev_client_send_message(handle: Pointer; buddyjid, msg: PChar): cint; cdecl;
begin
  if (handle = nil) or (H(handle)^.Client = nil) then Exit(0);
  if H(handle)^.Client.SendMessage(string(buddyjid), string(msg)) then Result := 1 else Result := 0;
end;

function barev_client_send_presence(handle: Pointer; status: cint; statusMsg: PChar): cint; cdecl;
var
  st: TBuddyStatus;
begin
  if (handle = nil) or (H(handle)^.Client = nil) then Exit(0);
  st := TBuddyStatus(status);
  if H(handle)^.Client.SendPresence(st, string(statusMsg)) then Result := 1 else Result := 0;
end;

function barev_client_send_typing(handle: Pointer; buddyjid: PChar): cint; cdecl;
begin
  if (handle = nil) or (H(handle)^.Client = nil) then Exit(0);
  if H(handle)^.Client.SendTyping(string(buddyjid)) then Result := 1 else Result := 0;
end;

function barev_client_send_paused(handle: Pointer; buddyjid: PChar): cint; cdecl;
begin
  if (handle = nil) or (H(handle)^.Client = nil) then Exit(0);
  if H(handle)^.Client.SendPaused(string(buddyjid)) then Result := 1 else Result := 0;
end;

procedure barev_set_userdata(handle: Pointer; userdata: Pointer); cdecl;
begin
  if (handle = nil) or (H(handle)^.Bridge = nil) then Exit;
  H(handle)^.Bridge.UserData := userdata;
end;

procedure barev_set_log_cb(handle: Pointer; cb: TBarevLogCB); cdecl;
begin
  if (handle = nil) or (H(handle)^.Bridge = nil) then Exit;
  H(handle)^.Bridge.LogCB := cb;
end;

procedure barev_set_typing_cb(handle: Pointer; cb: TBarevTypingCB); cdecl;
begin
  if (handle = nil) or (H(handle)^.Bridge = nil) then Exit;
  H(handle)^.Bridge.TypingCB := cb;
end;

procedure barev_set_buddy_status_cb(handle: Pointer; cb: TBarevBuddyStatusCB); cdecl;
begin
  if (handle = nil) or (H(handle)^.Bridge = nil) then Exit;
  H(handle)^.Bridge.BuddyStatusCB := cb;
end;

procedure barev_set_message_cb(handle: Pointer; cb: TBarevMessageCB); cdecl;
begin
  if (handle = nil) or (H(handle)^.Bridge = nil) then Exit;
  H(handle)^.Bridge.MessageCB := cb;
end;

procedure barev_set_conn_state_cb(handle: Pointer; cb: TBarevConnStateCB); cdecl;
begin
  if (handle = nil) or (H(handle)^.Bridge = nil) then Exit;
  H(handle)^.Bridge.ConnStateCB := cb;
end;

(*
  Buddy getters: return newly allocated C strings (free with barev_strfree).
  Buddy pointer is owned by Barev; do NOT free it from C.
*)

function barev_buddy_get_jid(buddy: Pointer): PChar; cdecl;
begin
  if buddy = nil then Exit(nil);
  Result := barev_strdup(TBarevBuddy(buddy).JID);
end;

function barev_buddy_get_nick(buddy: Pointer): PChar; cdecl;
begin
  if buddy = nil then Exit(nil);
  Result := barev_strdup(TBarevBuddy(buddy).Nick);
end;

function barev_buddy_get_ipv6(buddy: Pointer): PChar; cdecl;
begin
  if buddy = nil then Exit(nil);
  Result := barev_strdup(TBarevBuddy(buddy).IPv6Address);
end;

function barev_buddy_get_port(buddy: Pointer): cuint16; cdecl;
begin
  if buddy = nil then Exit(0);
  Result := TBarevBuddy(buddy).Port;
end;

function barev_buddy_get_status(buddy: Pointer): cint; cdecl;
begin
  if buddy = nil then Exit(-1);
  Result := Ord(TBarevBuddy(buddy).Status);
end;

function barev_buddy_get_status_message(buddy: Pointer): PChar; cdecl;
begin
  if buddy = nil then Exit(nil);
  Result := barev_strdup(TBarevBuddy(buddy).StatusMessage);
end;

exports
  // string helpers
  barev_strfree name 'barev_strfree',

  // client lifecycle & loop
  barev_client_new name 'barev_client_new',
  barev_client_free name 'barev_client_free',
  barev_client_start name 'barev_client_start',
  barev_client_stop name 'barev_client_stop',
  barev_client_process name 'barev_client_process',
  barev_client_myjid name 'barev_client_myjid',

  // buddy mgmt
  barev_client_add_buddy name 'barev_client_add_buddy',
  barev_client_remove_buddy name 'barev_client_remove_buddy',
  barev_client_find_buddy name 'barev_client_find_buddy',

  // messaging / presence / typing
  barev_client_connect name 'barev_client_connect',
  barev_client_send_message name 'barev_client_send_message',
  barev_client_send_presence name 'barev_client_send_presence',
  barev_client_send_typing name 'barev_client_send_typing',
  barev_client_send_paused name 'barev_client_send_paused',

  // callbacks
  barev_set_userdata name 'barev_set_userdata',
  barev_set_log_cb name 'barev_set_log_cb',
  barev_set_typing_cb name 'barev_set_typing_cb',
  barev_set_buddy_status_cb name 'barev_set_buddy_status_cb',
  barev_set_message_cb name 'barev_set_message_cb',
  barev_set_conn_state_cb name 'barev_set_conn_state_cb',

  // buddy getters
  barev_buddy_get_jid name 'barev_buddy_get_jid',
  barev_buddy_get_nick name 'barev_buddy_get_nick',
  barev_buddy_get_ipv6 name 'barev_buddy_get_ipv6',
  barev_buddy_get_port name 'barev_buddy_get_port',
  barev_buddy_get_status name 'barev_buddy_get_status',
  barev_buddy_get_status_message name 'barev_buddy_get_status_message';

begin
end.
