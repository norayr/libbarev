{
  Simple Barev Test Program
  Demonstrates basic usage of the Barev library
}

program TestBarev;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Barev, BarevTypes;

const
  // Log levels
  LOG_ERROR = 0;
  LOG_WARN  = 1;
  LOG_INFO  = 2;
  LOG_DEBUG = 3;


type
  TEventHandler = class
    procedure OnLog(const LogLevel, Message: string);
    procedure OnBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
    procedure OnMessageReceived(Buddy: TBarevBuddy; const MessageText: string);
    procedure OnConnectionState(Buddy: TBarevBuddy; State: TConnectionState);

    // --- Typing notifications ---
    procedure OnTypingNotification(Buddy: TBarevBuddy; IsTyping: Boolean);

    // --- File transfer events ---
    procedure OnFTOffer(Buddy: TBarevBuddy; const Sid, FileName: string; FileSize: Int64);
    procedure OnFTProgress(Buddy: TBarevBuddy; const Sid: string; BytesDone, BytesTotal: Int64);
    procedure OnFTComplete(Buddy: TBarevBuddy; const Sid, LocalPath: string);
    procedure OnFTError(Buddy: TBarevBuddy; const Sid, ErrMsg: string);
  end;

type
  TNetThread = class(TThread)
  private
    FClient: TBarevClient;
  protected
    procedure Execute; override;
  public
    constructor Create(AClient: TBarevClient);
  end;

var
  Client: TBarevClient;
  Quit: Boolean;
  Handler: TEventHandler;
  Command: string;
  Parts: TStringList;
  IPv6Addr: string;
  Buddy: TBarevBuddy;
  i: Integer;
  tmpNick, tmpAddr: string;
  NetThread: TNetThread;
  CurrentLogLevel: Integer = LOG_INFO;  // Default: show ERROR, WARN, INFO


function GetLogLevelValue(const Level: string): Integer;
begin
  if Level = 'ERROR' then Exit(LOG_ERROR);
  if Level = 'WARN' then Exit(LOG_WARN);
  if Level = 'INFO' then Exit(LOG_INFO);
  if Level = 'DEBUG' then Exit(LOG_DEBUG);
  Result := LOG_INFO;  // Default
end;


constructor TNetThread.Create(AClient: TBarevClient);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FClient := AClient;
end;

procedure TNetThread.Execute;
begin
  while not Terminated do
  begin
    FClient.Process;
    Sleep(20); // 20ms is fine for interactive
  end;
end;

procedure TEventHandler.OnTypingNotification(Buddy: TBarevBuddy; IsTyping: Boolean);
begin
  WriteLn;
  if IsTyping then
    WriteLn('*** ', Buddy.Nick, ' is typing...')
  else
    WriteLn('*** ', Buddy.Nick, ' stopped typing');
  Write('> ');
  Flush(Output);
end;

procedure TEventHandler.OnLog(const LogLevel, Message: string);
var
  MsgLevel: Integer;
begin
  if CurrentLogLevel < 0 then Exit;  // Silent mode

  MsgLevel := GetLogLevelValue(LogLevel);
  if MsgLevel <= CurrentLogLevel then
    WriteLn('[', LogLevel, '] ', Message);
end;

procedure TEventHandler.OnBuddyStatus(Buddy: TBarevBuddy; OldStatus, NewStatus: TBuddyStatus);
begin
  WriteLn('*** ', Buddy.Nick, ' is now ', StatusToString(NewStatus));
end;

procedure TEventHandler.OnMessageReceived(Buddy: TBarevBuddy; const MessageText: string);
begin
  WriteLn;
  WriteLn('*** Message from ', Buddy.Nick, ': ', MessageText);
  Write('> ');  // Re-show prompt
  Flush(Output);
end;

procedure TEventHandler.OnConnectionState(Buddy: TBarevBuddy; State: TConnectionState);
const
  StateNames: array[TConnectionState] of string = (
    'Disconnected', 'Connecting', 'StreamInit', 'Authenticated', 'Online'
  );
begin
  WriteLn('*** Connection to ', Buddy.Nick, ': ', StateNames[State]);
end;

procedure TEventHandler.OnFTOffer(Buddy: TBarevBuddy; const Sid, FileName: string; FileSize: Int64);
begin
  WriteLn;
  WriteLn('*** File offer from ', Buddy.JID);
  WriteLn('    ', FileName, ' (', FileSize, ' bytes)');
  WriteLn('    SID=', Sid);
  WriteLn('*** To accept:  acceptfile ', Sid, ' /path/to/save');
  WriteLn('*** To reject:  rejectfile ', Sid);
  Write('> ');
  Flush(Output);
end;

procedure TEventHandler.OnFTProgress(Buddy: TBarevBuddy; const Sid: string; BytesDone, BytesTotal: Int64);
begin
  // Keep it quiet by default (progress can spam the console).
  // If you want periodic output, uncomment:
  // WriteLn('*** FT ', Sid, ': ', BytesDone, '/', BytesTotal);
end;

procedure TEventHandler.OnFTComplete(Buddy: TBarevBuddy; const Sid, LocalPath: string);
begin
  WriteLn;
  WriteLn('*** File transfer complete from ', Buddy.JID);
  WriteLn('    saved as: ', LocalPath);
  WriteLn('    SID=', Sid);
  Write('> ');
  Flush(Output);
end;

procedure TEventHandler.OnFTError(Buddy: TBarevBuddy; const Sid, ErrMsg: string);
begin
  WriteLn;
  WriteLn('*** File transfer error from ', Buddy.JID);
  WriteLn('    SID=', Sid);
  WriteLn('    ', ErrMsg);
  Write('> ');
  Flush(Output);
end;


procedure ShowHelp;
begin
  WriteLn('Commands:');
  WriteLn('  help                  - Show this help');
  WriteLn('  add <nick@ipv6>       - Add a buddy');
  WriteLn('  list                  - List all buddies');
  WriteLn('  connect <nick@ipv6>   - Connect to a buddy');
  WriteLn('  msg <nick@ipv6> <text> - Send a message');
  WriteLn('  status <status> [msg] - Set your status (available/away/dnd)');
 WriteLn;
  WriteLn('  --- Configuration ---');
  WriteLn('  loadconfig <file>     - Load configuration from file');
  WriteLn('  saveconfig            - Save current configuration');
  WriteLn;
  WriteLn('  --- Avatars ---');
  WriteLn('  setavatar <path>      - Set your avatar image');
  WriteLn('  clearavatar           - Remove your avatar');
  WriteLn('  getavatar <nick@ipv6> - Request buddy''s avatar');
  WriteLn('  showavatarhash        - Show your current avatar hash');
  WriteLn;
  WriteLn('  --- Typing Notifications ---');
  WriteLn('  typing <nick@ipv6>    - Send typing notification');
  WriteLn('  paused <nick@ipv6>    - Send paused notification');
  WriteLn;
  WriteLn('  --- File Transfer ---');
  WriteLn('  sendfile <nick@ipv6> <path>      - Offer a file to buddy');
  WriteLn('  acceptfile <sid> <saveas>        - Accept an incoming offer');
  WriteLn('  rejectfile <sid>                 - Reject an incoming offer');
  WriteLn;
  WriteLn('  quit                  - Exit the program');
  WriteLn;
end;

procedure ShowHelpCLI;
begin
  WriteLn('testbarev options:');
  WriteLn;
  WriteLn(' --help|-h       This help');
  WriteLn(' --quiet|-q      Log Level: LOG_ERROR');
  WriteLn(' --verbose|-v    Log Level: LOG_DEBUG');
  WriteLn(' --log-level <LEVEL>');
  WriteLn('     LEVEL can be:');
  WriteLn('       warn, info, debug');
  WriteLn(' --silent        Silent');
  HALT;
end;

procedure ParseCommandLine;
var
  i: Integer;
  Param: string;
begin
  i := 1;  // Initialize
  while i <= ParamCount do  // Use while instead of for
  begin
    Param := ParamStr(i);

    if (Param = '--help') or (Param = '-h') then
      ShowHelpCLI
    else if (Param = '--quiet') or (Param = '-q') then
      CurrentLogLevel := LOG_ERROR
    else if (Param = '--verbose') or (Param = '-v') then
      CurrentLogLevel := LOG_DEBUG
    else if Param = '--silent' then
      CurrentLogLevel := -1
    else if Param = '--log-level' then
    begin
      if i < ParamCount then
      begin
        Inc(i);  // Now OK - consume next parameter
        Param := ParamStr(i);
        if Param = 'error' then CurrentLogLevel := LOG_ERROR
        else if Param = 'warn' then CurrentLogLevel := LOG_WARN
        else if Param = 'info' then CurrentLogLevel := LOG_INFO
        else if Param = 'debug' then CurrentLogLevel := LOG_DEBUG;
      end;
    end;

    Inc(i);  // Move to next parameter
  end;
end;

begin
  WriteLn('Barev Test Program');
  WriteLn('==================');
  WriteLn;

  ParseCommandLine;

  // Get local configuration
  Write('Enter your nick: ');
  ReadLn(Command);

  if Command = '' then
  begin
    WriteLn('Error: Nick is required');
    Halt(1);
  end;

  Write('Enter your Yggdrasil IPv6 address: ');
  ReadLn(IPv6Addr);

  if (IPv6Addr = '') or not IsYggdrasilAddress(IPv6Addr) then
  begin
    WriteLn('Error: Valid Yggdrasil IPv6 address is required');
    WriteLn('(Should start with 200:, 201:, 202:, 203:, 300:, 301:, 302:, 303:, 3ff:, etc.)');
    Halt(1);
  end;

  WriteLn;
  WriteLn('Starting Barev client...');

  Parts := TStringList.Create;
  // Create client
  Client := TBarevClient.Create(Command, IPv6Addr);

  if not Client.Start then
  begin
    WriteLn('Failed to start client');
    Client.Free;
    Halt(1);
  end;

  WriteLn('Client started successfully as ', Client.MyJID);
  WriteLn('Listening on port ', Client.Port);
  WriteLn;
  WriteLn('Type "help" for commands');
  WriteLn;

  Handler := TEventHandler.Create;

  Client.OnLog             := @Handler.OnLog;
  Client.OnBuddyStatus     := @Handler.OnBuddyStatus;
  Client.OnMessageReceived := @Handler.OnMessageReceived;
  Client.OnConnectionState := @Handler.OnConnectionState;

  Client.OnTypingNotification := @Handler.OnTypingNotification;

  // ---- File transfer event hooks (FPC-friendly) ----
  if Assigned(Client.FileTransfer) then
  begin
    Client.FileTransfer.OnFileOffer := @Handler.OnFTOffer;
    Client.FileTransfer.OnProgress  := @Handler.OnFTProgress;
    Client.FileTransfer.OnComplete  := @Handler.OnFTComplete;
    Client.FileTransfer.OnError     := @Handler.OnFTError;
  end;
  // ---- end file transfer hooks ----



  Parts := TStringList.Create;
  Quit := False;

  NetThread := TNetThread.Create(Client);

  try
    while not Quit do
    begin
      // Process network events
      Client.Process;

      // Check for user input (non-blocking would be better, but this is simple)
      Write('> ');
      ReadLn(Command);
      Command := Trim(Command);

      if Command = '' then Continue;

      // Split command into parts
      Parts.Clear;
      Parts.Delimiter := ' ';
      Parts.StrictDelimiter := True;
      Parts.DelimitedText := Command;

      if Parts.Count = 0 then Continue;

      // Process command
      case LowerCase(Parts[0]) of
        'help':
          ShowHelp;

        'quit', 'exit':
          Quit := True;

      'add':
        begin
          if Parts.Count < 2 then
            WriteLn('Usage: add <nick@ipv6>')
          else
            if ParseJID(Parts[1], tmpNick, tmpAddr) then
            begin
              Buddy := Client.AddBuddy(tmpNick, tmpAddr);
              if Assigned(Buddy) then
                WriteLn('Added buddy: ', Buddy.JID)
              else
                WriteLn('Failed to add buddy');
            end
            else
              WriteLn('Invalid JID format. Use: nick@ipv6');
        end;


        'list':
          begin
            WriteLn('Buddies (', Client.GetBuddyCount, '):');
            for i := 0 to Client.GetBuddyCount - 1 do
            begin
              Buddy := Client.GetBuddyByIndex(i);
              WriteLn('  ', Buddy.Nick, '@', Buddy.IPv6Address, ':', Buddy.Port,
                     ' - ', StatusToString(Buddy.Status));
            end;
          end;

        'connect':
          begin
            if Parts.Count < 2 then
              WriteLn('Usage: connect <nick@ipv6>')
            else
            begin
              if Client.ConnectToBuddy(Parts[1]) then
                WriteLn('Connecting to ', Parts[1], '...')
              else
                WriteLn('Failed to connect to ', Parts[1]);
            end;
          end;

        'msg':
          begin
            if Parts.Count < 3 then
              WriteLn('Usage: msg <nick@ipv6> <message>')
            else
            begin
              // Join all parts after the JID as the message
              Command := '';
              for i := 2 to Parts.Count - 1 do
              begin
                if i > 2 then Command := Command + ' ';
                Command := Command + Parts[i];
              end;

              if Client.SendMessage(Parts[1], Command) then
                WriteLn('Message sent to ', Parts[1])
              else
                WriteLn('Failed to send message to ', Parts[1]);
            end;
          end;

        'status':
          begin
            if Parts.Count < 2 then
              WriteLn('Usage: status <available|away|dnd> [message]')
            else
            begin
              Command := '';
              if Parts.Count > 2 then
              begin
                for i := 2 to Parts.Count - 1 do
                begin
                  if i > 2 then Command := Command + ' ';
                  Command := Command + Parts[i];
                end;
              end;

              if Client.SendPresence(StringToStatus(Parts[1]), Command) then
                WriteLn('Status updated')
              else
                WriteLn('Failed to update status');
            end;
          end;

          { Configuration commands }
          'loadconfig':
            begin
              if Parts.Count < 2 then
                WriteLn('Usage: loadconfig <file>')
              else
              begin
                if Client.LoadConfig(Parts[1]) then
                begin
                  WriteLn('Configuration loaded from: ', Parts[1]);
                  WriteLn('Nick: ', Client.Nick);
                  WriteLn('IPv6: ', Client.MyIPv6);
                  WriteLn('Port: ', Client.Port);
                  if Client.AvatarManager.MyAvatarHash <> '' then
                    WriteLn('Avatar: ', Client.AvatarManager.MyAvatarPath);
                  WriteLn('Contacts loaded: ', Client.GetBuddyCount);
                end
                else
                  WriteLn('Failed to load configuration');
              end;
            end;

          'saveconfig':
            begin
              if Client.SaveConfig then
                WriteLn('Configuration saved')
              else
                WriteLn('Failed to save configuration');
            end;

          { Avatar commands }
          'setavatar':
            begin
              if Parts.Count < 2 then
                WriteLn('Usage: setavatar <path>')
              else
              begin
                // Join path if it contains spaces
                Command := '';
                for i := 1 to Parts.Count - 1 do
                begin
                  if i > 1 then Command := Command + ' ';
                  Command := Command + Parts[i];
                end;

                if Client.LoadMyAvatar(Command) then
                begin
                  WriteLn('Avatar loaded successfully');
                  WriteLn('Hash: ', Client.GetMyAvatarHash);
                  // Re-send presence to broadcast new avatar
                  Client.SendPresence;
                end
                else
                  WriteLn('Failed to load avatar');
              end;
            end;

          'clearavatar':
            begin
              Client.ClearMyAvatar;
              WriteLn('Avatar cleared');
              // Re-send presence to broadcast avatar removal
              Client.SendPresence;
            end;

          'getavatar':
            begin
              if Parts.Count < 2 then
                WriteLn('Usage: getavatar <nick@ipv6>')
              else
              begin
                if Client.RequestBuddyAvatar(Parts[1]) then
                  WriteLn('Avatar request sent to ', Parts[1])
                else
                  WriteLn('Failed to request avatar from ', Parts[1]);
              end;
            end;

          'showavatarhash':
            begin
              if Client.GetMyAvatarHash <> '' then
                WriteLn('Your avatar hash: ', Client.GetMyAvatarHash)
              else
                WriteLn('No avatar set');
            end;

          { Typing notification commands }
          'typing':
            begin
              if Parts.Count < 2 then
                WriteLn('Usage: typing <nick@ipv6>')
              else
              begin
                if Client.SendTyping(Parts[1]) then
                  WriteLn('Typing notification sent to ', Parts[1])
                else
                  WriteLn('Failed to send typing notification');
              end;
            end;
            
          'paused':
            begin
              if Parts.Count < 2 then
                WriteLn('Usage: paused <nick@ipv6>')
              else
              begin
                if Client.SendPaused(Parts[1]) then
                  WriteLn('Paused notification sent to ', Parts[1])
                else
                  WriteLn('Failed to send paused notification');
              end;
            end;
            
          'enabletyping':
            begin
              Client.TypingNotificationsEnabled := True;
              WriteLn('Typing notifications enabled');
            end;

          'disabletyping':
            begin
              Client.TypingNotificationsEnabled := False;
              WriteLn('Typing notifications disabled');
            end;

          'typingstatus':
            begin
              if Client.TypingNotificationsEnabled then
                WriteLn('Typing notifications: ENABLED')
              else
                WriteLn('Typing notifications: DISABLED');
            end;

         'sendfile':
           begin
             if Parts.Count < 3 then
               WriteLn('Usage: sendfile <nick@ipv6> <path>')
             else
             begin
               // Re-join the filename in case it contains spaces
               Command := '';
               for i := 2 to Parts.Count - 1 do
               begin
                 if i > 2 then Command := Command + ' ';
                 Command := Command + Parts[i];
               end;

               if not Assigned(Client.FileTransfer) then
                 WriteLn('File transfer not available (Client.FileTransfer is nil)')
               else
               begin
                 // OfferFile expects a TBarevBuddy, so resolve the buddy first
                 Buddy := Client.GetBuddy(Parts[1]);
                 if not Assigned(Buddy) then
                 begin
                   // fallback: try ConnectToBuddy-style lookup by JID
                   Buddy := Client.FindBuddyByJID(Parts[1]);
                 end;

                 if not Assigned(Buddy) then
                   WriteLn('Unknown buddy: ', Parts[1], ' (add it first)')
                 else
                 begin
                   // OfferFile returns SID (string).
                   WriteLn('Offering file: ', Command);
                   WriteLn('SID: ', Client.FileTransfer.OfferFile(Buddy, Command));
                 end;
               end;
             end;
           end;

         'acceptfile':
           begin
             if Parts.Count < 3 then
               WriteLn('Usage: acceptfile <sid> <saveas>')
             else
             begin
               // join save path if it contains spaces
               Command := '';
               for i := 2 to Parts.Count - 1 do
               begin
                 if i > 2 then Command := Command + ' ';
                 Command := Command + Parts[i];
               end;

               if not Assigned(Client.FileTransfer) then
                 WriteLn('File transfer not available (Client.FileTransfer is nil)')
               else
               begin
                 if Client.FileTransfer.AcceptOffer(Parts[1], Command) then
                   WriteLn('Accept started: SID=', Parts[1])
                 else
                   WriteLn('Accept failed: SID=', Parts[1]);
               end;
             end;
           end;

         'rejectfile':
           begin
             if Parts.Count < 2 then
               WriteLn('Usage: rejectfile <sid>')
             else
             begin
               if not Assigned(Client.FileTransfer) then
                 WriteLn('File transfer not available (Client.FileTransfer is nil)')
               else
               begin
                 Client.FileTransfer.RejectOffer(Parts[1]);
                 WriteLn('Rejected: SID=', Parts[1]);
               end;
             end;
           end;

      else
        WriteLn('Unknown command: ', Parts[0]);
        WriteLn('Type "help" for commands');
      end;
    end;
  finally

      if Assigned(NetThread) then
      begin
        NetThread.Terminate;
        NetThread.WaitFor;
        NetThread.Free;
      end;

      if Assigned(Client) then
      begin
        Client.Stop;   // optional if Free calls destructor that closes sockets anyway
        Client.Free;
      end;

      FreeAndNil(Parts);
      FreeAndNil(Handler);

  end;

  WriteLn('Goodbye!');
end.