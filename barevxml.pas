{ 
  Barev Protocol - XML Helpers
  Simple XML generation and parsing for XMPP stanzas
}

unit BarevXML;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BarevTypes;

type
  { Simple XML node representation }
  TXMLAttribute = record
    Name: string;
    Value: string;
  end;
  
  TXMLAttributeArray = array of TXMLAttribute;
  
  { XML Builder }
  TXMLBuilder = class
  private
    FXMLString: string;
  public
    procedure Clear;
    procedure AddHeader;
    procedure StartElement(const Name: string; Attributes: TXMLAttributeArray = nil);
    procedure EndElement(const Name: string);
    procedure AddElement(const Name, Content: string; Attributes: TXMLAttributeArray = nil);
    procedure AddEmptyElement(const Name: string; Attributes: TXMLAttributeArray = nil);
    procedure AddText(const Text: string);
    function ToString: string; override;
  end;

{ Stream header functions }
function BuildStreamHeader(const FromJID, ToJID: string): string;
function BuildStreamEnd: string;

{ Presence functions }
function BuildPresence(const ToJID: string = '';
                      Status: TBuddyStatus = bsAvailable;
                      const StatusMessage: string = ''): string;
function BuildOfflinePresence(const ToJID: string = ''): string;

{ Message functions }
function BuildMessage(const FromJID, ToJID, Body: string): string;

{ Ping/Pong functions }
function BuildPing(const FromJID, ToJID, PingID: string): string;
function BuildPong(const ToJID, PingID: string): string;

{ IQ functions }
function BuildIQ(const IQType, ID, FromJID, ToJID: string; 
                const ChildElements: string = ''): string;

{ Parsing functions }
function ExtractElementName(const XML: string): string;
function ExtractAttribute(const XML, AttrName: string): string;
function ExtractIQAttribute(const XML, AttrName: string): string;  // Extracts from first <iq> tag only
function ExtractElementContent(const XML, ElementName: string): string;
function IsStreamStart(const XML: string): Boolean;
function IsStreamEnd(const XML: string): Boolean;
function IsPresence(const XML: string): Boolean;
function IsMessage(const XML: string): Boolean;
function IsPing(const XML: string): Boolean;
function IsPong(const XML: string; const ExpectedID: string): Boolean;

{ Helper functions }
function XMLEscape(const Text: string): string;
function XMLUnescape(const Text: string): string;
function MakeAttribute(const Name, Value: string): TXMLAttribute;
function MakeAttributes(const Names, Values: array of string): TXMLAttributeArray;

implementation

uses
  StrUtils;

{ TXMLBuilder }

procedure TXMLBuilder.Clear;
begin
  FXMLString := '';
end;

procedure TXMLBuilder.AddHeader;
begin
  FXMLString := FXMLString + XML_HEADER + LineEnding;
end;

procedure TXMLBuilder.StartElement(const Name: string; Attributes: TXMLAttributeArray);
var
  i: Integer;
begin
  FXMLString := FXMLString + '<' + Name;
  
  if Assigned(Attributes) then
    for i := 0 to High(Attributes) do
      FXMLString := FXMLString + ' ' + Attributes[i].Name + '="' + 
                    XMLEscape(Attributes[i].Value) + '"';
  
  FXMLString := FXMLString + '>';
end;

procedure TXMLBuilder.EndElement(const Name: string);
begin
  FXMLString := FXMLString + '</' + Name + '>';
end;

procedure TXMLBuilder.AddElement(const Name, Content: string; Attributes: TXMLAttributeArray);
begin
  StartElement(Name, Attributes);
  AddText(Content);
  EndElement(Name);
end;

procedure TXMLBuilder.AddEmptyElement(const Name: string; Attributes: TXMLAttributeArray);
var
  i: Integer;
begin
  FXMLString := FXMLString + '<' + Name;
  
  if Assigned(Attributes) then
    for i := 0 to High(Attributes) do
      FXMLString := FXMLString + ' ' + Attributes[i].Name + '="' + 
                    XMLEscape(Attributes[i].Value) + '"';
  
  FXMLString := FXMLString + '/>';
end;

procedure TXMLBuilder.AddText(const Text: string);
begin
  FXMLString := FXMLString + XMLEscape(Text);
end;

function TXMLBuilder.ToString: string;
begin
  Result := FXMLString;
end;

{ Stream functions }

function BuildStreamHeader(const FromJID, ToJID: string): string;
var
  Builder: TXMLBuilder;
  Attrs: TXMLAttributeArray;
begin
  Builder := TXMLBuilder.Create;
  try
    Builder.AddHeader;
    
    SetLength(Attrs, 4);
    Attrs[0] := MakeAttribute('xmlns', JABBER_CLIENT_NS);
    Attrs[1] := MakeAttribute('xmlns:stream', STREAM_NAMESPACE);
    Attrs[2] := MakeAttribute('from', FromJID);
    Attrs[3] := MakeAttribute('to', ToJID);
    
    Builder.StartElement('stream:stream', Attrs);
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function BuildStreamEnd: string;
begin
  Result := '</stream:stream>';
end;

{ Presence functions }

function BuildPresence(const ToJID: string; Status: TBuddyStatus; 
                      const StatusMessage: string): string;
var
  Builder: TXMLBuilder;
  Attrs: TXMLAttributeArray;
begin
  Builder := TXMLBuilder.Create;
  try
    SetLength(Attrs, 0);
    if ToJID <> '' then
    begin
      SetLength(Attrs, 1);
      Attrs[0] := MakeAttribute('to', ToJID);
    end;
    
    Builder.StartElement('presence', Attrs);
    
    // Add show element if not available
    if Status <> bsAvailable then
      Builder.AddElement('show', StatusToString(Status));
    
    // Add status message if provided
    if StatusMessage <> '' then
      Builder.AddElement('status', StatusMessage);
    
    Builder.EndElement('presence');
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

function BuildOfflinePresence(const ToJID: string): string;
var
  Builder: TXMLBuilder;
  Attrs: TXMLAttributeArray;
begin
  Builder := TXMLBuilder.Create;
  try
    SetLength(Attrs, 1);
    Attrs[0] := MakeAttribute('type', 'unavailable');
    
    if ToJID <> '' then
    begin
      SetLength(Attrs, 2);
      Attrs[1] := MakeAttribute('to', ToJID);
    end;
    
    Builder.AddEmptyElement('presence', Attrs);
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

{ Message functions }

function BuildMessage(const FromJID, ToJID, Body: string): string;
var
  Builder: TXMLBuilder;
  Attrs: TXMLAttributeArray;
begin
  Builder := TXMLBuilder.Create;
  try
    SetLength(Attrs, 3);
    Attrs[0] := MakeAttribute('to', ToJID);
    Attrs[1] := MakeAttribute('from', FromJID);
    Attrs[2] := MakeAttribute('type', 'chat');
    
    Builder.StartElement('message', Attrs);
    Builder.AddElement('body', Body);
    Builder.EndElement('message');
    
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

{ Ping/Pong functions }

function BuildPing(const FromJID, ToJID, PingID: string): string;
var
  Builder: TXMLBuilder;
  PingBuilder: TXMLBuilder;
  Attrs: TXMLAttributeArray;
  PingAttrs: TXMLAttributeArray;
begin
  Builder := TXMLBuilder.Create;
  PingBuilder := TXMLBuilder.Create;
  try
    // Build ping child element
    SetLength(PingAttrs, 1);
    PingAttrs[0] := MakeAttribute('xmlns', PING_NAMESPACE);
    PingBuilder.AddEmptyElement('ping', PingAttrs);
    
    // Build IQ wrapper
    SetLength(Attrs, 4);
    Attrs[0] := MakeAttribute('type', 'get');
    Attrs[1] := MakeAttribute('id', PingID);
    Attrs[2] := MakeAttribute('from', FromJID);
    Attrs[3] := MakeAttribute('to', ToJID);
    
    Builder.StartElement('iq', Attrs);
    Builder.AddText(PingBuilder.ToString);
    Builder.EndElement('iq');
    
    Result := Builder.ToString;
  finally
    Builder.Free;
    PingBuilder.Free;
  end;
end;

function BuildPong(const ToJID, PingID: string): string;
var
  Builder: TXMLBuilder;
  Attrs: TXMLAttributeArray;
begin
  Builder := TXMLBuilder.Create;
  try
    SetLength(Attrs, 3);
    Attrs[0] := MakeAttribute('type', 'result');
    Attrs[1] := MakeAttribute('id', PingID);
    Attrs[2] := MakeAttribute('to', ToJID);
    
    Builder.AddEmptyElement('iq', Attrs);
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

{ IQ functions }

function BuildIQ(const IQType, ID, FromJID, ToJID: string; 
                const ChildElements: string): string;
var
  Builder: TXMLBuilder;
  Attrs: TXMLAttributeArray;
begin
  Builder := TXMLBuilder.Create;
  try
    SetLength(Attrs, 4);
    Attrs[0] := MakeAttribute('type', IQType);
    Attrs[1] := MakeAttribute('id', ID);
    Attrs[2] := MakeAttribute('from', FromJID);
    Attrs[3] := MakeAttribute('to', ToJID);
    
    Builder.StartElement('iq', Attrs);
    if ChildElements <> '' then
      Builder.AddText(ChildElements);
    Builder.EndElement('iq');
    
    Result := Builder.ToString;
  finally
    Builder.Free;
  end;
end;

{ Parsing functions }

function ExtractElementName(const XML: string): string;
var
  StartPos, EndPos: Integer;
begin
  Result := '';
  StartPos := Pos('<', XML);
  if StartPos = 0 then Exit;
  
  Inc(StartPos);
  if (StartPos <= Length(XML)) and (XML[StartPos] = '/') then
    Inc(StartPos); // Skip closing tag slash
  
  EndPos := StartPos;
  while (EndPos <= Length(XML)) and 
        (XML[EndPos] <> ' ') and 
        (XML[EndPos] <> '>') and
        (XML[EndPos] <> '/') do
    Inc(EndPos);
  
  Result := Copy(XML, StartPos, EndPos - StartPos);
end;

function ExtractAttribute(const XML, AttrName: string): string;
var
  AttrPos, StartPos, EndPos: Integer;
  SearchStr: string;
begin
  Result := '';
  SearchStr := AttrName + '="';
  AttrPos := Pos(SearchStr, XML);
  
  if AttrPos = 0 then Exit;
  
  StartPos := AttrPos + Length(SearchStr);
  EndPos := StartPos;
  
  while (EndPos <= Length(XML)) and (XML[EndPos] <> '"') do
    Inc(EndPos);
  
  Result := Copy(XML, StartPos, EndPos - StartPos);
  Result := XMLUnescape(Result);
end;

{ ExtractIQAttribute - Extracts attribute from FIRST <iq> tag only }
function ExtractIQAttribute(const XML, AttrName: string): string;
var
  IQStart, IQEnd, AttrPos, StartPos, EndPos: Integer;
  IQTag, SearchStr: string;
begin
  Result := '';
  
  // Find the first <iq tag
  IQStart := Pos('<iq ', XML);
  if IQStart = 0 then
  begin
    IQStart := Pos('<iq>', XML);
    if IQStart = 0 then Exit;
  end;
  
  // Find the end of the opening <iq> tag (the '>' character)
  IQEnd := IQStart;
  while (IQEnd <= Length(XML)) and (XML[IQEnd] <> '>') do
    Inc(IQEnd);
  
  if IQEnd > Length(XML) then Exit;
  
  // Extract just the <iq> tag
  IQTag := Copy(XML, IQStart, IQEnd - IQStart + 1);
  
  // Now search for the attribute within this tag only
  SearchStr := AttrName + '="';
  AttrPos := Pos(SearchStr, IQTag);
  
  if AttrPos = 0 then
  begin
    // Try single quotes
    SearchStr := AttrName + '=''';
    AttrPos := Pos(SearchStr, IQTag);
    if AttrPos = 0 then Exit;
    
    // Extract with single quotes
    StartPos := AttrPos + Length(SearchStr);
    EndPos := StartPos;
    while (EndPos <= Length(IQTag)) and (IQTag[EndPos] <> '''') do
      Inc(EndPos);
    Result := Copy(IQTag, StartPos, EndPos - StartPos);
    Exit;
  end;
  
  // Extract with double quotes
  StartPos := AttrPos + Length(SearchStr);
  EndPos := StartPos;
  while (EndPos <= Length(IQTag)) and (IQTag[EndPos] <> '"') do
    Inc(EndPos);
  
  Result := Copy(IQTag, StartPos, EndPos - StartPos);
  Result := XMLUnescape(Result);
end;

function ExtractElementContent(const XML, ElementName: string): string;
var
  StartTag, EndTag: string;
  StartPos, EndPos: Integer;
begin
  Result := '';
  StartTag := '<' + ElementName;
  EndTag := '</' + ElementName + '>';
  
  StartPos := Pos(StartTag, XML);
  if StartPos = 0 then Exit;
  
  // Find the end of start tag
  StartPos := PosEx('>', XML, StartPos);
  if StartPos = 0 then Exit;
  Inc(StartPos);
  
  // Find end tag
  EndPos := Pos(EndTag, XML);
  if EndPos = 0 then Exit;
  
  Result := Copy(XML, StartPos, EndPos - StartPos);
  Result := XMLUnescape(Result);
end;

function IsStreamStart(const XML: string): Boolean;
begin
  Result := (Pos('<stream:stream', XML) > 0) or (Pos('stream:stream', XML) > 0);
end;

function IsStreamEnd(const XML: string): Boolean;
begin
  Result := Pos('</stream:stream>', XML) > 0;
end;

function IsPresence(const XML: string): Boolean;
begin
  Result := Pos('<presence', XML) > 0;
end;

function IsMessage(const XML: string): Boolean;
begin
  Result := Pos('<message', XML) > 0;
end;

function IsPing(const XML: string): Boolean;
begin
  Result := (Pos('<iq', XML) > 0) and 
            ((Pos('<ping', XML) > 0) or (Pos('urn:xmpp:ping', XML) > 0)) and
            (Pos('type="get"', XML) > 0);
end;

function IsPong(const XML: string; const ExpectedID: string): Boolean;
var
  IDAttr: string;
begin
  Result := False;
  if (Pos('<iq', XML) = 0) or (Pos('type="result"', XML) = 0) then
    Exit;
  
  if ExpectedID <> '' then
  begin
    IDAttr := ExtractAttribute(XML, 'id');
    Result := (IDAttr = ExpectedID);
  end
  else
    Result := True;
end;

{ Helper functions }

function XMLEscape(const Text: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(Text) do
  begin
    case Text[i] of
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '&': Result := Result + '&amp;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&apos;';
    else
      Result := Result + Text[i];
    end;
  end;
end;

function XMLUnescape(const Text: string): string;
begin
  Result := Text;
  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);
  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
  Result := StringReplace(Result, '&quot;', '"', [rfReplaceAll]);
  Result := StringReplace(Result, '&apos;', '''', [rfReplaceAll]);
end;

function MakeAttribute(const Name, Value: string): TXMLAttribute;
begin
  Result.Name := Name;
  Result.Value := Value;
end;

function MakeAttributes(const Names, Values: array of string): TXMLAttributeArray;
var
  i, Count: Integer;
begin
  Count := Length(Names);
  if Count > Length(Values) then
    Count := Length(Values);
  
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
  begin
    Result[i].Name := Names[i];
    Result[i].Value := Values[i];
  end;
end;

end.