unit uUtils;

interface

  uses regexpr, regularexpressions, idtcpclient, winapi.windows,
    winapi.messages,
    system.sysutils, system.variants,
    system.classes, vcl.graphics, vcl.stdctrls,
    vcl.controls, vcl.forms, vcl.dialogs,
    strutils, dateutils,
    math, vcl.menus,
    data.db, data.win.adodb,
    vcl.checklst, activex, typinfo, ufunctionconstvars, ulogit,
    idsslopenssl, iduri, idhttp, idglobal, encddecd, System.Character;

  type
    tutils = class
      class function stringreplaceall(text, byt, mot: String): String;
      class function clearstring(src: string): string;
      class function getpartwithregexpr(wtf: String; body: String): string;
      class function getpartwithregexprhealbroken(wtf: String;
        wtfbroken: string; body: String; bodybroken: string): string;
      class function checkurlconnection(host: String; port: integer = 80;
        timeout: integer = 1000): boolean;
      class function gettempdir(): string;
      class function gettempdirwithsubdir(subdir: String): String;
      class function gettempdirxp(): string;
      class function getspecialfolderlocation(const folder: integer;
        const foldernew: tguid): String;
      class function decodemysqldatetime(datetimestr: string): tdatetime;
      class function _getsslversions: tidsslversions;
      class function isheadermediatype(const aheaderline,
        amediatype: String): boolean;
      class function mediatypematches(const avalue, amediatype: String)
        : boolean;
      class function collectparamsuri(u: string): string;
      class function collectparamsshellurl(u: string; idmain: string): string;
      class function GetAppVersionStr: string;
      class function ISNULL(input: Variant; s: Double = 0.0): Double; overload;
      class function ISNULL(input: Variant; s: Integer = 0): Integer; overload;
      class function ISNULL(input: Variant; s: string = ''): string; overload;
      class function ReplaceNonDigits(const AStr: string): string;
      class function ReplaceNonDigitsAndPunto(const AStr: string): string;
    end;

implementation

  { TUtils }

class function tutils.ReplaceNonDigits(const AStr: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(AStr) do
  begin
    //if AStr[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] then
    if AStr[i].IsDigit then
      Result := Result + AStr[i];
  end;
end;

class function tutils.ReplaceNonDigitsAndPunto(const AStr: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(AStr) do
  begin
    if AStr[i] in ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', ','] then
      if AStr[i] in ['.', ','] then
        begin
          if Pos(',', Result)=0 then
             Result := Result + ReplaceStr(AStr[i], '.', ',');
        end else
      Result := Result + AStr[i];
  end;
end;

class function tutils.ISNULL(input: Variant; s: Double = 0): Double;
begin
  try
    if VarIsNull(input) then
      Result := s
    else
      Result := input;
  except
    Result := s;
  end;
end;

class function tutils.ISNULL(input: Variant; s: Integer = 0): Integer;
begin
  try
    if VarIsNull(input) then
      Result := s
    else
      Result := input;
  except
    Result := s;
  end;
end;

class function tutils.ISNULL(input: Variant; s: string = ''): string;
begin
  try
    if VarIsNull(input) then
      Result := s
    else
      Result := input;
  except
    Result := s;
  end;
end;

  class function tutils.GetAppVersionStr: string;
var
  Exe: string;
  Size, Handle: DWORD;
  Buffer: TBytes;
  FixedPtr: PVSFixedFileInfo;
begin
  Exe := ParamStr(0);
  Size := GetFileVersionInfoSize(PChar(Exe), Handle);
  if Size = 0 then
    RaiseLastOSError;
  SetLength(Buffer, Size);
  if not GetFileVersionInfo(PChar(Exe), Handle, Size, Buffer) then
    RaiseLastOSError;
  if not VerQueryValue(Buffer, '\', Pointer(FixedPtr), Size) then
    RaiseLastOSError;
  // major
  Result := Format('%d.%d.%d.%d', [LongRec(FixedPtr.dwFileVersionMS).Hi,
    LongRec(FixedPtr.dwFileVersionMS).Lo, // minor
    LongRec(FixedPtr.dwFileVersionLS).Hi, // release
    LongRec(FixedPtr.dwFileVersionLS).Lo]) // build
end;

  class function tutils.collectparamsuri(u: string): string;
    var
      params: tstringlist;
      i: integer;
      uri: tiduri;
      url: string;
      bytes: tidbytes;
      base64string: String;
    begin
      {
        Bytes := ...; // array of bytes
        Base64String := TIdEncoderMIME.EncodeBytes(Bytes);
        Bytes:= TIdDecoderMIME.DecodeBytes(Base64String);
      }
      // INN-ORGNAME-TELNUM-EMAIL
      params := tstringlist.create;
      try

        for i := 0 to params.count - 1 do
          begin
            if params.names[i] = 'ORGNAME' then
              params.strings[i] := tiduri.paramsencode(params.names[i], enutf8)
                + '=' + encodestring(params.valuefromindex[i])
            else
              params.strings[i] := tiduri.paramsencode(params.names[i], enutf8)
                + '=' + tiduri.paramsencode(params.valuefromindex[i], enutf8);
            {
              EncodeString(TIdURI.ParamsEncode(Params.ValueFromIndex[i], enUTF8));
            }
            params.strings[i] := stringreplace(params.strings[i], ' ', '+',
              [rfreplaceall]);
          end;

        params.delimiter := '&';
        // Params.StrictDelimiter:=true;

        uri := tiduri.create(u);
        try
          uri.params := params.delimitedtext;
          url := uri.uri;
        finally
          uri.free;
        end;
      finally
        params.free;
      end;
      result := url;
    end;

  class function tutils.collectparamsshellurl(u: string;
    idmain: string): string;
    var
      params: tstringlist;
      i: integer;
      uri: tiduri;
      url: string;
      bytes: tidbytes;
      base64string: String;
    begin
      params := tstringlist.create;
      try
        if idmain <> '' then
          params.add('idMain=' + idmain);

        for i := 0 to params.count - 1 do
          begin
            params.strings[i] := tiduri.paramsencode(params.names[i], enutf8) +
              '=' + tiduri.paramsencode(params.valuefromindex[i], enutf8);
            params.strings[i] := stringreplace(params.strings[i], ' ', '+',
              [rfreplaceall]);
          end;

        params.delimiter := '&';
        // Params.StrictDelimiter:=true;

        uri := tiduri.create(u);
        try
          uri.params := params.delimitedtext;
          url := uri.uri;
        finally
          uri.free;
        end;
      finally
        params.free;
      end;
      result := url;
    end;

  class function tutils._getsslversions: tidsslversions;
    var
      i: tidsslversion;
    begin
      result := [];
      for i := Low(tidsslversion) to High(tidsslversion) do
        begin
          include(result, i);
        end;
    end;

  class function tutils.isheadermediatype(const aheaderline,
    amediatype: String): boolean;
    begin
      result := tutils.mediatypematches((aheaderline), amediatype);
    end;

  class function tutils.mediatypematches(const avalue,
    amediatype: String): boolean;
    begin
      if pos('/', amediatype) > 0 then
        begin
          result := textissame(avalue, amediatype);
        end
      else
        begin
          result := textstartswith(avalue, amediatype + '/');
        end;
    end;

  class function tutils.decodemysqldatetime(datetimestr: string): tdatetime;
      function part(startindex, endindex: integer): integer;
        begin
          try
            result := strtointdef(trim(copy(datetimestr, startindex,
              endindex - startindex)), 0);
          except
            logapp.writetolog('Ошибка DecodeMySQLDateTime Part', lmterror,
              lmlmax, true);
          end;
        end;

      function nextpart(nextdelimiter: string; var indexstart: integer;
        out value: integer): boolean;
        var
          aendindex: integer;
        begin
          try
            aendindex := posex(nextdelimiter, datetimestr, indexstart);
            result := aendindex > 0;
            if result then
              begin
                value := part(indexstart, aendindex);
                indexstart := aendindex + length(nextdelimiter);

                { skip spaces }
                while (indexstart <= length(datetimestr)) and
                  (datetimestr[indexstart] = ' ') do
                  inc(indexstart)

              end
            else
              value := 0;
          except
            logapp.writetolog('Ошибка DecodeMySQLDateTime NextPart', lmterror,
              lmlmax, true);
          end;
        end;

    var
      aindexstart: integer;
      ayear, amonth, aday, ahour, aminute, asecond: integer;
    begin
      try
        aindexstart := 1;

        if nextpart('-', aindexstart, ayear) and
          nextpart('-', aindexstart, amonth) and
          nextpart(' ', aindexstart, aday) and nextpart(':', aindexstart, ahour)
          and nextpart(':', aindexstart, aminute) then
          asecond := part(aindexstart, length(datetimestr) + 1)
        else
          raise exception.create('Invalid date time format');

        result := encodedatetime(ayear, amonth, aday, ahour, aminute,
          asecond, 0);
      except
        logapp.writetolog('Ошибка DecodeMySQLDateTime', lmterror, lmlmax, true);
      end;
    end;

  class function tutils.stringreplaceall(text, byt, mot: String): String;
    var
      plats: integer;
    begin
      try
        While pos(byt, text) > 0 do
          begin
            plats := pos(byt, text);
            delete(text, plats, length(byt));
            insert(mot, text, plats);
          end;
        result := text;
      except
        logapp.writetolog('Ошибка StringReplaceAll ' + text, lmterror,
          lmldebug, true);
      end;
    end;

  class function tutils.clearstring(src: string): string;
    var
      s: string;
    begin
      try
        s := '';
        s := src;
        s := tutils.stringreplaceall(s, #9, '');
        s := tutils.stringreplaceall(s, #10, '');
        s := tutils.stringreplaceall(s, #13, '');
        s := tutils.stringreplaceall(s, char(vk_return), '');
        s := tutils.stringreplaceall(s, char(vk_tab), '');
        s := tutils.stringreplaceall(s, '  ', ' ');
        s := tutils.stringreplaceall(s, ', , ', '');

        s := trim(s);
        if s <> '' then
          while s[1] = ',' do
            begin
              s := trim(rightstr(s, length(s) - 1));
              if s = '' then
                s := ' ';
            end;
        if s <> '' then
          while s[length(s)] = ',' do
            begin
              s := trim(leftstr(s, length(s) - 1));
              if s = '' then
                s := ' ';
            end;

        result := s;
      except
        on e: exception do
          logapp.writetolog('Ошибка в ClearString: ' + e.message, lmterror,
            lmldebug, true);
      end;
    end;

  class function tutils.getpartwithregexprhealbroken(wtf: String;
    wtfbroken: string; body: String; bodybroken: string): string;
    var
      regexp: tregex;
      matches: tmatchcollection;
    begin

      result := '';
      try
        regexp.create(wtf);
        matches := regexp.matches(body);
        if matches.count > 0 then
          begin
            if matches.item[0].success then
              begin
                if matches.item[0].groups.count > 0 then
                  begin
                    result := matches.item[0].groups.item[1].value;
                  end;
              end;

            result := tutils.stringreplaceall
              (tutils.stringreplaceall(tutils.stringreplaceall(result, '&gt;',
              '>'), '&nbsp;', ' '), '<br>', char(10));
          end
        else
          begin
            regexp.create(wtf);
            matches := regexp.matches(body);
            if matches.count > 0 then
              //
              regexp.create(wtfbroken);
            matches := regexp.matches(body + bodybroken);
            if matches.count > 0 then
              begin
                if matches.item[0].success then
                  begin
                    if matches.item[0].groups.count > 0 then
                      begin
                        result := matches.item[0].groups.item[1].value;
                      end;
                  end;

                result := tutils.stringreplaceall
                  (tutils.stringreplaceall(tutils.stringreplaceall(result,
                  '&gt;', '>'), '&nbsp;', ' '), '<br>', char(10));
              end
            else
              result := 'Текст сеанса в файле';
          end;
      finally

      end;
    end;

  class function tutils.getpartwithregexpr(wtf: String; body: String): string;
    var
      regexp: tregexpr;
    begin
      result := '';
      regexp := tregexpr.create;
      try
        regexp.expression := wtf;
        if regexp.exec(body) then
          begin
            result := regexp.match[1];
          end;
      finally
        regexp.free;
      end;
    end;

  class function tutils.checkurlconnection(host: String; port: integer = 80;
    timeout: integer = 1000): boolean;
    var
      tcp: tidtcpclient;
    begin
      tcp := tidtcpclient.create(nil);
      try
        tcp.host := host;
        tcp.port := port;
        tcp.connecttimeout := timeout;
        try
          tcp.connect;
          result := tcp.connected;
        except
          result := false;
        end;
      finally
        freeandnil(tcp);
      end;
    end;

  class function tutils.gettempdirxp(): string;
    var
      p: pchar;
      shgetfolderpath: function(hwnd: hwnd; csidl: integer; htoken: thandle;
        dwflags: dword; pszpath: pchar): hresult; stdcall;
    begin
      p := nil;
      try
        p := allocmem(max_path);
        if shgetfolderpath(0, csidl_appdata { CSIDL_LOCAL_APPDATA } , 0, 0, p) = s_ok
        then
          result := p + '\'
        else
          result := '';
      finally
        freemem(p);
      end;
    end;

  class function tutils.getspecialfolderlocation(const folder: integer;
    const foldernew: tguid): String;
    const
      kf_flag_dont_verify = $00004000;
    var
      folderpath: pwidechar;
      shgetfolderpath: function(hwnd: hwnd; csidl: integer; htoken: thandle;
        dwflags: dword; pszpath: pwidechar): hresult; stdcall;
      shgetknownfolderpath: function(const rfid: tiid; dwflags: dword;
        htoken: thandle; var ppszpath: pwidechar): hresult; stdcall;
    begin
      result := '';

      if not comparemem(@foldernew, @guid_null, sizeof(tguid)) then
        begin
          shgetknownfolderpath := getprocaddress(getmodulehandle('Shell32.dll'),
            'SHGetKnownFolderPath');
          if assigned(shgetknownfolderpath) then
            begin
              folderpath := nil;
              setlasterror(cardinal(shgetknownfolderpath(foldernew,
                kf_flag_dont_verify, 0, folderpath)));
              if succeeded(hresult(getlasterror)) then
                begin
                  result := folderpath;
                  cotaskmemfree(folderpath);
                end;
            end;
        end;

      if (result = '') and (folder >= 0) then
        begin
          shgetfolderpath := getprocaddress(getmodulehandle('Shell32.dll'),
            'SHGetFolderPathW');
          if assigned(shgetfolderpath) then
            begin
              folderpath := allocmem((max_path + 1) * sizeof(widechar));
              setlasterror(cardinal(shgetfolderpath(0, folder, 0, 0,
                folderpath)));
              if succeeded(hresult(getlasterror)) then
                result := folderpath;
              freemem(folderpath);
            end;
        end;

      if result <> '' then
        result := includetrailingpathdelimiter(result);
    end;

  class function tutils.gettempdirwithsubdir(subdir: String): String;
    var
      td: string;
    begin
      td := '';
      try
        if checkwin32version(6, 0) then
          begin
            gettempdirwithsubdir := getspecialfolderlocation(-1,
              folderid_downloads) + subdir + '\'; // Vista +
            td := getspecialfolderlocation(-1, folderid_downloads) +
              subdir + '\';
          end
        else
          begin
            gettempdirwithsubdir := gettempdirxp + subdir + '\'; // XP -
            td := gettempdirxp + subdir + '\';
          end;
      except
        forcedirectories(extractfilepath(application.exename) + 'TempFiles\' +
          subdir + '\');
        gettempdirwithsubdir := extractfilepath(application.exename) +
          'TempFiles\' + subdir + '\';
        td := extractfilepath(application.exename) + 'TempFiles\' +
          subdir + '\';
      end;
      if td = '' then
        begin
          forcedirectories(extractfilepath(application.exename) + 'TempFiles\' +
            subdir + '\');
          gettempdirwithsubdir := extractfilepath(application.exename) +
            'TempFiles\' + subdir + '\';
        end
      else
        forcedirectories(td);
    end;

  class function tutils.gettempdir(): string;
    var
      buf: string;
      len: uint;
    begin
      try
        setlength(buf, max_path + 1);
        len := gettemppath(max_path, pwidechar(buf));
        setlength(buf, len);
        gettempdir := buf
      except
        on e: exception do
          logapp.writetolog('Ошибка в GetTempDir: ' + e.message, lmterror,
            lmldebug, true);
      end;
    end;

end.
