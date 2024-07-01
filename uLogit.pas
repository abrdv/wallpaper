unit uLogit;

interface

  uses
    windows, sysutils, syncobjs, usngltn;

  type
    tlogmsgtype = (lmtinfo, lmtwarning, lmterror);

    tlogmsglevel = (lmlmax, lmlinfo, lmldebug);

    tlogmanagerevent = procedure(const alogmsgtext: string;
      alogmsgtype: tlogmsgtype);
    tlogmanagereventobj = procedure(const alogmsgtext: string;
      alogmsgtype: tlogmsgtype) of object;

    tlogmanager = class(tsingleton)
      private
        fcs: tcriticalsection;
        ffilename: string; // файл лога
        fmaxloglevel: tlogmsglevel;
        // уровень, который опредеяет записывать сообщение или нет
        flogevent: boolean; // вызывать свойства?
        fonlogevent: tlogmanagerevent;
        fonlogeventobj: tlogmanagereventobj;
        feventcountsave: integer;
        feventcountmax: integer;
        function formatlogtime(adt: tdatetime): string;
        function formatlogmsgtype(alogmsgtype: tlogmsgtype): string;
        function formatlogmsgtext(atext: string;
          alogmsgtype: tlogmsgtype): string;
        procedure dologevent(alogmsgtext: string; alogmsgtype: tlogmsgtype;
          awritetoevent: boolean);
      protected
        constructor create; override;
      public
        procedure writetolog(atext: string; alogmsgtype: tlogmsgtype = lmtinfo;
          alvl: tlogmsglevel = Low(tlogmsglevel);
          awritetoevent: boolean = true);
      public
        destructor destroy; override;
        property filename: string read ffilename write ffilename; // куда писать
        property maxloglevel: tlogmsglevel read fmaxloglevel write fmaxloglevel;
        // максимальный доступный уровень
        property logevent: boolean read flogevent write flogevent;
        // вызывать события лога?
        property onlogevent: tlogmanagerevent read fonlogevent
          write fonlogevent;
        // событие лога
        property onlogeventobj: tlogmanagereventobj read fonlogeventobj
          write fonlogeventobj; // событие лога для объектов
        property eventcountsave: integer read feventcountsave
          write feventcountsave;
        // Count of Rows to Save in EventObj
        property eventcountmax: integer read feventcountmax
          write feventcountmax;
        // Max Count Rows in EventObj
    end;

    // funcs
  function logapp: tlogmanager;

implementation

  function logapp: tlogmanager;
    begin
      result := tlogmanager.getinstance;
    end;

  { TLogManager }

  constructor tlogmanager.create;
    begin
      inherited;
      fcs := tcriticalsection.create;
      ffilename := '';
      fmaxloglevel := High(tlogmsglevel);
      flogevent := false;
    end;

  destructor tlogmanager.destroy;
    begin
      fcs.enter;
      try
      finally
        fcs.leave;
      end;
      fcs.free;
      inherited;
    end;

  function tlogmanager.formatlogtime(adt: tdatetime): string;
    begin
      datetimetostring(result, '[dd.mm.yyyy] [hh:mm:ss]', adt);
    end;

  function tlogmanager.formatlogmsgtype(alogmsgtype: tlogmsgtype): string;
    begin
      case alogmsgtype of
        lmtinfo:
          result := '[Info]';
        lmtwarning:
          result := '[Warning]';
        lmterror:
          result := '[Error]';
      end;
    end;

  function tlogmanager.formatlogmsgtext(atext: string;
    alogmsgtype: tlogmsgtype): string;
    begin
      result := format('%s %s: %s', [formatlogtime(now),
        formatlogmsgtype(alogmsgtype), atext]);
    end;

  procedure tlogmanager.dologevent(alogmsgtext: string;
    alogmsgtype: tlogmsgtype; awritetoevent: boolean);
    begin
      if not flogevent or not awritetoevent then
        exit;
      if assigned(fonlogevent) then
        fonlogevent(alogmsgtext, alogmsgtype);
      if assigned(fonlogeventobj) then
        fonlogeventobj(alogmsgtext, alogmsgtype);
    end;

  procedure tlogmanager.writetolog(atext: string; alogmsgtype: tlogmsgtype;
    alvl: tlogmsglevel; awritetoevent: boolean);
    var
      fh: thandle;
      lmsg: string;
      card: cardinal;
    begin
      if ffilename = '' then
        begin
          lmsg := formatlogmsgtext(atext, alogmsgtype);
          lmsg := format('%s: %s', [formatlogmsgtype(lmterror),
            'Не определён файл логирования! LogMsg: ' + lmsg]);
          dologevent(lmsg, lmterror, true);
          exit;
        end;
      if alvl > fmaxloglevel then
        exit;
      fh := createfile(pchar(ffilename), generic_write, file_share_read or
        file_share_write, nil, open_always, file_attribute_normal, 0);
      if fh = invalid_handle_value then
        begin
          lmsg := formatlogmsgtext(atext, alogmsgtype);
          lmsg := format('%s: %s', [formatlogmsgtype(lmterror),
            'Ошибка лог-файла: INVALID_HANDLE_VALUE. LogMsg: ' + lmsg]);
          dologevent(lmsg, lmterror, true);
          exit;
        end;
      fcs.enter;
      try
        // формируем сообщение
        lmsg := formatlogmsgtext(atext, alogmsgtype);
        dologevent(lmsg, alogmsgtype, awritetoevent);
        lmsg := lmsg + #13#10;
        try
          setfilepointer(fh, 0, nil, file_end);
          writefile(fh, lmsg[1], sizeof(lmsg[1]) * length(lmsg), card, nil);
        except
          on e: exception do
            begin
              lmsg := format('%s: %s', [formatlogmsgtype(lmterror),
                'Ошибка записи в файл лога: ' + e.classname + ': ' +
                e.message]);
              dologevent(lmsg, lmterror, true);
            end;
          Else
            begin
              lmsg := format('%s: %s', [formatlogmsgtype(lmterror),
                'Неизвестная ошибка в TLogManager.WriteToLog!']);
              dologevent(lmsg, lmterror, true);
            end;
        end;
      finally
        closehandle(fh);
        fcs.leave;
      end;
    end;

end.
