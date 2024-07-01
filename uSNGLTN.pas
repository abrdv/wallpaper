unit uSNGLTN;

interface

  type

    tsingleton = class(tobject)
      private
        class procedure registerinstance(ainstance: tsingleton);
        procedure unregisterinstance;
        class function findinstance: tsingleton;
      protected
        // ������������� ����������� ������ � ���� ������������, � �� � GetInstance.
        // �� ������������� �������� ���� ����������� �� ������ protected
        constructor create; virtual;
      public
        class function newinstance: tobject; override;
        procedure beforedestruction; override;
        constructor getinstance; // ����� ������� � ����������
    end;

implementation

  uses
    contnrs;

  var
    singletonlist: tobjectlist;

    { TSingleton }

  class procedure tsingleton.registerinstance(ainstance: tsingleton);
    begin
      singletonlist.add(ainstance);
    end;

  procedure tsingleton.unregisterinstance;
    begin
      singletonlist.extract(self);
    end;

  class function tsingleton.findinstance: tsingleton;
    var
      i: integer;
    begin
      result := nil;
      for i := 0 to singletonlist.count - 1 do
        if singletonlist[i].classtype = self then
          begin
            result := tsingleton(singletonlist[i]);
            break;
          end;
    end;

  constructor tsingleton.create;
    begin
      inherited create;
    end;

  class function tsingleton.newinstance: tobject;
    begin
      result := findinstance;
      if result = nil then
        begin
          result := inherited newinstance;
          tsingleton(result).create;
          registerinstance(tsingleton(result));
        end;
    end;

  procedure tsingleton.beforedestruction;
    begin
      unregisterinstance;
      inherited beforedestruction;
    end;

  constructor tsingleton.getinstance;
    begin
      inherited create;
    end;

initialization

  singletonlist := tobjectlist.create(true);

finalization

  singletonlist.free;

end.
