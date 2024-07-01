// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program PWPCH;

uses
  Vcl.Forms,
  UWPCH in 'UWPCH.pas' {FWallPaper},
  uWallPaperGuarder in 'uWallPaperGuarder.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFWallPaper, FWallPaper);
  Application.Run;
end.
