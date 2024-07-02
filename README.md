## Application for change wallpaper on Windows desktop
- uSNGLTN - simple singleton base unit
- uUtils - util unit, can be throw away
- uTimerThread - simple thread based timer
- uLogit - log unit based on uSNGLTN, can be used ```function logapp: tlogmanager;```
- uWallPaperGuarder - main class 
- UWPCH - Tform for manage
## Examples
use uLogit
```Pascal 
uses ...uLogit;
...
Logapp.writetolog(strErrorCreateFormText);
```
use TWallPaperGuarder
```Pascal 
uses ...uWallPaperGuarder;
...
var  WallPaperGuarder: TWallPaperGuarder;
...
begin
WallPaperGuarder := TWallPaperGuarder.Create;
WallPaperGuarder.Refresh;
end;
```

## ToDo

- [ ] list of directorys to find jpg
- [ ] add reaction on waiter
- [ ] optimize space
- [ ] add dunitx

> [!WARNING]
> AS IS
