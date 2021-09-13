{ --------------------------------- }
{ ------------= ZenGL =------------ }
{ --------------------------------- }
{ }
{ version:  0.3.12 }
{ date:     2013.08.12 }
{ license:  zlib }
{ homepage: http://zengl.org }
{ }
{ --------- developed by: --------- }
{ }
{ Andrey Kemka aka Andru }
{ }
{ e-mail: dr.andru@gmail.com }
{ jabber: dr.andru@googlemail.com }
{ icq:    496929849 }
{ skype:  andru-kun }
{ }
{ --------------------------------- }
unit zglHeader;

{
  *  Copyright (c) 2012 Andrey Kemka
  *
  *  This software is provided 'as-is', without any express or
  *  implied warranty. In no event will the authors be held
  *  liable for any damages arising from the use of this software.
  *
  *  Permission is granted to anyone to use this software for any purpose,
  *  including commercial applications, and to alter it and redistribute
  *  it freely, subject to the following restrictions:
  *
  *  1. The origin of this software must not be misrepresented;
  *     you must not claim that you wrote the original software.
  *     If you use this software in a product, an acknowledgment
  *     in the product documentation would be appreciated but
  *     is not required.
  *
  *  2. Altered source versions must be plainly marked as such,
  *     and must not be misrepresented as being the original software.
  *
  *  3. This notice may not be removed or altered from any
  *     source distribution.
}

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$DEFINE WINDOWS}
{$ENDIF}
{$IFDEF DARWIN}
{$IF DEFINED(iPHONESIM) or (DEFINED(DARWIN) and DEFINED(CPUARM))}
{$DEFINE iOS}
{$ELSE}
{$DEFINE MACOSX}
{$IFEND}
{$ENDIF}
{$IF DEFINED(LINUX) and DEFINED(CPUARM)}
{$DEFINE ANDROID}
{$IFEND}

interface

{$IFDEF MACOSX}

uses
  MacOSAll;
{$ENDIF}

type
  Ptr = {$IFDEF CPU64}QWORD{$ELSE}LongWord{$ENDIF};
{$IFDEF WINDOWS}
  HANDLE = Ptr;
  HDC = Ptr;
  HGLRC = Ptr;
{$ENDIF}
  PByteArray = ^TByteArray;
  TByteArray = array [0 .. High(LongWord) shr 1 - 1] of Byte;
  PWordArray = ^TWordArray;
  TWordArray = array [0 .. High(LongWord) shr 2 - 1] of Word;
  PLongWordArray = ^TLongWordArray;
  TLongWordArray = array [0 .. High(LongWord) shr 3 - 1] of LongWord;

type
  zglTStringList = record
    Count: Integer;
    Items: array of UTF8String;
  end;

type
  zglTFile = Ptr;
  zglTFileList = zglTStringList;

type
  zglPMemory = ^zglTMemory;

  zglTMemory = record
    Memory: Pointer;
    Size: LongWord;
    Position: LongWord;
  end;

const
{$IFDEF LINUX}
  libZenGL = 'libZenGL.so';
{$ENDIF}
{$IFDEF WINDOWS}
  libZenGL = 'ZenGL.dll';
{$ENDIF}
{$IFDEF MACOSX}
  libZenGL = 'libZenGL.dylib';
{$ENDIF}
function zglLoad(LibraryName: AnsiString; Error: Boolean = TRUE): Boolean;
procedure zglFree;

var
  zgl_Init: procedure(FSAA: Byte = 0; StencilBits: Byte = 0);
  zgl_InitToHandle: procedure(HANDLE: Ptr; FSAA: Byte = 0; StencilBits: Byte = 0);
  zgl_Exit: procedure;

const
  SYS_APP_INIT = $000001;
  SYS_APP_LOOP = $000002;
  SYS_LOAD = $000003;
  SYS_DRAW = $000004;
  SYS_UPDATE = $000005;
  SYS_EXIT = $000006;
  SYS_ACTIVATE = $000007;
  SYS_CLOSE_QUERY = $000008;

  INPUT_MOUSE_MOVE = $000020;
  INPUT_MOUSE_PRESS = $000021;
  INPUT_MOUSE_RELEASE = $000022;
  INPUT_MOUSE_WHEEL = $000023;
  INPUT_KEY_PRESS = $000030;
  INPUT_KEY_RELEASE = $000031;
  INPUT_KEY_CHAR = $000032;

  TEX_FORMAT_EXTENSION = $000100;
  TEX_FORMAT_FILE_LOADER = $000101;
  TEX_FORMAT_MEM_LOADER = $000102;
  TEX_CURRENT_EFFECT = $000103;

  SND_FORMAT_EXTENSION = $000110;
  SND_FORMAT_FILE_LOADER = $000111;
  SND_FORMAT_MEM_LOADER = $000112;
  SND_FORMAT_DECODER = $000113;

  VIDEO_FORMAT_DECODER = $000130;

var
  zgl_Reg: procedure(What: LongWord; UserData: Pointer);

const
  ZENGL_VERSION = 1; // Major shr 16, ( Minor and $FF00 ) shr 8, Revision and $FF
  ZENGL_VERSION_STRING = 2; // PAnsiChar
  ZENGL_VERSION_DATE = 3; // PAnsiChar

  DIRECTORY_APPLICATION = 101; // PAnsiChar
  DIRECTORY_HOME = 102; // PAnsiChar

  LOG_FILENAME = 203; // PPAnsiChar

  DESKTOP_WIDTH = 300;
  DESKTOP_HEIGHT = 301;
  RESOLUTION_LIST = 302; // zglPResolutionList

  WINDOW_HANDLE = 400; // TWindow(GNU/Linux), HWND(Windows), WindowRef(MacOS X)
  WINDOW_X = 401;
  WINDOW_Y = 402;
  WINDOW_WIDTH = 403;
  WINDOW_HEIGHT = 404;

  GAPI_CONTEXT = 500; // GLXContext(GNU/Linux), HGLRC(Windows), TAGLContext(MacOS X)
  GAPI_DEVICE = 500; // For ZenGL with Direct3D render only
  GAPI_MAX_TEXTURE_SIZE = 501;
  GAPI_MAX_TEXTURE_UNITS = 502;
  GAPI_MAX_ANISOTROPY = 503;
  GAPI_CAN_BLEND_SEPARATE = 504; // Boolean
  GAPI_CAN_AUTOGEN_MIPMAP = 505; // Boolean

  VIEWPORT_WIDTH = 600;
  VIEWPORT_HEIGHT = 601;
  VIEWPORT_OFFSET_X = 602;
  VIEWPORT_OFFSET_Y = 603;

  RENDER_FPS = 700;
  RENDER_BATCHES_2D = 701;
  RENDER_CURRENT_MODE = 702;
  RENDER_CURRENT_TARGET = 703;
  RENDER_VRAM_USED = 704;

  MANAGER_TIMER = 800; // zglPTimerManager
  MANAGER_TEXTURE = 801; // zglPTextureManager
  MANAGER_FONT = 802; // zglPFontManager
  MANAGER_RTARGET = 803; // zglPRenderTargetManager
  MANAGER_SOUND = 804; // zglPSoundManager
  MANAGER_EMITTER2D = 805; // zglPEmitter2DManager

var
  zgl_Get: function(What: LongWord): Ptr;
  zgl_GetMem: procedure(out Mem: Pointer; Size: LongWord);
  zgl_FreeMem: procedure(var Mem: Pointer);
  zgl_FreeStrList: procedure(var List: zglTStringList);

const
  COLOR_BUFFER_CLEAR = $000001;
  DEPTH_BUFFER = $000002;
  DEPTH_BUFFER_CLEAR = $000004;
  DEPTH_MASK = $000008;
  STENCIL_BUFFER_CLEAR = $000010;
  CORRECT_RESOLUTION = $000020;
  CORRECT_WIDTH = $000040;
  CORRECT_HEIGHT = $000080;
  APP_USE_AUTOPAUSE = $000100;
  APP_USE_LOG = $000200;
  APP_USE_ENGLISH_INPUT = $000400;
  APP_USE_DT_CORRECTION = $000800;
  WND_USE_AUTOCENTER = $001000;
  SND_CAN_PLAY = $002000;
  SND_CAN_PLAY_FILE = $004000;
  CLIP_INVISIBLE = $008000;

var
  zgl_Enable: procedure(What: LongWord);
  zgl_Disable: procedure(What: LongWord);

  // LOG
  log_Add: procedure(const Message: UTF8String; Timings: Boolean = TRUE);

  // WINDOW
  wnd_SetCaption: procedure(const NewCaption: UTF8String);
  wnd_SetSize: procedure(Width, Height: Integer);
  wnd_SetPos: procedure(X, Y: Integer);
  wnd_ShowCursor: procedure(Show: Boolean);

  // SCREEN
type
  zglPResolutionList = ^zglTResolutionList;

  zglTResolutionList = record
    Count: Integer;
    Width: array of Integer;
    Height: array of Integer;
  end;

const
  REFRESH_MAXIMUM = 0;
  REFRESH_DEFAULT = 1;

var
  scr_Clear: procedure;
  scr_Flush: procedure;
  scr_SetVSync: procedure(VSync: Boolean);
  // RU: ВНИМАНИЕ: Функция уничтожает контекст OpenGL, что потребует перезагрузку ресурсов
  // EN: WARNING: Function will destroy OpenGL context, so all resources must be reloaded
  scr_SetFSAA: procedure(FSAA: Byte);
  scr_SetOptions: function(Width, Height, Refresh: Word; FullScreen, VSync: Boolean): Boolean;
  scr_CorrectResolution: procedure(Width, Height: Word);
  scr_ReadPixels: procedure(var pData: Pointer; X, Y, Width, Height: Word);

  // GL
const
  TARGET_SCREEN = 1;
  TARGET_TEXTURE = 2;

var
  Set2DMode: procedure;
  Set3DMode: procedure(FOVY: Single = 45);

  // Z BUFFER
  zbuffer_SetDepth: procedure(zNear, zFar: Single);
  zbuffer_Clear: procedure;

  // SCISSOR
  scissor_Begin: procedure(X, Y, Width, Height: Integer; ConsiderCamera: Boolean = TRUE);
  scissor_End: procedure;

  // INI
  ini_LoadFromFile: function(const FileName: UTF8String): Boolean;
  ini_SaveToFile: procedure(const FileName: UTF8String);
  ini_Free: procedure;
  ini_Add: procedure(const Section, Key: UTF8String);
  ini_Del: procedure(const Section, Key: UTF8String);
  ini_Clear: procedure(const Section: UTF8String);
  ini_IsSection: function(const Section: UTF8String): Boolean;
  ini_IsKey: function(const Section, Key: UTF8String): Boolean;
  _ini_ReadKeyStr: function(const Section, Key: UTF8String): PAnsiChar;
  ini_ReadKeyInt: function(const Section, Key: UTF8String): Integer;
  ini_ReadKeyFloat: function(const Section, Key: UTF8String): Single;
  ini_ReadKeyBool: function(const Section, Key: UTF8String): Boolean;
  ini_WriteKeyStr: function(const Section, Key, Value: UTF8String): Boolean;
  ini_WriteKeyInt: function(const Section, Key: UTF8String; Value: Integer): Boolean;
  ini_WriteKeyFloat: function(const Section, Key: UTF8String; Value: Single; Digits: Integer = 2): Boolean;
  ini_WriteKeyBool: function(const Section, Key: UTF8String; Value: Boolean): Boolean;

function ini_ReadKeyStr(const Section, Key: UTF8String): UTF8String;

// TIMERS
type
  zglPTimer = ^zglTTimer;

  zglTTimer = record
    Active: Boolean;
    Custom: Boolean;
    UserData: Pointer;
    Interval: LongWord;
    LastTick: Double;
    OnTimer: procedure;
    OnTimerEx: procedure(Timer: zglPTimer);

    prev, next: zglPTimer;
  end;

type
  zglPTimerManager = ^zglTTimerManager;

  zglTTimerManager = record
    Count: Integer;
    First: zglTTimer;
  end;

var
  timer_Add: function(OnTimer: Pointer; Interval: LongWord; UseSenderForCallback: Boolean = FALSE; UserData: Pointer = nil): zglPTimer;
  timer_Del: procedure(var Timer: zglPTimer);
  timer_GetTicks: function: Double;
  timer_Reset: procedure;

  // MOUSE
const
  M_BLEFT = 0;
  M_BMIDDLE = 1;
  M_BRIGHT = 2;
  M_WUP = 0;
  M_WDOWN = 1;

var
  mouse_X: function: Integer;
  mouse_Y: function: Integer;
  mouse_DX: function: Integer;
  mouse_DY: function: Integer;
  mouse_Down: function(Button: Byte): Boolean;
  mouse_Up: function(Button: Byte): Boolean;
  mouse_Click: function(Button: Byte): Boolean;
  mouse_DblClick: function(Button: Byte): Boolean;
  mouse_Wheel: function(Axis: Byte): Boolean;
  mouse_ClearState: procedure;
  mouse_Lock: procedure(X: Integer = -1; Y: Integer = -1);

  // KEYBOARD
const
  K_SYSRQ = $B7;
  K_PAUSE = $C5;
  K_ESCAPE = $01;
  K_ENTER = $1C;
  K_KP_ENTER = $9C;

  K_UP = $C8;
  K_DOWN = $D0;
  K_LEFT = $CB;
  K_RIGHT = $CD;

  K_BACKSPACE = $0E;
  K_SPACE = $39;
  K_TAB = $0F;
  K_TILDE = $29;

  K_INSERT = $D2;
  K_DELETE = $D3;
  K_HOME = $C7;
  K_END = $CF;
  K_PAGEUP = $C9;
  K_PAGEDOWN = $D1;

  K_CTRL = $FF - $01;
  K_CTRL_L = $1D;
  K_CTRL_R = $9D;
  K_ALT = $FF - $02;
  K_ALT_L = $38;
  K_ALT_R = $B8;
  K_SHIFT = $FF - $03;
  K_SHIFT_L = $2A;
  K_SHIFT_R = $36;
  K_SUPER = $FF - $04;
  K_SUPER_L = $DB;
  K_SUPER_R = $DC;
  K_APP_MENU = $DD;

  K_CAPSLOCK = $3A;
  K_NUMLOCK = $45;
  K_SCROLL = $46;

  K_BRACKET_L = $1A; // [ {
  K_BRACKET_R = $1B; // ] }
  K_BACKSLASH = $2B; // \
  K_SLASH = $35; // /
  K_COMMA = $33; // ,
  K_DECIMAL = $34; // .
  K_SEMICOLON = $27; // : ;
  K_APOSTROPHE = $28; // ' "

  K_0 = $0B;
  K_1 = $02;
  K_2 = $03;
  K_3 = $04;
  K_4 = $05;
  K_5 = $06;
  K_6 = $07;
  K_7 = $08;
  K_8 = $09;
  K_9 = $0A;

  K_MINUS = $0C;
  K_EQUALS = $0D;

  K_A = $1E;
  K_B = $30;
  K_C = $2E;
  K_D = $20;
  K_E = $12;
  K_F = $21;
  K_G = $22;
  K_H = $23;
  K_I = $17;
  K_J = $24;
  K_K = $25;
  K_L = $26;
  K_M = $32;
  K_N = $31;
  K_O = $18;
  K_P = $19;
  K_Q = $10;
  K_R = $13;
  K_S = $1F;
  K_T = $14;
  K_U = $16;
  K_V = $2F;
  K_W = $11;
  K_X = $2D;
  K_Y = $15;
  K_Z = $2C;

  K_KP_0 = $52;
  K_KP_1 = $4F;
  K_KP_2 = $50;
  K_KP_3 = $51;
  K_KP_4 = $4B;
  K_KP_5 = $4C;
  K_KP_6 = $4D;
  K_KP_7 = $47;
  K_KP_8 = $48;
  K_KP_9 = $49;

  K_KP_SUB = $4A;
  K_KP_ADD = $4E;
  K_KP_MUL = $37;
  K_KP_DIV = $B5;
  K_KP_DECIMAL = $53;

  K_F1 = $3B;
  K_F2 = $3C;
  K_F3 = $3D;
  K_F4 = $3E;
  K_F5 = $3F;
  K_F6 = $40;
  K_F7 = $41;
  K_F8 = $42;
  K_F9 = $43;
  K_F10 = $44;
  K_F11 = $57;
  K_F12 = $58;

  KA_DOWN = 0;
  KA_UP = 1;

var
  key_Down: function(KeyCode: Byte): Boolean;
  key_Up: function(KeyCode: Byte): Boolean;
  key_Press: function(KeyCode: Byte): Boolean;
  key_Last: function(KeyAction: Byte): Byte;
  key_BeginReadText: procedure(const Text: UTF8String; MaxSymbols: Integer = -1);
  key_UpdateReadText: procedure(const Text: UTF8String; MaxSymbols: Integer = -1);
  _key_GetText: function: PAnsiChar;
  key_EndReadText: procedure;
  key_ClearState: procedure;

function key_GetText: UTF8String;

// JOYSTICK
type
  zglPJoyInfo = ^zglTJoyInfo;

  zglTJoyInfo = record
    Name: UTF8String;

    Count: record
      Axes: Integer;
      Buttons: Integer;
    end;

    Caps: LongWord;
  end;

const
  JOY_HAS_Z = $000001;
  JOY_HAS_R = $000002;
  JOY_HAS_U = $000004;
  JOY_HAS_V = $000008;
  JOY_HAS_POV = $000010;

  JOY_AXIS_X = 0;
  JOY_AXIS_Y = 1;
  JOY_AXIS_Z = 2;
  JOY_AXIS_R = 3;
  JOY_AXIS_U = 4;
  JOY_AXIS_V = 5;
  JOY_POVX = 6;
  JOY_POVY = 7;

var
  joy_Init: function: Byte;
  joy_GetInfo: function(JoyID: Byte): zglPJoyInfo;
  joy_AxisPos: function(JoyID, Axis: Byte): Single;
  joy_Down: function(JoyID, Button: Byte): Boolean;
  joy_Up: function(JoyID, Button: Byte): Boolean;
  joy_Press: function(JoyID, Button: Byte): Boolean;
  joy_ClearState: procedure;

  // 2D
type
  zglPPoint2D = ^zglTPoint2D;

  zglTPoint2D = record
    X, Y: Single;
  end;

type
  zglPPoints2D = ^zglTPoints2D;
  zglTPoints2D = array [0 .. 0] of zglTPoint2D;

type
  zglPLine = ^zglTLine;

  zglTLine = record
    x0, y0: Single;
    x1, y1: Single;
  end;

type
  zglPRect = ^zglTRect;

  zglTRect = record
    X, Y, W, H: Single;
  end;

type
  zglPCircle = ^zglTCircle;

  zglTCircle = record
    cX, cY: Single;
    Radius: Single;
  end;

  // RESOURCES
var
  res_BeginQueue: procedure(QueueID: Byte);
  res_EndQueue: procedure;
  res_GetPercentage: function(QueueID: Byte): Integer;
  res_GetCompleted: function: Integer;
  res_Proc: procedure;

  // TEXTURES
type
  zglPTextureCoord = ^zglTTextureCoord;
  zglTTextureCoord = array [0 .. 3] of zglTPoint2D;

type
  zglPTexture = ^zglTTexture;

  zglTTexture = record
    ID: LongWord;
    Width, Height: Word;
    Format: Word;
    U, V: Single;
    FramesCoord: array of zglTTextureCoord;
    Flags: LongWord;

    prev, next: zglPTexture;
  end;

type
  zglPTextureFormat = ^zglTTextureFormat;

  zglTTextureFormat = record
    Extension: UTF8String;
    FileLoader: procedure(const FileName: UTF8String; out pData: PByteArray; out W, H, Format: Word);
    MemLoader: procedure(const Memory: zglTMemory; out pData: PByteArray; out W, H, Format: Word);
  end;

type
  zglPTextureManager = ^zglTTextureManager;

  zglTTextureManager = record
    Count: record
      Items: Integer;
      Formats: Integer;
    end;

    First: zglTTexture;
    Formats: array of zglTTextureFormat;
  end;

const
  TEX_FORMAT_RGBA = $01;
  TEX_FORMAT_RGBA_PVR2 = $10;
  TEX_FORMAT_RGBA_PVR4 = $11;
  TEX_FORMAT_RGBA_DXT1 = $20;
  TEX_FORMAT_RGBA_DXT3 = $21;
  TEX_FORMAT_RGBA_DXT5 = $22;

  TEX_NO_COLORKEY = $FF000000;

  TEX_MIPMAP = $000001;
  TEX_CLAMP = $000002;
  TEX_REPEAT = $000004;
  TEX_COMPRESS = $000008;

  TEX_CONVERT_TO_POT = $000010;
  TEX_CALCULATE_ALPHA = $000020;

  TEX_GRAYSCALE = $000040;
  TEX_INVERT = $000080;
  TEX_CUSTOM_EFFECT = $000100;

  TEX_FILTER_NEAREST = $000200;
  TEX_FILTER_LINEAR = $000400;
  TEX_FILTER_BILINEAR = $000800;
  TEX_FILTER_TRILINEAR = $001000;
  TEX_FILTER_ANISOTROPY = $002000;

  TEX_DEFAULT_2D = TEX_CLAMP or TEX_FILTER_LINEAR or TEX_CONVERT_TO_POT or TEX_CALCULATE_ALPHA;

var
  tex_Add: function: zglPTexture;
  tex_Del: procedure(var Texture: zglPTexture);
  tex_Create: function(var Data: PByteArray; Width, Height: Word; Format: Word = TEX_FORMAT_RGBA; Flags: LongWord = TEX_DEFAULT_2D): zglPTexture;
  tex_CreateZero: function(Width, Height: Word; Color: LongWord = $000000; Flags: LongWord = TEX_DEFAULT_2D): zglPTexture;
  tex_LoadFromFile: function(const FileName: UTF8String; TransparentColor: LongWord = TEX_NO_COLORKEY; Flags: LongWord = TEX_DEFAULT_2D): zglPTexture;
  tex_LoadFromMemory: function(const Memory: zglTMemory; const Extension: UTF8String; TransparentColor: LongWord = TEX_NO_COLORKEY;
    Flags: LongWord = TEX_DEFAULT_2D): zglPTexture;
  tex_SetFrameSize: procedure(var Texture: zglPTexture; FrameWidth, FrameHeight: Word);
  tex_SetMask: procedure(var Texture: zglPTexture; Mask: zglPTexture);
  tex_SetData: procedure(Texture: zglPTexture; pData: PByteArray; X, Y, Width, Height: Word; Stride: Integer = 0);
  tex_GetData: procedure(Texture: zglPTexture; out pData: PByteArray);
  tex_Filter: procedure(Texture: zglPTexture; Flags: LongWord);
  tex_SetAnisotropy: procedure(Level: Byte);

  // RENDER TARGETS
type
  zglPRenderTarget = ^zglTRenderTarget;

  zglTRenderTarget = record
    Type_: Byte;
    HANDLE: Pointer;
    Surface: zglPTexture;
    Flags: Byte;

    prev, next: zglPRenderTarget;
  end;

type
  zglPRenderTargetManager = ^zglTRenderTargetManager;

  zglTRenderTargetManager = record
    Count: Integer;
    First: zglTRenderTarget;
  end;

type
  zglTRenderCallback = procedure(Data: Pointer);

const
  RT_DEFAULT = $00;
  RT_FULL_SCREEN = $01;
  RT_USE_DEPTH = $02;
  RT_CLEAR_COLOR = $04;
  RT_CLEAR_DEPTH = $08;
  RT_SAVE_CONTENT = $10; // Direct3D only!

var
  rtarget_Add: function(Surface: zglPTexture; Flags: Byte): zglPRenderTarget;
  rtarget_Del: procedure(var Target: zglPRenderTarget);
  rtarget_Set: procedure(Target: zglPRenderTarget);
  rtarget_DrawIn: procedure(Target: zglPRenderTarget; RenderCallback: zglTRenderCallback; Data: Pointer);

  // FX
const
  FX_BLEND_NORMAL = $00;
  FX_BLEND_ADD = $01;
  FX_BLEND_MULT = $02;
  FX_BLEND_BLACK = $03;
  FX_BLEND_WHITE = $04;
  FX_BLEND_MASK = $05;

  FX_COLOR_MIX = $00;
  FX_COLOR_SET = $01;

  FX_BLEND = $100000;
  FX_COLOR = $200000;

var
  fx_SetBlendMode: procedure(Mode: Byte; SeparateAlpha: Boolean = TRUE);
  fx_SetColorMode: procedure(Mode: Byte);
  fx_SetColorMask: procedure(R, G, B, Alpha: Boolean);

  // FX 2D
const
  FX2D_FLIPX = $000001;
  FX2D_FLIPY = $000002;
  FX2D_VCA = $000004;
  FX2D_VCHANGE = $000008;
  FX2D_SCALE = $000010;
  FX2D_RPIVOT = $000020;

var
  fx2d_SetColor: procedure(Color: LongWord);
  fx2d_SetVCA: procedure(c1, c2, c3, c4: LongWord; a1, a2, a3, a4: Byte);
  fx2d_SetVertexes: procedure(x1, y1, x2, y2, x3, y3, x4, y4: Single);
  fx2d_SetScale: procedure(scaleX, scaleY: Single);
  fx2d_SetRotatingPivot: procedure(X, Y: Single);

  // Camera 2D
type
  zglPCamera2D = ^zglTCamera2D;

  zglTCamera2D = record
    X, Y: Single;
    Angle: Single;
    Zoom: zglTPoint2D;
    Center: zglTPoint2D;
  end;

var
  cam2d_Init: procedure(out Camera: zglTCamera2D);
  cam2d_Set: procedure(Camera: zglPCamera2D);
  cam2d_Get: function: zglPCamera2D;

  // Render 2D
  batch2d_Begin: procedure;
  batch2d_End: procedure;
  batch2d_Flush: procedure;

  // Primitives 2D
const
  PR2D_FILL = $010000;
  PR2D_SMOOTH = $020000;

var
  pr2d_Pixel: procedure(X, Y: Single; Color: LongWord = $FFFFFF; Alpha: Byte = 255);
  pr2d_Line: procedure(x1, y1, x2, y2: Single; Color: LongWord = $FFFFFF; Alpha: Byte = 255; FX: LongWord = 0);
  pr2d_Rect: procedure(X, Y, W, H: Single; Color: LongWord = $FFFFFF; Alpha: Byte = 255; FX: LongWord = 0);
  pr2d_Circle: procedure(X, Y, Radius: Single; Color: LongWord = $FFFFFF; Alpha: Byte = 255; Quality: Word = 32; FX: LongWord = 0);
  pr2d_Ellipse: procedure(X, Y, xRadius, yRadius: Single; Color: LongWord = $FFFFFF; Alpha: Byte = 255; Quality: Word = 32; FX: LongWord = 0);
  pr2d_TriList: procedure(Texture: zglPTexture; TriList, TexCoords: zglPPoints2D; iLo, iHi: Integer; Color: LongWord = $FFFFFF; Alpha: Byte = 255;
    FX: LongWord = FX_BLEND);

  // Sprites 2D
type
  zglPSprite2D = ^zglTSprite2D;
  zglPSEngine2D = ^zglTSEngine2D;

  zglTSEngine2D = record
    Count: Integer;
    List: array of zglPSprite2D;
  end;

  zglTSprite2D = record
    ID: Integer;
    Manager: zglPSEngine2D;
    Texture: zglPTexture;
    Destroy: Boolean;
    Layer: Integer;
    X, Y: Single;
    W, H: Single;
    Angle: Single;
    Frame: Single;
    Alpha: Integer;
    FxFlags: LongWord;
    Data: Pointer;

    OnInit: procedure(Sprite: zglPSprite2D);
    OnDraw: procedure(Sprite: zglPSprite2D);
    OnProc: procedure(Sprite: zglPSprite2D);
    OnFree: procedure(Sprite: zglPSprite2D);
  end;

type
  zglPTiles2D = ^zglTTiles2D;

  zglTTiles2D = record
    Count: record
      X, Y: Integer;
    end;

    Size: record
      W, H: Single;
    end;

    Tiles: array of array of Integer;
  end;

type
  zglPGrid2D = ^zglTGrid2D;

  zglTGrid2D = record
    Cols: Integer;
    Rows: Integer;
    Grid: array of array of zglTPoint2D;
  end;

var
  sengine2d_AddSprite: function(Texture: zglPTexture; Layer: Integer; OnInit, OnDraw, OnProc, OnFree: Pointer): zglPSprite2D;
  sengine2d_AddCustom: function(Texture: zglPTexture; Size: LongWord; Layer: Integer; OnInit, OnDraw, OnProc, OnFree: Pointer): zglPSprite2D;
  sengine2d_DelSprite: procedure(ID: Integer);
  sengine2d_ClearAll: procedure;
  sengine2d_Set: procedure(SEngine: zglPSEngine2D);
  sengine2d_Draw: procedure;
  sengine2d_Proc: procedure;

  texture2d_Draw: procedure(Texture: zglPTexture; const TexCoord: array of zglTPoint2D; X, Y, W, H, Angle: Single; Alpha: Byte = 255;
    FX: LongWord = FX_BLEND);
  ssprite2d_Draw: procedure(Texture: zglPTexture; X, Y, W, H, Angle: Single; Alpha: Byte = 255; FX: LongWord = FX_BLEND);
  asprite2d_Draw: procedure(Texture: zglPTexture; X, Y, W, H, Angle: Single; Frame: Word; Alpha: Byte = 255; FX: LongWord = FX_BLEND);
  csprite2d_Draw: procedure(Texture: zglPTexture; X, Y, W, H, Angle: Single; const CutRect: zglTRect; Alpha: Byte = 255; FX: LongWord = FX_BLEND);
  tiles2d_Draw: procedure(Texture: zglPTexture; X, Y: Single; Tiles: zglPTiles2D; Alpha: Byte = 255; FX: LongWord = FX_BLEND);
  sgrid2d_Draw: procedure(Texture: zglPTexture; X, Y: Single; Grid: zglPGrid2D; Alpha: Byte = 255; FX: LongWord = FX_BLEND);
  agrid2d_Draw: procedure(Texture: zglPTexture; X, Y: Single; Grid: zglPGrid2D; Frame: Integer; Alpha: Byte = 255; FX: LongWord = FX_BLEND);
  cgrid2d_Draw: procedure(Texture: zglPTexture; X, Y: Single; Grid: zglPGrid2D; const CutRect: zglTRect; Alpha: Byte = 255; FX: LongWord = FX_BLEND);

  // Particles
const
  EMITTER_MAX_PARTICLES = 1024;

  EMITTER_NONE = 0;
  EMITTER_POINT = 1;
  EMITTER_LINE = 2;
  EMITTER_RECTANGLE = 3;
  EMITTER_CIRCLE = 4;
  EMITTER_RING = 5;

type
  PDiagramByte = ^TDiagramByte;
  PDiagramLW = ^TDiagramLW;
  PDiagramSingle = ^TDiagramSingle;
  zglPParticle2D = ^zglTParticle2D;
  zglPEmitterPoint = ^zglTEmitterPoint;
  zglPEmitterLine = ^zglTEmitterLine;
  zglPEmitterRect = ^zglTEmitterRect;
  zglPEmitterCircle = ^zglTEmitterCircle;
  zglPEmitterRing = ^zglTEmitterRing;
  zglPParticleParams = ^zglTParticleParams;
  zglPEmitter2D = ^zglTEmitter2D;
  zglPPEmitter2D = ^zglPEmitter2D;
  zglPPEngine2D = ^zglTPEngine2D;
  zglPEmitter2DManager = ^zglTEmitter2DManager;

  TDiagramByte = record
    Life: Single;
    Value: Byte;
  end;

  TDiagramLW = record
    Life: Single;
    Value: LongWord;
  end;

  TDiagramSingle = record
    Life: Single;
    Value: Single;
  end;

  zglTParticle2D = record
    _private: record
      lColorID: Integer;
      lAlphaID: Integer;
      lSizeXID: Integer;
      lSizeYID: Integer;
      lVelocityID: Integer;
      laVelocityID: Integer;
      lSpinID: Integer;
    end;

    ID: Integer;

    Life: Single;
    LifeTime: Integer;
    Time: Double;

    Frame: Word;
    Color: LongWord;
    Alpha: Byte;

    Position: zglTPoint2D;
    Size: zglTPoint2D;
    SizeS: zglTPoint2D;
    Angle: Single;
    Direction: Single;

    Velocity: Single;
    VelocityS: Single;
    aVelocity: Single;
    aVelocityS: Single;
    Spin: Single;
  end;

  zglTEmitterPoint = record
    Direction: Single;
    Spread: Single;
  end;

  zglTEmitterLine = record
    Direction: Single;
    Spread: Single;
    Size: Single;
    TwoSide: Boolean;
  end;

  zglTEmitterRect = record
    Direction: Single;
    Spread: Single;
    Rect: zglTRect;
  end;

  zglTEmitterCircle = record
    Direction: Single;
    Spread: Single;
    cX, cY: Single;
    Radius: Single;
  end;

  zglTEmitterRing = record
    Direction: Single;
    Spread: Single;
    cX, cY: Single;
    Radius0: Single;
    Radius1: Single;
  end;

  zglTParticleParams = record
    Texture: zglPTexture;
    BlendMode: Byte;
    ColorMode: Byte;

    LifeTimeS: Integer;
    LifeTimeV: Integer;
    Frame: array [0 .. 1] of Integer;
    Color: array of TDiagramLW;
    Alpha: array of TDiagramByte;
    SizeXYBind: Boolean;
    SizeXS: Single;
    SizeYS: Single;
    SizeXV: Single;
    SizeYV: Single;
    SizeXD: array of TDiagramSingle;
    SizeYD: array of TDiagramSingle;
    AngleS: Single;
    AngleV: Single;
    VelocityS: Single;
    VelocityV: Single;
    VelocityD: array of TDiagramSingle;
    aVelocityS: Single;
    aVelocityV: Single;
    aVelocityD: array of TDiagramSingle;
    SpinS: Single;
    SpinV: Single;
    SpinD: array of TDiagramSingle;
  end;

  zglTEmitter2D = record
    _private: record
      pengine: zglPPEngine2D;
      particle: array [0 .. EMITTER_MAX_PARTICLES - 1] of zglTParticle2D;
      List: array [0 .. EMITTER_MAX_PARTICLES - 1] of zglPParticle2D;
      parCreated: Integer;
      texFile: UTF8String;
      texHash: LongWord;
    end;

    ID: Integer;
    Type_: Byte;

    Params: record
      Layer: Integer;
      LifeTime: Integer;
      Loop: Boolean;
      Emission: Integer;
      Position: zglTPoint2D;
    end;

    ParParams: zglTParticleParams;

    Life: Single;
    Time: Double;
    LastSecond: Double;
    Particles: Integer;

    BBox: record
      MinX, MaxX: Single;
      MinY, MaxY: Single;
    end;

    case Byte of
      EMITTER_POINT:
        (AsPoint: zglTEmitterPoint);
      EMITTER_LINE:
        (AsLine: zglTEmitterLine);
      EMITTER_RECTANGLE:
        (AsRect: zglTEmitterRect);
      EMITTER_CIRCLE:
        (AsCircle: zglTEmitterCircle);
      EMITTER_RING:
        (AsRing: zglTEmitterRing);
  end;

  zglTPEngine2D = record
    Count: record
      Emitters: Integer;
      Particles: Integer;
    end;

    List: array of zglPEmitter2D;
    ListU: array of zglPPEmitter2D;
  end;

  zglTEmitter2DManager = record
    Count: Integer;
    List: array of zglPEmitter2D;
  end;

var
  pengine2d_Set: procedure(pengine: zglPPEngine2D);
  pengine2d_Get: function: zglPPEngine2D;
  pengine2d_Draw: procedure;
  pengine2d_Proc: procedure(dt: Double);
  pengine2d_AddEmitter: procedure(Emitter: zglPEmitter2D; Result: zglPPEmitter2D = nil; X: Single = 0; Y: Single = 0);
  pengine2d_DelEmitter: procedure(ID: Integer);
  pengine2d_ClearAll: procedure;
  emitter2d_Add: function: zglPEmitter2D;
  emitter2d_Del: procedure(var Emitter: zglPEmitter2D);
  emitter2d_LoadFromFile: function(const FileName: UTF8String): zglPEmitter2D;
  emitter2d_LoadFromMemory: function(const Memory: zglTMemory): zglPEmitter2D;
  emitter2d_Init: procedure(Emitter: zglPEmitter2D);
  emitter2d_Free: procedure(var Emitter: zglPEmitter2D);
  emitter2d_Draw: procedure(Emitter: zglPEmitter2D);
  emitter2d_Proc: procedure(Emitter: zglPEmitter2D; dt: Double);

  // Text
type
  zglPCharDesc = ^zglTCharDesc;

  zglTCharDesc = record
    Page: Word;
    Width: Byte;
    Height: Byte;
    ShiftX: Integer;
    ShiftY: Integer;
    ShiftP: Integer;
    TexCoords: array [0 .. 3] of zglTPoint2D;
  end;

type
  zglPFont = ^zglTFont;

  zglTFont = record
    Count: record
      Pages: Word;
      Chars: Word;
    end;

    Pages: array of zglPTexture;
    CharDesc: array [0 .. 65535] of zglPCharDesc;
    MaxHeight: Integer;
    MaxShiftY: Integer;
    Padding: array [0 .. 3] of Byte;

    prev, next: zglPFont;
  end;

type
  zglPFontManager = ^zglTFontManager;

  zglTFontManager = record
    Count: Integer;
    First: zglTFont;
  end;

const
  TEXT_HALIGN_LEFT = $000001;
  TEXT_HALIGN_CENTER = $000002;
  TEXT_HALIGN_RIGHT = $000004;
  TEXT_HALIGN_JUSTIFY = $000008;
  TEXT_VALIGN_TOP = $000010;
  TEXT_VALIGN_CENTER = $000020;
  TEXT_VALIGN_BOTTOM = $000040;
  TEXT_CLIP_RECT = $000080;
  TEXT_FX_VCA = $000100;
  TEXT_FX_LENGTH = $000200;

var
  font_Add: function: zglPFont;
  font_Del: procedure(var Font: zglPFont);
  font_LoadFromFile: function(const FileName: UTF8String): zglPFont;
  font_LoadFromMemory: function(const Memory: zglTMemory): zglPFont;
  text_Draw: procedure(Font: zglPFont; X, Y: Single; const Text: UTF8String; Flags: LongWord = 0);
  text_DrawEx: procedure(Font: zglPFont; X, Y, Scale, Step: Single; const Text: UTF8String; Alpha: Byte = 255; Color: LongWord = $FFFFFF;
    Flags: LongWord = 0);
  text_DrawInRect: procedure(Font: zglPFont; const Rect: zglTRect; const Text: UTF8String; Flags: LongWord = 0);
  text_DrawInRectEx: procedure(Font: zglPFont; const Rect: zglTRect; Scale, Step: Single; const Text: UTF8String; Alpha: Byte = 0;
    Color: LongWord = $FFFFFF; Flags: LongWord = 0);
  text_GetWidth: function(Font: zglPFont; const Text: UTF8String; Step: Single = 0.0): Single;
  text_GetHeight: function(Font: zglPFont; Width: Single; const Text: UTF8String; Scale: Single = 1.0; Step: Single = 0.0): Single;
  textFx_SetLength: procedure(Length: Integer; LastCoord: zglPPoint2D = nil; LastCharDesc: zglPCharDesc = nil);

  // Sound
const
  SND_FORMAT_MONO8 = 1;
  SND_FORMAT_MONO16 = 2;
  SND_FORMAT_STEREO8 = 3;
  SND_FORMAT_STEREO16 = 4;

  SND_VOLUME_DEFAULT = -1;

  SND_SOUNDS = nil;
  SND_STREAM = nil;
  SND_ALL_SOURCES = -1;
  SND_ALL_SOURCES_LOOPED = -2;
  SND_ALL_STREAMS = -3;
  SND_ALL_STREAMS_LOOPED = -4;

  SND_STATE_PLAYING = 1;
  SND_STATE_LOOPED = 2;
  SND_STATE_PERCENT = 3;
  SND_STATE_TIME = 4;
  SND_INFO_DURATION = 5;

type
  zglPSound = ^zglTSound;
  zglPSoundStream = ^zglTSoundStream;
  zglPSoundDecoder = ^zglTSoundDecoder;
  zglPSoundFormat = ^zglTSoundFormat;
  zglPSoundManager = ^zglTSoundManager;

  zglTSoundChannel = record
    Source: Ptr;
    Speed: Single;
    Volume: Single;

    Position: record
      X, Y, Z: Single;
    end;
  end;

  zglTSound = record
    Buffer: LongWord;
    SourceCount: LongWord;
    Channel: array of zglTSoundChannel;

    Data: PByteArray;
    Size: LongWord;
    Duration: Double;
    Frequency: LongWord;

    prev, next: zglPSound;
  end;

  zglTSoundStream = record
    _data: Pointer;
    _file: zglTFile;
    _memory: zglTMemory;
    _decoder: zglPSoundDecoder;
    _playing: Boolean;
    _paused: Boolean;
    _waiting: Boolean;
    _complete: Double;
    _lastTime: Double;

    ID: Integer;

    Buffer: PByteArray;
    BufferSize: LongWord;

    Bits: LongWord;
    Frequency: LongWord;
    Channels: LongWord;
    Duration: Double;

    Loop: Boolean;
  end;

  zglTSoundDecoder = record
    Ext: UTF8String;
    Open: function(var Stream: zglTSoundStream; const FileName: UTF8String): Boolean;
    OpenMem: function(var Stream: zglTSoundStream; const Memory: zglTMemory): Boolean;
    Read: function(var Stream: zglTSoundStream; Buffer: PByteArray; Bytes: LongWord; out _End: Boolean): LongWord;
    Loop: procedure(var Stream: zglTSoundStream);
    Seek: procedure(var Stream: zglTSoundStream; Milliseconds: Double);
    Close: procedure(var Stream: zglTSoundStream);
  end;

  zglTSoundFormat = record
    Extension: UTF8String;
    Decoder: zglPSoundDecoder;
    FileLoader: procedure(const FileName: UTF8String; out Data: PByteArray; out Size, Format, Frequency: LongWord);
    MemLoader: procedure(const Memory: zglTMemory; out Data: PByteArray; out Size, Format, Frequency: LongWord);
  end;

  zglTSoundManager = record
    Count: record
      Items: Integer;
      Formats: Integer;
    end;

    First: zglTSound;
    Formats: array of zglTSoundFormat;
  end;

var
  snd_Init: function: Boolean;
  snd_Free: procedure;
  snd_Add: function(SourceCount: Integer): zglPSound;
  snd_Del: procedure(var Sound: zglPSound);
  snd_LoadFromFile: function(const FileName: UTF8String; SourceCount: Integer = 8): zglPSound;
  snd_LoadFromMemory: function(const Memory: zglTMemory; const Extension: UTF8String; SourceCount: Integer = 8): zglPSound;
  snd_Play: function(Sound: zglPSound; Loop: Boolean = FALSE; X: Single = 0; Y: Single = 0; Z: Single = 0;
    Volume: Single = SND_VOLUME_DEFAULT): Integer;
  snd_Stop: procedure(Sound: zglPSound; ID: Integer);
  snd_SetPos: procedure(Sound: zglPSound; ID: Integer; X, Y, Z: Single);
  snd_SetVolume: procedure(Sound: zglPSound; ID: Integer; Volume: Single);
  snd_SetSpeed: procedure(Sound: zglPSound; ID: Integer; Speed: Single);
  snd_Get: function(Sound: zglPSound; ID, What: Integer): Integer;
  snd_PlayFile: function(const FileName: UTF8String; Loop: Boolean = FALSE; Volume: Single = SND_VOLUME_DEFAULT): Integer;
  snd_PlayMemory: function(const Memory: zglTMemory; const Extension: UTF8String; Loop: Boolean = FALSE; Volume: Single = SND_VOLUME_DEFAULT)
    : Integer;
  snd_PauseStream: procedure(ID: Integer);
  snd_StopStream: procedure(ID: Integer);
  snd_ResumeStream: procedure(ID: Integer);
  snd_SeekStream: procedure(ID: Integer; Milliseconds: Double);

  // Video
type
  zglPVideoStream = ^zglTVideoStream;
  zglPVideoDecoder = ^zglTVideoDecoder;
  zglPVideoManager = ^zglTVideoManager;

  zglTVideoStream = record
    _private: record
      Data: Pointer;
      File_: zglTFile;
      Memory: zglTMemory;
      Decoder: zglPVideoDecoder;
    end;

    Data: Pointer;
    Texture: zglPTexture;
    Frame: Integer;
    Time: Double;

    Info: record
      Width: Word;
      Height: Word;
      FrameRate: Single;
      Duration: Double;
      Frames: Integer;
    end;

    Loop: Boolean;

    prev, next: zglPVideoStream;
  end;

  zglTVideoDecoder = record
    Extension: UTF8String;
    Open: function(var Stream: zglTVideoStream; const FileName: UTF8String): Boolean;
    OpenMem: function(var Stream: zglTVideoStream; const Memory: zglTMemory): Boolean;
    Update: procedure(var Stream: zglTVideoStream; Milliseconds: Double; Data: PByteArray);
    Seek: procedure(var Stream: zglTVideoStream; Milliseconds: Double);
    Loop: procedure(var Stream: zglTVideoStream);
    Close: procedure(var Stream: zglTVideoStream);
  end;

  zglTVideoManager = record
    Count: record
      Items: Integer;
      Decoders: Integer;
    end;

    First: zglTVideoStream;
    Decoders: array of zglPVideoDecoder;
  end;

var
  video_Add: function: zglPVideoStream;
  video_Del: procedure(var Stream: zglPVideoStream);
  video_OpenFile: function(const FileName: UTF8String): zglPVideoStream;
  video_OpenMemory: function(const Memory: zglTMemory; const Extension: UTF8String): zglPVideoStream;
  video_Update: procedure(var Stream: zglPVideoStream; Milliseconds: Double; Loop: Boolean = FALSE);
  video_Seek: procedure(var Stream: zglPVideoStream; Milliseconds: Double);

  // MATH
const
  pi = 3.141592654;
  rad2deg = 57.29578049;
  deg2rad = 0.017453292;

  ORIENTATION_LEFT = -1;
  ORIENTATION_RIGHT = 1;
  ORIENTATION_ZERO = 0;

var
  m_Cos: function(Angle: Integer): Single;
  m_Sin: function(Angle: Integer): Single;
  m_Distance: function(x1, y1, x2, y2: Single): Single;
  m_FDistance: function(x1, y1, x2, y2: Single): Single;
  m_Angle: function(x1, y1, x2, y2: Single): Single;
  m_Orientation: function(X, Y, x1, y1, x2, y2: Single): Integer;

  tess_Triangulate: procedure(Contour: zglPPoints2D; iLo, iHi: Integer; AddHoles: Boolean = FALSE);
  tess_AddHole: procedure(Contour: zglPPoints2D; iLo, iHi: Integer; LastHole: Boolean = TRUE);
  tess_GetData: function(out TriPoints: zglPPoints2D): Integer;

  // COLLISION 2D
  col2d_PointInRect: function(X, Y: Single; const Rect: zglTRect): Boolean;
  col2d_PointInTriangle: function(X, Y: Single; const P1, P2, P3: zglTPoint2D): Boolean;
  col2d_PointInCircle: function(X, Y: Single; const Circle: zglTCircle): Boolean;
  // line 2d
  col2d_Line: function(const A, B: zglTLine; ColPoint: zglPPoint2D): Boolean;
  col2d_LineVsRect: function(const Line: zglTLine; const Rect: zglTRect): Boolean;
  col2d_LineVsCircle: function(const Line: zglTLine; const Circle: zglTCircle): Boolean;
  // rect
  col2d_Rect: function(const Rect1, Rect2: zglTRect): Boolean;
  col2d_ClipRect: function(const Rect1, Rect2: zglTRect): zglTRect;
  col2d_RectInRect: function(const Rect1, Rect2: zglTRect): Boolean;
  col2d_RectInCircle: function(const Rect: zglTRect; const Circle: zglTCircle): Boolean;
  col2d_RectVsCircle: function(const Rect: zglTRect; const Circle: zglTCircle): Boolean;
  // circle
  col2d_Circle: function(const Circle1, Circle2: zglTCircle): Boolean;
  col2d_CircleInCircle: function(const Circle1, Circle2: zglTCircle): Boolean;
  col2d_CircleInRect: function(const Circle: zglTCircle; const Rect: zglTRect): Boolean;

const
  FILE_ERROR = {$IFNDEF WINDOWS} 0 {$ELSE} LongWord(-1) {$ENDIF};

  // Open Mode
  FOM_CREATE = $01; // Create
  FOM_OPENR = $02; // Read
  FOM_OPENRW = $03; // Read&Write

  // Seek Mode
  FSM_SET = $01;
  FSM_CUR = $02;
  FSM_END = $03;

var
  file_Open: function(out FileHandle: zglTFile; const FileName: UTF8String; Mode: Byte): Boolean;
  file_MakeDir: function(const Directory: UTF8String): Boolean;
  file_Remove: function(const Name: UTF8String): Boolean;
  file_Exists: function(const Name: UTF8String): Boolean;
  file_Seek: function(FileHandle: zglTFile; Offset, Mode: Integer): LongWord;
  file_GetPos: function(FileHandle: zglTFile): LongWord;
  file_Read: function(FileHandle: zglTFile; var Buffer; Bytes: LongWord): LongWord;
  file_Write: function(FileHandle: zglTFile; const Buffer; Bytes: LongWord): LongWord;
  file_GetSize: function(FileHandle: zglTFile): LongWord;
  file_Flush: procedure(const FileHandle: zglTFile);
  file_Close: procedure(var FileHandle: zglTFile);
  file_Find: procedure(const Directory: UTF8String; out List: zglTFileList; FindDir: Boolean);
  _file_GetName: function(const FileName: UTF8String): PAnsiChar;
  _file_GetExtension: function(const FileName: UTF8String): PAnsiChar;
  _file_GetDirectory: function(const FileName: UTF8String): PAnsiChar;
  file_SetPath: procedure(const Path: UTF8String);
  file_OpenArchive: function(const FileName: UTF8String; const Password: UTF8String = ''): Boolean;
  file_CloseArchive: procedure;

function file_GetName(const FileName: UTF8String): UTF8String;
function file_GetExtension(const FileName: UTF8String): UTF8String;
function file_GetDirectory(const FileName: UTF8String): UTF8String;

var
  mem_LoadFromFile: function(out Memory: zglTMemory; const FileName: UTF8String): Boolean;
  mem_SaveToFile: function(var Memory: zglTMemory; const FileName: UTF8String): Boolean;
  mem_Seek: function(var Memory: zglTMemory; Offset, Mode: Integer): LongWord;
  mem_Read: function(var Memory: zglTMemory; var Buffer; Bytes: LongWord): LongWord;
  mem_Write: function(var Memory: zglTMemory; const Buffer; Bytes: LongWord): LongWord;
  mem_SetSize: procedure(var Memory: zglTMemory; Size: LongWord);
  mem_Free: procedure(var Memory: zglTMemory);

  // String Utils
function u_IntToStr(Value: Integer): UTF8String;
function u_StrToInt(const Value: UTF8String): Integer;
function u_FloatToStr(Value: Single; Digits: Integer = 2): UTF8String;
function u_StrToFloat(const Value: UTF8String): Single;
function u_BoolToStr(Value: Boolean): UTF8String;
function u_StrToBool(const Value: UTF8String): Boolean;
// RU: Только для латинских символов попадающих в диапазон 0..127
// EN: Only for latin symbols in range 0..127
function u_StrUp(const Str: UTF8String): UTF8String;
function u_StrDown(const Str: UTF8String): UTF8String;

function utf8_Copy(const Str: UTF8String): UTF8String; overload;
function utf8_Copy(const Str: UTF8String; FromPosition, Count: Integer): UTF8String; overload;
procedure utf8_Delete(var Str: UTF8String; FromPosition, Count: Integer);

var
  utf8_Length: function(const Str: UTF8String): Integer;
  utf8_GetShift: procedure(const Str: UTF8String; Pos: Integer; out NewPos: Integer; Chars: Integer = 1);
  utf8_GetID: function(const Str: UTF8String; Pos: Integer; Shift: PInteger): LongWord;

  // Utils
var
  u_SortList: procedure(var List: zglTStringList; iLo, iHi: Integer);
  u_Sleep: procedure(Milliseconds: LongWord);
  u_Hash: function(const Str: UTF8String): LongWord;

{$IFDEF UNIX}
function dlopen(Name: PAnsiChar; Flags: longint): Pointer; cdecl; external 'dl';
function dlclose(Lib: Pointer): longint; cdecl; external 'dl';
function dlsym(Lib: Pointer; Name: PAnsiChar): Pointer; cdecl; external 'dl';
{$ENDIF}
{$IFDEF WINDOWS}
function dlopen(lpLibFileName: PAnsiChar): HMODULE; stdcall; external 'kernel32.dll' name 'LoadLibraryA';
function dlclose(hLibModule: HMODULE): Boolean; stdcall; external 'kernel32.dll' name 'FreeLibrary';
function dlsym(HMODULE: HMODULE; lpProcName: PAnsiChar): Pointer; stdcall; external 'kernel32.dll' name 'GetProcAddress';

function MessageBoxA(hWnd: LongWord; lpText, lpCaption: PAnsiChar; uType: LongWord): Integer; stdcall; external 'user32.dll';
{$ENDIF}

implementation

var
  zglLib: {$IFDEF UNIX} Pointer {$ENDIF} {$IFDEF WINDOWS} HMODULE {$ENDIF};
{$IFDEF MACOSX}
  mainBundle: CFBundleRef;
  tmpCFURLRef: CFURLRef;
  tmpCFString: CFStringRef;
  tmpPath: array [0 .. 8191] of AnsiChar;
  outItemHit: SInt16;
  mainPath: UTF8String;
{$ENDIF}

function ini_ReadKeyStr(const Section, Key: UTF8String): UTF8String;
var
  tmp: PAnsiChar;
begin
  tmp := _ini_ReadKeyStr(Section, Key);
  Result := utf8_Copy(tmp);
  zgl_FreeMem(Pointer(tmp));
end;

function key_GetText: UTF8String;
var
  tmp: PAnsiChar;
begin
  tmp := _key_GetText();
  Result := utf8_Copy(tmp);
  zgl_FreeMem(Pointer(tmp));
end;

function file_GetName(const FileName: UTF8String): UTF8String;
var
  tmp: PAnsiChar;
begin
  tmp := _file_GetName(FileName);
  Result := utf8_Copy(tmp);
  zgl_FreeMem(Pointer(tmp));
end;

function file_GetExtension(const FileName: UTF8String): UTF8String;
var
  tmp: PAnsiChar;
begin
  tmp := _file_GetExtension(FileName);
  Result := utf8_Copy(tmp);
  zgl_FreeMem(Pointer(tmp));
end;

function file_GetDirectory(const FileName: UTF8String): UTF8String;
var
  tmp: PAnsiChar;
begin
  tmp := _file_GetDirectory(FileName);
  Result := utf8_Copy(tmp);
  zgl_FreeMem(Pointer(tmp));
end;

function u_IntToStr(Value: Integer): UTF8String;
begin
  Str(Value, Result);
end;

function u_StrToInt(const Value: UTF8String): Integer;
var
  e: Integer;
begin
  Val(Value, Result, e);
  if e <> 0 Then
    Result := 0;
end;

function u_FloatToStr(Value: Single; Digits: Integer = 2): UTF8String;
begin
  Str(Value: 0: Digits, Result);
end;

function u_StrToFloat(const Value: UTF8String): Single;
var
  e: Integer;
begin
  Val(Value, Result, e);
  if e <> 0 Then
    Result := 0;
end;

function u_BoolToStr(Value: Boolean): UTF8String;
begin
  if Value Then
    Result := 'TRUE'
  else
    Result := 'FALSE';
end;

function u_StrToBool(const Value: UTF8String): Boolean;
begin
  if Value = '1' Then
    Result := TRUE
  else if u_StrUp(Value) = 'TRUE' Then
    Result := TRUE
  else
    Result := FALSE;
end;

function u_StrUp(const Str: UTF8String): UTF8String;
var
  i, l: Integer;
begin
  l := Length(Str);
  SetLength(Result, l);
  for i := 1 to l do
    if (Byte(Str[i]) >= 97) and (Byte(Str[i]) <= 122) Then
      Result[i] := AnsiChar(Byte(Str[i]) - 32)
    else
      Result[i] := Str[i];
end;

function u_StrDown(const Str: UTF8String): UTF8String;
var
  i, l: Integer;
begin
  l := Length(Str);
  SetLength(Result, l);
  for i := 1 to l do
    if (Byte(Str[i]) >= 65) and (Byte(Str[i]) <= 90) Then
      Result[i] := AnsiChar(Byte(Str[i]) + 32)
    else
      Result[i] := Str[i];
end;

function utf8_Copy(const Str: UTF8String): UTF8String;
var
  len: Integer;
begin
  len := Length(Str);
  SetLength(Result, len);
  if len > 0 Then
    System.Move(Str[1], Result[1], len);
end;

function utf8_Copy(const Str: UTF8String; FromPosition, Count: Integer): UTF8String;
var
  i, j, len: Integer;
begin
  len := utf8_Length(Str);
  if FromPosition < 1 Then
    FromPosition := 1;
  if (FromPosition > len) or (Count < 1) Then
  begin
    Result := '';
    exit;
  end;
  if FromPosition + Count > len + 1 Then
    Count := len - FromPosition + 1;

  i := 1;
  utf8_GetShift(Str, i, i, FromPosition - 1);
  utf8_GetShift(Str, i, j, Count);
  SetLength(Result, j - i);
  System.Move(Str[i], Result[1], j - i);
end;

procedure utf8_Delete(var Str: UTF8String; FromPosition, Count: Integer);
var
  i, j, len: Integer;
  Result: UTF8String;
begin
  len := utf8_Length(Str);
  if FromPosition < 1 Then
    FromPosition := 1;
  if (FromPosition > len) or (Count < 1) Then
    exit;
  if FromPosition + Count > len + 1 Then
    Count := len - FromPosition + 1;
  if (FromPosition = 1) and (Count = len) Then
  begin
    Str := '';
    exit;
  end;

  len := Length(Str);
  i := 1;
  utf8_GetShift(Str, i, i, FromPosition - 1);
  utf8_GetShift(Str, i, j, Count);
  SetLength(Result, len - j + i);
  System.Move(Str[1], Result[1], i - 1);
  if j <= len Then
    System.Move(Str[j], Result[i], len - (j - 1));
  Str := Result;
end;

function zglLoad(LibraryName: AnsiString; Error: Boolean = TRUE): Boolean;
begin
  Result := FALSE;
{$IFDEF LINUX}
  zglLib := dlopen(PAnsiChar('./' + LibraryName), $001);
  if not Assigned(zglLib) Then
{$ENDIF}
{$IFDEF MACOSX}
    mainBundle := CFBundleGetMainBundle;
  tmpCFURLRef := CFBundleCopyBundleURL(mainBundle);
  tmpCFString := CFURLCopyFileSystemPath(tmpCFURLRef, kCFURLPOSIXPathStyle);
  CFStringGetFileSystemRepresentation(tmpCFString, @tmpPath[0], 8192);
  mainPath := tmpPath + '/Contents/';
  LibraryName := mainPath + 'Frameworks/' + LibraryName;
{$ENDIF}
  zglLib := dlopen(PAnsiChar(LibraryName) {$IFDEF UNIX}, $001 {$ENDIF} );

  if zglLib <> {$IFDEF UNIX} nil {$ENDIF} {$IFDEF WINDOWS} 0 {$ENDIF} Then
  begin
    Result := TRUE;
    zgl_Init := dlsym(zglLib, 'zgl_Init');
    zgl_InitToHandle := dlsym(zglLib, 'zgl_InitToHandle');
    zgl_Exit := dlsym(zglLib, 'zgl_Exit');
    zgl_Reg := dlsym(zglLib, 'zgl_Reg');
    zgl_Get := dlsym(zglLib, 'zgl_Get');
    zgl_GetMem := dlsym(zglLib, 'zgl_GetMem');
    zgl_FreeMem := dlsym(zglLib, 'zgl_FreeMem');
    zgl_FreeStrList := dlsym(zglLib, 'zgl_FreeStrList');
    zgl_Enable := dlsym(zglLib, 'zgl_Enable');
    zgl_Disable := dlsym(zglLib, 'zgl_Disable');

    log_Add := dlsym(zglLib, 'log_Add');

    wnd_SetCaption := dlsym(zglLib, 'wnd_SetCaption');
    wnd_SetSize := dlsym(zglLib, 'wnd_SetSize');
    wnd_SetPos := dlsym(zglLib, 'wnd_SetPos');
    wnd_ShowCursor := dlsym(zglLib, 'wnd_ShowCursor');

    scr_Clear := dlsym(zglLib, 'scr_Clear');
    scr_Flush := dlsym(zglLib, 'scr_Flush');
    scr_SetVSync := dlsym(zglLib, 'scr_SetVSync');
    scr_SetFSAA := dlsym(zglLib, 'scr_SetFSAA');
    scr_SetOptions := dlsym(zglLib, 'scr_SetOptions');
    scr_CorrectResolution := dlsym(zglLib, 'scr_CorrectResolution');
    scr_ReadPixels := dlsym(zglLib, 'scr_ReadPixels');

    ini_LoadFromFile := dlsym(zglLib, 'ini_LoadFromFile');
    ini_SaveToFile := dlsym(zglLib, 'ini_SaveToFile');
    ini_Free := dlsym(zglLib, 'ini_Free');
    ini_Add := dlsym(zglLib, 'ini_Add');
    ini_Del := dlsym(zglLib, 'ini_Del');
    ini_Clear := dlsym(zglLib, 'ini_Clear');
    ini_IsSection := dlsym(zglLib, 'ini_IsSection');
    ini_IsKey := dlsym(zglLib, 'ini_IsKey');
    _ini_ReadKeyStr := dlsym(zglLib, 'ini_ReadKeyStr');
    ini_ReadKeyInt := dlsym(zglLib, 'ini_ReadKeyInt');
    ini_ReadKeyFloat := dlsym(zglLib, 'ini_ReadKeyFloat');
    ini_ReadKeyBool := dlsym(zglLib, 'ini_ReadKeyBool');
    ini_WriteKeyStr := dlsym(zglLib, 'ini_WriteKeyStr');
    ini_WriteKeyInt := dlsym(zglLib, 'ini_WriteKeyInt');
    ini_WriteKeyFloat := dlsym(zglLib, 'ini_WriteKeyFloat');
    ini_WriteKeyBool := dlsym(zglLib, 'ini_WriteKeyBool');

    timer_Add := dlsym(zglLib, 'timer_Add');
    timer_Del := dlsym(zglLib, 'timer_Del');
    timer_GetTicks := dlsym(zglLib, 'timer_GetTicks');
    timer_Reset := dlsym(zglLib, 'timer_Reset');

    mouse_X := dlsym(zglLib, 'mouse_X');
    mouse_Y := dlsym(zglLib, 'mouse_Y');
    mouse_DX := dlsym(zglLib, 'mouse_DX');
    mouse_DY := dlsym(zglLib, 'mouse_DY');
    mouse_Down := dlsym(zglLib, 'mouse_Down');
    mouse_Up := dlsym(zglLib, 'mouse_Up');
    mouse_Click := dlsym(zglLib, 'mouse_Click');
    mouse_DblClick := dlsym(zglLib, 'mouse_DblClick');
    mouse_Wheel := dlsym(zglLib, 'mouse_Wheel');
    mouse_ClearState := dlsym(zglLib, 'mouse_ClearState');
    mouse_Lock := dlsym(zglLib, 'mouse_Lock');

    key_Down := dlsym(zglLib, 'key_Down');
    key_Up := dlsym(zglLib, 'key_Up');
    key_Press := dlsym(zglLib, 'key_Press');
    key_Last := dlsym(zglLib, 'key_Last');
    key_BeginReadText := dlsym(zglLib, 'key_BeginReadText');
    key_UpdateReadText := dlsym(zglLib, 'key_UpdateReadText');
    _key_GetText := dlsym(zglLib, 'key_GetText');
    key_EndReadText := dlsym(zglLib, 'key_EndReadText');
    key_ClearState := dlsym(zglLib, 'key_ClearState');

    joy_Init := dlsym(zglLib, 'joy_Init');
    joy_GetInfo := dlsym(zglLib, 'joy_GetInfo');
    joy_AxisPos := dlsym(zglLib, 'joy_AxisPos');
    joy_Down := dlsym(zglLib, 'joy_Down');
    joy_Up := dlsym(zglLib, 'joy_Up');
    joy_Press := dlsym(zglLib, 'joy_Press');
    joy_ClearState := dlsym(zglLib, 'joy_ClearState');

    res_BeginQueue := dlsym(zglLib, 'res_BeginQueue');
    res_EndQueue := dlsym(zglLib, 'res_EndQueue');
    res_GetPercentage := dlsym(zglLib, 'res_GetPercentage');
    res_GetCompleted := dlsym(zglLib, 'res_GetCompleted');
    res_Proc := dlsym(zglLib, 'res_Proc');

    tex_Add := dlsym(zglLib, 'tex_Add');
    tex_Del := dlsym(zglLib, 'tex_Del');
    tex_Create := dlsym(zglLib, 'tex_Create');
    tex_CreateZero := dlsym(zglLib, 'tex_CreateZero');
    tex_LoadFromFile := dlsym(zglLib, 'tex_LoadFromFile');
    tex_LoadFromMemory := dlsym(zglLib, 'tex_LoadFromMemory');
    tex_SetFrameSize := dlsym(zglLib, 'tex_SetFrameSize');
    tex_SetMask := dlsym(zglLib, 'tex_SetMask');
    tex_SetData := dlsym(zglLib, 'tex_SetData');
    tex_GetData := dlsym(zglLib, 'tex_GetData');
    tex_Filter := dlsym(zglLib, 'tex_Filter');
    tex_SetAnisotropy := dlsym(zglLib, 'tex_SetAnisotropy');

    Set2DMode := dlsym(zglLib, 'Set2DMode');
    Set3DMode := dlsym(zglLib, 'Set3DMode');

    zbuffer_SetDepth := dlsym(zglLib, 'zbuffer_SetDepth');
    zbuffer_Clear := dlsym(zglLib, 'zbuffer_Clear');

    scissor_Begin := dlsym(zglLib, 'scissor_Begin');
    scissor_End := dlsym(zglLib, 'scissor_End');

    rtarget_Add := dlsym(zglLib, 'rtarget_Add');
    rtarget_Del := dlsym(zglLib, 'rtarget_Del');
    rtarget_Set := dlsym(zglLib, 'rtarget_Set');
    rtarget_DrawIn := dlsym(zglLib, 'rtarget_Set');

    fx_SetBlendMode := dlsym(zglLib, 'fx_SetBlendMode');
    fx_SetColorMode := dlsym(zglLib, 'fx_SetColorMode');
    fx_SetColorMask := dlsym(zglLib, 'fx_SetColorMask');
    fx2d_SetColor := dlsym(zglLib, 'fx2d_SetColor');
    fx2d_SetVCA := dlsym(zglLib, 'fx2d_SetVCA');
    fx2d_SetVertexes := dlsym(zglLib, 'fx2d_SetVertexes');
    fx2d_SetScale := dlsym(zglLib, 'fx2d_SetScale');
    fx2d_SetRotatingPivot := dlsym(zglLib, 'fx2d_SetRotatingPivot');

    cam2d_Init := dlsym(zglLib, 'cam2d_Init');
    cam2d_Set := dlsym(zglLib, 'cam2d_Set');
    cam2d_Get := dlsym(zglLib, 'cam2d_Get');

    batch2d_Begin := dlsym(zglLib, 'batch2d_Begin');
    batch2d_End := dlsym(zglLib, 'batch2d_End');
    batch2d_Flush := dlsym(zglLib, 'batch2d_Flush');

    pr2d_Pixel := dlsym(zglLib, 'pr2d_Pixel');
    pr2d_Line := dlsym(zglLib, 'pr2d_Line');
    pr2d_Rect := dlsym(zglLib, 'pr2d_Rect');
    pr2d_Circle := dlsym(zglLib, 'pr2d_Circle');
    pr2d_Ellipse := dlsym(zglLib, 'pr2d_Ellipse');
    pr2d_TriList := dlsym(zglLib, 'pr2d_TriList');

    sengine2d_AddSprite := dlsym(zglLib, 'sengine2d_AddSprite');
    sengine2d_AddCustom := dlsym(zglLib, 'sengine2d_AddCustom');
    sengine2d_DelSprite := dlsym(zglLib, 'sengine2d_DelSprite');
    sengine2d_ClearAll := dlsym(zglLib, 'sengine2d_ClearAll');
    sengine2d_Set := dlsym(zglLib, 'sengine2d_Set');
    sengine2d_Draw := dlsym(zglLib, 'sengine2d_Draw');
    sengine2d_Proc := dlsym(zglLib, 'sengine2d_Proc');

    texture2d_Draw := dlsym(zglLib, 'texture2d_Draw');
    ssprite2d_Draw := dlsym(zglLib, 'ssprite2d_Draw');
    asprite2d_Draw := dlsym(zglLib, 'asprite2d_Draw');
    csprite2d_Draw := dlsym(zglLib, 'csprite2d_Draw');
    tiles2d_Draw := dlsym(zglLib, 'tiles2d_Draw');
    sgrid2d_Draw := dlsym(zglLib, 'sgrid2d_Draw');
    agrid2d_Draw := dlsym(zglLib, 'agrid2d_Draw');
    cgrid2d_Draw := dlsym(zglLib, 'cgrid2d_Draw');

    pengine2d_Set := dlsym(zglLib, 'pengine2d_Set');
    pengine2d_Get := dlsym(zglLib, 'pengine2d_Get');
    pengine2d_Draw := dlsym(zglLib, 'pengine2d_Draw');
    pengine2d_Proc := dlsym(zglLib, 'pengine2d_Proc');
    pengine2d_AddEmitter := dlsym(zglLib, 'pengine2d_AddEmitter');
    pengine2d_DelEmitter := dlsym(zglLib, 'pengine2d_DelEmitter');
    pengine2d_ClearAll := dlsym(zglLib, 'pengine2d_ClearAll');
    emitter2d_Add := dlsym(zglLib, 'emitter2d_Add');
    emitter2d_Del := dlsym(zglLib, 'emitter2d_Del');
    emitter2d_LoadFromFile := dlsym(zglLib, 'emitter2d_LoadFromFile');
    emitter2d_LoadFromMemory := dlsym(zglLib, 'emitter2d_LoadFromMemory');
    emitter2d_Init := dlsym(zglLib, 'emitter2d_Init');
    emitter2d_Free := dlsym(zglLib, 'emitter2d_Free');
    emitter2d_Draw := dlsym(zglLib, 'emitter2d_Draw');
    emitter2d_Proc := dlsym(zglLib, 'emitter2d_Proc');

    font_Add := dlsym(zglLib, 'font_Add');
    font_Del := dlsym(zglLib, 'font_Del');
    font_LoadFromFile := dlsym(zglLib, 'font_LoadFromFile');
    font_LoadFromMemory := dlsym(zglLib, 'font_LoadFromMemory');
    text_Draw := dlsym(zglLib, 'text_Draw');
    text_DrawEx := dlsym(zglLib, 'text_DrawEx');
    text_DrawInRect := dlsym(zglLib, 'text_DrawInRect');
    text_DrawInRectEx := dlsym(zglLib, 'text_DrawInRectEx');
    text_GetWidth := dlsym(zglLib, 'text_GetWidth');
    text_GetHeight := dlsym(zglLib, 'text_GetHeight');
    textFx_SetLength := dlsym(zglLib, 'textFx_SetLength');

    snd_Init := dlsym(zglLib, 'snd_Init');
    snd_Free := dlsym(zglLib, 'snd_Free');
    snd_Add := dlsym(zglLib, 'snd_Add');
    snd_Del := dlsym(zglLib, 'snd_Del');
    snd_LoadFromFile := dlsym(zglLib, 'snd_LoadFromFile');
    snd_LoadFromMemory := dlsym(zglLib, 'snd_LoadFromMemory');
    snd_Play := dlsym(zglLib, 'snd_Play');
    snd_Stop := dlsym(zglLib, 'snd_Stop');
    snd_SetPos := dlsym(zglLib, 'snd_SetPos');
    snd_SetVolume := dlsym(zglLib, 'snd_SetVolume');
    snd_SetSpeed := dlsym(zglLib, 'snd_SetSpeed');
    snd_Get := dlsym(zglLib, 'snd_Get');
    snd_PlayFile := dlsym(zglLib, 'snd_PlayFile');
    snd_PlayMemory := dlsym(zglLib, 'snd_PlayMemory');
    snd_PauseStream := dlsym(zglLib, 'snd_PauseStream');
    snd_StopStream := dlsym(zglLib, 'snd_StopStream');
    snd_ResumeStream := dlsym(zglLib, 'snd_ResumeStream');
    snd_SeekStream := dlsym(zglLib, 'snd_SeekStream');

    video_Add := dlsym(zglLib, 'video_Add');
    video_Del := dlsym(zglLib, 'video_Del');
    video_OpenFile := dlsym(zglLib, 'video_OpenFile');
    video_OpenMemory := dlsym(zglLib, 'video_OpenMemory');
    video_Update := dlsym(zglLib, 'video_Update');
    video_Seek := dlsym(zglLib, 'video_Seek');

    m_Cos := dlsym(zglLib, 'm_Cos');
    m_Sin := dlsym(zglLib, 'm_Sin');
    m_Distance := dlsym(zglLib, 'm_Distance');
    m_FDistance := dlsym(zglLib, 'm_FDistance');
    m_Angle := dlsym(zglLib, 'm_Angle');
    m_Orientation := dlsym(zglLib, 'm_Orientation');

    tess_Triangulate := dlsym(zglLib, 'tess_Triangulate');
    tess_AddHole := dlsym(zglLib, 'tess_AddHole');
    tess_GetData := dlsym(zglLib, 'tess_GetData');

    col2d_PointInRect := dlsym(zglLib, 'col2d_PointInRect');
    col2d_PointInTriangle := dlsym(zglLib, 'col2d_PointInTriangle');
    col2d_PointInCircle := dlsym(zglLib, 'col2d_PointInCircle');
    col2d_Line := dlsym(zglLib, 'col2d_Line');
    col2d_LineVsRect := dlsym(zglLib, 'col2d_LineVsRect');
    col2d_LineVsCircle := dlsym(zglLib, 'col2d_LineVsCircle');
    col2d_Rect := dlsym(zglLib, 'col2d_Rect');
    col2d_ClipRect := dlsym(zglLib, 'col2d_ClipRect');
    col2d_RectInRect := dlsym(zglLib, 'col2d_RectInRect');
    col2d_RectInCircle := dlsym(zglLib, 'col2d_RectInCircle');
    col2d_RectVsCircle := dlsym(zglLib, 'col2d_RectVsCircle');
    col2d_Circle := dlsym(zglLib, 'col2d_Circle');
    col2d_CircleInCircle := dlsym(zglLib, 'col2d_CircleInCircle');
    col2d_CircleInRect := dlsym(zglLib, 'col2d_CircleInRect');

    file_Open := dlsym(zglLib, 'file_Open');
    file_MakeDir := dlsym(zglLib, 'file_MakeDir');
    file_Remove := dlsym(zglLib, 'file_Remove');
    file_Exists := dlsym(zglLib, 'file_Exists');
    file_Seek := dlsym(zglLib, 'file_Seek');
    file_GetPos := dlsym(zglLib, 'file_GetPos');
    file_Read := dlsym(zglLib, 'file_Read');
    file_Write := dlsym(zglLib, 'file_Write');
    file_GetSize := dlsym(zglLib, 'file_GetSize');
    file_Flush := dlsym(zglLib, 'file_Flush');
    file_Close := dlsym(zglLib, 'file_Close');
    file_Find := dlsym(zglLib, 'file_Find');
    _file_GetName := dlsym(zglLib, 'file_GetName');
    _file_GetExtension := dlsym(zglLib, 'file_GetExtension');
    _file_GetDirectory := dlsym(zglLib, 'file_GetDirectory');
    file_SetPath := dlsym(zglLib, 'file_SetPath');
    file_OpenArchive := dlsym(zglLib, 'file_OpenArchive');
    file_CloseArchive := dlsym(zglLib, 'file_CloseArchive');

    mem_LoadFromFile := dlsym(zglLib, 'mem_LoadFromFile');
    mem_SaveToFile := dlsym(zglLib, 'mem_SaveToFile');
    mem_Seek := dlsym(zglLib, 'mem_Seek');
    mem_Read := dlsym(zglLib, 'mem_Read');
    mem_Write := dlsym(zglLib, 'mem_Write');
    mem_SetSize := dlsym(zglLib, 'mem_SetSize');
    mem_Free := dlsym(zglLib, 'mem_Free');

    utf8_Length := dlsym(zglLib, 'utf8_Length');
    utf8_GetShift := dlsym(zglLib, 'utf8_GetShift');
    utf8_GetID := dlsym(zglLib, 'utf8_GetID');

    u_SortList := dlsym(zglLib, 'u_SortList');
    u_Hash := dlsym(zglLib, 'u_Hash');
    u_Sleep := dlsym(zglLib, 'u_Sleep');
  end
  else if Error Then
  begin
{$IFDEF LINUX}
    WriteLn('Error while loading ZenGL');
{$ENDIF}
{$IFDEF WINDOWS}
    MessageBoxA(0, 'Error while loading ZenGL', 'Error', $00000010);
{$ENDIF}
{$IFDEF MACOSX}
    StandardAlert(kAlertNoteAlert, 'Error', 'Error while loading ZenGL', nil, outItemHit);
{$ENDIF}
  end;
end;

procedure zglFree;
begin
  dlclose(zglLib);
end;

end.
