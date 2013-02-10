%  NUTS-style escaping for colour tags.
-module(colour).
-export([interpolate/1]).

% interpolate(String) -> EscapedString
%  Control:
%   ~RS: Reset terminal
%   ~RV: Reverse video
%   ~OL: Bold
%   ~LI: Blink
%   ~UL: Underline
%  Foreground:
%   ~FK: Black
%   ~FR: Red
%   ~FG: Green
%   ~FY: Yellow
%   ~FB: Blue
%   ~FM: Magenta
%   ~FT: Turquoise
%   ~FW: White
%  Background is ~BK for black, ~BR for red, etc.
%  You can escape a ~ with /, e.g. /~FG will print ~FG unescaped.
%  Colour sequences are automatically reset at the end of each line and at
%  the end of the string itself.
 
interpolate("/~" ++ Tail) ->
  [$~ | interpolate(Tail)];

interpolate([$~, Ground, Colour | Tail]) ->
  case [Ground, Colour] of
    % Control
    "RS" -> "\e[0m";
    "OL" -> "\e[1m";
    "UL" -> "\e[4m";
    "LI" -> "\e[5m";
    "RV" -> "\e[7m";
    % Foreground;     Background
    "FK" -> "\e[30m"; "BK" -> "\e[40m";
    "FR" -> "\e[31m"; "BR" -> "\e[41m";
    "FG" -> "\e[32m"; "BG" -> "\e[42m";
    "FY" -> "\e[33m"; "BY" -> "\e[43m";
    "FB" -> "\e[34m"; "BB" -> "\e[44m";
    "FM" -> "\e[35m"; "BM" -> "\e[45m";
    "FT" -> "\e[36m"; "BT" -> "\e[46m";
    "FW" -> "\e[37m"; "BW" -> "\e[47m";
    % Pass through non-matches.
    Other -> [$~ | Other]
  end ++ interpolate(Tail);

interpolate([$\n | Tail]) ->
  "\e[0m\n" ++ interpolate(Tail);

interpolate([Other | Tail]) ->
  [Other | interpolate(Tail)];

interpolate([]) ->
  "\e[0m".
