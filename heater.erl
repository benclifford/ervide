-module(heater).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_info/2]).

% How often the heater state will be retransmitted to the
% heater control socket in the absence of any explicit
% changes.
-define(INTERVAL, 5634). % about 5 seconds

init([]) -> 
  io:fwrite("heater: initialising GPIO pins\n"),

  % PinNN numbers are board pins, but numbers
  % passed to gpio:init are BCM numbers, I think.
  Pin11 = gpio:init(17, out),
  Pin13 = gpio:init(27, out),
  Pin15 = gpio:init(22, out),
  Pin16 = gpio:init(23, out),
  Pin18 = gpio:init(24, out),
  Pin22 = gpio:init(25, out),

  gpio:write(Pin22, 0),

% based on energenie's examples:

%# Set the modulator to ASK for On Off Keying
%# by setting MODSEL pin lo
  gpio:write(Pin18, 0),

%# Initialise K0-K3 inputs of the encoder to 0000
  gpio:write(Pin11, 0),
  gpio:write(Pin15, 0),
  gpio:write(Pin16, 0),
  gpio:write(Pin13, 0),

  erlang:send_after(?INTERVAL, self(), refresh),

  io:fwrite("heater: init finished\n"),
  {ok, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, 0]}.

handle_cast(Bit, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, LastBit]) ->
  io:fwrite("heater: control power ~w on/off (was ~w)\n", [Bit, LastBit]),
  drive_radio(Bit, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22]),

  io:fwrite("heater: setting heater bit finished.\n"),
  {noreply, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, Bit]}.

handle_info(refresh, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, LastBit]) ->
  io:fwrite("heater: handle_info: refreshing\n"),
  drive_radio(LastBit, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22]),
  io:fwrite("heater: transmitter off\n", []),

  erlang:send_after(?INTERVAL, self(), refresh),
  {noreply, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, LastBit]}.

drive_radio(Bit, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22]) ->
  io:fwrite("heater: drive_radio: setting to ~w\n", [Bit]),
  gpio:write(Pin11, 0),
  gpio:write(Pin15, 1),
  gpio:write(Pin16, 1),
  gpio:write(Pin13, Bit),
  timer:sleep(250),
  gpio:write(Pin22, 1),
  io:fwrite("heater: drive_radio: transmitter on\n", []),
  timer:sleep(250),
  gpio:write(Pin22, 0),
  io:fwrite("heater: drive_radio: transmitter off\n", []).
