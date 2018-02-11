-module(heater).
-behaviour(gen_server).
-export([init/1, handle_cast/2]).

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

  io:fwrite("heater: init finished\n"),
  {ok, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22]}.

%  heaterloop(0, Pin11, Pin13, Pin15, Pin16, Pin18, Pin22).

handle_cast(Bit, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22]) ->
  io:fwrite("heater: control power ~w on/off\n", [Bit]),
  gpio:write(Pin11, 0),
  gpio:write(Pin15, 1),
  gpio:write(Pin16, 1),
  gpio:write(Pin13, Bit),
  timer:sleep(250),
  gpio:write(Pin22, 1),
  io:fwrite("heater: transmitter on\n", []),
  timer:sleep(250),
  gpio:write(Pin22, 0),
  io:fwrite("heater: transmitter off\n", []),

  io:fwrite("heater: setting heater bit finished.\n"),
  {noreply, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22]}.

