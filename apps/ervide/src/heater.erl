-module(heater).
-behaviour(gen_server).
-export([init/1, handle_cast/2, handle_info/2, terminate/2, start/0]).

% How often the heater state will be retransmitted to the
% heater control socket in the absence of any explicit
% changes. Units of ms.
-define(INTERVAL, 15322).


start() ->
           {ok, Heater_process} = gen_server:start_link(heater, [], []),
           io:fwrite("heater: start: Heater_process = ~w (pid)\n", [Heater_process]),
           {ok, Heater_process}.


init([]) -> 
  io:fwrite("heater: init: trapping exits\n"),

  process_flag(trap_exit, true),


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

  io:fwrite("heater: init: registering heater name\n"),
  register(heater, self()),
  io:fwrite("heater: init finished\n"),
  {ok, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, 0, 0], ?INTERVAL}.

handle_cast(Bit, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, LastBit, LastTime]) ->
  io:fwrite("heater: received command to turn power ~w on/off (was ~w at ~w)\n", [Bit, LastBit, LastTime]),
  set_bit(Bit, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, LastBit, LastTime]).

handle_info(timeout, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, LastBit, LastTime]) ->
  io:fwrite("heater: handle_info: refreshing remote power unit with current state\n"),
  set_bit(LastBit, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, LastBit, LastTime]).

set_bit(Bit, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, LastBit, LastTime]) ->
  Now = os:system_time(second),
  if
    (Now - LastTime < 5) and (LastBit == Bit) ->
      io:fwrite("heater: set_bit: suppressing radio as recently set to ~w\n", [Bit]),
      {noreply, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, LastBit, LastTime], ?INTERVAL};
    true ->
      io:fwrite("heater: set_bit: setting ~w\n", [Bit]),
      drive_radio(Bit, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22]),
      {noreply, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, Bit, os:system_time(second)], ?INTERVAL}
  end.

drive_radio(Bit, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22]) ->
  io:fwrite("heater: drive_radio: setting to ~w\n", [Bit]),
  gpio:write(Pin11, 0),
  gpio:write(Pin15, 1),
  gpio:write(Pin16, 1),
  gpio:write(Pin13, Bit),
  timer:sleep(100),
  gpio:write(Pin22, 1),
  timer:sleep(250),
  gpio:write(Pin22, 0),
  io:fwrite("heater: drive_radio: transmitted\n", []).


terminate(Reason, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22, LastBit, LastTime]) ->
  io:fwrite("heater: terminating, reason ~w\n", [Reason]),
  drive_radio(0, [Pin11, Pin13, Pin15, Pin16, Pin18, Pin22]),
  io:fwrite("heater: termination complete\n", []).

