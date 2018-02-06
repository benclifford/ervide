-module(ervide).
-export([start/0,
	 pwmmer/0, propctrl/0, integctrl/0, sumctrl/0,
	 tempmeasure/0, errorctrl/0
	]).

startup_message() -> io:fwrite("ervide - an Erlang sous vide controller\n").

start() -> startup_message(),

	   % At present, there's a defined process startup order because
	   % none of these processes can cope with a target process
	   % not existing when it begins and sends its initial value.
	   % That could be fixed...
	   PWM_process = spawn(ervide, pwmmer, []),
	   io:fwrite("start: PWM_process = ~w (pid)\n", [PWM_process]),
	   register(pwm, PWM_process),

	   Summer_process = spawn(ervide, sumctrl, []),
	   io:fwrite("start: Summer_process = ~w (pid)\n", [Summer_process]),
	   register(summer, Summer_process),


	   Proportional_process = spawn(ervide, propctrl, []),
	   io:fwrite("start: Proportional_process = ~w (pid)\n", [Proportional_process]),
	   register(proportional, Proportional_process),

	   Integral_process = spawn(ervide, integctrl, []),
	   io:fwrite("start: Integral_process = ~w (pid)\n", [Integral_process]),
	   register(integral, Integral_process),

	   Errorterm_process = spawn(ervide, errorctrl, []),
	   io:fwrite("start: Errorterm_process = ~w (pid)\n", [Errorterm_process]),
	   register(errorterm, Errorterm_process),

	   Temperature_process = spawn(ervide, tempmeasure, []),
	   io:fwrite("start: Temperature_process = ~w (pid)\n", [Temperature_process]),
	   register(temperature, Temperature_process),

	   io:fwrite("start: reached end of startup\n").


errorctrl() ->
  io:fwrite("errorterm: loop start\n"),
  errorloop(72).

errorloop(Setpoint) ->
  io:fwrite("errorloop: with setpoint ~w degrees C\n", [Setpoint]),
  receive
    T -> io:fwrite("errorloop: received temperature ~w\n", [T]),
         E = Setpoint - T, % degrees C, how many more degrees we need to add in heat to get to the setpoint
	 io:fwrite("errorloop: temperature error is ~w degrees C\n", [E]),
         proportional ! T,
         integral ! T,
         errorloop(Setpoint)
  after 61434 -> errorloop(Setpoint) % wait a whole minute because this is not something that needs refreshing that often other than when a temperature arrives
  end.



% proportional needs to know:
%   Kp - hard coded constant for now
%   Setpoint - hard coded constant for now?
%   Current Temperature - needs to be acquired from sensor
propctrl() -> io:fwrite("proportional: loop start\n"),
	      Kp = 0.725, % PWMs per degree Celcius

              receive
                T -> Fraction = Kp * T,
                     io:fwrite("proportional: calculated new PWM fraction ~w%\n", [Fraction * 100]),

	             summer ! {proportional, Fraction}
              end, % no timeout persistent behaviour here - relying on summer to be able to store the relevant state
	      propctrl().

integctrl() -> io:fwrite("integral: loop... nothing to do but message the summer with 1%\n"),
	       summer ! {integral, 0.01},
	       timer:sleep(17543),
	       integctrl().

% summer should receive partial-PWMs from the other components:
% proportional, integral and derivative, sum them to produce
% a final PWM and pass it on to the pwmmer to make happen.
%
% We should periodically update the pwmmer in case it has forgotten (?).
%
% It will receive a name atom rather than a PID, I guess, because the
% PIDs might change as services are restarted. For supercrazy
% live upgradability, it should perhaps forgot a name after a while
% (10 minutes?) if we haven't had an update? Soft-state sum components!
sumctrl() -> io:fwrite("summer: startup\n"),
	     sumloop(maps:new()).

sumloop(Last) ->
	     io:fwrite("summer: waiting, with Last = ~w\n", [Last]),
             New = receive 
		     {From, Val} -> 
		        io:fwrite("summer: received new PWM component from From ~w for ~w%\n", [From, Val*100]),
		       	maps:put(From, Val, Last);

		     Token ->
			io:fwrite("summer: Unknown message ~w\n", [Token]),
			Last
             after 25090 -> Last
	     end,
	     Sumloopfold = fun(K, V, AccIn) -> AccIn + V end,
	     New_pwm = maps:fold(Sumloopfold, 0, New),
	     io:fwrite("summer: new pwm = ~w%\n", [New_pwm * 100]),
	     io:fwrite("summer: new map is ~w\n", [New]),
	     pwm ! New_pwm,
	     sumloop(New).

% pwmmer is supplied with a single PWM fraction value stream, and
% makes the output PWM at that fraction. It doesn't care for the maths
% that go towards adding up that PWM.
%
% It will start at 0% = off, and await information from the summer.

pwmmer() -> io:fwrite("pwmmer: starting\n"),
	    pwmmer_loop(0).

pwmmer_loop(Fraction) ->
	io:fwrite("pwmmer: loop calculating\n"),

	PWM_pulse_resolution_ms = 6137,
	PWM_period_seconds = 50,

	Time = erlang:system_time(second),
	Position_in_period = Time rem PWM_period_seconds,
	Fraction_in_period = Position_in_period / PWM_period_seconds,

	io:fwrite("pwmmer: Percentage: ~w%\n", [Fraction * 100]),
	io:fwrite("pwmmer: System time: ~w s\n", [Time]),
	io:fwrite("pwmmer: Position in period: ~w s / ~w s \n", [Position_in_period, PWM_period_seconds]),
	io:fwrite("pwmmer: Percent through period: ~w% \n", [Fraction_in_period * 100]),

	if
		Fraction > Fraction_in_period -> io:fwrite("pwmmer: power ON\n");
		true -> io:fwrite("pwmmer: power OFF\n")
	end,

        io:fwrite("pwmmer: loop waiting for message or pwm interval\n"),

	receive
		New_time -> pwmmer_loop(New_time)
		% loop without waiting for the whole delay. This means we might switch immediately. Although there's perhaps some rate limiting to be done on how fast we switch the physical hardware. This might not be the place to do it? Looping without delay also means we'll absorb new messages fairly fast, rather than at a max rate of one per period.
	        % timeout for PWM loop could be dynamically calculated as time to expected next transition (because if we don't get a message interrupting this, we'll only change at the next transition).
	after PWM_pulse_resolution_ms -> pwmmer_loop(Fraction)
	end.


tempmeasure() ->
  io:fwrite("temperature: start of loop\n"),
  io:fwrite("temperature: attempting to connect\n"),

  {ok, Sock} = gen_tcp:connect("10.11.13.239", 23, [{active, false}, list], 2000),

  io:fwrite("temperature: socket open - waiting for data\n"),

  {ok, Json} = gen_tcp:recv(Sock, 0, 8000), % longer timeout because the remote goes off and does a temperature measurement

  io:fwrite("temperature: received from socket: ~w\n", [Json]),

  gen_tcp:close(Sock),

  io:fwrite("temperature: socket closed\n"),

  J = jiffy:decode(Json),


  io:fwrite("temperature: J-decoded: ~w\n", [J]),
  
  [_,T] = J, % the 2nd sensor
  io:fwrite("temperature: T-decoded: ~w\n", [T]),

  % send out current temperature to whoever needs it:
  % that is: an error calculator, which then forwards the error to
  % proportional and integral components
  % and 
  % the derivative process, which should compute using the
  % derivative of the measured value, not of the error term
  % (so as to cope better with controller step changes)

  errorterm ! T,

  timer:sleep(28014),
  tempmeasure().

