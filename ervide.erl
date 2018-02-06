-module(ervide).
-export([start/0, pwmmer/0, propctrl/0, integctrl/0, sumctrl/0]).

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

	   % as an example of changing the PWM fraction
           timer:sleep(10000),
	   pwm ! 0.6,   % try a bigger duty cycle

	   io:fwrite("start: End of start\n").


propctrl() -> io:fwrite("proportional: loop... nothing to do but message the summer with 5%\n"),
	      summer ! {proportional, 0.05},
	      timer:sleep(18876),
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
	after PWM_pulse_resolution_ms -> pwmmer_loop(Fraction)
	end.

