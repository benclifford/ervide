-module(ervide).
-export([start/0, pwmmer/0, propctrl/0, integctrl/0, sumctrl/0]).

startup_message() -> io:fwrite("ervide - an Erlang sous vide controller\n").

start() -> startup_message(),

	   PWM_process = spawn(ervide, pwmmer, []),
	   io:fwrite("start: PWM_process = ~w (pid)\n", [PWM_process]),
	   register(pwm, PWM_process),

	   Proportional_process = spawn(ervide, propctrl, []),
	   io:fwrite("start: Proportional_process = ~w (pid)\n", [Proportional_process]),
	   register(proportional, Proportional_process),

	   Integral_process = spawn(ervide, integctrl, []),
	   io:fwrite("start: Integral_process = ~w (pid)\n", [Integral_process]),
	   register(integral, Integral_process),

	   Summer_process = spawn(ervide, sumctrl, []),
	   io:fwrite("start: Summer_process = ~w (pid)\n", [Summer_process]),
	   register(summer, Summer_process),

	   % as an example of changing the PWM fraction
           timer:sleep(10000),
	   pwm ! 0.6,   % try a bigger duty cycle

	   io:fwrite("start: End of start\n").


propctrl() -> io:fwrite("proportional: loop... nothing to do but message the summer with 0%\n"),
	      timer:sleep(18876),
	      propctrl().

integctrl() -> io:fwrite("integral: loop... nothing to do but message the summer with 1%\n"),
	       timer:sleep(17543),
	       integctrl().

% summer should receive partial-PWMs from the other components:
% proportional, integral and derivative, sum them to produce
% a final PWM and pass it on the the PWMer to make happen.
sumctrl() -> io:fwrite("summer: loop... nothing to do\n"),
	     timer:sleep(25090),
	     sumctrl().


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

