-module(ervide).
-export([hello_world/0,start/0, pwmmer/0]).

hello_world() -> io:fwrite("hello, world - from ervide\n").

start() -> hello_world(),
	   hello_world(),
	   hello_world(),
	   spawn(ervide, pwmmer, []),

	   io:fwrite("End of start\n").

pwmmer() -> io:fwrite("pwmmer starting\n"),
	    pwmmer_loop(0.3).

pwmmer_loop(Fraction) ->
	io:fwrite("pwmmer loop calculating\n"),

	PWM_pulse_resolution_ms = 1137,
	PWM_period_seconds = 30,

	Time = erlang:system_time(second),
	Position_in_period = Time rem PWM_period_seconds,
	Fraction_in_period = Position_in_period / PWM_period_seconds,

	io:fwrite("Percentage: ~w%\n", [Fraction * 100]),
	io:fwrite("System time: ~w s\n", [Time]),
	io:fwrite("Position in period: ~w s / ~w s \n", [Position_in_period, PWM_period_seconds]),
	io:fwrite("Percent through period: ~w% \n", [Fraction_in_period * 100]),

	if
		Fraction > Fraction_in_period -> io:fwrite("ON\n");
		true -> io:fwrite("OFF\n")
	end,

        io:fwrite("pwmmer loop waiting\n"),
	timer:sleep(PWM_pulse_resolution_ms),
	pwmmer_loop(Fraction).

