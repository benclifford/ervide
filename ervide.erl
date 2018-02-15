-module(ervide).

-behaviour(application).
-behaviour(supervisor).
-export([pwmmer/0, propctrl/0, integctrl/0, diffctrl/0, sumctrl/0,
	 tempmeasure/0, errorctrl/0, init/1,
         start/2, stop/1, prep_stop/1,
         startpwmmer/0, startsummer/0, startproportional/0,
         startintegral/0, starterrorterm/0, starttemperature/0,
         startdifferential/0
	]).

startup_message() -> io:fwrite("ervide - an Erlang sous vide controller\n").


% for application behaviour

start(Type, Args) -> 
  startup_message(),
  supervisor:start_link(ervide, []).
  % if this supervisor stops - in current use most often because the
  % temperature measurement isn't reachable - then this needs to be
  % some kind of loud obvious notification sent to the user
  % so they can attempt to save their food.

prep_stop(State) -> 
  io:fwrite("ervide: prep_stop: preparing to stop application\n"),
  ok.

stop(State) ->
  io:fwrite("ervide: stop: end of stop application\n"),
  ok.

% for supervisor behaviour

% At present, there's a defined process startup order because
% none of these processes can cope with a target process
% not existing when it begins and sends its initial value.
% That could be fixed...

init(Args) ->
  io:fwrite("ervide: init: in init...\n", []),
  SupFlags = #{strategy => one_for_one},
  ChildSpec = [
      #{id => statsprocess, start => {statslogger, start, []} },
      #{id => heaterprocess, start => {heater, start, []} },
      #{id => pwmmerprocess, start => {ervide, startpwmmer, []} },
      #{id => summerprocess, start => {ervide, startsummer, []} },
      #{id => proportionalprocess, start => {ervide, startproportional, []} },
      #{id => integralprocess, start => {ervide, startintegral, []} },
      #{id => differentialprocess, start => {ervide, startdifferential, []} },
      #{id => errortermprocess, start => {ervide, starterrorterm, []} },

      % there's a bug in the temperature process wrt supervisors at the
      % moment: two quick restarts in a row are causing the supervisor
      % to shut down (which is correct supervisor behaviour) but
      % this should be more lenient: 3 minutes gap is probably ok for
      % not being able to measure temperature, for example.
      #{id => temperatureprocess, start => {ervide, starttemperature, []} }
    ],
  {ok, {SupFlags, ChildSpec}}.

startpwmmer() -> 
	   PWM_process = spawn(ervide, pwmmer, []),
	   io:fwrite("start: PWM_process = ~w (pid)\n", [PWM_process]),
	   register(pwm, PWM_process),
           {ok, PWM_process}.

startsummer() ->
	   Summer_process = spawn(ervide, sumctrl, []),
	   io:fwrite("start: Summer_process = ~w (pid)\n", [Summer_process]),
	   register(summer, Summer_process),
           {ok, Summer_process}.


startproportional() ->
	   Proportional_process = spawn(ervide, propctrl, []),
	   io:fwrite("start: Proportional_process = ~w (pid)\n", [Proportional_process]),
	   register(proportional, Proportional_process),
           {ok, Proportional_process}.

startintegral() ->
	   Integral_process = spawn(ervide, integctrl, []),
	   io:fwrite("start: Integral_process = ~w (pid)\n", [Integral_process]),
	   register(integral, Integral_process),
           {ok, Integral_process}.

startdifferential() ->
	   Differential_process = spawn(ervide, diffctrl, []),
	   io:fwrite("start: Differential_process = ~w (pid)\n", [Differential_process]),
	   register(differential, Differential_process),
           {ok, Differential_process}.

starterrorterm() ->
	   Errorterm_process = spawn(ervide, errorctrl, []),
	   io:fwrite("start: Errorterm_process = ~w (pid)\n", [Errorterm_process]),
	   register(errorterm, Errorterm_process),
           {ok, Errorterm_process}.

starttemperature() ->
	   Temperature_process = spawn_link(ervide, tempmeasure, []),
	   io:fwrite("start: Temperature_process = ~w (pid)\n", [Temperature_process]),
	   register(temperature, Temperature_process),
           {ok, Temperature_process}.



errorctrl() ->
  io:fwrite("errorterm: loop start\n"),
  Start_Setpoint = 62.0001,
  gen_server:cast(statslogger, {setpoint, Start_Setpoint}),
  errorloop(Start_Setpoint).

errorloop(Setpoint) ->
  io:fwrite("errorloop: with setpoint ~w degrees C\n", [Setpoint]),
  receive
    {temperature, T} ->
         io:fwrite("errorloop: received temperature ~w\n", [T]),
         E = Setpoint - T, % degrees C, how many more degrees we need to add in heat to get to the setpoint
	 io:fwrite("errorloop: temperature error is ~w degrees C\n", [E]),
         proportional ! E,
         integral ! E,
         errorloop(Setpoint);
    {setpoint, S} ->
         io:fwrite("errorloop: received new setpoint ~w\n", [S]),
         gen_server:cast(statslogger, {setpoint, S}),
         errorloop(S)
  after 61434 -> errorloop(Setpoint) % wait a whole minute because this is not something that needs refreshing that often other than when a temperature arrives
  end.



% proportional needs to know:
%   Kp - hard coded constant for now
%   Setpoint - hard coded constant for now?
%   Current Temperature - needs to be acquired from sensor
propctrl() -> io:fwrite("proportional: loop start\n"),
	      Kp = 0.3, % PWMs per degree Celcius

              receive
                T -> Fraction = Kp * T,
                     io:fwrite("proportional: calculated new PWM fraction ~w%\n", [Fraction * 100]),

                     gen_server:cast(statslogger, {pwm_proportional, Fraction}),
	             summer ! {proportional, Fraction}
              end, % no timeout persistent behaviour here - relying on summer to be able to store the relevant state
	      propctrl().

integctrl() ->
  io:fwrite("integral: starting\n"),
  integloop(0, os:system_time(second)).

integloop(Sum, Prev_time) ->
               % the integral sum is a sum of temperature over time
               % so the unit of Sum is (kelvin . sec)
               % so Ki is PWMs per (kelvin . sec)
               Ki = 0.0002836,
               Kp = 0.3, % copy from above - TODO: distribute better
               io:fwrite("integral: loop, sum = ~w kelvin-seconds\n", [Sum]),
               receive ErrK ->
                 % in the python impl, this decision is based on whether we are within the band in which proportional control is not saturating the pwm controller. This abs test is different, but very broadly similar. The main point is to stop adjusting the integral when we are far from the correct point, to avoid integral windup.
                 if abs(ErrK) < (1/Kp) -> 
                   io:fwrite("integral: error is within interesting zone\n"),
                   Now = os:system_time(second),
                   Delta_seconds = Now - Prev_time,
                   NewSum = Sum + ErrK * Delta_seconds,
                   Pwm_frac = Ki * NewSum,
                   io:fwrite("integral: delta seconds ~w sec\n", [Delta_seconds]),
                   io:fwrite("integral: calculated new PWM fraction ~w%\n", [Pwm_frac * 100]),
                   gen_server:cast(statslogger, {pwm_integral, Pwm_frac}),
	           summer ! {integral, Pwm_frac},
	           integloop(NewSum, Now);
                 true -> io:fwrite("integral: error is outside of interesting zone. Not adjusting integral\n"),
                   integloop(Sum, os:system_time(second))
                 end
               end.

diffctrl() ->
  io:fwrite("differential: starting\n"),
  diffloop([]).

diffloop(State) ->
  io:fwrite("differential: loop\n"),
  receive {temperature, T} ->
    io:fwrite("differential: received temperature ~w\n", [T]),
    Pwm_frac = 0,
    gen_server:cast(statslogger, {pwm_differential, Pwm_frac}),
    summer ! {differential, Pwm_frac}
  end,
  diffloop(State).

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
		        io:fwrite("summer: received new PWM component from ~w for ~w%\n", [From, Val*100]),
		       	maps:put(From, Val, Last);

		     Token ->
			io:fwrite("summer: Unknown message ~w\n", [Token]),
			Last
             after 25090 -> Last
	     end,
	     io:fwrite("summer: new component map is ~w\n", [New]),
	     Sumloopfold = fun(K, V, AccIn) -> AccIn + V end,
	     New_pwm = maps:fold(Sumloopfold, 0, New),
	     io:fwrite("summer: new total PWM ~w%\n", [New_pwm * 100]),
	     pwm ! New_pwm,
	     sumloop(New).

% pwmmer is supplied with a single PWM fraction value stream, and
% makes the output PWM at that fraction. It doesn't care for the maths
% that go towards adding up that PWM.
%
% It will start at 0% = off, and await information from the summer.

pwmmer() -> io:fwrite("pwmmer: starting\n"),
            gen_server:cast(statslogger, {pwm_output, 0}),
	    pwmmer_loop(0).

pwmclip(Fraction) -> max(0, min(1, Fraction)).

pwmmer_loop(Fraction) ->
	io:fwrite("pwmmer: loop calculating\n"),

	PWM_period_seconds = 50,

	Time = erlang:system_time(second),
	Position_in_period = Time rem PWM_period_seconds,
	Fraction_in_period = Position_in_period / PWM_period_seconds,

	io:fwrite("pwmmer: Percentage: ~w%\n", [Fraction * 100]),
	io:fwrite("pwmmer: System time: ~w s\n", [Time]),
	io:fwrite("pwmmer: Position in period: ~w s / ~w s \n", [Position_in_period, PWM_period_seconds]),
	io:fwrite("pwmmer: Percent through period: ~w% \n", [Fraction_in_period * 100]),

	Next_change_delay = if
		Fraction > Fraction_in_period ->
                  io:fwrite("pwmmer: power ON\n"),
                  gen_server:cast(heater, 1),
                  Remaining_fraction = pwmclip(Fraction) - Fraction_in_period,
                  io:fwrite("pwmmer: remaining fraction = ~w pwms\n", [Remaining_fraction]),
                  max(1, Remaining_fraction * PWM_period_seconds);
                                          
		true -> io:fwrite("pwmmer: power OFF\n"),
                        gen_server:cast(heater, 0),
                        max(1, PWM_period_seconds - Position_in_period)
	end,

        io:fwrite("pwmmer: loop waiting for message or pwm refresh interval of ~w sec\n", [Next_change_delay]),

        Next_change_delay_ms = round(Next_change_delay * 1000),

	receive
		New_fraction -> 
                  gen_server:cast(statslogger, {pwm_output, New_fraction}),
                  pwmmer_loop(New_fraction)
		% loop without waiting for the whole delay. This means we might switch immediately. Although there's perhaps some rate limiting to be done on how fast we switch the physical hardware. This might not be the place to do it? Looping without delay also means we'll absorb new messages fairly fast, rather than at a max rate of one per period.
	        % timeout for PWM loop could be dynamically calculated as time to expected next transition (because if we don't get a message interrupting this, we'll only change at the next transition).
	after Next_change_delay_ms -> pwmmer_loop(Fraction)
	end.


tempmeasure() ->
  io:fwrite("temperature: start of loop\n"),
  process_flag(trap_exit, true), % really only needs to happen in init, but I think ok to keep doing?

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

  errorterm ! {temperature, T},
  differential ! {temperature, T},
  gen_server:cast(statslogger, {temperature, T}),

  timer:sleep(28014),
  tempmeasure().
