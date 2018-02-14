-module(statslogger).
-behaviour(gen_server).
-export([start/0, init/1, handle_cast/2, terminate/2]).

start() ->
  {ok, Stats_process} = gen_server:start_link(statslogger, [], []),
  io:fwrite("statslogger: start: Stats_process = ~w (pid)\n", [Stats_process]),
  {ok, Stats_process}.

init(Args) ->
  io:fwrite("statslogger: initialising"),

  process_flag(trap_exit, true),

  {ok, Log_IOD} = file:open("stats.csv", [append]),
  io:fwrite(Log_IOD, "update_reason,time,temperature,setpoint,pwm_output\n", []),

  register(statslogger, self()),
  io:fwrite("statslogger: initialised"),
  {ok, [Log_IOD, 0, 0, 0]}.

handle_cast({temperature, T}, [Log_IOD, Old_Temp, Old_Setpoint, Old_Output_PWM]) ->
  io:fwrite("statslogger: new temperature logged: ~w degrees\n", [T]),
  io:fwrite(Log_IOD, "~w,~w,~w,~w,~w\n",
    [temperature,
     os:system_time(second),
     T,
     Old_Setpoint,
     Old_Output_PWM
    ]),
  {noreply, [Log_IOD, T, Old_Setpoint, Old_Output_PWM]};

handle_cast({setpoint, S}, [Log_IOD, Old_Temp, Old_Setpoint, Old_Output_PWM]) ->
  io:fwrite("statslogger: new setpoint logged: ~w degrees\n", [S]),
  io:fwrite(Log_IOD, "~w,~w,~w,~w,~w\n",
    [setpoint,
     os:system_time(second),
     Old_Temp,
     S,
     Old_Output_PWM
    ]),
  {noreply, [Log_IOD, Old_Temp, S, Old_Output_PWM]};

handle_cast({pwm_output, PWM}, [Log_IOD, Old_Temp, Old_Setpoint, Old_Output_PWM]) ->
  io:fwrite("statslogger: new pwm logged: ~w (frac)\n", [PWM]),
  io:fwrite(Log_IOD, "~w,~w,~w,~w,~w\n",
    [pwm_output,
     os:system_time(second),
     Old_Temp,
     Old_Setpoint,
     PWM
    ]),
  {noreply, [Log_IOD, Old_Temp, Old_Setpoint, PWM]}.


terminate(Reason, [Log_IOD, T, Old_Setpoint, Old_Output_PWM]) ->
  io:fwrite("statslogger: terminating, reason ~w\n", [Reason]),
  file:close(Log_IOD),
  io:fwrite("statslogger: terminated.\n").
