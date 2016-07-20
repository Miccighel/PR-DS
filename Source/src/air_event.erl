%% ---- MODULO TEMPERATURE_EVENT   --- %%

%% Questo modulo ha il semplice compito di mandare in esecuzione un event handler OTP agganciando l'implementazione concreta
%% dell'handler corrispondente.

-module(air_event).
-compile(export_all).

%% Lancia l'event handler OTP e lo lega all'handler corrispondente.

start_link(Name) ->
  {ok, Pid} = gen_event:start_link({local, Name}),
  ok = gen_event:add_handler(Name, air_handler, []),
  {ok, Pid}.
