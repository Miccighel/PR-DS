%% ---- MODULO TEMPERATURE_EVENT   --- %%

%% Questo modulo ha il semplice compito di mandare in esecuzione un event handler OTP agganciando l'implementazione concreta
%% dell'handler corrispondente.

-module(window_event).
-compile(export_all).

%% Lancia l'event handler OTP e lo lega all'handler corrispondente.

start_link(Name, ClientName) ->
  {ok, Pid} = gen_event:start_link({local, Name}),
  ok = gen_event:add_handler(Name, window_handler, ClientName),
  {ok, Pid}.
