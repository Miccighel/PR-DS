%% ---- MODULO AIR_SUPERVISOR --- %%

%% Questo modulo rappresenta il supervisore del modulo dedicato alla climatizzazione e permette di eseguire l'impostazione
%% iniziale delle varie componenti del modulo stesso.

-module(air_supervisor).
-behaviour(supervisor).
-compile(export_all).

%% Lancia il supervisore e lega l'implementazione concreta dell'event handler al processo gen_event.

start_link(Name) ->
  {ok, Pid} = supervisor:start_link({local,Name},?MODULE, []),
  io:format("SUPERVISORE CLIMATIZZAZIONE: Il supervisore del modulo è stato avviato con identificatore: ~p~n", [Pid]),
  %% Necessario restituire tale tupla per l'application controller che ha il compito di avviare l'applicazione, altrimenti
  %% restituisce un errore bad_return_value.
  {ok, Pid}.

%% Durante la fase di inizializzazione del supervisore vengono impostati i nomi locali dei processi corrispondenti alle varie
%% componenti del sistema; vengono quindi indicati i figli sotto il controllo del supervisore da lanciare andando a definire
%% le loro specifiche ed infine viene impostata la strategia globale di supervisione.

init([]) ->
  process_flag(trap_exit, true),
  EventHandlerName = air_event_handler,
  Sensor1Name = air_sensor_1,
  Sensor2Name = air_sensor_2,
  %% Una singola ChildSpecification è nella forma: {ChildId, StartFunc, Restart, Shutdown, Type, Modules}.
  ChildSpecification =
    [
      {EventHandlerName, {air_event, start_link, [EventHandlerName]}, permanent, 5000, worker, [dynamic]},
      {Sensor1Name, {air_sensor, start_link, [EventHandlerName,Sensor1Name]}, permanent, 5000, worker, [air_sensor]},
      {Sensor2Name, {air_sensor, start_link, [EventHandlerName,Sensor2Name]}, permanent, 5000, worker, [air_sensor]}

    ],
  %% Utilizzare una strategia rest_for_one significa che se una componente del sistema termina per qualsiasi motivo, vengono riavviate
  %% LA COMPONENTE STESSA E TUTTE QUELLE AVVIATE DOPO DI ESSA. Se, ad esempio, il processo temperature_sensor_2 muore, vengono fatti ripartire:
  %% sensor_2, calculator e network_monitor, nell'ordine dato.
  Strategy = {{rest_for_one, 10, 6000}, ChildSpecification},
  {ok, Strategy}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione. Per il momento, nessuna.

terminate(Reason, _State) ->
  io:format("SUPERVISORE CLIMATIZZAZIONE: Il supervisore generale con identificatore ~p e stato terminato per il motivo: ~p~n", [self(),Reason]),
  ok.