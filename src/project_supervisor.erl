%% ---- MODULO PROJECT_SUPERVISOR --- %%

%% Questo modulo rappresenta il supervisore globale del progetto e permette di eseguire l'impostazione iniziale delle
%% varie componenti del sistema sulla macchina.

-module(project_supervisor).
-behaviour(supervisor).
-compile(export_all).

%% Lancia il super supervisore.

start() ->
  %% Viene recuperata la variabile d'ambiente che indica la modalità in cui eseguire l'applicazione
  {ok, Modality} = application:get_env(modality),
  io:format("L'applicazione è stata avviata in modalità: ~p~n", [Modality]),
  {ok, Pid} = supervisor:start_link({local,super_supervisor},?MODULE, Modality),
  io:format("Il super supervisore del progetto è stato avviato con identificatore: ~p~n", [Pid]),
  %% Necessario restituire tale tupla per l'application controller che ha il compito di avviare l'applicazione, altrimenti
  %% restituire un errore bad_return_value.
  {ok, Pid}.

init(sender) ->
  process_flag(trap_exit, true),
  TemperatureSupervisorName = temperature_supervisor,
  ProcessName = client1,
  %% Una singola ChildSpecification è nella forma: {ChildId, StartFunc, Restart, Shutdown, Type, Modules}.
  ChildSpecification =
    [
      {TemperatureSupervisorName, {temperature_supervisor, start_link, [TemperatureSupervisorName,ProcessName]}, permanent, 5000, supervisor, [temperature_supervisor]},
      {ProcessName, {project_network, start_link, [ProcessName,sender]}, permanent, 5000, worker, [project_network]}
    ],
  Strategy = {{rest_for_one, 10, 6000}, ChildSpecification},
  {ok, Strategy};

init(receiver) ->
  process_flag(trap_exit, true),
  ProcessName = client1,
  %% Una singola ChildSpecification è nella forma: {ChildId, StartFunc, Restart, Shutdown, Type, Modules}.
  ChildSpecification =
    [
      {ProcessName, {project_network, start_link, [ProcessName,receiver]}, permanent, 5000, worker, [project_network]}
    ],
  Strategy = {{rest_for_one, 10, 6000}, ChildSpecification},
  {ok, Strategy}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione. Per il momento, nessuna.
terminate(Reason, _State) ->
  io:format("Il supervisore generale del progetto con identificatore ~p e stato terminato per il motivo: ~p~n", [self(),Reason]),
  ok.