%% ---- MODULO PROJECT_SUPERVISOR ---- %%

%% Questo modulo rappresenta il supervisore globale del progetto e permette di eseguire l'impostazione iniziale delle
%% varie componenti del sistema sulla macchina.

-module(project_supervisor).
-behaviour(supervisor).
-compile(export_all).

%% ---- FUNZIONI STANDARD DI SUPERVISOR ---- %%

%% Lancia il super supervisore.

start() ->
  %% Viene recuperata la variabile d'ambiente che indica la modalità in cui eseguire l'applicazione
  {ok, Modality} = application:get_env(modality),
  io:format("SUPER SUPERVISORE: L'applicazione è stata avviata in modalità: ~p~n", [Modality]),
  {ok, Pid} = supervisor:start_link({local,super_supervisor},?MODULE, Modality),
  io:format("SUPER SUPERVISORE: Il super supervisore del progetto è stato avviato con identificatore: ~p~n", [Pid]),
  %% Necessario restituisce tale tupla per l'application controller che ha il compito di avviare l'applicazione, altrimenti
  %% restituisce un errore bad_return_value.
  {ok, Pid}.

%% Funzione di inizilizzazione che viene lanciata quando l'applicazione viene avviata in modalità sender. Il supervisore, in tale caso,
%% manda in esecuzione i moduli dedicati alla gestione di finestre e temperatura, per poi avviare il client di rete in modalità, appunto,
%% sender.

init(sender) ->
  process_flag(trap_exit, true),
  WindowSupervisorName = window_supervisor,
  TemperatureSupervisorName = temperature_supervisor,
  ProcessName = client1,
  MonitorName = project_network_monitor,
  %% Una singola ChildSpecification è nella forma: {ChildId, StartFunc, Restart, Shutdown, Type, Modules}.
  ChildSpecification =
    [
      {WindowSupervisorName, {window_supervisor, start_link, [WindowSupervisorName,ProcessName]}, permanent, 5000, supervisor, [window_supervisor]},
      {TemperatureSupervisorName, {temperature_supervisor, start_link, [TemperatureSupervisorName,ProcessName]}, permanent, 5000, supervisor, [temperature_supervisor]},
      {ProcessName, {project_network, start_link, [ProcessName,sender]}, permanent, 5000, worker, [project_network]},
      {MonitorName, {project_network_monitor, start_link, [ProcessName,MonitorName]}, permanent, 5000, worker, [project_network_monitor]}
    ],
  %% Utilizzare una strategia rest_for_one significa che se una componente del sistema termina per qualsiasi motivo, vengono riavviate
  %% LA COMPONENTE STESSA E TUTTE QUELLE AVVIATE DOPO DI ESSA. Se, ad esempio, il processo window_supervisor muore, vengono fatti ripartire:
  %% temperature_supervisor e client1, nell'ordine dato.
  Strategy = {{rest_for_one, 10, 6000}, ChildSpecification},
  {ok, Strategy};

%% Funzione di inizilizzazione che viene lanciata quando l'applicazione viene avviata in modalità receiver. Il supervisore, in tale caso,
%% manda in esecuzione il modulo dedicato alla gestione della climatizzazione ed avvia il client di rete in modalità, appunto receiver.

init(receiver) ->
  process_flag(trap_exit, true),
  AirSupervisorName = air_supervisor,
  ProcessName = client1,
  %% Una singola ChildSpecification è nella forma: {ChildId, StartFunc, Restart, Shutdown, Type, Modules}.
  ChildSpecification =
    [
      {AirSupervisorName, {air_supervisor, start_link, [AirSupervisorName]}, permanent, 5000, supervisor, [air_supervisor]},
      {ProcessName, {project_network, start_link, [ProcessName,receiver]}, permanent, 5000, worker, [project_network]}
    ],
  Strategy = {{rest_for_one, 10, 6000}, ChildSpecification},
  {ok, Strategy}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione.

terminate(Reason, _State) ->
  io:format("SUPER SUPERVISORE: Il supervisore generale del progetto con identificatore ~p e stato terminato per il motivo: ~p~n", [self(),Reason]),
  ok.