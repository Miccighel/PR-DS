%% ---- MODULO WINDOW_NETWORK_MONITOR ---- %%

%% Questo modulo fornisce un'interfaccia per definire un contratto di QoS verso i sensori, il modulo deputato ai calcoli
%% e verso l'event handler, al fine di regolare il traffico presente sulla rete, generato per la maggior parte dal modulo
%% della temperatura.

-module(window_network_monitor).
-behaviour(gen_server).
-compile(export_all).

% --- FUNZIONI STANDARD DI GEN_SERVER --- %

%% Lancia il monitor legandolo all'event handler e registrandolo con il nome desiderato. Tali elementi corrispondono ai parametri passati.

start_link(EventManager, Name) ->
  {ok, _Pid} = gen_server:start_link({local, Name}, ?MODULE, EventManager, []).

%% Durante la fase di inizializzazione viene memorizzato il riferimento all'event handler nello stato del monitor.

init(EventManager) ->
  process_flag(trap_exit, true),
  EventManager,
  {ok, LogSender} = file:open("../log/Log_Sender.txt", [append]),
  io:format(LogSender, "~p~p~n", ["MONITOR FINESTRE: Monitor del traffico di rete generato dal modulo in esecuzione con identificatore: ", self()]),
  io:format("MONITOR FINESTRE: Monitor del traffico di rete generato dal modulo in esecuzione con identificatore: ~p~n", [self()]),
  file:close(LogSender),
  State = EventManager,
  {ok, State}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione. Per il momento, nessuna.

terminate(Reason, _State) ->
  {ok, LogSender} = file:open("../log/Log_Sender.txt", [append]),
  io:format(LogSender, "~p~p~p~p~n", ["MONITOR FINESTRE: Il monitor del traffico di rete generato dal modulo con identificatore ", self(), " e stato terminato per il motivo: ", Reason]),
  io:format("MONITOR FINESTRE: Il monitor del traffico di rete generato dal modulo con identificatore ~p e stato terminato per il motivo: ~p~n", [self(), Reason]),
  file:close(LogSender),
  ok.

%% Gestione della modifica a runtime del codice.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% --- FUNZIONI DI SUPPORTO ED EVENTUALE MESSAGGISTICA --- %

%% La seguente funzione è il cuore del monitor di rete e consentono di inviare all'event handler richieste per aggiornare
%% l'intervallo con cui viene rilevato ed inviato lo stato delle finestre

update_interval_between_status(EventManager, Value) ->
  gen_event:notify(EventManager, {update_interval_between_status, Value}),
  ok.

% --- GESTIONE DELLE CHIAMATE SINCRONE --- %

%% Intercetta qualsiasi richiesta sincrona bloccando la chiamata. Se viene eseguita una chiamata sincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_call(_Request, _From, _State) ->
  {stop, normal, "MONITOR FINESTRE: Chiamate sincrone non permesse", _State}.

% --- GESTIONE DELLE CHIAMATE ASINCRONE --- %

%% Intercetta qualsiasi richiesta asincrona bloccando la chiamata. Se viene eseguita una chiamata asincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_cast(_Request, _State) ->
  {stop, normal, "MONITOR FINESTRE: Chiamate asincrone non permesse", _State}.

% --- GESTIONE DEI MESSAGGI --- %

%% Non viene effettuata alcuna particolare gestione di eventuali messaggi non trattati con le funzioni precedenti.

handle_info(Message, State) ->
  {ok, LogSender} = file:open("../log/Log_Sender.txt", [append]),
  io:format(LogSender, "~p~p~n", ["MONITOR FINESTRE: Messaggio ricevuto: ", Message]),
  io:format("MONITOR FINESTRE: Messaggio ricevuto: ~p~n", [Message]),
  file:close(LogSender),
  {noreply, State}.

