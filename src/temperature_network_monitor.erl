%% ---- MODULO TEMPERATURE_NETWORK_MONITOR ---- %%

%% Questo modulo fornisce un'interfaccia per definire un contratto di QoS verso i sensori, il modulo deputato ai calcoli
%% e verso l'event handler, al fine di regolare il traffico presente sulla rete, generato per la maggior parte dal modulo
%% della temperatura.

-module(temperature_network_monitor).
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
  io:format(LogSender, "~p~p~n", ["MONITOR TEMPERATURA: Monitor del traffico di rete generato dal modulo in esecuzione con identificatore: ", self()]),
  io:format("MONITOR TEMPERATURA: Monitor del traffico di rete generato dal modulo in esecuzione con identificatore: ~p~n", [self()]),
  file:close(LogSender),
  State = EventManager,
  {ok, State}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione. Per il momento, nessuna.

terminate(Reason, _State) ->
  {ok, LogSender} = file:open("../log/Log_Sender.txt", [append]),
  io:format(LogSender, "~p~p~p~p~n", ["MONITOR TEMPERATURA: Il monitor del traffico di rete generato dal modulo con identificatore ", self(), " e stato terminato per il motivo: ", Reason]),
  io:format("MONITOR TEMPERATURA: Il monitor del traffico di rete generato dal modulo con identificatore ~p e stato terminato per il motivo: ~p~n", [self(), Reason]),
  file:close(LogSender),
  ok.

%% Gestione della modifica a runtime del codice.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% --- FUNZIONI DI SUPPORTO ED EVENTUALE MESSAGGISTICA --- %

%% Le seguenti funzioni sono il cuore del monitor di rete e consentono di inviare all'event handler richieste per aggiornare
%% la dimensione del buffer per l'immagazzinamento dei valori medi calcolati e gli intervalli con cui i sensori devono inviare
%% i valori istantanei di temperature e con cui il processo dedicato ai calcoli deve inviare richieste di calcolo della media,
%% al fine di effettuare la gestione del traffico generato sulla rete.

update_buffer_size(EventManager, Value) ->
  gen_event:notify(EventManager, {update_buffer_size, Value}),
  ok.

update_interval_between_values(EventManager, Value) ->
  gen_event:notify(EventManager, {update_interval_between_values, Value}),
  ok.

update_interval_between_means(EventManager, Value) ->
  gen_event:notify(EventManager, {update_interval_between_means, Value}),
  ok.

% --- GESTIONE DELLE CHIAMATE SINCRONE --- %

%% Intercetta qualsiasi richiesta sincrona bloccando la chiamata. Se viene eseguita una chiamata sincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_call(_Request, _From, _State) ->
  {stop, normal, "MONITOR TEMPERATURA: Chiamate sincrone non permesse", _State}.

% --- GESTIONE DELLE CHIAMATE ASINCRONE --- %

%% Intercetta qualsiasi richiesta asincrona bloccando la chiamata. Se viene eseguita una chiamata asincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_cast(_Request, _State) ->
  {stop, normal, "MONITOR TEMPERATURA: Chiamate asincrone non permesse", _State}.

% --- GESTIONE DEI MESSAGGI --- %

%% Non viene effettuata alcuna particolare gestione di eventuali messaggi non trattati con le funzioni precedenti.

handle_info(Message, State) ->
  {ok, LogSender} = file:open("../log/Log_Sender.txt", [append]),
  io:format(LogSender, "~p~p~n", ["MONITOR TEMPERATURA: Messaggio ricevuto: ", Message]),
  io:format("MONITOR TEMPERATURA: Messaggio ricevuto: ~p~n", [Message]),
  file:close(LogSender),
  {noreply, State}.

