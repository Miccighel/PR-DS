%% ---- MODULO PROJECT_NETWORK_MONITOR ---- %%

%% Questo modulo fornisce un'interfaccia per definire un contratto di QoS verso i sensori, il modulo deputato ai calcoli
%% e verso l'event handler, al fine di regolare il traffico presente sulla rete, generato per la maggior parte dal modulo
%% della temperatura.

-module(project_network_monitor).
-behaviour(gen_server).
-compile(export_all).

% --- FUNZIONI STANDARD DI GEN_SERVER --- %

%% Lancia il monitor legandolo al client di rete e registrandolo con il nome desiderato. Tali elementi corrispondono ai parametri passati.

start_link(ClientName,Name) ->
  {ok, _Pid} = gen_server:start_link({local,Name},?MODULE, ClientName,[]).

%% Durante la fase di inizializzazione viene memorizzato il riferimento all'event handler nello stato del monitor.

init(ClientName) ->
  process_flag(trap_exit, true),
  io:format("MONITOR CLIENT DI RETE: Monitor del traffico di rete generato dal modulo in esecuzione con identificatore: ~p~n", [self()]),
  State = ClientName,
  {ok, State}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione. Per il momento, nessuna.

terminate(Reason, _State) ->
  io:format("MONITOR CLIENT DI RETE: Il monitor del traffico di rete generato dal modulo con identificatore ~p e stato terminato per il motivo: ~p~n", [self(),Reason]),
  ok.

%% Gestione della modifica a runtime del codice.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% --- FUNZIONI DI SUPPORTO ED EVENTUALE MESSAGGISTICA --- %

%% La seguente funzione è il cuore del monitor di rete e consente di inviare al client una richiesta per aggiornare l'intervallo
%% temporale con cui esegue l'invio dei dati sul broker o al modulo dedicato alla climatizzazione, a seconda della modalità in cui
%% viene avviato.

update_interval(ClientName, Value)->
  gen_server:cast(ClientName, Value),
  ok.

% --- GESTIONE DELLE CHIAMATE SINCRONE --- %

%% Intercetta qualsiasi richiesta sincrona bloccando la chiamata. Se viene eseguita una chiamata sincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_call(_Request, _From, _State) ->
  {stop, normal, "MONITOR CLIENT DI RETE: Chiamate sincrone non permesse", _State}.

% --- GESTIONE DELLE CHIAMATE ASINCRONE --- %

%% Intercetta qualsiasi richiesta asincrona bloccando la chiamata. Se viene eseguita una chiamata asincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_cast(_Request, _State) ->
  {stop, normal, "MONITOR CLIENT DI RETE: Chiamate asincrone non permesse", _State}.

% --- GESTIONE DEI MESSAGGI --- %

%% Non viene effettuata alcuna particolare gestione di eventuali messaggi non trattati con le funzioni precedenti.

handle_info(Message, State) ->
  io:format("MONITOR FINESTRE: Messaggio ricevuto: ~p~n", [Message]),
  {noreply, State}.

