%% ---- MODULO TEMPERATURE_SENSOR --- %%

%% Questo modulo permette di modellare un singolo sensore in grado azionare la climatizzazione in una stanza.

-module(air_sensor).
-compile(export_all).
-behaviour(gen_server).

% --- FUNZIONI STANDARD DI GEN_SERVER --- %

%% Lancia il sensore legandolo all'event handler e registrandolo con il nome desiderato. Tali elementi corrispondono ai parametri passati.

start_link(EventManager, Name) ->
  {ok, _Pid} = gen_server:start_link({local, Name}, ?MODULE, EventManager, []).

%% Durante la fase di inizializzazione viene eseguita la registrazione del sensore presso l'event handler,
%% viene preparato lo stato necessario al sensore (l'intervallo con cui inviare i dati all'event handler stesso) ed
%% il timer per regolare tale invio di dati.

init(EventManager) ->
  process_flag(trap_exit, true),
  io:format("SENSORE CLIMATIZZAZIONE: Sensore in esecuzione con identificatore: ~p~n", [self()]),
  subscribe(EventManager),
  State = {turn_off},
  {ok, State}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione. Per il momento, nessuna.

terminate(Reason, _State) ->
  io:format("SENSORE CLIMATIZZAZIONE: Il sensore con identificatore ~p e stato terminato per il motivo: ~p~n", [self(), Reason]),
  ok.

%% Gestione della modifica a runtime del codice.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% --- MESSAGGISTICA --- %

%% Operazione di registrazione presso l'event handler, che consiste nell'inviare a quest'ultimo il proprio identificatore,
%% in questo caso il nome.

subscribe(EventManager) ->
  gen_event:notify(EventManager, {register, self()}),
  ok.

% --- GESTIONE DELLE CHIAMATE SINCRONE --- %

%% Intercetta qualsiasi richiesta sincrona bloccando la chiamata. Se viene eseguita una chiamata sincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_call(_Request, _From, _State) ->
  {stop, normal, "SENSORE CLIMATIZZAZIONE: Chiamate sincrone non permesse", _State}.

% --- GESTIONE DELLE CHIAMATE ASINCRONE --- %

%% Intercetta qualsiasi richiesta sincrona bloccando la chiamata. Se viene eseguita una chiamata sincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_cast({update_status, Value}, _State) ->
  case Value of
    turn_on ->
      io:format("SENSORE CLIMATIZZAZIONE: Il climatizzatore gestito da ~p viene acceso.~n", [self()]),
      NewState = {turn_on},
      {noreply,NewState};
    turn_off ->
      io:format("SENSORE CLIMATIZZAZIONE: Il climatizzatore gestito da ~p viene spento.~n", [self()]),
      NewState = {turn_off},
      {noreply,NewState}
  end.

% --- GESTIONE DEI MESSAGGI RIMANENTI --- %

%% Nella seguente funzione vengono gestiti i messaggi che non previsti nel pattern matching delle handle_cast e delle
%% handle_call; in questo caso sono i messaggi legati al timer che il processo invia a se stesso. Quello che viene fatto
%% consiste nel cancellare il vecchio timer presente nello stato, eseguire la funzione per inviare il valore rilevato
%% all'event handler e ricreare il timer sulla base dell'intervallo tra ciascun invio definito, aggiornando infine lo stato
%% del processo.

handle_info(Message, State) ->
  io:format("SENSORE CLIMATIZZAZIONE: Messaggio ricevuto: ~p~n", [Message]),
  {noreply, State}.
