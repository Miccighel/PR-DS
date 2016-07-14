%% ---- MODULO WINDOW_SENSOR --- %%

%% Questo modulo permette di modellare un singolo sensore in grado di rilevare lo stato di una finestra in una stanza.

-module(window_sensor).
-compile(export_all).
-behaviour(gen_server).

% --- FUNZIONI STANDARD DI GEN_SERVER --- %

%% Lancia il sensore legandolo all'event handler e registrandolo con il nome desiderato. Tali elementi corrispondono ai parametri passati.

start_link(EventManager,Name) ->
  {ok, _Pid} = gen_server:start_link({local,Name},?MODULE, EventManager,[]).

%% Durante la fase di inizializzazione viene eseguita la registrazione del sensore presso l'event handler,
%% viene preparato lo stato necessario al sensore (l'intervallo con cui inviare i dati all'event handler stesso) ed
%% il timer per regolare tale invio di dati.

init(EventManager) ->
  Interval = 3000,
  process_flag(trap_exit, true),
  io:format("SENSORE FINESTRE: Sensore in esecuzione con identificatore: ~p~n", [self()]),
  subscribe(EventManager),
  Data = {intervalBetweenStatus,Interval},
  Timer = erlang:send_after(1, self(), {send,EventManager}),
  State = {Timer,Data},
  {ok, State}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione. Per il momento, nessuna.

terminate(Reason, _State) ->
  io:format("SENSORE FINESTRE: Il sensore con identificatore ~p e stato terminato per il motivo: ~p~n", [self(),Reason]),
  ok.

%% Gestione della modifica a runtime del codice.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% --- FUNZIONI DI SUPPORTO ED EVENTUALE MESSAGGISTICA --- %

%% Operazione di registrazione presso l'event handler, che consiste nell'inviare a quest'ultimo il proprio identificatore,
%% in questo caso il nome.

subscribe(EventManager)->
  gen_event:notify(EventManager, {register, self()}),
  ok.

%% Invia all'event handler il valore rilevato dal sensore (ovvero un numero casuale che può essere zero oppure uno).

send_status(EventManager)->
  %% L'istruzione seguente permette di ottenere un seed univoco per generare il numero casuale.
  random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
  Status = (random:uniform(2))-1,
  gen_event:notify(EventManager, {send, Status, self()}),
  ok.

% --- GESTIONE DELLE CHIAMATE SINCRONE --- %

%% Intercetta qualsiasi richiesta sincrona bloccando la chiamata. Se viene eseguita una chiamata sincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_call(_Request, _From, _State) ->
  {stop, normal, "SENSORE FINESTRE: Chiamate sincrone non permesse", _State}.

% --- GESTIONE DELLE CHIAMATE ASINCRONE --- %

%% Il sensore può ricevere una chiamata asincrona dall'event handler per aggiornare l'intervallo di invio dei valori
%% descriventi lo stato delle finestre. Per fare ciò, viene recuperata ed aggiornata l'apposita componente dello stato
%% del processo. Lo stato risultante viene, infine, restituito.

handle_cast({update_interval_between_status,Value}, State) ->
  {Timer,_} = State,
  UpdatedData = {intervalBetweenStatus,Value},
  NewState = {Timer, UpdatedData},
  {noreply, NewState}.

% --- GESTIONE DEI MESSAGGI --- %

%% Nella seguente funzione vengono gestiti i messaggi che non previsti nel pattern matching delle handle_cast e delle
%% handle_call; in questo caso sono i messaggi legati al timer che il processo invia a se stesso. Quello che viene fatto
%% consiste nel cancellare il vecchio timer presente nello stato, eseguire la funzione per inviare il valore rilevato
%% all'event handler e ricreare il timer sulla base dell'intervallo tra ciascun invio definito, aggiornando infine lo stato
%% del processo.

handle_info({send,EventManager}, State) ->
  {OldTimer,Data} = State,
  {intervalBetweenStatus,Interval} = Data,
  erlang:cancel_timer(OldTimer),
  send_status(EventManager),
  Timer = erlang:send_after(Interval, self(), {send,EventManager}),
  NewState = {Timer,Data},
  {noreply, NewState}.
