%% ---- MODULO AIR_HANDLER --- %%

%% Questo modulo è l'implementazione concreta di uno degli handler associabili ad un event handler OTP, che ha, come scopo,
%% la gestione di tutti gli eventi legati ai sensori per la gestione della climatizzazione.

-module(air_handler).
-behaviour(gen_event).
-compile(export_all).

% --- FUNZIONI STANDARD DI GEN_EVENT --- %

%% Inizializza lo stato dell'handler, preparando la struttura dati per immagazzinare gli identificatori dei sensori.
%% Tale struttura viene poi salvata nello stato.

init([]) ->
  process_flag(trap_exit, true),
  io:format("GESTORE CLIMATIZZAZIONE: Gestore di eventi in esecuzione con identificatore: ~p~n", [self()]),
  Sensors = [],
  Data = {{sensors, Sensors}},
  {ok, Data}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione. Per il momento, nessuna.

terminate(Reason, _State) ->
  io:format("GESTORE CLIMATIZZAZIONE: Il gestore di eventi con identificatore ~p e stato terminato per il motivo: ~p~n", [self(), Reason]),
  ok.

%% Gestione della modifica a runtime del codice.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% --- GESTIONE DEGLI EVENTI --- %

%% Gestione di un evento di registrazione di un sensore. Viene prevelata la struttura dati contenente gli identificatori
%% dei sensori ed essa viene aggiornata inserendo il nuovo identificatore giunto mediante notify. La struttura dati, infine,
%% viene aggiornata e reinserita nello stato.

handle_event({register, Value}, State) ->
  {{sensors, Sensors}} = State,
  UpdatedSensors = lists:append(Sensors, [{erlang:localtime(), Value}]),
  io:format("GESTORE CLIMATIZZAZIONE: Nuovo sensore registrato presso l'event handler con identificatore: ~p~n", [Value]),
  NewState = {{sensors, UpdatedSensors}},
  {ok, NewState};

%% Gestione di un evento di ricezione di una coppia media più recente/stato delle finestre inviata dal client in modalità receiver.
%% Si tratta della funzione finale che conclude un ciclo di business del sistema. Quello che viene svolto consiste nel confrontare
%% la coppia di valori ottenuta. Il climatizzatore va messo in funzione sono la media è superiore ad una certa costante e se le finestre
%% (tutte) risultano chiuse. In qualsiasi altro caso, il climatizzatore va spento. Dopo aver eseguito la verifica, vengono prelevati
%% gli identificatori di ogni sensore e viene inviato un messaggio di accensione o spegnimento, a seconda dell'esito del confronto,
%% a ciascuno di essi, in quanto ogni sensore rappresenta un modulo presente in una stanza dell'impianto di climatizzazione.

handle_event({check_status, {mean, Value}, {windows, Status}}, State) ->
  {{sensors, Sensors}} = State,
  if
    Value > 24 andalso Status == all_windows_are_closed ->
      io:format("GESTORE CLIMATIZZAZIONE: La temperatura è superiore a 24 gradi e le finestre sono chiuse. Si procede con l'accensione del climatizzatore.~n"),
      visit_sensor_list(Sensors, turn_on);
    Value > 24 andalso Status == there_are_open_windows ->
      io:format("GESTORE CLIMATIZZAZIONE: La temperatura è superiore a 24 gradi e le finestre sono aperte. Si procede con la chiusura del climatizzatore.~n"),
      visit_sensor_list(Sensors, turn_off);
    Value =< 24 andalso Status == there_are_open_windows ->
      io:format("GESTORE CLIMATIZZAZIONE: La temperatura è inferiore a 24 gradi e le finestre sono aperte. Si procede con la chiusura del climatizzatore.~n"),
      visit_sensor_list(Sensors, turn_off);
    Value =< 24 andalso Status == all_windows_are_closed ->
      io:format("GESTORE CLIMATIZZAZIONE: La temperatura è inferiore a 24 gradi e le finestre sono chiuse. Si procede con la chiusura del climatizzatore.~n"),
      visit_sensor_list(Sensors, turn_off);
    true ->
      io:format("GESTORE CLIMATIZZAZIONE: Il sender non è ancora attivo.~n")
  end,
  {ok, State};

% --- FUNZIONI DI SUPPORTO ED EVENTUALE MESSAGGISTICA --- %

%% Funzione di supporto per l'esplorazione della lista contente tutti i sensori registrati presso l'event handler. Per ogni
%% sensore contenuto nella lista viene inviato il messaggio d'accensione o spegnimento, a seconda del caso..

visit_sensor_list([], _Value) -> finished;
visit_sensor_list([S | C], Value) ->
  visit_sensor_list(C, Value),
  {_, Sensor} = S,
  case Value of
    turn_on -> gen_server:cast(Sensor, {update_status, turn_on});
    turn_off -> gen_server:cast(Sensor, {update_status, turn_off})
  end.

% --- GESTIONE DELLE CHIAMATE SINCRONE --- %

%% Intercetta qualsiasi richiesta sincrona bloccando la chiamata. Se viene eseguita una chiamata sincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_call(_Request, _State) ->
  {stop, normal, "GESTORE CLIMATIZZAZIONE: Chiamate sincrone non permesse", _State}.

% --- GESTIONE DEI MESSAGGI --- %

%% Non viene effettuata alcuna particolare gestione di eventuali messaggi non trattati con le funzioni precedenti.

handle_info(Message, State) ->
  io:format("GESTORE CLIMATIZZAZIONE: Messaggio ricevuto: ~p~n", [Message]),
  {noreply, State}.


