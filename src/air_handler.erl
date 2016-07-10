%% ---- MODULO AIR_HANDLER --- %%

%% Questo modulo è l'implementazione concreta di uno degli handler associabili ad un event handler OTP, che ha, come scopo,
%% la gestione di tutti gli eventi legati ai sensori per la gestione della climatizzazione.

-module(air_handler).
-behaviour(gen_event).
-compile(export_all).

% --- FUNZIONI STANDARD DI GEN_EVENT --- %

%% Inizializza lo stato dell'handler, preparando le strutture dati per i valori ricevuti dai sensori, per gli identificatori
%% dei sensori, e per le medie calcolate dal processo apposito. Viene inoltre definita la dimensione massima della struttura
%% dati contenente lo storico delle medie stesse. I riferimenti a tali strutture vengono poi salvati nello stato.

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
      visit_sensor_list(Sensors, turn_off)
  end,
  {ok, State};

handle_event({check_status, {mean, notset}, {windows, notset}}, State) ->
  io:format("ATTENZIONE: L'applicazione sender non ha ancora inviato dati utili. Verificare l'accensione.~n"),
  {ok, State}.

%% Funzione di supporto per l'esplorazione della lista contente tutti i sensori registrati presso l'event handler. Per ogni
%% sensore contenuto nella lista viene inviato il messaggio d'aggiornamento.

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


% --- GESTIONE DEI MESSAGGI RIMANENTI --- %

%% Non viene effettuata alcuna particolare gestione di eventuali messaggi non trattati con le funzioni precedenti.

handle_info(Message, State) ->
  io:format("GESTORE CLIMATIZZAZIONE: Messaggio ricevuto: ~p~n", [Message]),
  {noreply, State}.


