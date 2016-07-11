%% ---- MODULO TEMPERATURE_HANDLER ---- %%

%% Questo modulo è l'implementazione concreta di uno degli handler associabili ad un event handler OTP, che ha, come scopo,
%% la gestione di tutti gli eventi legati ai sensori per la misurazione della temperatura.

-module(temperature_handler).
-behaviour(gen_event).
-compile(export_all).

% --- FUNZIONI STANDARD DI GEN_EVENT --- %

%% Inizializza lo stato dell'handler, preparando le strutture dati per i valori ricevuti dai sensori, per gli identificatori
%% dei sensori, e per le medie calcolate dal processo apposito. Viene inoltre definito il nome del client in modalità sender
%% a cui inviare i dati su richiesta. I riferimenti a tali strutture vengono poi salvati nello stato.

init(ClientName) ->
  process_flag(trap_exit, true),
  io:format("GESTORE TEMPERATURA: Gestore di eventi in esecuzione con identificatore: ~p~n", [self()]),
  Temperature = [],
  Sensors = [],
  Means = [],
  Data = {{temperature, Temperature}, {sensors, Sensors}, {means, Means}, {bufferSize, 3000}, {calculator, notset}, {client,ClientName}},
  {ok, Data}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione. Per il momento, nessuna.

terminate(Reason, _State) ->
  io:format("GESTORE TEMPERATURA: Il gestore di eventi con identificatore ~p e stato terminato per il motivo: ~p~n", [self(), Reason]),
  ok.

%% Gestione della modifica a runtime del codice.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% --- GESTIONE DEGLI EVENTI --- %

%% Gestione di un evento di registrazione di un sensore. Viene prevelata la struttura dati contenente gli identificatori
%% dei sensori ed essa viene aggiornata inserendo il nuovo identificatore giunto mediante notify. La struttura dati, infine,
%% viene aggiornata e reinserita nello stato.

handle_event({register, Value}, State) ->
  {_Dataslot_1, Dataslot_2, _Dataslot_3, _Dataslot_4, _Dataslot_5, _Dataslot_6} = State,
  {sensors, Sensors} = Dataslot_2,
  UpdatedSensors = lists:append(Sensors, [{erlang:localtime(), Value}]),
  io:format("GESTORE TEMPERATURA: Nuovo sensore registrato presso l'event handler con identificatore: ~p~n", [Value]),
  NewState = {_Dataslot_1, {sensors, UpdatedSensors}, _Dataslot_3, _Dataslot_4, _Dataslot_5, _Dataslot_6},
  {ok, NewState};

%% Gestione di un evento di registrazione del componente dedicato ai calcoli. Viene prevelata la tupla contenente l'identificatore
%% della componente (potrebbe esistere già, e ne deve esistere al massimo una) ed essa viene aggiornata inserendo il
%% nuovo identificatore giunto mediante notify. La tupla, infine, viene reinserita nello stato.

handle_event({register_calculator, Value}, State) ->
  {_Dataslot_1, _Dataslot_2, _Dataslot_3, _Dataslot_4, Dataslot_5, _Dataslot_6} = State,
  {calculator, _} = Dataslot_5,
  UpdatedCalculator = {calculator, Value},
  io:format("GESTORE TEMPERATURA: Nuovo processo dedicato ai calcoli registrato presso l'event handler con identificatore: ~p~n", [Value]),
  NewState = {_Dataslot_1, _Dataslot_2, _Dataslot_3, _Dataslot_4, UpdatedCalculator, _Dataslot_6},
  {ok, NewState};

%% Gestione di un evento di invio di un valore grezzo da parte di un sensore. Viene prevelata la struttura dati contenente i valori
%% istantanei di temperatura ed essa viene aggiornata inserendo il nuovo valore giunto mediante notify. La struttura dati, infine,
%% viene aggiornata e reinserita nello stato.

handle_event({send, Value, From}, State) ->
  {Dataslot_1, _Dataslot_2, _Dataslot_3, _Dataslot_4, _Dataslot_5, _Dataslot_6} = State,
  {temperature, Temperature} = Dataslot_1,
  UpdatedTemperature = lists:append(Temperature, [{erlang:localtime(), Value}]),
  io:format("GESTORE TEMPERATURA: Valore ricevuto pari a: ~p gradi~n", [Value]),
  io:format("GESTORE TEMPERATURA: Il valore è stato inviato dal sensore con identificatore: ~p~n", [From]),
  NewState = {{temperature, UpdatedTemperature}, _Dataslot_2, _Dataslot_3, _Dataslot_4, _Dataslot_5, _Dataslot_6},
  {ok, NewState};

%% Gestione di un evento di calcolo della media notificato dal processo atto ai calcoli. Viene prevelata la struttura dati
%% contente i valori di media calcolati precedentemente (immaganizzati per avere lo storico di un preciso intervallo
%% temporale) e viene eseguito un controllo sulla sua dimensione. Se tale dimensione è maggiore del massimo consentito,
%% la struttura dati viene resettata, altrimenti viene aggiornata inserendo il nuovo valore di media calcolato.

handle_event({mean, Value}, State) ->
  {Dataslot_1, Dataslot_2, Dataslot_3, Dataslot_4, Dataslot_5, _Dataslot_6} = State,
  {means, Means} = Dataslot_3,
  {bufferSize, BufferSize} = Dataslot_4,
  case length(Means) of
    N when N > BufferSize ->
      UpdatedMeans = [],
      io:format("GESTORE TEMPERATURA: Il buffer per le medie è stato resettato~n"),
      NewState = {Dataslot_1, Dataslot_2, {means, UpdatedMeans}, Dataslot_4, Dataslot_5, _Dataslot_6},
      {ok, NewState};
    _ ->
      UpdatedMeans = lists:append(Means, [{erlang:localtime(), Value}]),
      io:format("GESTORE TEMPERATURA: valore medio ricevuto pari a: ~p gradi~n", [Value]),
      NewState = {Dataslot_1, Dataslot_2, {means, UpdatedMeans}, Dataslot_4, Dataslot_5, _Dataslot_6},
      {ok, NewState}
  end;

%% Gestione di un evento di richiesta di modifica nella dimensione del buffer dedicato all'immagazzinamento delle medie
%% calcolate, originato dal monitor di rete. Si tratta di salvare nello stato del componente la nuova dimensione concessa
%% per il buffer e restituire il nuovo stato così ottenuto.

handle_event({update_buffer_size, Value}, State) ->
  {_Dataslot_1, _Dataslot_2, _Dataslot_3, _Dataslot_4, _Dataslot_5, _Dataslot_6} = State,
  UpdatedBufferSize = {bufferSize, Value},
  NewState = {_Dataslot_1, _Dataslot_2, _Dataslot_3, UpdatedBufferSize, _Dataslot_5, _Dataslot_6},
  {ok, NewState};

handle_event({update_interval_between_means, Value}, State) ->
  {_Dataslot_1, _Dataslot_2, _Dataslot_3, _Dataslot_4, Dataslot_5, _Dataslot_6} = State,
  {calculator, Calculator} = Dataslot_5,
  gen_server:cast(Calculator, {update_interval_between_means, Value}),
  {ok, State};

handle_event({update_interval_between_values, Value}, State) ->
  {_Dataslot_1, Dataslot_2, _Dataslot_3, _Dataslot_4, _Dataslot_5, _Dataslot_6} = State,
  {sensors, Sensors} = Dataslot_2,
  visit_sensor_list(Sensors, Value),
  {ok, State};

%% Gestione di un evento di richiesta di cancellazione del buffer dedicato all'immagazzinamento dei valori istantanei di
%% temperatura rilevati, generati dal processo dedicato ai calcoli. Quello che viene fatto, semplicemente, consiste nel
%% prelevare l'apposita struttura dati dallo stato del processo per poi resettarla, salvandola nuovamente nello stato stesso.

handle_event(erase_old_values, State) ->
  {_Dataslot_1, Dataslot_2, Dataslot_3, _Dataslot_4, _Dataslot_5, _Dataslot_6} = State,
  NewTemperature = [],
  NewState = {{temperature, NewTemperature}, Dataslot_2, Dataslot_3, _Dataslot_4, _Dataslot_5, _Dataslot_6},
  {ok, NewState}.

% --- FUNZIONI DI SUPPORTO ED EVENTUALE MESSAGGISTICA --- %

%% Funzione di supporto per l'esplorazione della lista contente tutti i sensori registrati presso l'event handler. Per ogni
%% sensore contenuto nella lista viene inviato il messaggio d'aggiornamento.

visit_sensor_list([], _Value) -> finished;
visit_sensor_list([S | C], Value) ->
  visit_sensor_list(C, Value),
  {_, Sensor} = S,
  gen_server:cast(Sensor, {update_interval_between_values, Value}).

% --- GESTIONE DELLE CHIAMATE SINCRONE --- %

%% Le chiamate seguenti servono a restituire le tre strutture dati che compongono lo stato di questo particolare handler
%% (valori instantanei, identificatori, medie)

handle_call(ask_for_values, State) ->
  {{temperature, Temperature}, _Dataslot_2, _Dataslot_3, _Dataslot_4, _Dataslot_5, _Dataslot_6} = State,
  {ok, Temperature, State};

handle_call(ask_for_subscribers, State) ->
  {_Dataslot_1, {sensors, Sensors}, _Dataslot_3, _Dataslot_4, _Dataslot_5, _Dataslot_6} = State,
  {ok, Sensors, State};

handle_call(ask_for_means, State) ->
  {_Dataslot_1, _Dataslot_2, {means, Means}, _Dataslot_4, _Dataslot_5, _Dataslot_6} = State,
  {ok, Means, State}.

% --- GESTIONE DEI MESSAGGI --- %

%% Non viene effettuata alcuna particolare gestione di eventuali messaggi non trattati con le funzioni precedenti.

handle_info(Message, State) ->
  io:format("GESTORE TEMPERATURA: Messaggio ricevuto: ~p~n", [Message]),
  {noreply, State}.


