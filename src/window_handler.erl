%% ---- MODULO TEMPERATURE_HANDLER --- %%

%% Questo modulo è l'implementazione concreta di uno degli handler associabili ad un event handler OTP, che ha, come scopo,
%% la gestione di tutti gli eventi legati ai sensori per la verifica dello stato delle finestre.

-module(window_handler).
-behaviour(gen_event).
-compile(export_all).

% --- FUNZIONI STANDARD DI GEN_EVENT --- %

%% Inizializza lo stato dell'handler, preparando le strutture dati per i valori ricevuti dai sensori, per gli identificatori
%% dei sensori, e per le medie calcolate dal processo apposito. Viene inoltre definita la dimensione massima della struttura
%% dati contenente lo storico delle medie stesse. I riferimenti a tali strutture vengono poi salvati nello stato.

init(ClientName) ->
  process_flag(trap_exit, true),
  io:format("Gestore di eventi di temperatura in esecuzione con identificatore: ~p~n", [self()]),
  Status = dict:new(),
  Sensors = [],
  Data = {{status, Status}, {sensors, Sensors}, {client,ClientName}},
  {ok, Data}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione. Per il momento, nessuna.

terminate(Reason, _State) ->
  io:format("Il gestore di eventi legati alle finestre con identificatore ~p e stato terminato per il motivo: ~p~n", [self(), Reason]),
  ok.

%% Gestione della modifica a runtime del codice.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% --- GESTIONE DEGLI EVENTI --- %

%% Gestione di un evento di registrazione di un sensore. Viene prevelata la struttura dati contenente gli identificatori
%% dei sensori ed essa viene aggiornata inserendo il nuovo identificatore giunto mediante notify. La struttura dati, infine,
%% viene aggiornata e reinserita nello stato.

handle_event({register, Value}, State) ->
  {_Dataslot_1, Dataslot_2, _Dataslot_3} = State,
  {sensors, Sensors} = Dataslot_2,
  UpdatedSensors = lists:append(Sensors, [{erlang:localtime(), Value}]),
  io:format("Nuovo sensore registrato presso l'event handler delle finestre con identificatore: ~p~n", [Value]),
  NewState = {_Dataslot_1, {sensors, UpdatedSensors}, _Dataslot_3},
  {ok, NewState};

%% Gestione di un evento di invio di un valore grezzo da parte di un sensore. Viene prevelata la struttura dati contenente i valori
%% istantanei di temperatura ed essa viene aggiornata inserendo il nuovo valore giunto mediante notify. La struttura dati, infine,
%% viene aggiornata e reinserita nello stato.

handle_event({send, Value, From}, State) ->
  {Dataslot_1, _Dataslot_2, _Dataslot_3} = State,
  {status, Status} = Dataslot_1,
  UpdatedStatus = dict:store(From, Value, Status),
  %%io:format("Stato della finestra ricevuto pari a: ~p~n", [Value]),
  %%io:format("Il valore è stato inviato dal sensore con identificatore: ~p~n", [From]),
  NewState = {{status, UpdatedStatus}, _Dataslot_2, _Dataslot_3},
  {ok, NewState}.

% --- GESTIONE DELLE CHIAMATE SINCRONE --- %

%% Intercetta qualsiasi richiesta sincrona bloccando la chiamata. Se viene eseguita una chiamata sincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_call(ask_for_status, State) ->
  {Dataslot_1, _Dataslot_2, _Dataslot_3} = State,
  {status,Status} = Dataslot_1,
  List = dict:to_list(Status),
  Sum = lists:foldl(fun({_S,V}, Sum) -> V + Sum end, 0, List),
  case Sum of
    N when N == length(List) ->
      {ok,all_windows_are_closed,State};
    _ ->
      {ok,dict:to_list(Status),State}
  end.

% --- GESTIONE DEI MESSAGGI RIMANENTI --- %

%% Non viene effettuata alcuna particolare gestione di eventuali messaggi non trattati con le funzioni precedenti.

handle_info(Message, State) ->
  io:format("Messaggio ricevuto: ~p~n", [Message]),
  {noreply, State}.


