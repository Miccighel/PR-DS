%% ---- MODULO PROJECT_NETWORK --- %%

%% Modulo che modella il componente del sistema che si pone tra i moduli di temperatura e finestre e broker MQTT per l'operazione
%% di publish e tra il broker MQTT ed il modulo di climatizzazione per l'operazione di subscribe. Può essere avviato in un due diverse
%% modalità a seconda della configurazione statica definita nel file di configurazione.

-module(project_network).
-behaviour(gen_server).
-compile(export_all).

%% Record contenente le informazioni sul client

-record(state, {mqttc, seq}).

% --- FUNZIONI STANDARD DI GEN_SERVER --- %

%% Le seguenti funzioni si occupano di avviare il client in modalità sender o receiver, lanciando le apposite funzioni di inizializzazione.

start_link(Name, sender) ->
  {ok, _Pid} = gen_server:start_link({global, Name}, ?MODULE, sender, []);

start_link(Name, receiver) ->
  {ok, _Pid} = gen_server:start_link({global, Name}, ?MODULE, receiver, []).

%% Funzione di inizializzazione per l'avvio del client in modalità sender. Nello stato vengono memorizzate le informazioni relative
%% al client e gli elementi per eseguire l'invio temporizzato di un messaggio di richiesta dati ai moduli di temperatura e finestre.

init(sender) ->
  Interval = interval(),
  ClientName = io_lib:format("~p", [self()]),
  Result = "Client_" ++ lists:flatten(ClientName),
  {ok, Client} = emqttc:start_link([{host, host()}, {client_id, Result}, {logger, info}, {reconnect, {2, 50, 3}}]),
  io:format("CLIENT DI RETE - SENDER: Il client con nome ~p e identificatore ~p è stato lanciato come PUBLISHER per i topic: temperature, window~n", [Result, self()]),
  Timer = erlang:send_after(Interval, self(), ask_for_data),
  State = {#state{mqttc = Client, seq = 1}, Timer, Interval},
  {ok, State};

%% Funzione di inizializzazione per l'avvio del client in modalità receiver. Nello stato vengono memorizzate le informazioni relative
%% al client e viene preparato lo spazio per salvare di volta in volta la media e lo stato delle finestre ricevuti attraverso il broker
%% e viene svolta l'operazione di subscribe ai topic a cui fanno riferimento tali valori.

init(receiver) ->
  Interval = interval(),
  ClientName = io_lib:format("~p", [self()]),
  Result = "Client_" ++ lists:flatten(ClientName),
  {ok, Client} = emqttc:start_link([{host, host()}, {client_id, Result}, {logger, info}, {reconnect, {2, 50, 5}}]),
  %% Viene eseguita l'operazione di subscribe ai topic
  emqttc:subscribe(Client, <<"temperature">>),
  emqttc:subscribe(Client, <<"window">>),
  io:format("CLIENT DI RETE - RECEIVER: Il client con nome ~p e identificatore ~p è stato lanciato come SUBSCRIBER per i topic: temperature, window~n", [Result, self()]),
  Timer = erlang:send_after(Interval, self(), load_state),
  State = {#state{mqttc = Client, seq = 1}, Timer, Interval, {mean, notset}, {windows, notset}},
  {ok, State}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione. Per il momento, nessuna.

terminate(Reason, _State) ->
  io:format("CLIENT DI RETE: Il client MQTT con identificatore ~p e stato terminato per il motivo: ~p~n", [self(), Reason]),
  ok.

%% Gestione della modifica a runtime del codice.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% --- FUNZIONI DI SUPPORTO ED EVENTUALE MESSAGGISTICA --- %

%% Funzione che definisce l'intervallo entro cui inviare una richiesta dati ai moduli di temperatura e finestre quando il client
%% viene avviato in modalità sender e ed entro cui inviare i dati al modulo della climatizzazione quando il client viene avviato
%% come receiver.

interval()->
  6000.

%% Funzione che definisce l'indirizzo sul quale il broker MQTT è in ascolto.

host() ->
  "localhost".

%% Funzione che viene utilizzata per inviare all'event handler la coppia media/stato delle fineste ottenuta attaverso il broker MQTT
%% quando il client viene avviato in modalità receiver.

send_data(Mean,Windows) ->
  gen_event:notify(air_event_handler, {check_status, Mean, Windows}).

% --- GESTIONE DELLE CHIAMATE SINCRONE --- %

%% Intercetta qualsiasi richiesta sincrona bloccando la chiamata. Se viene eseguita una chiamata sincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_call(_Request, _From, _State) ->
  {stop, normal, "CLIENT DI RETE - GENERALE: Chiamate sincrone non permesse", _State}.

% --- GESTIONE DELLE CHIAMATE ASINCRONE --- %

%% Il sensore può ricevere una chiamata asincrona dal monitor di rete per aggiornare l'intervallo di invio dei valori
%% destinati al modulo di climatizzazione o alla pubblicazione sul broker, a seconda della modalità in cui viene avviato.
%% Per fare ciò, viene recuperata ed aggiornata l'apposita componente dello stato del processo. Lo stato risultante
%% viene, infine, restituito.

handle_cast(Value, State) ->
  Interval = Value,
  NewState = setelement(2, State, Interval),
  {noreply, NewState}.

% --- GESTIONE DEI MESSAGGI - GENERALE --- %

%% Funzione che stampa un messaggio di conferma quando la connessione al broker avviene con successo.

handle_info({mqttc, Client, connected}, State) ->
  io:format("CLIENT DI RETE - GENERALE: Il client MQTT con identificatore ~p è connesso al broker~n", [Client]),
  {noreply, State};

%% Funzione che stampa un messaggio di conferma quando la disconnessione dal broker avviene con successo.

handle_info({mqttc, Client, disconnected}, State) ->
  io:format("CLIENT DI RETE - GENERALE: Il client MQTT con identificatore ~p non è più connesso al broker~n", [Client]),
  {noreply, State};

% --- GESTIONE DEI MESSAGGI - SENDER --- %

%% Funzione che gestisce la richiesta dei dati da mandare in publish sul broker MQTT ai moduli di temperatura e finestre,
%% quando il client viene avviato come sender.

handle_info(ask_for_data, State) ->
  {Info, OldTimer, Interval} = State,
  erlang:cancel_timer(OldTimer),
  %% I dati calcolati dai moduli vengono prelevati
  Means = gen_event:call(temperature_event_handler, temperature_handler, ask_for_means),
  Windows = gen_event:call(window_event_handler, window_handler, ask_for_status),
  case Windows of
    %% Il messaggio viene convertito a livello di bit, come richiesto al broker
    all_windows_are_closed ->
      StatusMessage = <<"all_windows_are_closed">>;
    _ ->
      StatusMessage = <<"there_are_open_windows">>
  end,
  %% Viene prelevata la media più recente, nel caso ci fossero ritardi nella consegna dei messaggi da parte degli altri due moduli,
  %% in quanto nella situazione ideale si instaura un modello produttore consumatore dove nel modulo della temperatura viene immagazzinato sempre
  %% un solo valore di media che viene consumato dal client mentre il modulo produce quello successivo.
  RecentMeans = lists:reverse(Means),
  [MostRecentMean | _] = RecentMeans,
  {_MeanTime, MeanValue} = MostRecentMean,
  RoundMean = round(MeanValue),
  %% I valori vengono finalmente pubblicati sul broker
  emqttc:publish(Info#state.mqttc, <<"temperature">>, <<RoundMean>>),
  emqttc:publish(Info#state.mqttc, <<"window">>, StatusMessage),
  Timer = erlang:send_after(Interval, self(), ask_for_data),
  NewState = {Info, Timer, Interval},
  {noreply, NewState};

% --- GESTIONE DEI MESSAGGI - RECEIVER --- %

%% Funzione che gestisce la ricezione di un messaggio relativo al topic descrivente la temperatura contenente la media,
%% a seguito di un'operazione di subscribe, quando il client viene avviato in modalità receiver.

handle_info({publish, <<"temperature">>, Value}, State) ->
  %% Il valore ricevuto viene convertito nella rappresentazione come intero
  [ParsedValue] = (binary_to_list(Value)),
  io:format("CLIENT DI RETE - RECEIVER: Messaggio ricevuto per il topic ~s: ~p~n", [temperature, ParsedValue]),
  {Info, _Timer, _Interval, _Mean, Status} = State,
  %% Il valore viene memorizzto nella componente apposita dello stato.
  NewMean = {mean, ParsedValue},
  NewState = {Info, _Timer, _Interval, NewMean, Status},
  {noreply, NewState};

%% Funzione che gestisce la ricezione di un messaggio relativo al topic descrivente lo stato delle finestre contenente
%% un messaggio descrivente lo stato stesso, a seguito di un'operazione di subscribe, quando il client viene avviato in
%% modalità receiver.

handle_info({publish, <<"window">>, Value}, State) ->
  %% Il valore ricevuto con la sintassi a livello di bit viene trasformato in un atomo che descrive lo stato delle finestre
  case Value of
    <<"there_are_open_windows">> -> ParsedValue = there_are_open_windows;
    <<"all_windows_are_closed">> -> ParsedValue = all_windows_are_closed
  end,
  io:format("CLIENT DI RETE - RECEIVER: Messaggio ricevuto per il topic ~s: ~p~n", [window, ParsedValue]),
  {_Info, _Timer, _Interval, _Mean, _Windows} = State,
  %% Il valore viene memorizzto nella componente apposita dello stato.
  NewWindows = {windows, ParsedValue},
  NewState = {_Info, _Timer, _Interval,  _Mean, NewWindows},
  {noreply, NewState};

%% Funzione che gestisce il caricamento delle coppie {media più recente / stato delle fineste più recente} nel modulo dedicato
%% alla climatizzazione.

handle_info(load_state, State) ->
  {Info, OldTimer, Interval, Mean, Windows} = State,
  erlang:cancel_timer(OldTimer),
  %% Si sfrutta l'apposita funzione di supporto per inviare il messaggio
  send_data(Mean,Windows),
  Timer = erlang:send_after(Interval, self(), load_state),
  NewState = {Info, Timer, Interval, Mean, Windows},
  {noreply, NewState}.


