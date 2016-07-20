%% ---- MODULO TEMPERATURE_CALCULATOR ---- %%

%% Questo modulo modella il componente del sistema atto a svolgere tutti i calcoli necessari sui dati grezzi rilevati dai
%% sensori.

-module(temperature_calculator).
-compile(export_all).
-behaviour(gen_server).

% --- FUNZIONI STANDARD DI GEN_SERVER --- %

%% Lancia il componente legandolo all'event handler e registrandolo con il nome desiderato. Tali elementi corrispondono ai parametri passati.

start_link(EventManager, Name) ->
  {ok, _Pid} = gen_server:start_link({local, Name}, ?MODULE, EventManager, []).

%% Durante la fase di inizializzazione viene preparato lo stato necessario al componente (l'intervallo con cui inviare
%% le richieste di calcolo della media all'event handler) ed il timer per regolare tale invio di tali richieste.

init(EventManager) ->
  Interval = 5500,
  process_flag(trap_exit, true),
  {ok, LogSender} = file:open("../log/Log_Sender.txt", [append]),
  io:format(LogSender, "~p~p~n", ["CALCOLATORE TEMPERATURA: Processo in esecuzione con identificatore: ", self()]),
  io:format("CALCOLATORE TEMPERATURA: Processo in esecuzione con identificatore: ~p~n", [self()]),
  file:close(LogSender),
  subscribe(EventManager),
  Data = {intervalBetweenMeans, Interval},
  Timer = erlang:send_after(Interval, self(), mean),
  State = {Timer, Data, EventManager},
  {ok, State}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione. Per il momento, nessuna.

terminate(Reason, _State) ->
  {ok, LogSender} = file:open("../log/Log_Sender.txt", [append]),
  io:format(LogSender, "~p~p~p~p~n", ["CALCOLATORE TEMPERATURA: Il processo con identificatore ", self(), " e stato terminato per il motivo: ", Reason]),
  io:format("CALCOLATORE TEMPERATURA: Il processo con identificatore ~p e stato terminato per il motivo: ~p~n", [self(), Reason]),
  file:close(LogSender),
  ok.

%% Gestione della modifica a runtime del codice.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% --- FUNZIONI DI SUPPORTO ED EVENTUALE MESSAGGISTICA --- %

%% Operazione di registrazione presso l'event handler, che consiste nell'inviare a quest'ultimo il proprio identificatore,
%% in questo caso il nome.

subscribe(EventManager) ->
  gen_event:notify(EventManager, {register_calculator, self()}),
  ok.

%% Consente di inviare una richiesta per eseguire il calcolo della media all'event handler. Ottiene i dati raccolti
%% per mezzo di una funzione ausiliaria ed, una volta fatto ciò, la struttura dati viene trasformata in una lista
%% dalla quale si ottiene il valore finale di media, che viene restituito.

mean(EventManager) ->
  Values = ask_for_values(EventManager),
  case length(Values) of
    N when N > 0 ->
      AllValues = lists:foldl(fun({_, X}, Sum) -> Sum + X end, 0, Values),
      Mean = AllValues / length(Values),
      Mean;
    _ ->
      Default = 0,
      Default
  end.

%% Le seguenti funzioni sono funzioni di supporto necessarie per ottenere i dati immagazzinati nello stato dell'event handler
%% mediante chiamate sincrone.

ask_for_subscribers(EventManager) ->
  Sensors = gen_event:call(EventManager, temperature_handler, ask_for_subscribers),
  Sensors.

ask_for_values(EventManager) ->
  Temperature = gen_event:call(EventManager, temperature_handler, ask_for_values),
  Temperature.

ask_for_means(EventManager) ->
  Means = gen_event:call(EventManager, temperature_handler, ask_for_means),
  Means.

% --- GESTIONE DELLE CHIAMATE SINCRONE --- %

%% Intercetta qualsiasi richiesta sincrona bloccando la chiamata. Se viene eseguita una chiamata sincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_call(_Request, _From, _State) ->
  {stop, normal, "Chiamate sincrone non permesse", _State}.

% --- GESTIONE DELLE CHIAMATE ASINCRONE --- %

%% Intercetta una richiesta di modifica dell'intervallo con cui il processo invia richieste di calcolo del valor medio,
%% prelevato la tupla contenente tale intervallo dallo stato ed aggiornandone il valore.

handle_cast({update_interval_between_means, Value}, State) ->
  {Timer, _, EventManager} = State,
  UpdatedData = {intervalBetweenMeans, Value},
  NewState = {Timer, UpdatedData, EventManager},
  {noreply, NewState}.

% --- GESTIONE DEI MESSAGGI --- %

%% Nella seguente funzione vengono gestiti i messaggi che non previsti nel pattern matching delle handle_cast e delle
%% handle_call; in questo caso sono i messaggi legati al timer che il processo invia a se stesso. Quello che viene fatto
%% consiste nel cancellare il vecchio timer presente nello stato, eseguire la funzione per il calcolo della media, notificare
%% la media calcolata all'event handler per l'aggiornamento del suo stato  e successivamente ricreare il timer sulla base
%% dell'intervallo tra ciascun invio definito, aggiornando infine lo stato del processo.

handle_info(mean, State) ->
  {OldTimer, Data, EventManager} = State,
  {intervalBetweenMeans, Interval} = Data,
  erlang:cancel_timer(OldTimer),
  Mean = mean(EventManager),
  gen_event:notify(EventManager, {mean, Mean}),
  gen_event:notify(EventManager, erase_old_values),
  Timer = erlang:send_after(Interval, self(), mean),
  NewState = {Timer, Data, EventManager},
  {noreply, NewState}.