%% ---- MODULO AIR_SENSOR --- %%

%% Questo modulo permette di modellare un singolo sensore in grado azionare la climatizzazione in una stanza.

-module(air_sensor).
-compile(export_all).
-behaviour(gen_server).

% --- FUNZIONI STANDARD DI GEN_SERVER --- %

%% Lancia il sensore legandolo all'event handler e registrandolo con il nome desiderato. Tali elementi corrispondono ai parametri passati.

start_link(EventManager, Name) ->
  {ok, _Pid} = gen_server:start_link({local, Name}, ?MODULE, EventManager, []).

%% Durante la fase di inizializzazione viene eseguita la registrazione del sensore presso l'event handler,
%% viene preparato lo stato necessario al sensore (ovvero un atomo per rappresentare l'accensione o lo spegnimento
%% sensore stesso, ovvero del modulo dell'impianto di climatizzazione.

init(EventManager) ->
  process_flag(trap_exit, true),
  io:format("SENSORE CLIMATIZZAZIONE: Sensore in esecuzione con identificatore: ~p~n", [self()]),
  {ok, LogReceiver} = file:open("log/Log_Receiver.txt", [append]),
  io:format(LogReceiver, "~p~p~n", ["SENSORE CLIMATIZZAZIONE: Sensore in esecuzione con identificatore: ", self()]),
  file:close(LogReceiver),
  subscribe(EventManager),
  State = {turn_off},
  {ok, State}.

%% Operazioni di deinizializzazione da compiere in caso di terminazione. Per il momento, nessuna.

terminate(Reason, _State) ->
  io:format("SENSORE CLIMATIZZAZIONE: Il sensore con identificatore ~p e stato terminato per il motivo: ~p~n", [self(), Reason]),
  {ok, LogReceiver} = file:open("log/Log_Receiver.txt", [append]),
  io:format(LogReceiver, "~p~p~p~p~n", ["SENSORE CLIMATIZZAZIONE: Il sensore con identificatore ", self(), " e stato terminato per il motivo:", Reason]),
  file:close(LogReceiver),
  ok.

%% Gestione della modifica a runtime del codice.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

% --- FUNZIONI DI SUPPORTO ED EVENTUALE MESSAGGISTICA --- %

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

%% La seguente funzione consente di gestire un messaggio generato dall'event handler che permette al sensore di aggiornare il proprio
%% stato, preparandosi per l'accensione o lo spegnimento definitivo.

handle_cast({update_status, Value}, _State) ->
  case Value of
    turn_on ->
      io:format("SENSORE CLIMATIZZAZIONE: Il climatizzatore gestito da ~p viene acceso.~n", [self()]),
      {ok, LogReceiver} = file:open("log/Log_Receiver.txt", [append]),
      io:format(LogReceiver, "~p~p~p~n", ["SENSORE CLIMATIZZAZIONE: Il climatizzatore gestito da ", self(), " viene acceso"]),
      NewState = {turn_on},
      file:close(LogReceiver),
      {noreply, NewState};
    turn_off ->
      io:format("SENSORE CLIMATIZZAZIONE: Il climatizzatore gestito da ~p viene spento.~n", [self()]),
      {ok, LogReceiver} = file:open("log/Log_Receiver.txt", [append]),
      io:format(LogReceiver, "~p~p~p~n", ["SENSORE CLIMATIZZAZIONE: Il climatizzatore gestito da ", self(), " viene spento"]),
      NewState = {turn_off},
      file:close(LogReceiver),
      {noreply, NewState}
  end.
% --- GESTIONE DEI MESSAGGI RIMANENTI --- %

%% Non viene effettuata alcuna particolare gestione di eventuali messaggi non trattati con le funzioni precedenti.

handle_info(Message, State) ->
  io:format("SENSORE CLIMATIZZAZIONE: Messaggio ricevuto: ~p~n", [Message]),
  {ok, LogReceiver} = file:open("log/Log_Receiver.txt", [append]),
  io:format(LogReceiver, "~p~p~n", ["SENSORE CLIMATIZZAZIONE: Messaggio ricevuto: ", Message]),
  file:close(LogReceiver),
  {noreply, State}.

