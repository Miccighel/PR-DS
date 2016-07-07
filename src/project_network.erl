-module(project_network).
-compile(export_all).
-behaviour(gen_server).

-record(state, {mqttc, seq}).

start_link(Name) ->
  {ok, _Pid} = gen_server:start_link({global,Name},?MODULE,[],[]).

terminate(Reason, _State) ->
  io:format("Il client MQTT con identificatore ~p e stato terminato per il motivo: ~p~n", [self(),Reason]),
  ok.

%% Gestione della modifica a runtime del codice.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

init([]) ->
  Interval = 11000,
  ClientName = io_lib:format("~p",[self()]),
  Result = "Client_" ++ lists:flatten(ClientName),
  {ok, Client} = emqttc:start_link([{host, "localhost"}, {client_id, Result}, {logger, info}]),
  emqttc:subscribe(Client, <<"temperature">>),
  io:format("Il client con nome ~p e identificatore ~p ha eseguito l'operazione di subscribe per il topic temperature~n", [Result,self()]),
  Timer = erlang:send_after(Interval, self(), ask_for_means),
  State = {#state{mqttc = Client, seq = 1}, Timer, Interval, means},
  {ok, State}.

% --- GESTIONE DELLE CHIAMATE SINCRONE --- %

%% Intercetta qualsiasi richiesta sincrona bloccando la chiamata. Se viene eseguita una chiamata sincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_call(_Request, _From, _State) ->
  {stop, normal, "Chiamate sincrone non permesse", _State}.

% --- GESTIONE DELLE CHIAMATE ASINCRONE --- %

%% Intercetta qualsiasi richiesta asincrona bloccando la chiamata. Se viene eseguita una chiamata sincrona
%% al componente, infatti, ci si trova in una situazione d'errore.

handle_cast(_Request, _State) ->
  {stop, normal, "Chiamate asincrone non permesse", _State}.

handle_info({mqttc, Client, connected}, State) ->
  io:format("Il client MQTT con identificatore ~p è connesso al broker~n", [Client]),
  {noreply, State};

handle_info({mqttc, Client, disconnected}, State) ->
  io:format("Il client MQTT con identificatore ~p non è più connesso al broker~n", [Client]),
  {noreply, State};

handle_info({publish, Topic, Payload}, State) ->
  io:format("Message from ~s: ~p~n", [Topic, Payload]),
  {noreply, State};

handle_info(ask_for_means, State) ->
  {Info,OldTimer,Interval, _Means} = State,
  erlang:cancel_timer(OldTimer),
  NewMeans = gen_event:call(event_handler, temperature_handler, ask_for_means),
  io:format("Medie ricevute dall'event handler~n"),
  emqttc:publish(Info#state.mqttc, <<"temperature">>, <<"POKEMON GO">>),
  Timer = erlang:send_after(Interval, self(), ask_for_means),
  NewState = {Info,Timer,Interval, NewMeans},
  {noreply, NewState}.
