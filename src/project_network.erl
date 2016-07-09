-module(project_network).
-compile(export_all).
-behaviour(gen_server).

-record(state, {mqttc, seq}).

start_link(Name, sender) ->
  {ok, _Pid} = gen_server:start_link({global,Name},?MODULE,sender,[]);

start_link(Name, receiver) ->
  {ok, _Pid} = gen_server:start_link({global,Name},?MODULE,receiver,[]).

terminate(Reason, _State) ->
  io:format("Il client MQTT con identificatore ~p e stato terminato per il motivo: ~p~n", [self(),Reason]),
  ok.

%% Gestione della modifica a runtime del codice.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

init(sender) ->
  Interval = 4000,
  ClientName = io_lib:format("~p",[self()]),
  Result = "Client_" ++ lists:flatten(ClientName),
  {ok, Client} = emqttc:start_link([{host, host()}, {client_id, Result}, {logger, info}, {reconnect, {2, 50, 3}}]),
  io:format("Il client con nome ~p e identificatore ~p è stato lanciato come PUBLISHER per i topic: temperature, window~n", [Result,self()]),
  Timer = erlang:send_after(Interval, self(), ask_for_data),
  State = {#state{mqttc = Client, seq = 1}, Timer, Interval, means, windows},
  {ok, State};

init(receiver) ->
  ClientName = io_lib:format("~p",[self()]),
  Result = "Client_" ++ lists:flatten(ClientName),
  {ok, Client} = emqttc:start_link([{host, host()}, {client_id, Result}, {logger, info}, {reconnect, {2, 50, 5}}]),
  emqttc:subscribe(Client, <<"temperature">>),
  emqttc:subscribe(Client, <<"window">>),
  io:format("Il client con nome ~p e identificatore ~p è stato lanciato come SUBSCRIBER per i topic: temperature, window~n", [Result,self()]),
  State = {#state{mqttc = Client, seq = 1}},
  {ok, State}.

host()->
  "localhost".

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

handle_info(ask_for_data, State) ->
  {Info,OldTimer,Interval, _Means, _Status} = State,
  erlang:cancel_timer(OldTimer),
  Means = gen_event:call(temperature_event_handler, temperature_handler, ask_for_means),
  Status = gen_event:call(window_event_handler, window_handler, ask_for_status),
  case Status of
    all_windows_are_closed ->
      StatusMessage = <<"all_windows_are_closed">>;
    _ ->
      StatusMessage = <<"there_are_open_windows">>
  end,
  RecentMeans = lists:reverse(Means),
  [MostRecentMean|_] = RecentMeans,
  {_MeanTime,MeanValue} = MostRecentMean,
  RoundMean = round(MeanValue),
  emqttc:publish(Info#state.mqttc, <<"temperature">>, <<RoundMean>>),
  emqttc:publish(Info#state.mqttc, <<"window">>, StatusMessage),
  Timer = erlang:send_after(Interval, self(), ask_for_data),
  NewState = {Info,Timer,Interval, Means, Status},
  {noreply, NewState}.



