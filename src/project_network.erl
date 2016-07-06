-module(project_network).
-compile(export_all).
-behaviour(gen_server).

-record(state, {mqttc, seq}).

start_link(Name) ->
  {ok, _Pid} = gen_server:start_link({global,Name},?MODULE, [],[]).

terminate(Reason, _State) ->
  io:format("Il client MQTT con identificatore ~p e stato terminato per il motivo: ~p~n", [self(),Reason]),
  ok.

init(_) ->
  Interval = 13000,
  {ok, Client} = emqttc:start_link([{host, "localhost"}, {client_id, <<"Client">>}, {logger, info}]),
  Timer = erlang:send_after(Interval, self(), ask_for_means),
  State = {#state{mqttc = Client, seq = 1}, Timer, Interval, means},
  {ok, State}.

%% Client connected
handle_info({mqttc, Client, connected}, State) ->
  io:format("Il client MQTT con identificatore ~p è connesso al broker~n", [Client]),
  {noreply, State};

%% Client disconnected
handle_info({mqttc, Client, disconnected}, State) ->
  io:format("Il client MQTT con identificatore ~p non è più connesso al broker~n", [Client]),
  {noreply, State};

handle_info(ask_for_means, State) ->
  {Info,OldTimer,Interval, _Means} = State,
  erlang:cancel_timer(OldTimer),
  NewMeans = gen_event:call(event_handler, temperature_handler, ask_for_means),
  io:format("Medie ricevute dall'event ~n"),
  Timer = erlang:send_after(Interval, self(), ask_for_means),
  NewState = {Info,Timer,Interval, NewMeans},
  {noreply, NewState}.
