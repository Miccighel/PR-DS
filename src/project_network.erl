-module(project_network).
-compile(export_all).
-behaviour(gen_server).

-record(state, {mqttc, seq}).

start_link(SenderName) ->
  {ok, _Pid} = gen_server:start_link({global,SenderName},?MODULE, [],[]).

terminate(Reason, _State) ->
  io:format("Il client MQTT con identificatore ~p e stato terminato per il motivo: ~p~n", [self(),Reason]),
  ok.

init(_) ->
  {ok, Client} = emqttc:start_link([{host, "localhost"}, {client_id, <<"Client">>}, {logger, info}]),
  {ok, #state{mqttc = Client, seq = 1}}.

%% Client connected
handle_info({mqttc, Client, connected}, State = #state{mqttc = Client}) ->
  io:format("Il client MQTT con identificatore ~p è connesso al broker~n", [Client]),
  {noreply, State};

%% Client disconnected
handle_info({mqttc, Client, disconnected}, State = #state{mqttc = Client}) ->
  io:format("Il client MQTT con identificatore ~p non è più connesso al broker~n", [Client]),
  {noreply, State}.