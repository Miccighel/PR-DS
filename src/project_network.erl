-module(project_network).
-compile(export_all).
-behaviour(gen_server).

-record(state, {mqttc, seq}).

start_link() ->
  {ok, _Pid} = gen_server:start_link({local,network},?MODULE, [],[]).

terminate(Reason, _State) ->
  io:format("Il client MQTT con identificatore ~p e stato terminato per il motivo: ~p~n", [self(),Reason]),
  ok.

init(_Args) ->
  {ok, C} = emqttc:start_link([{host, "localhost"},
    {client_id, <<"simpleClient">>},
    {logger, info}]),
  emqttc:subscribe(C, <<"TopicA">>, qos1),
  self() ! publish,
  {ok, #state{mqttc = C, seq = 1}}.

%% Receive Publish Message from TopicA...
handle_info({publish, Topic, Payload}, State) ->
  io:format("Message from ~s: ~p~n", [Topic, Payload]),
  {noreply, State};

%% Client connected
handle_info({mqttc, C, connected}, State = #state{mqttc = C}) ->
  io:format("Client ~p is connected~n", [C]),
  emqttc:subscribe(C, <<"TopicA">>, 1),
  emqttc:subscribe(C, <<"TopicB">>, 2),
  self() ! publish,
  {noreply, State};

%% Client disconnected
handle_info({mqttc, C, disconnected}, State = #state{mqttc = C}) ->
  io:format("Client ~p is disconnected~n", [C]),
  {noreply, State}.