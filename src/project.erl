%% ---- MODULO PROJECT --- %%

%% Questo modulo implementa l'application controller che consente di eseguire tutta l'applicazione con una sola chiamata
%% di funzione e rappresenta la base per la distribuzione dell'app stessa.

-module(project).
-behaviour(application).
-export([start/2, stop/1]).

%% ---- FUNZIONI STANDARD DI APPLICATION ---- %%

start(normal, _Args) ->
  project_supervisor:start().

stop(_State) ->
  ok.
