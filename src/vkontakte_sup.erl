-module(vkontakte_sup).
-author('Max Lapshin <max@maxidoors.ru>').
-version(1.0).

-behaviour(supervisor).

-export([init/1, start_link/0]).

-export([start_request/2]).

%%--------------------------------------------------------------------
%% @spec () -> any()
%% @doc A startup function for whole supervisor. Started by application
%% @end 
%%--------------------------------------------------------------------
-spec start_link() -> {'error',_} | {'ok',pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_request(Method, Args) ->
  supervisor:start_child(vkontakte_request_sup, [Method, Args]).


init([vkontakte_request]) ->
	{ok, APPID} = application:get_env(vkontakte, app_id),
	{ok, APPSecret} = application:get_env(vkontakte, app_secret),
	{ok, {_Status, _Headers, Body}} = httpc:request(lists:concat(["https://oauth.vk.com/access_token?client_id=", APPID, "&client_secret=", APPSecret, "&grant_type=client_credentials"])),
	{ok, Decoded, _} = rfc4627:decode(Body),
	AccessToken = rfc4627:get_field(Decoded, "access_token", error),

  {ok,
    {{simple_one_for_one, 10, 100},
      [
        {   undefined,                               % Id       = internal id
          {vkontakte_request,start_link,[APPSecret, AccessToken]},                  % StartFun = {M, F, A}
           temporary,                               % Restart  = permanent | transient | temporary
           2000,                                    % Shutdown = brutal_kill | int() >= 0 | infinity
           worker,                                  % Type     = worker | supervisor
           []                                       % Modules  = [Module] | dynamic
        }
      ]
    }
  };

  
init([]) ->
	Childs = [
    {   vkontakte_request_sup,
        {supervisor,start_link,[{local, vkontakte_request_sup}, ?MODULE, [vkontakte_request]]},
        permanent,                               % Restart  = permanent | transient | temporary
        infinity,                                % Shutdown = brutal_kill | int() >= 0 | infinity
        supervisor,                              % Type     = worker | supervisor
        []                                       % Modules  = [Module] | dynamic
    }
  ],
  
  {ok, {{one_for_one, 3, 10}, Childs}}.
