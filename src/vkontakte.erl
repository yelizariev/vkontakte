%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Worker module for plugin example
%%% @reference  See <a href="http://erlyvideo.org/" target="_top">http://erlyvideo.org/</a> for more information
%%% @end
%%%
%%%
%%% The MIT License
%%%
%%% Copyright (c) 2009 Max Lapshin
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%
%%%---------------------------------------------------------------------------------------
-module(vkontakte).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(application).

% Application API
-export([start/2, stop/1, config_change/3]).


% PLUGIN API
-export([start/0, stop/0]).

-export([call/2]).

-export([archive/0, reload/0]).

-export([check_auth/2]).

call(Method, Args) ->
	{ok, Pid} = vkontakte_sup:start_request(Method, Args),
	gen_server:call(Pid, {get_result}).

check_auth(ViewerID, AuthKey)->
	{ok, APPID} = application:get_env(vkontakte, app_id),
	{ok, APPSecret} = application:get_env(vkontakte, app_secret),
	AuthKey =:= vkontakte_request:binary_to_hexstr(erlang:md5(lists:concat([APPID,'_',ViewerID,'_',APPSecret]))).

start() ->
	application:start(inets),
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(vkontakte),
	ok.


reload() ->
  {ok, Modules} = application:get_key(vkontakte,modules),
  [begin
    code:soft_purge(Module),
    code:load_file(Module)
  end || Module <- Modules].

  

archive() ->
  make:all([load]),
  application:load(vkontakte),
  {ok, Version} = application:get_key(vkontakte,vsn),
  zip:create("vkontakte-"++Version++".ez", ["vkontakte/ebin", "vkontakte/wwwroot"], [{cwd, "../"},{compress,all},{uncompress,all},verbose]).
  
  
%%--------------------------------------------------------------------
%% @spec (Type::any(), Args::list()) -> any()
%% @doc Starts RTMP library
%% @end 
%%--------------------------------------------------------------------

start(_Type, _Args) ->
  vkontakte_sup:start_link().



%%--------------------------------------------------------------------
%% @spec (Any::any()) -> ok()
%% @doc Stop RTMP library
%% @end 
%%--------------------------------------------------------------------
stop(_S) ->
  ok.


%%--------------------------------------------------------------------
%% @spec (Any::any(),Any::any(),Any::any()) -> any()
%% @doc Reload ErlMedia Application config
%% @end 
%%--------------------------------------------------------------------
config_change(_Changed, _New, _Remove) ->
  ok.
  
stop() -> 
  application:stop(vkontakte),
  application:unload(vkontakte).
