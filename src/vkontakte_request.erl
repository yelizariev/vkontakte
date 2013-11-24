%%% @author     Max Lapshin <max@maxidoors.ru> [http://erlyvideo.org]
%%% @copyright  2009 Max Lapshin
%%% @doc        Example of gen_server
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
-module(vkontakte_request).
-author('Max Lapshin <max@maxidoors.ru>').
-behaviour(gen_server).


%% External API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([binary_to_hexstr/1]).

-export([url_encode/1]).

-record(request, {
  socket,
  length,
  buffer,
  from
}).


start_link(APPSecret, AccessToken, Method, Args) ->
  gen_server:start_link(?MODULE, [APPSecret, AccessToken, Method, Args], []).
 

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%%----------------------------------------------------------------------
%% @spec (Port::integer()) -> {ok, State}           |
%%                            {ok, State, Timeout}  |
%%                            ignore                |
%%                            {stop, Reason}
%%
%% @doc Called by gen_server framework at process startup.
%%      Create listening socket.
%% @end
%%----------------------------------------------------------------------


init([APPSecret, AccessToken, Method, Args]) ->
  {ok, Socket} = vkontakte_invoke(APPSecret, AccessToken, Method, Args),
  {ok, #request{socket = Socket, buffer = <<>>}}.

%%-------------------------------------------------------------------------
%% @spec (Request, From, State) -> {reply, Reply, State}          |
%%                                 {reply, Reply, State, Timeout} |
%%                                 {noreply, State}               |
%%                                 {noreply, State, Timeout}      |
%%                                 {stop, Reason, Reply, State}   |
%%                                 {stop, Reason, State}
%% @doc Callback for synchronous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_call({get_result}, From, State) ->
  {noreply, State#request{from = From}};
  
handle_call(Request, _From, State) ->
  {stop, {unknown_call, Request}, State}.


%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for asyncrous server calls.  If `{stop, ...}' tuple
%%      is returned, the server is stopped and `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_cast(_Msg, State) ->
  {stop, {unknown_cast, _Msg}, State}.

%%-------------------------------------------------------------------------
%% @spec (Msg, State) ->{noreply, State}          |
%%                      {noreply, State, Timeout} |
%%                      {stop, Reason, State}
%% @doc Callback for messages sent directly to server's mailbox.
%%      If `{stop, ...}' tuple is returned, the server is stopped and
%%      `terminate/2' is called.
%% @end
%% @private
%%-------------------------------------------------------------------------
handle_info({http, {_ReqID, Result}}, SD)->
	{_Status, _Headers, Body} = Result,
	{ok, Decoded, _} = rfc4627:decode(Body),
	gen_server:reply(SD#request.from, {ok, Decoded}),
	{noreply, SD#request{buffer= <<>> }, 10};
%% OLD:
handle_info({http, Socket, {http_response, _Version, 200, _Status}}, State) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, State};

handle_info({http, Socket, {http_header, _, 'Content-Length', _, Length}}, State) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, State#request{length = list_to_integer(Length)}};

handle_info({http, Socket, {http_header, _, Header, _, Value}}, State) ->
  inet:setopts(Socket, [{active, once}]),
  {noreply, State};

handle_info({http, Socket, http_eoh}, State) ->
  inet:setopts(Socket, [{active, once},{packet,raw}]),
  {noreply, State};

handle_info({tcp, Socket, Bin}, #request{buffer = Buffer, length = Length} = State) when size(Buffer) + size(Bin) < Length ->
  inet:setopts(Socket, [{active, once},{packet,raw}]),
  {noreply, State#request{buffer = <<Buffer/binary, Bin/binary>>}};

handle_info({tcp, Socket, Bin}, #request{buffer = Buffer, from = From} = State) ->
  Reply = <<Buffer/binary, Bin/binary>>,
  {ok, Decoded, _} = rfc4627:decode(Reply),
  gen_server:reply(From, {ok, Decoded}),
  {noreply, State#request{buffer = <<>>}, 10};

handle_info(timeout, State) ->
  {stop, normal, State};

handle_info({'DOWN', process, Client, _Reason}, Server) ->
  {noreply, Server};

handle_info(_Info, State) ->
  {noreply, State}.

%%-------------------------------------------------------------------------
%% @spec (Reason, State) -> any
%% @doc  Callback executed on server shutdown. It is only invoked if
%%       `process_flag(trap_exit, true)' is set by the server process.
%%       The return value is ignored.
%% @end
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, _State) ->
  ok.

%%-------------------------------------------------------------------------
%% @spec (OldVsn, State, Extra) -> {ok, NewState}
%% @doc  Convert process state when code is changed.
%% @end
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(A) when is_integer(A) -> integer_to_list(A);
to_list(A) when is_float(A) -> integer_to_list(round(A));
to_list(A) when is_list(A) -> url_encode(A);
to_list(A) when is_binary(A) -> binary_to_list(A).

url_encode([H|T]) ->
    if
        H >= $a, $z >= H ->
            [H|url_encode(T)];
        H >= $A, $Z >= H ->
            [H|url_encode(T)];
        H >= $0, $9 >= H ->
            [H|url_encode(T)];
        H == $_; H == $.; H == $-; H == $/; H == $: -> % FIXME: more..
            [H|url_encode(T)];
        true ->
		    [X,Y]=int_to_hex(H),
		    [$%, X, Y | url_encode(T)]
            %% case yaws:integer_to_hex(H) of
            %%     [X, Y] ->
            %%         [$%, X, Y | url_encode(T)];
            %%     [X] ->
            %%         [$%, $0, X | url_encode(T)]
            %% end
     end;

url_encode([]) ->
    [].


binary_to_hexbin(L) ->
	list_to_binary(binary_to_hexstr(L)).
binary_to_hexstr(L)->
  lists:flatten(lists:map(fun(X) -> int_to_hex(X) end, binary_to_list(L))).

int_to_hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
  $0+N;
hex(N) when N >= 10, N < 16 ->
  $a + (N-10).


vkontakte_invoke(APPSecret, AccessToken, Method, Args) ->
  %{MegaSec, Sec, Usec} = erlang:now(),
  %Timestamp = MegaSec*1000000+Sec,
  %Rand = Sec*1000000+Usec,
	Required0 = [{"v", "5.4"}],
  Required =
		case Method of
			"users.get"->Required0;
			_-> [{"client_secret", APPSecret}, {"access_token", AccessToken}]++Required0
		end,
	URL = lists:concat(["https://api.vkontakte.ru/method/",
	                    Method, "?"]
	                   ++ lists:foldl(fun({Key, Value}, Acc) ->
			                                  [Key,
			                                   "=",
			                                   to_list(Value), "&" | Acc]
	                                  end, [], Required++Args)),
	%io:format("URL=~p~n", [URL]),
	{ok, Request} = httpc:request(get, {URL, []}, [], [{sync, false}, {receiver, self()}]), %% TODO: stream
  {ok, Request}.


