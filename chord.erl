-module(chord).
-compile(export_all).
-behavior(gen_server).

init([NumNodes, NumRequests]) ->
  % AvgHops, HopList, NumNodes
  {ok, {[], NumNodes, NumRequests}}.

get_number_of_bits(NumNodes) ->
  erlang:trunc(math:ceil(math:log2(NumNodes))).

main(Args) ->
  NumNodes = lists:nth(1+ 0, Args),
  NumRequests = lists:nth(1+ 1, Args),

  M = get_number_of_bits(NumNodes),
  Keys = get_keys(erlang:trunc(math:pow(2, M)), NumNodes, []),
  NodeData = {Keys, NumNodes, NumRequests},

  gen_server:start_link(?MODULE, [NumNodes, NumRequests], []),
  % Create Chord Ring
  io:format("Spawning ~p nodes~n", [NumNodes]),
  % creating peer nodes
  io:format("~p nodes created~n", [NumNodes]),
  peer:create(NumNodes, Keys, M, NumRequests),
  % Initiate numRequest random lookups in each of NumNodes ,
  [?MODULE:initiate_lookup(I, NodeData) || I <- lists:seq(1, NumNodes)],
  % Get average number of hops
  peer:get_average_hops(NodeData),
  % Finish Execution
  exit(self(), "Finished Execution").

get_keys(_N, Limit, Keys) when Limit =:= 1 ->
  lists:sort(Keys);
get_keys(N, Limit, Keys) when Limit > 1, erlang:length(Keys) =:= 0 ->
  get_keys(N, Limit, Keys ++ [rand:uniform(N)]);
get_keys(N, Limit, Keys) when Limit > 1 ->
  RandomKey = rand:uniform(N),

  case lists:member(RandomKey, Keys) of 
    true ->
      get_keys(N, Limit, Keys)
  ; false ->
      get_keys(N, Limit - 1, Keys ++ [RandomKey])
  end.

% Spawns A Single GenServer Link 
initiate_lookup(I, {Keys, NumNodes, NumRequests}) ->
  KeyI = lists:nth(I, Keys),
  timer:sleep(erlang:round(1000 * NumNodes / NumRequests)),
  % Send Key to calling function,
  KeyI.

% Node hibernate finale
handle_cast({hibernate, Avg}, 
            {HopList, NumNodes, NumRequests}) ->
  % io:format("Received Hibernate~n")
  NewList = HopList ++ [Avg],
  
  {noreply, {NewList, NumNodes, NumRequests}}.