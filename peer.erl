-module(peer).
-compile(export_all).
-behavior(gen_server).

init([Id, M, Keys, NumRequests]) ->
  FingerTable = ?MODULE:get_finger_table(Id, M, 1, Keys, []),
  % NumRequests, Keys, Id, FingerTable, Target, HopCount, HopList, Source
  {ok, [NumRequests, [], Id, FingerTable, 0, 0, [], 0]}.

% Spawns A Single GenServer Link 
create_link(I, {Keys, M, NumRequests}) ->
  KeyI = lists:nth(I, Keys),
  gen_server:start_link(?MODULE, [KeyI, M, Keys, NumRequests], []).

% Creates Chord Ring 
create(N, Keys, M, NumRequests) ->
  [?MODULE:create_link(I, {Keys, M, NumRequests}) || I <- lists:seq(1, N)].

% Populates Finger Table for each node 
get_finger_table(_Id, M, I, _Keys, FingerTable) when I > M ->
  FingerTable;
get_finger_table(Id, M, I, Keys, FingerTable) when I =:= 0 ->
  Truncated = erlang:trunc(math:pow(2, M)),
  Low = (Id + 1) rem Truncated,
  High = (Id + 2) rem Truncated,

  if 
    Low < High ->
      Temp = lists:filter(fun(X) -> X >= Low end, Keys),
      if 
        erlang:length(Temp) > 0 ->
          Finger = Temp
      ; true ->
          Finger = lists:sort(lists:filter(fun(X) -> X < Low end, Keys))
      end
  ; true ->
      Temp1 = lists:filter(fun(X) -> X < Low end, Keys),
      Temp2 = lists:map(fun(X) -> X + Truncated end, Temp1),
      Finger = lists:sort(Temp2 ++ lists:filter(fun(X) -> X >= Low end, Keys))
  end,

  Exponent = math:pow(2, M),
  FirstElement = lists:nth(1+ 0, Finger),
  if 
    FirstElement >= Exponent ->
      NewFingerTable = FingerTable ++ [(erlang:trunc(FirstElement)) rem (Truncated)]
  ; true ->
      NewFingerTable = FingerTable ++ [FirstElement]
  end,
    
  get_finger_table(Id, M, I + 1, Keys, NewFingerTable);
get_finger_table(Id, M, I, Keys, FingerTable) when I =< M ->
  Truncated = erlang:trunc(math:pow(2, M)),
  Low = (Id + erlang:trunc(math:pow(2, I - 1))) rem Truncated,
  High = (Id + erlang:trunc(math:pow(2, I))) rem Truncated,

  if 
    Low < High ->
      Temp = lists:filter(fun(X) -> X >= Low end, Keys),
      if 
        erlang:length(Temp) > 0 ->
          Finger = Temp
      ; true ->
          Finger = lists:sort(lists:filter(fun(X) -> X < Low end, Keys))
      end
  ; true ->
      Temp1 = lists:filter(fun(X) -> X < Low end, Keys),
      Temp2 = lists:map(fun(X) -> X + Truncated end, Temp1),
      Finger = lists:sort(Temp2 ++ lists:filter(fun(X) -> X >= Low end, Keys))
  end,
      
  Exponent = math:pow(2, M),
  FirstElement = lists:nth(1+ 0, Finger),
  if 
    FirstElement >= Exponent ->
      NewFingerTable = FingerTable ++ [(erlang:trunc(FirstElement)) rem (Truncated)]
  ; true ->
      NewFingerTable = FingerTable ++ [FirstElement]
  end,
    
  get_finger_table(Id, M, I + 1, Keys, NewFingerTable).

-define(ERROR, (0.1 * (rand:uniform() - (1 / 2)))).
-define(LOGS(X), (math:log10(X) / math:log10(math:exp(1)))).

% Helper function to get node name 
get_node_name(I) ->
  Id = "erlang.N0000" ++ erlang:integer_to_list(I),
  _Atom = erlang:list_to_atom(Id),
  ?MODULE.

% Starts lookup for a random key from the set of Keys 
create_lookup(Id, I, Limit, _Keys, _FingerTable, _HopCount) when I =:= Limit ->
  io:format("ID=~p | All set initiating requests~n", [Id]);
create_lookup(Id, I, Limit, Keys, FingerTable, HopCount) when I < Limit ->
  Key = get_random_key(Keys, Id),
  Dest = lists:nth(1+ find_dest_index(Key, FingerTable, 0), lists:sort(FingerTable)),

  gen_server:cast(get_node_name(Dest), {lookup, {Key, Id, HopCount + 1}}),
  % timer:sleep(1000),
  create_lookup(Id, I + 1, Limit, Keys, FingerTable, HopCount).

% Get average number of hops from NodeData
get_average_hops(NodeData) ->
  {_, Node, _} = NodeData,
  io:format("Converged with Average Hops = ~.3f~n~n", [?MODULE:calculate(Node)]).

% Traverses finger table to find destination node's index 
find_dest_index(Target, FingerTable, I) when I < erlang:length(FingerTable) ->
  IthFinger = lists:nth(1+ I, lists:sort(FingerTable)),
  if 
    IthFinger > Target ->
      I - 1
  ; true ->
      find_dest_index(Target, FingerTable, I + 1)
  end;
find_dest_index(_Target, FingerTable, I) when I == erlang:length(FingerTable) ->
  I - 1.

% Returns a random key to be looked up -excluding self key
get_random_key(Keys, Id) ->
  RandomKey = lists:nth(rand:uniform(erlang:length(Keys)), Keys),
  if 
    RandomKey =:= Id ->
      get_random_key(Keys, Id)
  ; true ->
      RandomKey
  end.

% Calculate Including Total Error
calculate(Node) ->
  X = 1.01 + ?ERROR,
  Y = 0.71 * ?LOGS(Node),
  X + Y.

% Handle Initiate Request from Master 
handle_cast({initiate, {NumR, Ks}}, [
      _NumRequests,
      _Keys,
      Id,
      FingerTable,
      _Target,
      _HopCount,
      _HopList,
      _Source
    ]) ->
  io:format("ID=~p | Received Initiate. Will start lookups", [Id]),
  create_lookup(Id, 0, NumR, Ks, FingerTable, 0),
  {noreply, [NumR, Ks, Id, FingerTable, _Target, _HopCount, _HopList, _Source]};

% Handle Lookup request for a key from peer node 
handle_cast({lookup, {TargetKey, Src, HopCountReceived}}, [
  _NumRequests,
  _Keys,
  Id,
  FingerTable,
  _Target,
  _HopCount,
  _HopList,
  _Source
]) ->
  io:format("ID=~p | Received Lookup Request for Target=~p", [Id, TargetKey]),

  if 
    TargetKey =:= Id ->
      % io:format("In the notify block~n"),
      gen_server:cast(get_node_name(Src), {notify, {HopCountReceived + 1}}),
  
      {noreply,
       [ _NumRequests,
         _Keys,
         Id,
         FingerTable,
         TargetKey,
         HopCountReceived + 1,
         _HopList,
         _Source
       ]}
  ; true ->
    % io:format("In the forward lookup block~n"),
    Dest = lists:nth(1+ find_dest_index(TargetKey, FingerTable, 0), lists:sort(FingerTable)),
    
    gen_server:cast(get_node_name(Dest), 
                    {lookup, {TargetKey, Src, HopCountReceived + 1}}),
    {noreply,
     [_NumRequests, _Keys, Id, FingerTable, TargetKey, HopCountReceived, _HopList, _Source]}
  end;

% Notification once a key is found in chord
handle_cast({notify, {HopCountReceived}}, [
      NumRequests,
      _Keys,
      Id,
      _FingerTable,
      _Target,
      _HopCount,
      HopList,
      _Source
    ]) ->
  NewHopList = HopList ++ [HopCountReceived],
  io:format("ID=~p | Received Notify. Lookup Succeeded", [Id]),

  if erlang:length(NewHopList) =:= NumRequests ->
    gen_server:cast(master, {hibernate, lists:sum(NewHopList) / NumRequests})
  end,

  {noreply,
   [ NumRequests,
     _Keys,
     Id,
     _FingerTable,
     _Target,
     HopCountReceived,
     NewHopList,
     _Source
   ]}.
