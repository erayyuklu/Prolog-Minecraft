% eray yuklu
% 2021400273
% compiling: yes
% complete: yes


:- ['cmpefarm.pro'].
:- init_from_map.






% 1- agents_distance(+Agent1, +Agent2, -Distance)
agents_distance(Agent1, Agent2, Distance):- %get the positions of the agents and calculate the manhattan distance
    agent_position(Agent1, X1, Y1),
    agent_position(Agent2, X2, Y2),
    Distance is abs(X1 - X2) + abs(Y1 - Y2).

agent_position(Agent, X, Y) :-
    get_dict(x, Agent, X),
    get_dict(y, Agent, Y).

% 2- number_of_agents(+State, -NumberOfAgents)
number_of_agents(State, NumberOfAgents) :- %get the agents dictionary from the state and count the number of agents
    State = [Agents, _, _, _],
    count_agents(Agents, 0, NumberOfAgents).

count_agents(Agents, CountSoFar, NumberOfAgents) :-
    count_agents_recursive(Agents, CountSoFar, 0, NumberOfAgents).

count_agents_recursive(Agents, CountSoFar, Index, NumberOfAgents) :-%count the agents recursively
    (  get_dict(Index, Agents, _)
    -> NextIndex is Index + 1,
       NewCount is CountSoFar + 1,
       count_agents_recursive(Agents, NewCount, NextIndex, NumberOfAgents)
    ;  NumberOfAgents = CountSoFar
    ).

% 3- value_of_farm(+State, -Value)
value_of_farm(State, Value) :- %get the agents and objects dictionaries from the state and calculate the value of the farm
    State = [Agents, Objects, _, _], 
    dict_pairs(Agents, _, AgentPairs), 
    extract_values(AgentPairs, AgentValues),
    sum_of(AgentValues, AgentValue),

    dict_pairs(Objects, _, ObjectPairs), 
    extract_values(ObjectPairs, ObjectValues), 
    sum_of(ObjectValues, ObjValue), 
    
    Value is ObjValue + AgentValue. %sum the values of the agents and objects

% Helper predicate to extract values from a list of key-value pairs
extract_values([], []).
extract_values([_-Object|RestPairs], [Value|RestValues]) :- %get the value of the object and continue recursively
    value_of_object(Object, Value), 
    extract_values(RestPairs, RestValues).

% Predicate to get the value of a single object
value_of_object(Object, ObjectValue) :-
    get_dict(subtype, Object, ObjectType), 
    (value(ObjectType, ObjectValue) -> true ; ObjectValue = 0). % Get the value of the object type, defaulting to 0 if not found

sum_of([], 0).
sum_of([Value|Rest], Sum) :- 
    sum_of(Rest, RestSum),
    Sum is Value + RestSum.
% 4- find_food_coordinates(+State, +AgentId, -Coordinates)
find_food_coordinates(State, AgentId, Coordinates) :- %get the agents and objects dictionaries from the state and find the coordinates of the food
    State = [Agents, Objects, _, _],
    get_dict(AgentId, Agents, Agent),
    (
        Agent.subtype = wolf -> %if the agent is a wolf, find the coordinates of the animals
            findall([X,Y], (
                get_dict(OtherAgentId, Agents, OtherAgent),
                AgentId \= OtherAgentId,
                can_eat(Agent.subtype, OtherAgent.subtype),
                get_dict(x, OtherAgent, X),
                get_dict(y, OtherAgent, Y)
            ), Coordinates),
            (Coordinates = [] -> false; true);
        findall([X,Y], ( %if the agent is not a wolf, find the coordinates of the foods that the agent can eat
            get_dict(_, Objects, Object),
            can_eat(Agent.subtype, Object.subtype),
            get_dict(x, Object, X),
            get_dict(y, Object, Y)
        ), Coordinates),
        (Coordinates = [] -> false; true)
    ).

% 5- find_nearest_agent(+State, +AgentId, -Coordinates, -NearestAgent)
find_nearest_agent(State, AgentId, Coordinates, NearestAgent) :- %get the agents dictionary from the state and find the nearest agent
    State = [Agents, _, _, _],
    get_dict(AgentId, Agents, Agent),
    findall([Dist, X, Y, OtherAgent], ( %find the distances between the agent and the other agents
        get_dict(OtherAgentId, Agents, OtherAgent),
        AgentId \= OtherAgentId,
        get_dict(x, OtherAgent, X),
        get_dict(y, OtherAgent, Y),
        Dist is abs(X - Agent.x) + abs(Y - Agent.y)
    ), Distances),
    findMin(Distances, [_, X, Y, NearestAgent]), %find the nearest agent
    Coordinates = [X, Y],
    !.
    

findMin([X], X). %if there is only one element, return it
findMin([[Dist1, X1, Y1, OtherAgent1]|Rest], [DistMin, XMin, YMin, OtherMin]) :- %find the minimum distance between the agent and the other agents
    findMin(Rest, [Dist2, X2, Y2, OtherAgent2]),
    (   Dist1 < Dist2,
        DistMin = Dist1, XMin = X1, YMin = Y1, OtherMin = OtherAgent1
    ;   Dist1 >= Dist2,
        DistMin = Dist2, XMin = X2, YMin = Y2, OtherMin = OtherAgent2
    ).

% 6- find_nearest_food(+State, +AgentId, -Coordinates, -FoodType, -Distance)
find_nearest_food(State, AgentId, Coordinates, FoodType, Distance) :- %get the agents and objects dictionaries from the state and find the nearest food
    State = [Agents, Objects, _, _],
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype, Agent, Subtype),
    (   Subtype = wolf -> %if the agent is a wolf, find the nearest animal
        find_nearest_animal(Agents, Agent, cow, chicken, Coordinates, FoodType, Distance)
    ;   Subtype = cow -> %if the agent is a cow, find the nearest grass or grain
        find_nearest_food_type(Objects, Agent, [grass, grain], Coordinates, FoodType, Distance)
    ;   Subtype = chicken -> %if the agent is a chicken, find the nearest grain or corn
        find_nearest_food_type(Objects, Agent, [grain, corn], Coordinates, FoodType, Distance)
    ),!.

find_nearest_animal(Agents, Agent, Animal1, Animal2, Coordinates, FoodType, Distance) :-
    findall([Dist, X, Y, OtherAgent], (
        member(Animal, [Animal1, Animal2]),
        get_dict(_, Agents, OtherAgent),
        Animal \= subtype,
        get_dict(subtype, OtherAgent, Animal),
        get_dict(x, OtherAgent, X),
        get_dict(y, OtherAgent, Y),
        Dist is abs(X - Agent.x) + abs(Y - Agent.y)
    ), Distances),
    findMin(Distances, [Distance, X, Y, OtherAgent]), %find the nearest animal using the previously defined helper predicate
    Coordinates = [X, Y], 
    get_dict(subtype, OtherAgent, FoodType). 

find_nearest_food_type(Objects, Agent, FoodTypes, Coordinates, FoodType, Distance) :-
    findall([Dist, X, Y, ObjectType], (
        member(FoodType, FoodTypes),
        get_dict(_, Objects, Object),
        FoodType \= subtype,
        get_dict(subtype, Object, FoodType),
        get_dict(x, Object, X),
        get_dict(y, Object, Y),
        Dist is abs(X - Agent.x) + abs(Y - Agent.y)
    ), Distances),
    findMin(Distances, [Distance, X, Y, ObjectType]),
    Coordinates = [X, Y],
    get_dict(_, Objects, Object),
    get_dict(subtype, Object, FoodType).    
% 7- move_to_coordinate(+State, +AgentId, +X, +Y, -ActionList, +DepthLimit)
move_to_coordinate(State, AgentId, TargetX, TargetY, ActionList, DepthLimit) :- 
    % (is_occupied(TypeA, AgentId, TargetX, TargetY, Agents) -> false; true), %check if the target coordinates are occupied
    main_move_to_coordinate(State, AgentId, TargetX, TargetY, ActionList, DepthLimit), %using main_move to cut backtracking when the agent reaches the target coordinates
    %if actionlist is empty, return false
    (ActionList = [] -> false; true).
main_move_to_coordinate(State, AgentId, TargetX, TargetY, ActionList, DepthLimit) :- 
    State = [Agents, _, _, _],
    get_dict(AgentId, Agents, Agent),
    get_agent_from_position_(CurrentX, CurrentY, Agents, Agent),
    move_helper(State, AgentId, TargetX, TargetY, ActionList, DepthLimit, CurrentX, CurrentY, 1),!.

move_helper(_, _, _, _, [], DepthLimit, _, _, CurrentDepth) :- 
    CurrentDepth > DepthLimit, !. % Stop if depth limit exceeded

move_helper(State, AgentId, TargetX, TargetY, ActionList, DepthLimit, CurrentX, CurrentY, CurrentDepth) :-
    CurrentDepth =< DepthLimit,
    (helper_move_to_coordinate(State, AgentId, TargetX, TargetY, PartialActionList, CurrentDepth, CurrentX, CurrentY) ->
        (PartialActionList = [] -> % Check if PartialActionList is empty
            CurrentDepthNew is CurrentDepth + 1,
            move_helper(State, AgentId, TargetX, TargetY, ActionList, DepthLimit, CurrentX, CurrentY, CurrentDepthNew)
        ; % Otherwise, assign PartialActionList to ActionList
            ActionList = PartialActionList
        )
    ;
        % If helper_move_to_coordinate fails, proceed to the next depth level
        CurrentDepthNew is CurrentDepth + 1,
        move_helper(State, AgentId, TargetX, TargetY, ActionList, DepthLimit, CurrentX, CurrentY, CurrentDepthNew)
    ).

helper_move_to_coordinate(State, AgentId, TargetX, TargetY, ActionList, DepthLimit, CurrentX, CurrentY) :-
    DepthLimit > -1,
    State = [Agents, _, _, _],
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype, Agent, SubtypeA),
    get_dict(type, Agent, TypeA),
    
    (
        (CurrentX = TargetX, CurrentY = TargetY) ->
        ActionList = [] % Agent already at the target coordinates
    ;
        NewDepthLimit is DepthLimit - 1,
       
        (
            (Action = move_up, NewY is CurrentY - 1, NewX is CurrentX, can_move(SubtypeA, move_up), \+ is_occupied(TypeA, AgentId, NewX, NewY, Agents));
            (Action = move_down, NewY is CurrentY + 1, NewX is CurrentX, can_move(SubtypeA, move_down), \+ is_occupied(TypeA, AgentId, NewX, NewY, Agents));
            (Action = move_left, NewX is CurrentX - 1, NewY is CurrentY, can_move(SubtypeA, move_left), \+ is_occupied(TypeA, AgentId, NewX, NewY, Agents));
            (Action = move_right, NewX is CurrentX + 1, NewY is CurrentY, can_move(SubtypeA, move_right), \+ is_occupied(TypeA, AgentId, NewX, NewY, Agents));
            (Action = move_up_right, NewX is CurrentX + 1, NewY is CurrentY - 1, can_move(SubtypeA, move_up_right), \+ is_occupied(TypeA, AgentId, NewX, NewY, Agents));
            (Action = move_up_left, NewX is CurrentX - 1, NewY is CurrentY - 1, can_move(SubtypeA, move_up_left), \+ is_occupied(TypeA, AgentId, NewX, NewY, Agents));
            (Action = move_down_right, NewX is CurrentX + 1, NewY is CurrentY + 1, can_move(SubtypeA, move_down_right), \+ is_occupied(TypeA, AgentId, NewX, NewY, Agents));
            (Action = move_down_left, NewX is CurrentX - 1, NewY is CurrentY + 1, can_move(SubtypeA, move_down_left), \+ is_occupied(TypeA, AgentId, NewX, NewY, Agents))
        ),
        
        helper_move_to_coordinate(State, AgentId, TargetX, TargetY, RestOfActions, NewDepthLimit, NewX, NewY),
        
        ActionList = [Action | RestOfActions]
    ).

% 8- move_to_nearest_food(+State, +AgentId, -ActionList, +DepthLimit)
move_to_nearest_food(State, AgentId, ActionList, DepthLimit) :- %get the nearest food coordinates and move the agent to the food
    find_nearest_food(State, AgentId, [TargetX, TargetY], _, _),
    move_to_coordinate(State, AgentId, TargetX, TargetY, ActionList, DepthLimit). 

% 9- consume_all(+State, +AgentId, -NumberOfMoves, -Value, NumberOfChildren +DepthLimit)
find_consumable_foods(State, AgentId, ConsumableFoods) :-
    find_nearest_food(State, AgentId, _, _, _), % To ensure the state is consistent
    State = [Agents, Objects, _, _],
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype, Agent, Subtype),
    (
        Subtype = wolf ->
            find_consumable_animals(Agents, Agent, cow, chicken, ConsumableFoods)
        ; Subtype = cow ->
            find_consumable_food_types(Objects, Agent, [grass, grain], ConsumableFoods)
        ; Subtype = chicken ->
            find_consumable_food_types(Objects, Agent, [grain, corn], ConsumableFoods)
    ),!.

find_consumable_animals(Agents, Agent, Animal1, Animal2, ConsumableFoods) :- %find the consumable animals
    findall([Distance, FoodType, [X, Y]], (
        member(Animal, [Animal1, Animal2]),
        get_dict(_, Agents, OtherAgent),
        Animal \= subtype,
        get_dict(subtype, OtherAgent, Animal),
        get_dict(x, OtherAgent, X),
        get_dict(y, OtherAgent, Y),
        Distance is abs(X - Agent.x) + abs(Y - Agent.y),
        get_dict(subtype, OtherAgent, FoodType)
    ), AllFoods),
    mysort(AllFoods, SortedFoods), %sort the foods
    extract_coordinates(SortedFoods, ConsumableFoods). 

find_consumable_food_types(Objects, Agent, FoodTypes, ConsumableFoods) :- %find the consumable food types
    findall([Distance, FoodType, [X, Y]], (
        member(FoodType, FoodTypes),
        get_dict(_, Objects, Object),
        FoodType \= subtype,
        get_dict(subtype, Object, FoodType),
        get_dict(x, Object, X),
        get_dict(y, Object, Y),
        Distance is abs(X - Agent.x) + abs(Y - Agent.y)
    ), AllFoods),
    mysort(AllFoods, SortedFoods),
    extract_coordinates(SortedFoods, ConsumableFoods).

extract_coordinates([], []). 
extract_coordinates([[_, FoodType, Coordinates]|Rest], [[FoodType, Coordinates]|Remaining]) :- %extract the coordinates of the consumable foods
    extract_coordinates(Rest, Remaining).

mysort(List, Sorted) :-
    quicksort(List, Sorted).

quicksort([], []).
quicksort([X|Xs], Sorted) :- %sort the list using quicksort
    partition(Xs, X, Left, Right),
    quicksort(Left, SortedLeft),
    quicksort(Right, SortedRight),
    myappend(SortedLeft, [X|SortedRight], Sorted).

partition([], _, [], []).
partition([Y|Ys], Pivot, [Y|Left], Right) :-
    Y = [D1,_,_],
    Pivot = [D2,_,_],
    D1 =< D2,
    partition(Ys, Pivot, Left, Right).
partition([Y|Ys], Pivot, Left, [Y|Right]) :-
    Y = [D1,_,_],
    Pivot = [D2,_,_],
    D1 > D2,
    partition(Ys, Pivot, Left, Right).

myappend([], L, L).
myappend([H|T], L2, [H|Result]) :- %append the lists
    myappend(T, L2, Result).


consume_all(State, AgentId, NumberOfMoves, Value, NumberOfChildren, DepthLimit):- %consume all the consumable foods
    find_consumable_foods(State, AgentId, ConsumableFoods),
    consume_all_helper(State, AgentId, 0, DepthLimit,  ConsumableFoods, NumberOfMoves, Value, NumberOfChildren),!.

% Base case: When there are no more consumable foods left to eat
consume_all_helper(State, AgentId, TotalMoves, _, [], TotalMoves, Value, NumberOfChildren) :- %return the total moves and the value of the farm
    State = [Agents, _, _, _],
    get_dict(AgentId, Agents, Agent),
    get_dict(children, Agent, NumberOfChildren),
    value_of_farm(State, Value).

consume_all_helper(State, AgentId, TotalMoves, DepthLimit,FoodCoordinates, NumberOfMoves, Value, NumberOfChildren):-
    FoodCoordinates = [[_, [X, Y]]| RemainingFoods ],
   
    
    (
        move_to_coordinate(State, AgentId, X, Y, ActionList, DepthLimit) -> % if there is a valid move to the food coordinates do these:
            
            make_series_of_actions(ActionList, State, AgentId, NewState),
            
            % Increment the totalMoves by the length of the actionList
            mylength(ActionList, ActionListLength),
            NewTotalMoves is TotalMoves + ActionListLength,
            eat(NewState, AgentId, NewState2),
            
            % Find consumable foods in the new state
            
            (find_consumable_foods(NewState2, AgentId, ConsumableFoods) -> true; ConsumableFoods = []), %if there is no consumable food left, set ConsumableFoods to an empty list
            % Update the current moves
            % Recursive call
            
            consume_all_helper(NewState2, AgentId, NewTotalMoves, DepthLimit, ConsumableFoods, NumberOfMoves, Value, NumberOfChildren) %continue consuming the foods
        ;
        % No valid moves left
        
        consume_all_helper(State, AgentId, TotalMoves, DepthLimit, RemainingFoods, NumberOfMoves, Value, NumberOfChildren), %continue consuming the foods
     
       
       
        true
    ).

% Base case: When the list is empty, the length is 0
mylength([], 0).

% Recursive case: Increment the count and recurse on the tail of the list
mylength([_|Tail], Length) :- %calculate the length of the list
    mylength(Tail, TailLength),
    Length is TailLength + 1.



