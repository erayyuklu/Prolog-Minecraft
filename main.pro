% name surname
% id
% compiling: yes
% complete: yes


:- ['cmpefarm.pro'].
:- init_from_map.



agent_position(Agent, X, Y) :-
    get_dict(x, Agent, X),
    get_dict(y, Agent, Y).


% 1- agents_distance(+Agent1, +Agent2, -Distance)
agents_distance(Agent1, Agent2, Distance):-
    agent_position(Agent1, X1, Y1),
    agent_position(Agent2, X2, Y2),
    Distance is abs(X1 - X2) + abs(Y1 - Y2).

% 2- number_of_agents(+State, -NumberOfAgents)
number_of_agents(State, NumberOfAgents) :-
    State = [Agents, _, _, _],
    count_agents(Agents, 0, NumberOfAgents).

count_agents(Agents, CountSoFar, NumberOfAgents) :-
    count_agents_recursive(Agents, CountSoFar, 0, NumberOfAgents).

count_agents_recursive(Agents, CountSoFar, Index, NumberOfAgents) :-
    (  get_dict(Index, Agents, _)
    -> NextIndex is Index + 1,
       NewCount is CountSoFar + 1,
       count_agents_recursive(Agents, NewCount, NextIndex, NumberOfAgents)
    ;  NumberOfAgents = CountSoFar
    ).

% 3- value_of_farm(+State, -Value)
value_of_farm(State, Value) :-
    State = [Agents, Objects, _, _], % Assuming State is a list containing objects as the second element
    dict_pairs(Agents, _, AgentPairs), % Convert the agents dictionary to a list of key-value pairs  
    extract_values(AgentPairs, AgentValues), % Extract the values from the agent pairs
    sum_list(AgentValues, AgentValue),

    dict_pairs(Objects, _, ObjectPairs), % Convert the objects dictionary to a list of key-value pairs
    extract_values(ObjectPairs, ObjectValues), % Extract the values from the object pairs
    sum_list(ObjectValues, ObjValue), % Calculate the total value by summing up the values
    
    Value is ObjValue + AgentValue. % Calculate the total value by summing up the values

% Helper predicate to extract values from a list of key-value pairs
extract_values([], []).
extract_values([_-Object|RestPairs], [Value|RestValues]) :-
    value_of_object(Object, Value), % Get the value of the current object
    extract_values(RestPairs, RestValues).

% Predicate to get the value of a single object
value_of_object(Object, ObjectValue) :-
    get_dict(subtype, Object, ObjectType), % Get the type of the object
    (value(ObjectType, ObjectValue) -> true ; ObjectValue = 0). % Get the value of the object type, defaulting to 0 if not found

% 4- find_food_coordinates(+State, +AgentId, -Coordinates)
find_food_coordinates(State, AgentId, Coordinates) :-
    State = [Agents, Objects, _, _],
    get_dict(AgentId, Agents, Agent),
    (
        Agent.subtype = wolf ->
            findall([X,Y], (
                get_dict(OtherAgentId, Agents, OtherAgent),
                AgentId \= OtherAgentId,
                can_eat(Agent.subtype, OtherAgent.subtype),
                get_dict(x, OtherAgent, X),
                get_dict(y, OtherAgent, Y)
            ), Coordinates);
        findall([X,Y], (
            get_dict(_, Objects, Object),
            can_eat(Agent.subtype, Object.subtype),
            get_dict(x, Object, X),
            get_dict(y, Object, Y)
        ), Coordinates)
    ).
% 5- find_nearest_agent(+State, +AgentId, -Coordinates, -NearestAgent)
find_nearest_agent(State, AgentId, Coordinates, NearestAgent) :-
    State = [Agents, _, _, _],
    get_dict(AgentId, Agents, Agent),
    findall([Dist, X, Y, OtherAgent], (
        get_dict(OtherAgentId, Agents, OtherAgent),
        AgentId \= OtherAgentId,
        get_dict(x, OtherAgent, X),
        get_dict(y, OtherAgent, Y),
        Dist is abs(X - Agent.x) + abs(Y - Agent.y)
    ), Distances),
    findMin(Distances, [_, X, Y, NearestAgent]),
    Coordinates = [X, Y],
    !.
    

findMin([X], X).
findMin([[Dist1, X1, Y1, OtherAgent1]|Rest], [DistMin, XMin, YMin, OtherMin]) :-
    findMin(Rest, [Dist2, X2, Y2, OtherAgent2]),
    (   Dist1 < Dist2,
        DistMin = Dist1, XMin = X1, YMin = Y1, OtherMin = OtherAgent1
    ;   Dist1 >= Dist2,
        DistMin = Dist2, XMin = X2, YMin = Y2, OtherMin = OtherAgent2
    ).

% 6- find_nearest_food(+State, +AgentId, -Coordinates, -FoodType, -Distance)
find_nearest_food(State, AgentId, Coordinates, FoodType, Distance) :-
    State = [Agents, Objects, _, _],
    get_dict(AgentId, Agents, Agent),
    get_dict(subtype, Agent, Subtype),
    (   Subtype = wolf ->
        find_nearest_animal(Agents, Agent, cow, chicken, Coordinates, FoodType, Distance)
    ;   Subtype = cow ->
        find_nearest_food_type(Objects, Agent, [grass, grain], Coordinates, FoodType, Distance)
    ;   Subtype = chicken ->
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
    findMin(Distances, [Distance, X, Y, OtherAgent]),
    Coordinates = [X, Y],
    FoodType = Animal.

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
    FoodType = ObjectType.

% 7- move_to_coordinate(+State, +AgentId, +X, +Y, -ActionList, +DepthLimit)
move_to_coordinate(State, AgentId, TargetX, TargetY, ActionList, DepthLimit) :-
    State = [Agents, _, _, _],
    get_dict(AgentId, Agents, Agent),
    get_agent_from_position_(CurrentX, CurrentY, Agents, Agent),
    
    helper_move_to_coordinate(State, AgentId, TargetX, TargetY, ActionList, DepthLimit, CurrentX, CurrentY),!.
    

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
            (Action = move_left, NewX is CurrentX - 1, NewY is CurrentY, can_move(SubtypeA, move_left), \+ is_occupied(TypeA,AgentId, NewX, NewY, Agents));
            (Action = move_right, NewX is CurrentX + 1, NewY is CurrentY, can_move(SubtypeA, move_right), \+ is_occupied(TypeA,AgentId, NewX, NewY, Agents));
            (Action = move_up_right, NewX is CurrentX + 1, NewY is CurrentY - 1, can_move(SubtypeA, move_up_right), \+ is_occupied(TypeA,AgentId, NewX, NewY, Agents));
            (Action = move_up_left, NewX is CurrentX - 1, NewY is CurrentY - 1, can_move(SubtypeA, move_up_left), \+ is_occupied(TypeA,AgentId, NewX, NewY, Agents));
            (Action = move_down_right, NewX is CurrentX + 1, NewY is CurrentY + 1, can_move(SubtypeA, move_down_right), \+ is_occupied(TypeA,AgentId, NewX, NewY, Agents));
            (Action = move_down_left, NewX is CurrentX - 1, NewY is CurrentY + 1, can_move(SubtypeA, move_down_left), \+ is_occupied(TypeA,AgentId, NewX, NewY, Agents))
        ),
        
        helper_move_to_coordinate(State, AgentId, TargetX, TargetY, RestOfActions, NewDepthLimit, NewX, NewY),
        
        ActionList = [Action | RestOfActions]
    ),!.

% 8- move_to_nearest_food(+State, +AgentId, -ActionList, +DepthLimit)
move_to_nearest_food(State, AgentId, ActionList, DepthLimit) :-
    find_nearest_food(State, AgentId, [TargetX, TargetY], _, _),
    move_to_coordinate(State, AgentId, TargetX, TargetY, ActionList, DepthLimit). 

% 9- consume_all(+State, +AgentId, -NumberOfMoves, -Value, NumberOfChildren +DepthLimit)


