initial_state(state(pos(0, 0), 0)).

cell_type(pos(R, C), Type) :-
    grid(R, C, Type).

can_enter(floor).
can_enter(cheese).
can_enter(box).

victory_state(state(Pos, _)) :-
    cell_type(Pos, cheese).

cat_list(CatList) :-
    findall(pos(R, C), grid(R, C, cat), CatList).

adjacent(pos(R1, C1), pos(R2, C2)) :-
    abs(R1 - R2) =< 1,
    abs(C1 - C2) =< 1,
    (R1 \= R2 ; C1 \= C2).

adjacent_cat(PlayerPos) :-
    grid(CatR, CatC, cat),
    adjacent(PlayerPos, pos(CatR, CatC)),
    !.

deadly_state(State) :-
    State = state(PlayerPos, Timer),
    Timer mod 3 =:= 2,
    adjacent_cat(PlayerPos),
    cell_type(PlayerPos, Type),
    Type \= box,
    \+ victory_state(State).

action(up).
action(down).
action(left).
action(right).

calculate_desired_pos(pos(R, C), up,     pos(NR, C)) :- NR is R - 1.
calculate_desired_pos(pos(R, C), down,   pos(NR, C)) :- NR is R + 1.
calculate_desired_pos(pos(R, C), left,   pos(R, NC)) :- NC is C - 1.
calculate_desired_pos(pos(R, C), right,  pos(R, NC)) :- NC is C + 1.

move(state(CurrentPos, CurrentTimer), Action, state(DesiredPos, NewTimer)) :-
    calculate_desired_pos(CurrentPos, Action, DesiredPos),
    once((
        cell_type(DesiredPos, Type),
        can_enter(Type)
    )),
    NewTimer is (CurrentTimer + 1) mod 3.


    solve(Path) :-
        solve_bfs(Path).

    solve_bfs(Path) :-
        initial_state(Initial),
        bfs([[Initial, []]], [Initial], RevPath),
        reverse(RevPath, Path).

    bfs([[State, RevPath] | _], _, RevPath) :-
        victory_state(State), !.

    bfs([[State, RevPath] | Queue], Visited, Sol) :-
        findall(
            [Next, [Action | RevPath]],
            (
                action(Action),
                move(State, Action, Next),
                \+ deadly_state(Next),
                \+ member(Next, Visited)
            ),
            Succs
        ),
        append(Queue, Succs, NewQueue),
        extract_states(Succs, NewStates),
        append(Visited, NewStates, Vis2),
        bfs(NewQueue, Vis2, Sol).

    extract_states([], []).
    extract_states([[S, _] | T], [S | Ts]) :-
        extract_states(T, Ts).
