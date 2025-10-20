% :- multifile grid/3.

% grid(0, 0, wall).
% grid(0, 1, wall).
% grid(0, 2, wall).
% grid(0, 3, wall).
% grid(0, 4, wall).
% grid(1, 0, cheese).
% grid(1, 1, floor).
% grid(1, 2, floor).
% grid(1, 3, cat).
% grid(1, 4, wall).
% grid(2, 0, wall).
% grid(2, 1, floor).
% grid(2, 2, box).
% grid(2, 3, wall).
% grid(2, 4, floor).
% grid(3, 0, floor).
% grid(3, 1, cat).
% grid(3, 2, floor).
% grid(3, 3, floor).
% grid(3, 4, floor).
% grid(4, 0, floor).
% grid(4, 1, floor).
% grid(4, 2, floor).
% grid(4, 3, wall).
% grid(4, 4, mouse).

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
    cat_list(Cats),
    member(CatPos, Cats),
    adjacent(PlayerPos, CatPos),
    !.

deadly_state(state(PlayerPos, Timer)) :-
    Timer > 0,
    Timer mod 3 =:= 0,
    adjacent_cat(PlayerPos),
    cell_type(PlayerPos, Type),
    Type \= box.

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
    NewTimer is CurrentTimer + 1.

solve_min_length(K_min) :-
    initial_state(InitialState),
    between(1, 30, K_min),
    solve_with_limit(InitialState, [InitialState], _Path, K_min),
    !.

solve(Path) :-
    solve_min_length(K_min),
    initial_state(InitialState),
    solve_with_limit(InitialState, [InitialState], Path, K_min).

solve_with_limit(State, _Visited, [], _RemainingLimit) :-
    victory_state(State).

solve_with_limit(CurrentState, Visited, [Action | Rest], RemainingLimit) :-
    RemainingLimit > 0,
    action(Action),
    move(CurrentState, Action, NextState),
    \+ deadly_state(NextState),
    \+ member(NextState, Visited),
    NewLimit is RemainingLimit - 1,
    solve_with_limit(NextState, [NextState | Visited], Rest, NewLimit).

all_solutions :-
    findall(Path, solve(Path), All),
    (   All = []
        ->  write('No solution found.')
    ;   write('Solutions found:'), nl,
        print_solutions(All)
    ).

print_solutions([]).
print_solutions([H|T]) :-
    write(H), nl,
    print_solutions(T).
