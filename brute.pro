:- use_module(library(clpfd)).
:- use_module(library(lists)).

%     imp        set
status(0). status(1).

north([_, _, _, 1, _]).
east([_, _, _, _, 1]).
south([_, 1, _, _, _]).
west([_, _, 1, _, _]).

write_row([]).
write_row([[H,_,_,_,_]|T]):- write(H), write_row(T).

validate_grid(_, [], _, _, _, _, _, _, _).

validate_grid([], [R1|RT], [], [HR1|HRT], X, SX, FY, Size_Y, Size_X):-
                    validate_status(R1),
                    validate_S3_row(R1, HR1),
                    
                    validate_con(R1, X, 1, SX, FY, Size_Y, Size_X),

                    X1 is X +1,
                    validate_grid([], RT, [], HRT, X1, SX, FY, Size_Y, Size_X).

validate_grid([C1|CT], [R1|RT], [HC1|HCT], [HR1|HRT], X, SX, FY, Size_Y, Size_X):-
                    validate_status(R1),
                    validate_S3_row(R1, HR1),

                    validate_status_S2(C1),
                    validate_S12(C1, HC1),
                    
                    validate_con(R1, X, 1, SX, FY, Size_Y, Size_X),

                    X1 is X +1,
                    validate_grid(CT, RT, HCT, HRT, X1, SX, FY, Size_Y, Size_X).


validate_status([]).
validate_status([[[S,_,_,_,_]|_]|T]):- status(S), validate_status(T).

validate_status_S2([]).
validate_status_S2([[S,_,_,_,_]|T]):- status(S), validate_status_S2(T).

validate_con([], _, _, _, _, _, _).
validate_con([[ThisTrack,North,East,South,West]|T], X, Y, SX, FY, Size_Y, Size_X):- valid_connections(ThisTrack),
                                                                  (Y = Size_Y -> validate_s3_adj(ThisTrack, East, West, X, Y, SX); 
                                                                  validate_s3_left(ThisTrack, West, X, Y, SX)),
                                                                  (X = Size_X -> validate_s3_up_und(ThisTrack, North, South, X, Y, FY, Size_X); 
                                                                  validate_s3_up(ThisTrack, North, X, Y, FY, Size_X)),
                                                                  Y1 is Y+1, 
                                                                  validate_con(T, X, Y1, SX, FY, Size_Y, Size_X).

valid_connections([0,0,0,0,0]).
valid_connections([1,1,1,0,0]).
valid_connections([1,0,0,1,1]).
valid_connections([1,0,1,1,0]).
valid_connections([1,0,1,0,1]).
valid_connections([1,1,0,1,0]).
valid_connections([1,1,0,0,1]).

check_s3_adjacent([], _, _, _, _, _).
check_s3_adjacent([H|T], X, Y, SX, FY, Size_X):- validate_s3_adj(H, X, Y, SX, FY, Size_X), Y1 is Y+1, check_s3_adjacent(T, X, Y1, SX, FY, Size_X).

validate_s3_adj([0,_,_,_,_], E, W, _, _, _):- not(east(E)), not(west(W)).
validate_s3_adj([1, 1, 1, 0, 0], E, W, X, Y, SX):- (east(E), not(west(W))) ; (is_start(X, Y, SX)).
validate_s3_adj([1, 1, 0, 0, 1], E, W, X, Y, SX):- (not(east(E)), west(W)) ; (is_start(X, Y, SX)).
validate_s3_adj([1, 0, 0, 1, 1], E, W, X, Y, SX):- (not(east(E)), west(W)) ; (is_start(X, Y, SX)).
validate_s3_adj([1, 0, 1, 1, 0], E, W, X, Y, SX):- (east(E), not(west(W))) ; (is_start(X, Y, SX)).
validate_s3_adj([1, 0, 1, 0, 1], E, W, X, Y, SX):- (east(E), west(W)) ; (is_start(X, Y, SX)).
validate_s3_adj([1, 1, 0, 1, 0], E, W, X, Y, SX):- (not(east(E)), not(west(W))) ; (is_start(X, Y, SX)).

%check_s3_up_und([], _, _, _, _, _).
%check_s3_up_und([H|T], X, Y, SX, FY, Size_X):- validate_s3_up_und(H, X, Y, SX, FY, Size_X), Y1 is Y+1, check_s3_up_und(T, X, Y1, SX, FY, Size_X).

validate_s3_up_und([0,_,_,_,_], N, S, _, _, _, _):- not(north(N)), not(south(S)).
validate_s3_up_und([1, 1, 0, 0, 1], N, S, X, Y, FY, Size_X):- (not(south(S)), north(N)); (is_finish(Size_X, X, Y, FY)).
validate_s3_up_und([1, 1, 1, 0, 0], N, S, X, Y, FY, Size_X):- (not(south(S)), north(N)); (is_finish(Size_X, X, Y, FY)).
validate_s3_up_und([1, 0, 0, 1, 1], N, S, X, Y, FY, Size_X):- (south(S), not(north(N))); (is_finish(Size_X, X, Y, FY)).
validate_s3_up_und([1, 0, 1, 1, 0], N, S, X, Y, FY, Size_X):- (south(S), not(north(N))); (is_finish(Size_X, X, Y, FY)).
validate_s3_up_und([1, 0, 1, 0, 1], N, S, X, Y, FY, Size_X):- (not(south(S)), not(north(N))); (is_finish(Size_X, X, Y, FY)).
validate_s3_up_und([1, 1, 0, 1, 0], N, S, X, Y, FY, Size_X):- (south(S), north(N)); (is_finish(Size_X, X, Y, FY)).

%check_s3_up([], _, _, _, _, _).
%check_s3_up([H|T], X, Y, SX, FY, Size_X):- validate_s3_up(H, X, Y, SX, FY, Size_X), Y1 is Y+1, check_s3_up(T, X, Y1, SX, FY, Size_X).

validate_s3_up([0,_,_,_,_], N, _, _, _, _):- not(north(N)).
validate_s3_up([1, 1, 0, 0, 1], N, X, Y, FY, Size_X):- (north(N)); (is_finish(Size_X, X, Y, FY)).
validate_s3_up([1, 1, 1, 0, 0], N, X, Y, FY, Size_X):- (north(N)); (is_finish(Size_X, X, Y, FY)).
validate_s3_up([1, 0, 0, 1, 1], N, X, Y, FY, Size_X):- (not(north(N))); (is_finish(Size_X, X, Y, FY)).
validate_s3_up([1, 0, 1, 1, 0], N, X, Y, FY, Size_X):- (not(north(N))); (is_finish(Size_X, X, Y, FY)).
validate_s3_up([1, 0, 1, 0, 1], N, X, Y, FY, Size_X):- (not(north(N))); (is_finish(Size_X, X, Y, FY)).
validate_s3_up([1, 1, 0, 1, 0], N, X, Y, FY, Size_X):- (north(N)); (is_finish(Size_X, X, Y, FY)).

validate_s3_left([0,_,_,_,_], W, _, _, _):- not(west(W)).
validate_s3_left([1, 1, 0, 0, 1], W, X, Y, SX):- (west(W)) ; (is_start(X, Y, SX)).
validate_s3_left([1, 1, 1, 0, 0], W, X, Y, SX):- (not(west(W))) ; (is_start(X, Y, SX)).
validate_s3_left([1, 0, 0, 1, 1], W, X, Y, SX):- (west(W)) ; (is_start(X, Y, SX)).
validate_s3_left([1, 0, 1, 1, 0], W, X, Y, SX):- (not(west(W))) ; (is_start(X, Y, SX)).
validate_s3_left([1, 0, 1, 0, 1], W, X, Y, SX):- (west(W)) ; (is_start(X, Y, SX)).
validate_s3_left([1, 1, 0, 1, 0], W, X, Y, SX):- (not(west(W))) ; (is_start(X, Y, SX)).

is_start(X, 1, X).
is_finish(X, X, Y, Y).

s2(Rows, Columns):- transpose(Rows, Columns).

validate_S12(List, Hint):- sum_elements_s12(List, Sum), Sum #= Hint.

validate_S3_row(List, Hint):- sum_elements_s3(List, Sum), Sum #= Hint.

validate_all_S12([], []).
validate_all_S12([H1|T1], [H2|T2]):- validate_S12(H1, H2), validate_all_S12(T1, T2).

s1_w_coordinates(S1, S1_w_c):- get_s1_w_c_rows(S1, S1_w_c, 1).

get_s1_w_c_rows([], [], _).
get_s1_w_c_rows([H|T], [H1|T1], X):- get_s1_w_c_row(H, H1, X, 1), X1 is X+1, get_s1_w_c_rows(T, T1, X1).

get_s1_w_c_row([], [], _, _).
get_s1_w_c_row([H|T], [H1|T1], X, Y):- H1 = [H, X, Y], Y1 is Y+1, get_s1_w_c_row(T, T1, X, Y1).

s3(Grid, S3):- get_s1_w_c_rows(Grid, S1, 1), s3_rows(S1, S3, Grid).

s3_rows([], [], _).
s3_rows([H|T], [H1|T1], Grid):- s3_row(H, H1, Grid), s3_rows(T, T1, Grid).

s3_row([], [], _).
s3_row([[Status, X, Y]|T], [[Status, North, East, South, West]|T1], S1):-
                                          X1 is X-1, Y1 is Y+1, X2 is X+1, Y2 is Y-1,
                                          find_element(S1, X1, Y, North),
                                          find_element(S1, X, Y1, East),
                                          find_element(S1, X2, Y, South),
                                          find_element(S1, X, Y2, West),
                                          s3_row(T, T1, S1).

find_element(S1, X, Y, Element):- length(S1, MaxX),
                                  nth1(1, S1, FirstRow),
                                  length(FirstRow, MaxY),

                                  (between(1, MaxX, X), between(1, MaxY, Y)
                                  ->  nth1(X, S1, Row),
                                      nth1(Y, Row, Element)
                                  ;   Element = []).

enough_imp(S1, N):- count_zeros(S1, ZeroCount), N #= ZeroCount.

count_zeros(Grid, ZeroCount) :-
    findall(0, (member(Row, Grid), member(0, Row)), Zeros),
    length(Zeros, ZeroCount).

first_row_valid([Row|_]):- south(A), member(A, Row).


create_block_grid([_|[]], []).
create_block_grid([R1, R2|T], [H|T1]):- create_block_row(R1, R2, H), create_block_grid([R2|T], T1).

create_block_row([_|[]], [_|[]], []).
create_block_row([H1, H2|T1], [H3, H4|T2], [[H1, H2, H3, H4]|T3]):- create_block_row([H2|T1], [H4|T2], T3).

validate_block_grid([]).
validate_block_grid([H|T]):- validate_block_row(H), validate_block_grid(T).

validate_block_row([]).
validate_block_row([H|T]):- not(is_loop(H)), validate_block_row(T).

is_loop([[1, 0, 1, 1, 0], [1, 0, 0, 1, 1], [1, 1, 1, 0, 0], [1, 1, 0, 0, 1]]).

sum_elements_s3([], 0).
sum_elements_s3([[[H,_,_,_,_]|_]|T], Sum):- sum_elements_s3(T, Sum1), (H \= 0 -> Sum is Sum1 + 1; Sum is Sum1).

sum_elements_s12([], 0).
sum_elements_s12([[H,_,_,_,_]|T], Sum):- sum_elements_s12(T, Sum1), (H \= 0 -> Sum is Sum1 + 1; Sum is Sum1).

run_through_loop(S1, Sum_Elements, Size_X, SX, FY):- nth1(SX, S1, [H|_]), Sum1 is Sum_Elements - 1, loop_finder(S1, H, Sum1, west, SX, 1, Size_X, FY).

loop_finder(S1, Track, Sum, Inc, X, Y, Size_X, FY):- (Sum = 0, is_finish(Size_X, X, Y, FY)); 
                                  (Sum > 0, next_direction(Inc, Track, Out), next_coordinates(Out, NX, NY),
                                  X1 is X + NX, Y1 is Y + NY, nth1(X1, S1, R), nth1(Y1, R, Element), Sum1 is Sum-1,
                                  loop_finder(S1, Element, Sum1, Out, X1, Y1, Size_X, FY)).

%next_direction(Inc, [_, N, E, S, W], Out).
next_direction(north, [_, 1, E, S, W], Out):- (E = 1 -> Out = west); (W = 1 -> Out = east); (S = 1 -> Out = north).
next_direction(east, [_, N, 1, S, W], Out):- (N = 1 -> Out = south); (W = 1 -> Out = east); (S = 1 -> Out = north).
next_direction(south, [_, N, E, 1, W], Out):- (E = 1 -> Out = west); (W = 1 -> Out = east); (N = 1 -> Out = south).
next_direction(west, [_, N, E, S, 1], Out):- (E = 1 -> Out = west); (S = 1 -> Out = north); (N = 1 -> Out = south).

next_coordinates(north, 1, 0).
next_coordinates(east, 0, -1).
next_coordinates(south, -1, 0).
next_coordinates(west, 0, 1).