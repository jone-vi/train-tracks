:- set_prolog_flag(encoding, utf8).
:- include('brute.pro').

/********************* default files */
inputFile('unsolved.txt').
outputFile('solved.txt').

/********************** writing the output */
writePuzzle(Y, X, Grid, HintRow, HintCol):-
    format("size ~wx~w~n", [X, Y]), 
    maplist(writeHintCol, HintCol),
    nl,
    writeGrid(Grid, HintRow).

writeHintCol(Hint) :-
    format("~w ", [Hint]).

writeGrid([], []).
writeGrid([Row|Rows], [Hint|Hints]):-
    writeRow(Row),
    format("~w~n", [Hint]),
    writeGrid(Rows, Hints).

writeRow([]).
writeRow([Cell|Cells]):-
    mapCellToChar(Cell, Char),  
    format("~c ", [Char]),      
    writeRow(Cells).

mapCellToChar([1, 0, 1, 0, 1], 9552).  % Horizontal
mapCellToChar([1, 1, 0, 1, 0], 9553).  % Vertical
mapCellToChar([1, 1, 0, 0, 1], 9565).  % NW
mapCellToChar([1, 1, 1, 0, 0], 9562).  % NE
mapCellToChar([1, 0, 0, 1, 1], 9559).  % SW
mapCellToChar([1, 0, 1, 1, 0], 9556).  % SE
mapCellToChar(20, 50). %midlertidig testdata
mapCellToChar(30, 50). %midlertidig testdata
mapCellToChar([0, 0, 0, 0, 0], 32).    % Everything else to space

/********************** reading the input */
readProblem(Id):- 
  findKW(size), 
  readInt(X), 
  readInt(Y), 
  readHintLine(X,HintCol,X,Y,_,SX,FY),
  peek_code(M),
  (
    M = 32 -> get_code(_);
    M = 10 -> get_code(_);
    true
    ),
  readLines(X, Y, Grid, HintRow, Y,SX,FY),
  assertz(puzzle(Id,X,Y,Grid,HintRow,HintCol,SX,FY)).

findKW(KW):- 
    string_codes(KW,[H|T]), 
    peek_code(H), 
    readKW([H|T]), 
    !.

findKW(_):- 
    peek_code(-1), 
    !, 
    fail.

findKW(KW):- 
    get_code(_), 
    findKW(KW).

readKW([]):- 
    get_code(_).

readKW([H|T]):- 
    get_code(H), 
    readKW(T).

readHintLine(0,[],_,_,_,_,_).

readHintLine(N,[Hint|Hints],X,Y,YC,SX,FY):- 
    N > 0, 
    N1 is N - 1, 
    get_code(C), 
    (   
        C >= 48, C =< 57 -> 
        D is C - 48, 
        peek_code(C2), % Hvis det er 2 sifre i tall
        (   C2 >= 48, C2 =< 57 ->
            get_code(C2),
            D2 is C2 - 48,
            Hint is D * 10 + D2
        ;   Hint = D
        )
        %Cases for start/finish verdi
    ;   C = 9552, N=X -> Hint = [1, 0, 1, 0, 1], SXFY is YC+1-Y, FYSX is X+1-N %Venstre
    ;   C = 9565, N=X -> Hint = [1, 1, 0, 0, 1], SXFY is YC+1-Y, FYSX is X+1-N %Venstre
    ;   C = 9559, N=X -> Hint = [1, 0, 0, 1, 1], SXFY is YC+1-Y, FYSX is X+1-N %Venstre
    ;   C = 9552, N=1 -> Hint = [1, 0, 1, 0, 1], SXFY is YC+1-Y, FYSX is X+1-N %Høyre
    ;   C = 9562, N=1 -> Hint = [1, 1, 1, 0, 0], SXFY is YC+1-Y, FYSX is X+1-N %Høyre
    ;   C = 9556, N=1 -> Hint = [1, 0, 1, 1, 0], SXFY is YC+1-Y, FYSX is X+1-N %Høyre
    ;   C = 9553, Y=YC -> Hint = [1, 1, 0, 1, 0], SXFY is YC+1-Y, FYSX is X+1-N %Topp
    ;   C = 9565, Y=YC -> Hint = [1, 1, 0, 0, 1], SXFY is YC+1-Y, FYSX is X+1-N %Topp
    ;   C = 9562, Y=YC -> Hint = [1, 1, 1, 0, 0], SXFY is YC+1-Y, FYSX is X+1-N %Topp
    ;   C = 9553, Y=1 -> Hint = [1, 1, 0, 1, 0], SXFY is YC+1-Y, FYSX is X+1-N %Bunn
    ;   C = 9559, Y=1 -> Hint = [1, 0, 0, 1, 1], SXFY is YC+1-Y, FYSX is X+1-N %Bunn
    ;   C = 9556, Y=1 -> Hint = [1, 0, 1, 1, 0], SXFY is YC+1-Y, FYSX is X+1-N %Bunn
    ;

        %Cases for noder
        C = 9552 -> Hint = [1, 0, 1, 0, 1]  % Horizontal
    ;   C = 9553 -> Hint = [1, 1, 0, 1, 0]  % Vertical
    ;   C = 9565 -> Hint = [1, 1, 0, 0, 1]  % NW
    ;   C = 9562 -> Hint = [1, 1, 1, 0, 0]  % NE
    ;   C = 9559 -> Hint = [1, 0, 0, 1, 1]  % SW
    ;   C = 9556 -> Hint = [1, 0, 1, 1, 0]  % SE
    ;   C = 32   -> Hint = [_, _, _, _, _]  % Mellomrom
    ;   Hint = _              % Feil
    ),
    (
        nonvar(SXFY) -> 
        (
            var(SX) -> SX = SXFY
        ;   nonvar(SX) -> FY = FYSX
        )
    ;   true
    ),
    get_code(_), 
    readHintLine(N1, Hints,X,Y,YC,SX,FY).

readLines(_,0,[],[],_,_,_).
readLines(X,Y,[Row|Rows], [Hint|Hints], YC,SX,FY):- 
    Y > 0, 
    Y1 is Y - 1, 
    readHintLine(X,Row,X,Y,YC,SX,FY), 
    readHintLine(1,[Hint],X,Y,YC,SX,FY),
    readLines(X,Y1,Rows,Hints,YC,SX,FY).

readInt(N):- 
    get_code(M), 
    handleCode(M,N).

handleCode(M,N):- 
    is_number_code(M,N1), 
    !, 
    continueInt(N1,N).

handleCode(-1,_):- 
    !, 
    fail. /* EOF */

handleCode(_,N):- 
    readInt(N).

continueInt(O,N):- 
    get_code(M), 
    is_number_code(M,M1), 
    !, H is 10*O+M1, 
    continueInt(H,N).
continueInt(N,N).

is_number_code(N, N1):- 
    N>=48, N<58, N1 is N-48.
is_number_code(95,0).

/*********************** global control: starting the algorithm and the reading */
input_output(IF,OF):- 
    current_prolog_flag(argv,['--io',IF,OF]),
    !.
input_output(IF,OF):- 
    inputFile(IF), 
    outputFile(OF).

run:- 
    seeing(X), 
    write(seeing(X)), 
    nl, 
    fail.
run:- 
    input_output(IF,OF), 
    write("Reading from: "), 
    write(IF), 
    write(", writing to: "), 
    write(OF), 
    nl, 
    see(IF), 
    tell(OF), 
    readInt(N), 
    write('puzzles '), 
    write(N), 
    nl, 
    solvePuzzles(N), 
    told, 
    seen,
    %solvePuzzles(N),
    !. 
run:- told, seen. /* close the files in case of errors */

    
solvePuzzles(0).
solvePuzzles(N):- 
    N > 0, 
    readProblem(N),
    solve3(N),
    N1 is N - 1, 
    solvePuzzles(N1).
    
solve3(PuzzleId):- puzzle(PuzzleId, Size_Y, Size_X, S1, HR, HC, SX, FY),
                % Our code cant solve larger puzzles than 10x10
                % -------------------------------------------
                Size_X < 11,
                Size_Y < 11,
                % -------------------------------------------
                sum_list(HC, Sum_Elements),
                s2(S1, S2),
                s3(S1, S3),
                create_block_grid(S1, S1_block),
                !,
                validate_grid(S2, S3, HC, HR, 1, SX, FY, Size_Y, Size_X),
                validate_all_S12(S2, HC),
                validate_block_grid(S1_block),
                run_through_loop(S1, Sum_Elements, Size_X, SX, FY),
                writePuzzle(Size_X, Size_Y, S1, HR, HC),
                !.

:- run.
:- halt.
