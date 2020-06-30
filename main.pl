:-use_module(tables, [table_name/2]).
:-use_module(check_predicates, [grades_and_ratings/3, movies_and_ratings/3, check_names/3, names_and_ratings_pred/3]).

%% Functii date in enuntul temei

plus5(X,Y):- Y is X + 5.

make_format_str(MaxRowLen,Str) :-
    maplist(plus5,MaxRowLen,Rp), aux_format(Rp,Str).
aux_format([H],R) :- string_concat("~t~w~t~",H,R1),
    string_concat(R1,"+~n",R),!.
aux_format([H|T],R) :- string_concat("~t~w~t~",H,R1),
    string_concat(R1,"+ ",R2)
    ,aux_format(T,Rp)
    ,string_concat(R2,Rp,R).

% Functii din laboratoare si cursuri

csp(_,_,[],[]).
csp(X, Pr, [H|T], R) :- not((X = H, Pr)), csp(X,Pr,T,R), !.
csp(X, Pr, [H|T], [H|R]) :- csp(X,Pr,T,R).

repeat(R,1,[R]).
repeat(R,N,[R|L]) :- N>0 , Np is N-1 , repeat(R,Np,L).

% Functii auxiliare

max3(A,B,A):- A>=B.
max3(A,B,B):- B>A.

mymap(_,_,[],[]).
mymap(Pred2,El,[H|List],R) :- call(Pred2,El,H,Rez) , R = [Rez,Rp] , mymap(Pred2,El,List,Rp) .

zeros(N,L) :- repeat(0,N,L).

indexOf(_,[],-1).
indexOf(E,[E|_],0).
indexOf(E,[_|T],N) :- indexOf(E,T,Np) , ! , N is Np+1.

indices([],_,[]).
indices([H|T],List,[H2|T2]) :- indexOf(H,List,Indx) , H2 is Indx , !, indices(T,List,T2) .

reord([],_,[]).
reord([Int|Ints],T,R) :- nth0(Int,T,Rez) , R = [Rez|Rp] , reord(Ints,T,Rp ) . 
reord([(-1)|Ints],T,R) :- reord(Ints,T,R) . 

preselect([],_,[]).
preselect([H|T],Ints,R) :- reord(Ints,H,Rez) , R = [Rez|Rp] ,preselect(T,Ints,Rp ) .

% Functii pt tabele

cell_length(L,R):-string(L),string_length(L,R).
cell_length(N,R):-number(N),number_string(N,L),cell_length(L,R).

entry_lengths(L1,Rez):-maplist(cell_length,L1,Rez).

maximise([H|T],R) :- length(H, Len) ,zeros(Len,Z), lengths([H|T],Z,R).

changes(Len,H,R) :- entry_lengths(H,R1),maplist(max3,Len,R1,R).

lengths([H],Len,R):- changes(Len,H,R).
lengths([H|T],Len,R):- changes(Len,H,R2),lengths(T,R2,R).

myFormat(Elem,MaxRowLen) :- make_format_str(MaxRowLen,R),format(R,Elem).

format_through([H],MaxRowLen):- myFormat(H,MaxRowLen).
format_through([H|T],MaxRowLen):- myFormat(H,MaxRowLen) ,format_through(T,MaxRowLen).

%% Afisare Tabel

print_table_op(T):-maximise(T,R),format_through(T,R).

%% 1.3

% Operation Join

join_op(Op, Cols,[_|T1], [_|T2], [Cols|R] ) :- maplist(Op,T1,T2,R).

% Operation Select

select_op([Hr|En],Col,[Col|R]) :- indices(Col , Hr ,Rez) , preselect(En,Rez,R) .

% Operation filter

filter_op(Table,Vars,Pred,Rez) :- csp(Vars,Pred,Table,Rez) .

% Type Query
eval([H|T],[H|T]).
eval(table(Str),R):-table_name(Str,R) .
eval(tprint(Q),_):- eval(Q,T),print_table_op(T) .
eval(select(Col,Q),R):-eval(Q,T) , select_op(T,Col,R) .
eval(join(Pred, Cols, Q1, Q2),R) :- eval(Q1,T1) ,eval(Q2,T2) , join_op(Pred,Cols,T1,T2,R) .
eval(tfilter(S,G,Q),[H|R]) :- eval(Q,[H|T]) , filter_op(T,S,G,R).
%aux
eval(complex_query1(Q),R) :- eval(
    tfilter(
        [_,_,AA,PP,_,_,_],
            AA+PP > 12, 
    tfilter(
        [_,_,AA,PP,PC,PA,POO],
            AA+PP+PC+PA+POO > 25,
    tfilter(
        [_,Name,_,_,_,_,_],
            escu(Name),
    Q ) )),R).
eval(complex_query2(Genre,MinRating,MaxRating),R) :- gimme(R1),eval(
    tfilter(
        [_,_,Gen,_],
            sub_string(Gen,_,_,_,Genre),
    tfilter(
        [_,_,_,Rat],
            Rat >= MinRating,
    tfilter(
        [_,_,_,Rat],
            Rat =< MaxRating,
    join(append,
        ["movie_id","title","genres","rating"],
        select(["movie_id","title","genres"],table(movies)),
        select(["rating"],R1)
        )
    ) %3 filter
    ) %2 filter
    ) %1 filter
    ,R ).
% Functii String-uri

suffix4(4,S,S).
suffix4(N,[_|T],R) :- N>4 , Np is N-1 , suffix4(Np,T,R).

last4(A,R) :- string_chars(A, C),
    length(C,N),
    suffix4(N,C,Cr) , string_chars(R,Cr).

escu(Name) :- last4(Name,Escu) , Escu = "escu".

matches(_,[],[]).
matches(ID,[[F1,F2,ID,F3]|_], [F1,F2,ID,F3]) :- !.
matches(ID,[[_,_,_,_]|Rest],Rez) :- matches(ID,Rest,Rez).

id_is3th([],_,[]).
id_is3th([[ID]|IDS],Table,R) :- matches(ID,Table,Rez) , R = [Rez|Rp] , id_is3th(IDS,Table,Rp).

gimme([H2|R]):-eval(select(["movie_id"],table(movies)),[_|T1]),table_name(ratings,[H2|T2]),id_is3th(T1,T2,R).