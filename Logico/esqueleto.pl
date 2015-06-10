% Auxiliares

%% posValida(+Pos, +Tablero) sera verdadero si Pos pertenece dentro 
%% de los limites del tablero.
posValida(pos(X,Y), T) :-
  length(T, F), nth0(0, T, L), length(L, C),
  between(1, F, R), between(1, C, K),
  X is R - 1, Y is K - 1.

%% getPos(+Pos, +Tablero, -Value) sera verdadero cuando Value sea
%% el valor de la celda Pos en el Tablero.
getPos(pos(X, Y), T, Value) :-
  posValida(pos(X, Y), T), nth0(X, T, L), nth0(Y, L, Value), !.

%% boardSize(+T, -N) sera verdadero cuando N sea la cantidad
%% de filas por columnas del tablero.
boardSize(T, N) :-
  length(T, F),
  nth0(0, T, Row),
  length(Row, C),
  N is F*C.

%% nonmember(+Element, +List) check that Element is not a member of List.
nonmember(Arg,[Arg|_]) :- !, fail.
nonmember(Arg,[_|Tail]) :- !, nonmember(Arg,Tail).
nonmember(_,[]).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(0,_,[]).
tablero(F,C,[L|T]) :-
  length(L, C),
  Anterior is F - 1,
  tablero(Anterior, C, T), !.


%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(P, T) :- getPos(P, T, ocupada).


%% Ejercicio 3
%% vecino(+Pos, +Tablero, -PosVecino) será verdadero cuando PosVecino sea
%% un átomo de la forma pos(F', C') y pos(F',C') sea una celda contigua a
%% pos(F,C), donde Pos=pos(F,C). Las celdas contiguas puede ser a lo sumo cuatro
%% dado que el robot se moverá en forma ortogonal.

vecino(pos(F, C), T, pos(X, C)) :- X is F - 1, posValida(pos(X, C), T).
vecino(pos(F, C), T, pos(X, C)) :- X is F + 1, posValida(pos(X, C), T).
vecino(pos(F, C), T, pos(F, Y)) :- Y is C - 1, posValida(pos(F, Y), T).
vecino(pos(F, C), T, pos(F, Y)) :- Y is C + 1, posValida(pos(F, Y), T).


%% Ejercicio 4
%% vecinoLibre(+Pos, +Tablero, -PosVecino) idem vecino/3 pero además PosVecino
%% debe ser una celda transitable (no ocupada) en el Tablero
vecinoLibre(P,T,Q) :- vecino(P, T, Q), getPos(Q, T, Z), var(Z).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Definicion de caminos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 5
%% camino(+Inicio, +Fin, +Tablero, -Camino) será verdadero cuando Camino sea una lista
%% [pos(F1,C1), pos(F2,C2),..., pos(Fn,Cn)] que denoten un camino desde Inicio
%% hasta Fin pasando solo por celdas transitables.
%% Además se espera que Camino no contenga ciclos.
%% Notar que la cantidad de caminos es finita y por ende se tiene que poder recorrer
%% todas las alternativas eventualmente.
%% Consejo: Utilizar una lista auxiliar con las posiciones visitadas
% camino(I,F,T,P) :- nth0(0, P, I), last(P, F).
camino(I,F,T,P) :- camino(I, F, T, P, [I]).

camino(F,F,_T, [F], _Used).
camino(I,F,T, [I | Path], Used) :-
  vecinoLibre(I, T, Vecino),
  nonmember(Vecino, Used),
  camino(Vecino, F, T, Path, [Vecino | Used]).


%% Ejercicio 6
%% cantidadDeCaminos(+Inicio, +Fin, +Tablero, ?N) que indique la cantidad de caminos
%% posibles sin ciclos entre Inicio y Fin.
cantidadDeCaminos(I,F,T,N) :-
  findall(P, camino(I,F,T,P), L),
  length(L, N).


%% Ejercicio 7
%% camino2(+Inicio, +Fin, +Tablero, -Camino) ídem camino/4 pero se espera una heurística
%% que mejore las soluciones iniciales.
%% No se espera que la primera solución sea necesariamente la mejor.
%% Una solución es mejor mientras menos pasos se deba dar para llegar a
%% destino (distancia Manhattan). Por lo tanto, el predicado deberá devolver de a uno,
%% todos los caminos pero en orden creciente de longitud.

camino2(I,F,T,P) :- camino2(I, F, T, P, [I]).

camino2(F,F,_T, [F], _Used).
camino2(I,F,T, [I | Path], Used) :-
  vecinoLibre2(I, F, T, Vecino),
  nonmember(Vecino, Used),
  camino2(Vecino, F, T, Path, [Vecino | Used]).

vecinoLibre2(I,F,T,Q) :- vecino2(I, F, T, Q), getPos(Q, T, Z), var(Z).

vecino2(pos(F, C), pos(FE, _), T, pos(X, C)) :- X is F - 1, posValida(pos(X, C), T), FE < F.
vecino2(pos(F, C), pos(FE, _), T, pos(X, C)) :- X is F + 1, posValida(pos(X, C), T), FE > F.
vecino2(pos(F, C), pos(_, CE), T, pos(F, Y)) :- Y is C - 1, posValida(pos(F, Y), T), CE < C.
vecino2(pos(F, C), pos(_, CE), T, pos(F, Y)) :- Y is C + 1, posValida(pos(F, Y), T), CE > C.
vecino2(pos(F, C), pos(_, CE), T, pos(F, Y)) :- Y is C - 1, posValida(pos(F, Y), T), CE = C.
vecino2(pos(F, C), pos(_, CE), T, pos(F, Y)) :- Y is C + 1, posValida(pos(F, Y), T), CE = C.
vecino2(pos(F, C), pos(FE, _), T, pos(X, C)) :- X is F - 1, posValida(pos(X, C), T), FE > F.
vecino2(pos(F, C), pos(FE, _), T, pos(X, C)) :- X is F + 1, posValida(pos(X, C), T), FE < F.
vecino2(pos(F, C), pos(FE, _), T, pos(X, C)) :- X is F - 1, posValida(pos(X, C), T), FE = F.
vecino2(pos(F, C), pos(FE, _), T, pos(X, C)) :- X is F + 1, posValida(pos(X, C), T), FE = F.
vecino2(pos(F, C), pos(_, CE), T, pos(F, Y)) :- Y is C - 1, posValida(pos(F, Y), T), CE > C.
vecino2(pos(F, C), pos(_, CE), T, pos(F, Y)) :- Y is C + 1, posValida(pos(F, Y), T), CE < C.

%% Calcular todos los caminos de distancia 0.. F*C ir dandolos en orden.
camino2_alternativo(I,F,T,P) :-
  boardSize(T, BoardSize),
  between(1, BoardSize, Length),
  length(P, Length),
  camino(I,F,T,P).


%% Ejercicio 8
%% camino3(+Inicio, +Fin, +Tablero, -Camino) ídem camino2/4 pero se espera que
%% se reduzca drásticamente el espacio de búsqueda.
%% En el proceso de generar los potenciales caminos, se pueden ir sacando algunas conclusiones.
%% Por ejemplo, si se está en la celda (3,4) y se dieron ya 6 pasos desde el Inicio,
%% entonces no tiene sentido seguir evaluando cualquier camino que implique llegar a la celda (3,4)
%% desde Inicio en más de 6 pasos.
%% Notar que dos ejecuciones de camino3/4 con los mismos argumentos deben dar los mismos resultados.
%% En este ejercicio se permiten el uso de predicados: dynamic/1, asserta/1, assertz/1 y retractall/1.

camino3(I,F,T,P) :-
  retractall(masCorto(_,_)),
  asserta(masCorto(_,D) :- D is 0),
  camino3(I, F, T, P, [I]).

camino3(F,F,_T, [F], _Used) :- !.
camino3(I,F,T, [I | Path], Used) :-
  
  vecinoLibre3(I,F,T,Vecino,Used),
  
  camino3(Vecino, F, T, Path, [Vecino | Used]).

%% BORRAME
buscarCamino3(C) :- tablero(ej5x5,T),  camino3(pos(0,0),pos(3,3),T,C).

vecinoLibre3(I,F,T,Vecino,Used) :-
  
  vecinoLibre2(I,F,T,Vecino),
  nonmember(Vecino, Used),
  
  noHayMasCorto(Vecino,Used),
  length(Used,L),
  asserta(masCorto(Vecino,L):-!).

noHayMasCorto(Q,_U) :- masCorto(Q,D), X is 0, D==X, !.
noHayMasCorto(Q,U) :- masCorto(Q,D), length(U,L), L=<D, !.

:- dynamic
  masCorto/2.

masCorto(_,D) :- D is 0.


%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 9
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Nota: Es posible una implementación que resuelva en forma inmediata casos en los que trivialmente no existe camino dual posible.

caminoDual(I,F,T1,T2,P) :- T1 = T2, camino(I, F, T1, P).


%%%%%%%%%%% Tableros Predefinidos %%%%%%%%%%%

tablero(ej5x5, T) :-
  tablero(5, 5, T),
  ocupar(pos(1, 1), T),
  ocupar(pos(1, 2), T).
%%| | | | | |
%%| |O|O| | |
%%| | | | | |
%%| | | | | |

tablero(imposible, T) :-
  tablero(3,3,T),
  ocupar(pos(1,0), T),
  ocupar(pos(0,1), T),
  ocupar(pos(1,1), T),
  ocupar(pos(2,1), T),
  ocupar(pos(1,2), T).
%%| |O| |
%%|O|O|O|
%%| |O| |

tablero(dual1, T) :-
  tablero(4,4,T),
  ocupar(pos(0,3), T),
  ocupar(pos(1,2), T),
  ocupar(pos(2,0), T),
  ocupar(pos(3,1), T),
  ocupar(pos(3,2), T).
%%| | | |O|
%%| | |O| |
%%|O| | | |
%%| |O|O| |

tablero(dual2, T) :-
  tablero(4,4,T),
  ocupar(pos(0,1), T).
%%| |O| | |
%%| | | | |
%%| | | | |
%%| | | | |

tablero(libre3x3, T) :- tablero(3, 3, T).
tablero(libre20, T) :- tablero(20, 20, T).

%%%%%%%%%%% Tests %%%%%%%%%%%
tests :- forall(between(1, 16, N), test(N)).

% Test vecino
test(1) :-
  tablero(imposible, T), !,
  vecino(pos(0,0),T,pos(1,0)),
  vecino(pos(0,0),T,pos(0,1)),
  not(vecino(pos(0,0),T,pos(1,1))),
  not(vecino(pos(0,0),T,pos(-1,0))),
  not(vecino(pos(0,0),T,pos(0,-1))).

% Test vecinoLibre
test(2) :-
  tablero(imposible, T), !,
  not(vecinoLibre(pos(0,0),T,pos(1,0))),
  not(vecinoLibre(pos(0,0),T,pos(0,1))).

test(3) :-
  tablero(libre3x3, T), !,
  vecinoLibre(pos(1,1), T, pos(0,1)),
  vecinoLibre(pos(1,1), T, pos(1,0)),
  vecinoLibre(pos(1,1), T, pos(1,2)),
  vecinoLibre(pos(1,1), T, pos(2,1)).

% Test camino
test(4) :-
  tablero(libre3x3, T), !,
  camino(pos(0,0), pos(2,2),T,C),
  C = [pos(0, 0), pos(1, 0), pos(2, 0), pos(2,1), pos(1,1), pos(0,1), pos(0,2), pos(1,2), pos(2,2)].

test(5) :-
  tablero(libre3x3, T), !,
  camino(pos(0,0), pos(2,2),T,C),
  C = [pos(0, 0), pos(1, 0), pos(1, 1), pos(2,1), pos(2,2)].

% Test cantidadDeCaminos
test(6) :-
  tablero(ej5x5, T), !,
  cantidadDeCaminos(pos(0,0), pos(2,3), T, 287).

test(7) :-
  tablero(imposible, T), !,
  cantidadDeCaminos(pos(0,0), pos(2,2), T, 0).

% Test camino2
test(8) :-
  tablero(ej5x5, T), !,
  camino2(pos(0,0), pos(2,3), T, P),
  P = [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2), pos(2, 3)].

test(9) :-
  tablero(ej5x5, T), !,
  camino2(pos(0,0), pos(2,3), T, P),
  length(P, 8).

test(10) :-
  tablero(ej5x5, T), !,
  findall(P, camino2(pos(0,0),pos(2,3),T,P), L),
  length(L, 287).

test(11) :-
  tablero(libre3x3, T), !,
  camino2(pos(0,0), pos(2,2),T,C),
  C = [pos(0, 0), pos(1, 0), pos(1, 1), pos(2,1), pos(2,2)].

% Test camino3
test(12) :-
  tablero(ej5x5, T), !,
  camino3(pos(0,0), pos(2,3), T, P),
  P = [pos(0, 0), pos(1, 0), pos(2, 0), pos(2, 1), pos(2, 2), pos(2, 3)].

test(13) :-
  tablero(ej5x5, T), !,
  camino3(pos(0,0), pos(2,3), T, P),
  length(P, N),
  not(N > 6).

test(14) :-
  tablero(ej5x5, T), !,
  findall(P, camino3(pos(0,0),pos(2,3),T,P), L),
  length(L, 2).

test(15) :-
  tablero(libre3x3, T), !,
  camino3(pos(0,0), pos(2,2),T,C),
  C = [pos(0, 0), pos(1, 0), pos(1, 1), pos(2,1), pos(2,2)].

% Test caminoDual
test(16) :-
  tablero(dual1, T1), tablero(dual2,T2), !,
  caminoDual(pos(0,0),pos(1,3),T1,T2,P),
  P = [pos(0, 0), pos(1, 0), pos(1, 1), pos(2, 1), pos(2, 2), pos(2, 3), pos(1, 3)].
