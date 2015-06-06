% Auxiliares

desde(X, Y) :- nonvar(Y), Y > X.
desde(X, Y) :- var(Y), N is X + 1, desde(N, Y).

tablero(ej5x5, T) :-
  tablero(5, 5, T),
  ocupar(pos(1, 1), T),
  ocupar(pos(1, 2), T).

tablero(libre20, T) :- tablero(20, 20, T).

posValida(pos(X,Y), T) :-
  length(T, F), nth0(0, T, L), length(L, C),
  between(1, F, R), between(1, C, K),
  X is R - 1, Y is K - 1.

getPos(pos(X, Y), T, P) :-
  posValida(pos(X, Y), T), nth0(X, T, L), nth0(Y, L, P), !.

nonmember(Arg,[Arg|_]) :- !, fail.
nonmember(Arg,[_|Tail]) :- !, nonmember(Arg,Tail).
nonmember(_,[]).

%%% CAMINO 3
buscarCamino3(C) :- tablero(ej5x5,T), camino3(pos(0,0),pos(3,3),T,C).

vecinoLibre3(P,T,Q,U) :-
	vecinoLibre(P, T, Q),
	nonmember(Q, U),
	noHayMasCorto(Q,U),
	length(U,L),
	asserta(masCorto(Q,L)).

noHayMasCorto(Q,U) :- masCorto(Q,D), !, X is 0, D==X.
noHayMasCorto(Q,U) :- masCorto(Q,D), !, length(U,L), L<D.

:- dynamic
      masCorto/2.

masCorto(_,D) :- D is 0.

%%%


%%%%%%%%%%%%%%%%%%%%%%%%
%% Tablero
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 1
%% tablero(+Filas,+Columnas,-Tablero) instancia una estructura de tablero en blanco
%% de Filas x Columnas, con todas las celdas libres.
tablero(1,C,[L]) :- C > 0, length(L, C).
tablero(F,C,T) :-
  F > 1, tablero(1, C, Unario), Anterior is F - 1,
  tablero(Anterior, C, S), append(S, Unario, T).

% TODO: Si saco el append y lo mando a la definicion,
% se cuelga cuando uso ocupar


%% Ejercicio 2
%% ocupar(+Pos,?Tablero) será verdadero cuando la posición indicada esté ocupada.
ocupar(P, T) :- getPos(P, T, ocupada).

% TODO: Esto no es necesario hacerlo, pues no tenemos que dimensionar todos los tableros

%ocupar(pos(X, Y), tablero(Q, W, E)) :- var(tablero(Q, W, E)), desde(2, Limite),
%  CotaF is Limite - 1, between(1, CotaF, F),
%  F > X, C is Limite - F, C > Y,
%  tablero(F, C, T),
%  ocupar(pos(X, Y), T).



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

vecino2(pos(F, C), T, pos(FE, _), pos(X, C)) :- X is F - 1, posValida(pos(X, C), T), FE < F.
vecino2(pos(F, C), T, pos(FE, _), pos(X, C)) :- X is F + 1, posValida(pos(X, C), T), FE > F.
vecino2(pos(F, C), T, pos(_, CE), pos(F, Y)) :- Y is C - 1, posValida(pos(F, Y), T), CE < C.
vecino2(pos(F, C), T, pos(_, CE), pos(F, Y)) :- Y is C + 1, posValida(pos(F, Y), T), CE > C.
vecino2(pos(F, C), T, pos(_, CE), pos(F, Y)) :- Y is C - 1, posValida(pos(F, Y), T), CE = C.
vecino2(pos(F, C), T, pos(_, CE), pos(F, Y)) :- Y is C + 1, posValida(pos(F, Y), T), CE = C.
vecino2(pos(F, C), T, pos(FE, _), pos(X, C)) :- X is F - 1, posValida(pos(X, C), T), FE > F.
vecino2(pos(F, C), T, pos(FE, _), pos(X, C)) :- X is F + 1, posValida(pos(X, C), T), FE < F.
vecino2(pos(F, C), T, pos(FE, _), pos(X, C)) :- X is F - 1, posValida(pos(X, C), T), FE = F.
vecino2(pos(F, C), T, pos(FE, _), pos(X, C)) :- X is F + 1, posValida(pos(X, C), T), FE = F.
vecino2(pos(F, C), T, pos(_, CE), pos(F, Y)) :- Y is C - 1, posValida(pos(F, Y), T), CE > C.
vecino2(pos(F, C), T, pos(_, CE), pos(F, Y)) :- Y is C + 1, posValida(pos(F, Y), T), CE < C.

vecinoLibre2(P,T,F,Q) :- vecino2(P, T, F, Q), getPos(Q, T, Z), var(Z).



camino2(I,F,T,P) :- camino2(I, F, T, P, [I]).


camino2(F,F,_T, [F], _Used).
camino2(I,F,T, [I | Path], Used) :-
  vecinoLibre2(I, T, F, Vecino),
  nonmember(Vecino, Used),
  camino2(Vecino, F, T, Path, [Vecino | Used]).
  
  
  
%% Ejercicio 8
%% camino3(+Inicio, +Fin, +Tablero, -Camino) ídem camino2/4 pero se espera que
%% se reduzca drásticamente el espacio de búsqueda.
%% En el proceso de generar los potenciales caminos, se pueden ir sacando algunas conclusiones.
%% Por ejemplo, si se está en la celda (3,4) y se dieron ya 6 pasos desde el Inicio,
%% entonces no tiene sentido seguir evaluando cualquier camino que implique llegar a la celda (3,4)
%% desde Inicio en más de 6 pasos.
%% Notar que dos ejecuciones de camino3/4 con los mismos argumentos deben dar los mismos resultados.
%% En este ejercicio se permiten el uso de predicados: dynamic/1, asserta/1, assertz/1 y retractall/1.

camino3(I,F,T,P) :- camino3(I, F, T, P, [I]), retractall(masCorto(_,_)), asserta(masCorto(_,D) :- D is 0).

camino3(F,F,_T, [F], _Used).
camino3(I,F,T, [I | Path], Used) :-
  vecinoLibre3(I, T, Vecino,Used),
  nonmember(Vecino, Used),
  camino3(Vecino, F, T, Path, [Vecino | Used]).


%%%%%%%%%%%%%%%%%%%%%%%%
%% Tableros simultáneos
%%%%%%%%%%%%%%%%%%%%%%%%

%% Ejercicio 9
%% caminoDual(+Inicio, +Fin, +Tablero1, +Tablero2, -Camino) será verdadero
%% cuando Camino sea un camino desde Inicio hasta Fin pasando al mismo tiempo
%% sólo por celdas transitables de ambos tableros.
%% Nota: Es posible una implementación que resuelva en forma inmediata casos en los que trivialmente no existe camino dual posible.
caminoDual(_,_,_,_,_).
