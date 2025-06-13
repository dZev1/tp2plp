:- use_module(piezas).

% Completar ...

%! sublista(+Descartar, +Tomar, +L, -R)

sublista(Descartar, Tomar, L, R) :- 
    append(Descartados, Restantes, L), 
    length(Descartados, Descartar), 
    append(R, _, Restantes),
    length(R,Tomar).

%! cantColumnas(+K, +L) : devuelve true sii la lista L tiene tamaño K
cantColumnas(K, L) :- length(L, K).

%! tablero(+K, -T)

tablero(K, T) :-
    length(T, 5),
    maplist(cantColumnas(K), T).

%! tamaño(+M, -F, -C)

tamaño(M,F,C) :-
    length(M, F),
    maplist(cantColumnas(C), M).

%! coordenadas(+T, -IJ)

coordenadas(T, (I,J)) :-
    nth1(I, T, Fila),
    nth1(J, Fila, _).

%! kPiezas(+K, -PS)

kPiezas(K, PS) :-
    K is K,
    PS is PS.

%! extraerColumnas(+I, +C, +F, -SF)

extraerColumnas(I, C, F, SF) :-
    I1 is I - 1,
    sublista(I1, C, F, SF).

%! seccionTablero(+T, +ALTO, +ANCHO, +IJ, ?ST)

seccionTablero(T, ALTO, ANCHO, (I, J), ST) :-
    I1 is I - 1,
    sublista(I1, ALTO, T, Filas),
    maplist(extraerColumnas(J, ANCHO), Filas, ST).

