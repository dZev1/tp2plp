:- use_module(piezas).

%! sublista(+Descartar, +Tomar, +L, -R)

sublista(Descartar, Tomar, L, R) :- 
    append(Descartados, Restantes, L), 
    length(Descartados, Descartar), 
    append(R, _, Restantes),
    length(R,Tomar).

%! cantColumnas(+K, -L)
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

% lista_de_piezas(+K, +L1, -L2)
lista_de_piezas(0,_,[]).
lista_de_piezas(Cantidad, [Pieza|RestoPiezas], [Pieza|RestoSalida]) :-
    Cantidad > 0,
    Cantidad2 is Cantidad -1,
    length(RestoPiezas,PiezasRestantes),
    Cantidad2 =< PiezasRestantes,
    lista_de_piezas(Cantidad2,RestoPiezas,RestoSalida).
lista_de_piezas(Cantidad, [_|RestoPiezas], Salida) :-
    Cantidad > 0,
    length(RestoPiezas,PiezasRestantes),
    Cantidad =< PiezasRestantes,
    lista_de_piezas(Cantidad,RestoPiezas,Salida).

%! kPiezas(+K, -PS)

kPiezas(K, PS) :-
    nombrePiezas(Letras),
    lista_de_piezas(K, Letras, PS).

%! extraerColumnas(+I, +C, +F, -SF)

extraerColumnas(I, C, F, SF) :-
    I1 is I - 1,
    sublista(I1, C, F, SF).

%! seccionTablero(+T, +ALTO, +ANCHO, +IJ, ?ST)

seccionTablero(T, ALTO, ANCHO, (I, J), ST) :-
    I1 is I - 1,
    sublista(I1, ALTO, T, Filas),
    maplist(extraerColumnas(J, ANCHO), Filas, ST).

%! ubicarPieza(+Tablero, +Identificador)

ubicarPieza(Tablero, Identificador) :-
    pieza(Identificador, Pieza),            
    tamaño(Pieza, F, C),
    tamaño(Tablero, 5, K),
    FilaValidas is 5 - F + 1,
    between(1, FilaValidas, I),
    ColumnasValidas is K - C + 1,
    between(1, ColumnasValidas, J),
    seccionTablero(Tablero, F, C, (I, J), ST),
    ST = Pieza.

%! poda(+Poda, +Tablero)
poda(sinPoda, _).
poda(podaMod5, T) :- todosGruposLibresModulo5(T).

%! ubicarPiezas(+Tablero, +Poda, +Identificadores)
ubicarPiezas(Tablero, Poda, Identificadores) :-
    poda(Poda, Tablero),
    maplist(ubicarPieza(Tablero), Identificadores).

%! llenarTablero(+Poda, +Columnas, -Tablero)
llenarTablero(Poda, Columnas, Tablero) :-
    tablero(Columnas, Tablero),
    kPiezas(Columnas, Identificadores),
    ubicarPiezas(Tablero, Poda, Identificadores).

%! cantSoluciones(+Poda, +Columnas, -N)
cantSoluciones(Poda, Columnas, N) :-
    findall(T, llenarTablero(Poda, Columnas, T), TS),
    length(TS, N).

% 23,320,023 inferences, 2.133 CPU in 2.133 seconds (100% CPU, 10932656 Lips)
% N = 28.

% 1,274,648,399 inferences, 127.565 CPU in 127.519 seconds (100% CPU, 9992156 Lips)
% N = 200.