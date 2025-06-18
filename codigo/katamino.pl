:- use_module(piezas).

%! sublista(+Descartar, +Tomar, +L, -R)

sublista(Descartar, Tomar, L, R) :- 
    append(Descartados, Restantes, L), 
    length(Descartados, Descartar), 
    append(R, _, Restantes),
    length(R, Tomar).

% sublista/4 es reversible en su primer y cuarto argumento, porque:
% El predicado divide la lista L en dos partes: Descartados, cuya longitud es Descartar,
% y Restantes, que contiene a la lista R como prefijo. La longitud de esta lista R se obtiene o se verifica
% con length(R, Tomar). Como tanto append/3 como length/2 son predicados reversibles en todos sus argumentos,
% entonces sublista/4 preservará la reversibilidad para cualquier patrón donde esté instanciada la lista L.
% Por lo tanto, también se puede utilizar el patrón sublista(-Descartar, +Tomar, +L, +R).

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
poda(podaMod5, Tablero) :- todosGruposLibresModulo5(Tablero).

%! posLibres(+Tablero, -Pos)
posLibres(Tablero, (I,J)) :-
    nth1(I, Tablero, Fila),
    nth1(J, Fila, Casilla),
    var(Casilla).

%! todosGruposLibresModulo5(+Tablero)
todosGruposLibresModulo5(Tablero) :-
    tamaño(Tablero, Filas, Cols),
    findall((I,J), (between(1, Filas, I), between(1, Cols, J)), Coordenadas),

    findall((I,J), (member((I,J), Coordenadas), posLibres(Tablero, (I,J))), PosLibres),
    agrupar(PosLibres, VecinosLibres),

    forall(member(Grupo, VecinosLibres), (length(Grupo, Tamaño), mod(Tamaño, 5) =:= 0)).


%! ubicarPiezas(+Tablero, +Poda, +Identificadores)
ubicarPiezas(_, _, []).
ubicarPiezas(Tablero, Poda, [I|Is]) :-
    poda(Poda, Tablero),
    ubicarPieza(Tablero, I),
    ubicarPiezas(Tablero, Poda, Is).

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