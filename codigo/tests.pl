:- use_module(katamino).
:- use_module(piezas).

:- begin_tests(katamino).



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



test_kPiezas_unicos(ListaDePiezas) :-
    maplist(msort,ListaDePiezas,ListaDePiezasOrdenadas),    % ordeno cada lista alfabéticamente para detectar permutaciones
    length(ListaDePiezasOrdenadas, LongitudListaConRepetidos),
    sort(ListaDePiezasOrdenadas, ListaSinRepetidos),                             % sort borra las permutaciones (que a sus ojos son valores duplicados)
    length(ListaSinRepetidos, LongitudListaSinRepetidos),
    LongitudListaConRepetidos =:= LongitudListaSinRepetidos.


test(sublista_basica) :-
    sublista(2, 3, [a, b, c, d, e, f], R),
    R == [c, d, e].

test(tablero_basico) :-
    tablero(3, T),
    T = [[_,_,_],
         [_,_,_],
         [_,_,_],
         [_,_,_],
         [_,_,_]].

test(tamaño1) :-
    tablero(3, T), 
    tamaño(T,F,C),
    F==5,
    C==3.

test(tamaño2) :-
    findnsols(3,(F,C), (pieza(e, E), tamaño(E,F,C)),Pares),
    Pares == [(2,3),(3,2),(2,3)].
    
test(coordenadas) :-
    findnsols(15,(IJ), (tablero(3, T), coordenadas(T, IJ)),Pares),
    Pares == [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3),(3,1),(3,2),(3,3),(4,1),(4,2),(4,3),(5,1),(5,2),(5,3)].

test(kpiezas_3) :-
    findall(PS, kPiezas(3,PS), Resultados),
    test_kPiezas_unicos(Resultados).

test(kpiezas_12) :-
    findall(PS, kPiezas(12,PS), Resultados),
    test_kPiezas_unicos(Resultados).

test(kpiezas_2) :-
    findall(PS, kPiezas(2,PS), Resultados),
    test_kPiezas_unicos(Resultados).

test(kpiezas_11) :-
    findall(PS, kPiezas(11,PS), Resultados),
    test_kPiezas_unicos(Resultados).

test(seccionTablero_enRango) :-
    tablero(3, T),
    seccionTablero(T, 3, 2, (1,2), ST),
    tamaño(ST, F, C),
    F == 3,
    C == 2.

test(seccionTablero_FueraDeRango, [fail]) :-
    tablero(3, T),
    seccionTablero(T, 3, 3, (1,2), _).


test(ubicarPieza_sin_espacio, [fail]) :-
    tablero(1, T),
    ubicarPieza(T, e).


test(ubicarPieza_e) :-
    tablero(3, T),
    once(ubicarPieza(T, e)),
    T = [[_,e,e],
         [e,e,e],
         [_,_,_],
         [_,_,_],
         [_,_,_]].

test(ubicarPieza_e_fila_ocupada) :-
    tablero(3, T),
    nth1(1, T, [x,x,x]),
    once(ubicarPieza(T, e)),
    T = [[x,x,x],
         [_,e,e],
         [e,e,e],
         [_,_,_],
         [_,_,_]].

test(ubicarPieza_e_rotada) :-
    tablero(3, T),
    nth1(1, T, F1), nth1(1, F1, x),
    nth1(2, T, F2), nth1(1, F2, x),
    once(ubicarPieza(T, e)),    
    T = [[x,x,_],
         [x,e,e],
         [_,e,e],
         [_,_,e],
         [_,_,_]].

test(ubicarPiezas_imposible, [fail]) :-
    tablero(2, T),
    ubicarPiezas(T, sinPoda, [x]).

test(ubicarPiezas_conEspacios) :-
    tablero(4, T),
    once(ubicarPiezas(T, sinPoda, [e,f,g])),
    findall(v, (member(F,T), member(C,F), var(C), v=v), Vars),
    length(Vars, 5).


% se podria agregar un test para ubicarPieza cuando el tablero esta lleno 


test(llenarTablero_tablerolleno) :-
    once(llenarTablero(sinPoda, 3, T)),         
    \+ ( member(F, T),
         member(C, F),
         var(C) ).


test(cantSoluciones_5x2_K2_0) :-
    cantSoluciones(sinPoda, 2, N),
    N == 0.

test(cantSoluciones_5x3_K3_28) :-
    cantSoluciones(sinPoda, 3, N),
    N == 28.

test(cantSoluciones_5x4_K4_200) :-
    cantSoluciones(sinPoda, 4, N),
    N == 200.



:- end_tests(katamino).