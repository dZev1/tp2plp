:- use_module(katamino).
:- use_module(piezas).

:- begin_tests(katamino).

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
    tablero(3, T),
    ubicarPieza(T, l).


test(ubicarPieza_con_espacio) :-
    tablero(3, T),
    ubicarPieza(T, a),
    T == [[a,a,_],
          [a,,],  
          [a,,],
          [,,_],
          [,,_]].

test(ubicarPieza_con_espacio2) :-
    tablero(3, T),
    nth1(1, T, Fila1),
    nth1(1, Fila1, x),
    ubicarPieza(T, a),
     T == [[x,a,a],
          [,a,],
          [,a,],
          [,a,],
          [,,_]].

:- end_tests(katamino).