:- module(util, [lista_de_piezas/3,test_kPiezas_unicos/1]).

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