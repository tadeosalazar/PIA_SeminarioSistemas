:- use_module(library(pce)).
:- pce_global(@main_dialog, new(dialog('Cuestionario'))).

:- dynamic
    respuesta/2.

crear_interfaz :-
    new(@main_dialog, dialog('Cuestionario')),
    send(@main_dialog, append, new(Question1, menu('¿Eres feliz?'))),
    send_list(Question1, append, [si, no, 'No se']),
    send(@main_dialog, append, new(Question2, menu('¿Estás triste?'))),
    send_list(Question2, append, [si, no, 'No se']),
    send(@main_dialog, append, new(Question3, menu('¿Te preocupa algo?'))),
    send_list(Question3, append, [si, no, 'No se']),
    send(@main_dialog, append, button(finalizar, message(@prolog, finalizar_cuestionario))),
    send(@main_dialog, open).

finalizar_cuestionario :-
    puntuar_respuestas,
    calcular_puntaje(Puntaje),
    atom_concat('Tu puntaje es: ', Puntaje, Mensaje),
    send(@main_dialog, report, inform, Mensaje),
    retractall(respuesta(_, _)).

puntuar_respuestas :-
    findall(P, respuesta(_, P), Puntuaciones),
    sumlist(Puntuaciones, _).

calcular_puntaje(Puntaje) :-
    findall(P, respuesta(_, P), Puntuaciones),
    sumlist(Puntuaciones, Total),
    evaluar_puntaje(Total, Puntaje).

evaluar_puntaje(Total, 'Feliz') :-
    Total > 10.
evaluar_puntaje(Total, 'Confundido') :-
    Total =< 10,
    Total > 5.
evaluar_puntaje(_, 'Triste').

main :-
    crear_interfaz,
    send(@main_dialog, open).

menu_response(1, 3).
menu_response(2, 2).
menu_response(3, 1).

send(@main_dialog, confirm_message, message(@prolog, on_confirm)).

on_confirm(D, R) :- 
    send(D, return, @on),
    send(D, destroy),
    functor(R, Name, 3),
    arg(2, R, Index),
    arg(3, R, Value),
    menu_response(Index, Puntaje),
    asserta(respuesta(Name, Puntaje)).

:- main.
