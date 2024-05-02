:- use_module(library(pce)).

:- dynamic
    respuesta/2,
    grupos_completados/1.

crear_interfaz :-
    new(@main_dialog, dialog('Cuestionario')),
    send(@main_dialog, append, button('Grupo 1', message(@prolog, abrir_grupo, 1))),
    send(@main_dialog, append, button('Grupo 2', message(@prolog, abrir_grupo, 2))),
    send(@main_dialog, append, button('Grupo 3', message(@prolog, abrir_grupo, 3))),
    send(@main_dialog, open).

abrir_grupo(NumeroGrupo) :-
    atomic_list_concat(['Grupo ', NumeroGrupo], NombreGrupo),
    new(Dialog, dialog(NombreGrupo)),
    preguntas_grupo(NumeroGrupo, Preguntas),
    crear_preguntas(Dialog, Preguntas),
    send(Dialog, append, button(finalizar, message(@prolog, finalizar_grupo, Dialog))),
    send(Dialog, open).

preguntas_grupo(1, ['¿Crees en ti?', '¿La felicidad es algo que se busca o algo que se encuentra?', '¿Estás satisfecho contigo mismo?', '¿Qué es más importante para ti: la realización personal o el reconocimiento externo?', '¿Te sientes motivado y entusiasmado con tus metas y aspiraciones?']).
preguntas_grupo(2, ['Si pudieras volver atrás en el tiempo, ¿cambiarías algo o lo dejarías todo como está?', '¿Existe un destino ya escrito o lo creamos nosotros con nuestros actos?', '¿Soy feliz?', '¿Con qué frecuencia experimentas emociones positivas?', '¿Qué tan satisfecho estás con tus relaciones personales?']).
preguntas_grupo(3, ['¿De verdad valoro a quienes me rodean?', '¿He hecho las paces con mis emociones?', '¿Qué tanto influye tu pasado en tus decisiones presentes?', '¿Cómo sueles reaccionar ante situaciones estresantes?', '¿Cuánto tiempo dedicas al autocuidado y la relajación?']).

crear_preguntas(Dialog, Preguntas) :-
    forall(member(Pregunta, Preguntas),
           (   new(Menu, menu(Pregunta)),
               send_list(Menu, append, ['Si totalmente', 'A veces', 'No nunca']),
               send(Dialog, append, Menu)
           )).

finalizar_grupo(Dialog) :-
    puntuar_respuestas,
    retractall(respuesta(_, _)),
    retract(grupos_completados(GruposCompletados)), % Retractamos el número de grupos completados
    NuevoGrupo is GruposCompletados + 1,
    asserta(grupos_completados(NuevoGrupo)), % Incrementamos el número de grupos completados
    (   NuevoGrupo =:= 3 -> finalizar_cuestionario
    ;   send(Dialog, report, inform, 'Grupo completado'),
        send(Dialog, destroy)
    ).

finalizar_cuestionario :-
    grupos_completados(3), % Verificamos si se completaron los tres grupos de preguntas
    puntuar_respuestas,
    findall(P, respuesta(_, P), Puntuaciones),
    length(Puntuaciones, NumPreguntas),
    (   NumPreguntas > 0 ->
        PuntajeMaximo is NumPreguntas * 3, % Máximo puntaje posible: 3 puntos por pregunta
        sumlist(Puntuaciones, Total),
        evaluar_puntaje(Total, PuntajeMaximo, Resultado),
        mostrar_resultados(Resultado)
    ;   send(@main_dialog, report, warning, 'No hay preguntas para evaluar.')
    ).


puntuar_respuestas :-
    findall(P, respuesta(_, P), Puntuaciones),
    sumlist(Puntuaciones, _).

evaluar_puntaje(_, 0, 'No hay preguntas para evaluar') :- !.

evaluar_puntaje(Puntaje, PuntajeMaximo, Resultado) :-
    Porcentaje is Puntaje * 100 / PuntajeMaximo,
    (   Porcentaje >= 75 -> Resultado = 'Muy feliz'
    ;   Porcentaje >= 50 -> Resultado = 'Feliz'
    ;   Porcentaje >= 25 -> Resultado = 'Bien pero puedes mejorar'
    ;   Resultado = 'Triste'
    ).


mostrar_resultados(Resultado) :-
    atom_concat('Tu estado emocional es: ', Resultado, Mensaje),
    new(Dialog, dialog('Resultados')),
    send(Dialog, append, new(Label, label(Mensaje))), % Crear la etiqueta de mensaje
    send(Dialog, append, button(cerrar, message(Dialog, destroy))),
    send(Dialog, open).

menu_response(1, 3).
menu_response(2, 2).
menu_response(3, 1).

send(@main_dialog, confirm_message, message(@prolog, on_confirm)).

on_confirm(D, R) :- 
    send(D, return, @on),
    functor(R, Name, 3),
    arg(2, R, Index),
    arg(3, R, Value),
    menu_response(Index, Puntaje),
    asserta(respuesta(Name, Puntaje)).

main :-
    asserta(grupos_completados(0)), % Inicializar grupos completados a 0
    crear_interfaz.

:- main.
