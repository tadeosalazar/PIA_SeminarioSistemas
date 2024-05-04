:- use_module(library(pce)).

% Predicado para mostrar una pregunta y obtener la respuesta del usuario
mostrar_pregunta_y_responder(Pregunta, Opciones, Puntos, Respuesta) :-
   new(D, dialog('Pregunta')),
   send(D, append, label(pregunta, Pregunta)),
   send(D, append, new(M, menu(opcion, marked))),
   forall(member(Opcion, Opciones),
          send(M, append, menu_item(Opcion, message(D, return, Opcion)))),
   send(D, append, button(finalizar, message(D, return, finalizar))),
   send(D, default_button, finalizar),
   send(D, transient_for, @main),
   send(D, open_centered),
   send(D, wait),
   get(D, confirm, Respuesta),
   puntos(Respuesta, Opciones, Puntos),
   send(D, destroy).

% Predicado para calcular los puntos de una respuesta
puntos(Respuesta, Opciones, Puntos) :-
   nth1(Index, Opciones, Respuesta),
   puntos_index(Index, Puntos).

puntos_index(1, 3).
puntos_index(2, 2).
puntos_index(3, 1).

% Predicado principal para hacer las preguntas
preguntas :-
   new(@main, dialog('Preguntas y Respuestas')),
   send(@main, append, label(nombre, 'Por favor, responde las siguientes preguntas una por una:')),
   forall(pregunta(N, Pregunta, Opciones), (
       send(@main, append, label(nombre, N)),
       mostrar_pregunta_y_responder(Pregunta, Opciones, Puntos, Respuesta),
       atom_concat('Respuesta ', Respuesta, RespuestaAtom),
       send(@main, append, label(respuesta, RespuestaAtom)),
       send(@main, append, label(separador, '-----------------------')),
       sumar_puntos(Puntos)
   )),
   mostrar_resultado,
   send(@main, destroy).

% Predicado para sumar los puntos
sumar_puntos(Puntos) :-
   retractall(puntos_totales(_)),
   (   puntos_totales(PuntosTotales) -> 
       PuntosTotales1 is PuntosTotales + Puntos
   ;   PuntosTotales1 = Puntos
   ),
   assertz(puntos_totales(PuntosTotales1)).

% Predicado para mostrar el resultado
mostrar_resultado :-
   puntos_totales(PuntosTotales),
   new(D, dialog('Resultado')),
   (   PuntosTotales > 25 -> 
       send(D, append, label(resultado, 'Estoy extremadamente feliz'))
   ;   PuntosTotales > 20 -> 
       send(D, append, label(resultado, 'Estoy muy feliz'))
   ;   PuntosTotales > 15 -> 
       send(D, append, label(resultado, 'Estoy feliz'))
   ;   PuntosTotales > 10 -> 
       send(D, append, label(resultado, 'Estoy neutral'))
   ;   PuntosTotales > 8 -> 
       send(D, append, label(resultado, 'Estoy un poco triste'))
   ;   PuntosTotales =< 5 -> 
       send(D, append, label(resultado, 'Estoy muy triste'))
   ),
   send(D, append, button(cerrar, message(D, destroy))),
   send(D, open_centered).

% Preguntas y opciones
pregunta(1, '¿Crees en ti?', ['Sí, totalmente', 'A veces', 'No, nunca']).
pregunta(2, '¿La felicidad es algo que se busca o algo que se encuentra?', ['Se busca', 'Se encuentra', 'Depende de la situación']).
pregunta(3, '¿Estás satisfecho contigo mismo?', ['Sí, muy satisfecho', 'No del todo', 'Estoy trabajando en ello']).
pregunta(4, 'Si pudieras volver atrás en el tiempo, ¿cambiarías algo o lo dejarías todo como está?', ['Cambiaría algo', 'Lo dejaría todo como está', 'No estoy seguro']).
pregunta(5, '¿Existe un destino ya escrito o lo creamos nosotros con nuestros actos?', ['Existe un destino ya escrito', 'Lo creamos nosotros con nuestros actos', 'Es una combinación de ambos']).
pregunta(6, '¿Soy feliz?', ['Sí, muy feliz', 'No del todo', 'En ocasiones']).
pregunta(7, '¿De verdad valoro a quienes me rodean?', ['Sí, mucho', 'A veces me cuesta', 'No tanto como debería']).
pregunta(8, '¿He hecho las paces con mis emociones?', ['Sí, completamente', 'Todavía estoy trabajando en ello', 'No, tengo dificultades para manejarlas']).
pregunta(9, '¿Qué tanto influye tu pasado en tus decisiones presentes?', ['Mucho, mi pasado afecta significativamente mis decisiones', 'Algo, pero trato de no dejar que influya en gran medida', 'Poco, mis decisiones están basadas en mi situación actual']).
pregunta(10, '¿Estás en paz contigo mismo y con el mundo?', ['Sí, totalmente', 'No del todo', 'En ocasiones']).

% Predicado para mostrar la pregunta y responderla
mostrar_pregunta_y_responder(Pregunta, Opciones, Puntos, Respuesta) :-
   new(A, list_browser(Pregunta)),
   forall(member(Op, Opciones), (
       atom_concat('Punto ', Puntos1, PuntoAtom),
       new(Punto, label(Op, PuntoAtom)),
       send(A, append, Punto),
       (   Op == 'Sí, totalmente' -> 
           Puntos1 = 5
       ;   Op == 'A veces' -> 
           Puntos1 = 3
       ;   Op == 'No del todo' -> 
           Puntos1 = 1
       ;   Op == 'Estoy trabajando en ello' -> 
           Puntos1 = 2
       ;   Op == 'No, nunca' -> 
           Puntos1 = 0
       ;   Op == 'Se busca' -> 
           Puntos1 = 2
       ;   Op == 'Se encuentra' -> 
           Puntos1 = 4
       ;   Op == 'Depende de la situación' -> 
           Puntos1 = 1
       ;   Op == 'Sí, muy satisfecho' -> 
           Puntos1 = 4
       ;   Op == 'No del todo' -> 
           Puntos1 = 2
       ;   Op == 'En ocasiones' -> 
           Puntos1 = 1
       ;   Op == 'Sí, mucho' -> 
           Puntos1 = 4
       ;   Op == 'A veces me cuesta' -> 
           Puntos1 = 3
       ;   Op == 'No, tengo dificultades para manejarlas' -> 
           Puntos1 = 0
       ;   Op == 'Sí, completamente' -> 
           Puntos1 = 5
       ;   Op == 'Todavía estoy trabajando en ello' -> 
           Puntos1 = 2
       ;   Op == 'Poco, mis decisiones están basadas en mi situación actual' -> 
           Puntos1 = 1
       ;   Op == 'No estoy seguro' -> 
           Puntos1 = 1
       ;   Op == 'Sí, muy feliz' -> 
           Puntos1 = 4
       ;   Op == 'No del todo' -> 
           Puntos1 = 2
       ;   Op == 'En ocasiones' -> 
           Puntos1 = 1
       ;   Op == 'Poco, mis decisiones están basadas en mi situación actual' -> 
           Puntos1 = 1
       ;   Op == 'Sí, completamente' -> 
           Puntos1 = 5
       ;   Op == 'Algo, pero trato de no dejar que influya en gran medida' -> 
           Puntos1 = 3
       ;   Op == 'Existe un destino ya escrito' -> 
           Puntos1 = 3
       ;   Op == 'Lo creamos nosotros con nuestros actos' -> 
           Puntos1 = 2
       ;   Op == 'Es una combinación de ambos' -> 
           Puntos1 = 1
       ;   Op == 'Sí, muy triste' -> 
           Puntos1 = 0
       ;   Op == 'No del todo, estoy trabajando en ello' -> 
           Puntos1 = 1
       ;   Op == 'Sí, mucho' -> 
           Puntos1 = 4
       ;   Op == 'A veces me cuesta' -> 
           Puntos1 = 3
       ;   Op == 'Sí, estoy feliz' -> 
           Puntos1 = 2
       ;   Op == 'Sí, mucho' -> 
           Puntos1 = 4
      ;   Op == 'A veces me cuesta' -> 
           Puntos1 = 3
       ;   Op == 'No tanto como debería' -> 
           Puntos1 = 1
       )),
       send(A, open_centered),
       send(A, wait_for_selection),
       get(A, selection, Respuesta)
   ).

% Ejecutar el programa
:- preguntas.