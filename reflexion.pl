:- use_module(library(pce)).
% Predicado para mostrar una pregunta y obtener la respuesta del usuario
mostrar_pregunta_y_responder(Pregunta, Opciones, Respuesta) :-
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
   send(D, destroy).
% Predicado principal para hacer las preguntas
preguntas :-
   new(@main, dialog('Preguntas y Respuestas')),
   send(@main, append, label(nombre, 'Por favor, responde las siguientes preguntas una por una:')),
   forall(pregunta(N, Pregunta, Opciones), (
       send(@main, append, label(nombre, N)),
       mostrar_pregunta_y_responder(Pregunta, Opciones, Respuesta),
       atom_concat('Respuesta ', Respuesta, RespuestaAtom),
       send(@main, append, label(respuesta, RespuestaAtom)),
       send(@main, append, label(separador, '-----------------------'))
   )),
   send(@main, append, button(finalizar, message(@prolog, mostrar_respuestas))),
   send(@main, default_button, finalizar),
   send(@main, open_centered).
% Predicado para mostrar las respuestas en otra ventana
mostrar_respuestas :-
   new(D, dialog('Respuestas')),
   forall(between(1, 10, I), (
       pregunta(I, Pregunta, _),
       atom_concat('Respuesta ', I, RespuestaAtom),
       send(D, append, label(pregunta, Pregunta)),
       send(D, append, label(respuesta, RespuestaAtom)),
       send(D, append, label(separador, '-----------------------'))
   )),
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
pregunta(9, '¿Qué tanto influye tu pasado en tus decisiones presentes?', ['Mucho, mi pasado afecta significativamente mis decisiones', 'Algo, pero trato de no dejar que influya demasiado', 'No mucho, prefiero enfocarme en el presente y el futuro']).
pregunta(10, '¿Qué es más importante para ti: la realización personal o el reconocimiento externo?', ['La realización personal', 'El reconocimiento externo', 'Ambos son importantes, pero priorizo la realización personal']).
% Ejecutar el programa
:- preguntas.