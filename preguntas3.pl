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
   send(D, destroy).  % Destruir el diálogo después de obtener la respuesta

% Predicado principal para hacer las preguntas
preguntas :-
   new(@main, dialog('Preguntas y Respuestas')),
   send(@main, append, label(nombre, 'Por favor, responde las siguientes preguntas una por una:')),
   forall(pregunta(N, Pregunta, Opciones), (
       send(@main, append, label(nombre, N)),
       mostrar_pregunta_y_responder(Pregunta, Opciones, Respuesta),
       sumar_puntos(Respuesta)
   )),
   mostrar_resultado,
   send(@main, destroy).

% Predicado para sumar los puntos
sumar_puntos(Respuesta) :-
   puntos(Respuesta, Puntos),
   (   retract(puntos_totales(PuntosTotales)) ->
       NuevoPuntosTotales is PuntosTotales + Puntos
   ;   NuevoPuntosTotales = Puntos
   ),
   assertz(puntos_totales(NuevoPuntosTotales)).

% Predicado para calcular los puntos de una respuesta
puntos(Respuesta, Puntos) :-
    string_chars(Respuesta, Opciones),
    contar_opciones(Opciones, 'A', ContA),
    contar_opciones(Opciones, 'B', ContB),
    contar_opciones(Opciones, 'C', ContC),
    Puntos is ContA * 10 + ContB * 5 + ContC * 2.

% Predicado para contar la cantidad de veces que aparece una opción en la respuesta
contar_opciones([], _, 0).
contar_opciones([Opcion|Resto], Opcion, Cont) :-
    contar_opciones(Resto, Opcion, ContResto),
    Cont is ContResto + 1.
contar_opciones([Opcion|Resto], OpcionBuscada, Cont) :-
    Opcion \= OpcionBuscada,
    contar_opciones(Resto, OpcionBuscada, Cont).

% Predicado para mostrar el resultado
mostrar_resultado :-
   puntos_totales(PuntosTotales),
   obtener_mensaje_reflexivo(PuntosTotales, Mensaje),
   new(D, dialog('Resultado')),
   send(D, append, label(resultado, 'Autorreflexión')),
   send(D, append, label(mensaje, Mensaje)),
   send(D, append, button(cerrar, message(D, destroy))),
   send(D, open_centered).

% Predicado para obtener el mensaje reflexivo según el puntaje
obtener_mensaje_reflexivo(PuntosTotales, Mensaje) :-
    (   PuntosTotales =:= 100 -> Mensaje = '¡Felicidades! Obtuviste el puntaje máximo. ¡Estás muy feliz!'
    ;   PuntosTotales =:= 20 -> Mensaje = 'Obtuviste el puntaje mínimo. Reflexiona sobre cómo mejorar.'
    ;   PuntosTotales =< 35 -> Mensaje = 'Tu puntaje es bajo. Tómate un momento para reflexionar.'
    ;   PuntosTotales =< 50 -> Mensaje = 'Tu puntaje es neutral. Reflexiona sobre lo que te hace feliz.'
    ;   PuntosTotales =< 75 -> Mensaje = 'Tu puntaje es alto. ¡Sigue así!'
    ;   Mensaje = '¡Excelente! Obtuviste un puntaje muy alto. ¡Estás muy feliz!'
    ).

% Preguntas y opciones
pregunta(1, '¿Crees en ti?', ['A) Sí, totalmente', 'B) A veces', 'C) No, nunca']).
pregunta(2, '¿La felicidad es algo que se busca o algo que se encuentra?', ['A) Se busca', 'B) Se encuentra', 'C) Depende de la situación']).
pregunta(3, '¿Estás satisfecho contigo mismo?', ['A) Sí, muy satisfecho', 'B) No del todo', 'C) Estoy trabajando en ello']).
pregunta(4, 'Si pudieras volver atrás en el tiempo, ¿cambiarías algo o lo dejarías todo como está?', ['A) Cambiaría algo', 'B) Lo dejaría todo como está', 'C) No estoy seguro']).
pregunta(5, '¿Existe un destino ya escrito o lo creamos nosotros con nuestros actos?', ['A) Existe un destino ya escrito', 'B) Lo creamos nosotros con nuestros actos', 'C) Es una combinación de ambos']).
pregunta(6, '¿Soy feliz?', ['A) Sí, muy feliz', 'B) No del todo', 'C) En ocasiones']).
pregunta(7, '¿De verdad valoro a quienes me rodean?', ['A) Sí, mucho', 'B) A veces me cuesta', 'C) No tanto como debería']).
pregunta(8, '¿He hecho las paces con mis emociones?', ['A) Sí, completamente', 'B) Todavía estoy trabajando en ello', 'C) No, tengo dificultades para manejarlas']).
pregunta(9, '¿Qué tanto influye tu pasado en tus decisiones presentes?', ['A) Mucho, mi pasado afecta significativamente mis decisiones', 'B) Algo, pero trato de no dejar que influya en gran medida', 'C) Poco, mis decisiones están basadas en mi situación actual']).
pregunta(10, '¿Estás en paz contigo mismo y con el mundo?', ['A) Sí, totalmente', 'B) No del todo', 'C) En ocasiones']).

% Ejecutar el programa
:- preguntas.
