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
    send(D, open_centered),
    get(D, confirm, Respuesta),
    send(D, destroy).  % Destruir el diálogo después de obtener la respuesta

% Predicado principal para hacer las preguntas
preguntas :-
    new(@main, dialog('Preguntas y Respuestas')),
    new(Bitmap, bitmap('C:/Users/PC/Desktop/Prolog/PIA_SeminarioSistemas/IMG/gatoFeliz.jpg')),  % Use an absolute file path
    send(@main, append, Bitmap),  % Append the image to the dialog
    send(@main, append, label(nombre, 'Por favor, responde las siguientes preguntas una por una:')),
    send(@main, append, button(comenzar, message(@prolog, hacer_preguntas))),
    send(@main, open_centered).

hacer_preguntas :-
    forall(pregunta(_, Pregunta, Opciones), (
        mostrar_pregunta_y_responder(Pregunta, Opciones, Respuesta),
        sumar_puntos(Respuesta)
    )),
    mostrar_resultado.

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
    send(D, append, label(resultado, 'Autorreflexion')),
    send(D, append, label(mensaje, Mensaje)),
    new(Bitmap, bitmap('C:/Users/PC/Desktop/Prolog/PIA_SeminarioSistemas/IMG/gatoLike.jpg')),
    send(D, append, Bitmap), % Append the image to the dialog
    send(D, append, button(cerrar, message(D, destroy))),
    send(D, open_centered).

% Predicado para obtener el mensaje reflexivo según el puntaje
obtener_mensaje_reflexivo(PuntosTotales, Mensaje) :-
    (   PuntosTotales =:= 100 -> Mensaje = 'Felicidades! Obtuviste el puntaje maximo. Tienes una comprensin clara de ti mismo y estas en sintonia con tus pensamientos y emociones. Sigue creciendo y aprendiendo'
    ;   PuntosTotales =:= 20 -> Mensaje = 'Obtuviste el puntaje minimo. Dedica tiempo a explorar tus pensamientos y emociones. Cada paso cuenta en tu crecimiento personal. Sigue adelante'
    ;   PuntosTotales =< 35 -> Mensaje = 'Tu puntaje es bajo. Sigue dedicando tiempo a la introspeccion y a comprender tus pensamientos y emociones. Cada avance es valioso'
    ;   PuntosTotales =< 50 -> Mensaje = 'Tu puntaje es neutral.  Sigue explorando tus pensamientos y emociones para profundizar tu comprension de ti mismo. Sigue asi, vas por buen camino'
    ;   PuntosTotales =< 75 -> Mensaje = 'Tu puntaje es alto. Estas cerca de alcanzar una profunda claridad sobre ti mismo. Continua asi, estas haciendo un gran trabajo!'
    ;   Mensaje = 'Excelente! Obtuviste un puntaje muy alto. Has desarrollado una comprension profunda de ti mismo. Sigue asi, tu crecimiento personal es admirable'
    ).
% Preguntas y opciones
pregunta(1, 'Crees en ti?', ['A) Si, totalmente', 'B) A veces', 'C) No, nunca']).
pregunta(2, 'La felicidad es algo que se busca o algo que se encuentra?', ['A) Se busca', 'B) Se encuentra', 'C) Depende de la situacion']).
pregunta(3, 'Estas satisfecho contigo mismo?', ['A) Si, muy satisfecho', 'B) No del todo', 'C) Estoy trabajando en ello']).
pregunta(4, 'Si pudieras volver atras en el tiempo, cambiarias algo o lo dejarias todo como esta?', ['A) Cambiaria algo', 'B) Lo dejaria todo como esta', 'C) No estoy seguro']).
pregunta(5, 'Existe un destino ya escrito o lo creamos nosotros con nuestros actos?', ['A) Existe un destino ya escrito', 'B) Lo creamos nosotros con nuestros actos', 'C) Es una combinacion de ambos']).
pregunta(6, 'Soy feliz?', ['A) Si, muy feliz', 'B) No del todo', 'C) En ocasiones']).
pregunta(7, 'De verdad valoro a quienes me rodean?', ['A) Si, mucho', 'B) A veces me cuesta', 'C) No tanto como deberia']).
pregunta(8, 'He hecho las paces con mis emociones?', ['A) Si, completamente', 'B) Todavia estoy trabajando en ello', 'C) No, tengo dificultades para manejarlas']).
pregunta(9, 'Que tanto influye tu pasado en tus decisiones presentes?', ['A) Mucho, mi pasado afecta significativamente mis decisiones', 'B) Algo, pero trato de no dejar que influya en gran medida', 'C) Poco, mis decisiones estan basadas en mi situacion actual']).
pregunta(10, 'Estas en paz contigo mismo y con el mundo?', ['A) Si, totalmente', 'B) No del todo', 'C) En ocasiones']).

% Inicializar puntos totales
:- dynamic puntos_totales/1.
puntos_totales(0).

% Ejecutar el programa
:- preguntas.