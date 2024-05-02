:- use_module(library(pce)).

% Definici�n de las preguntas y respuestas
pregunta_respuesta(1, '�Crees en ti?', [
    ['S�, totalmente.', 'a'],
    ['A veces.', 'b'],
    ['No, nunca.', 'c']
]).

pregunta_respuesta(2, '�La felicidad es algo que se busca o algo que se encuentra?', [
    ['Se busca.', 'a'],
    ['Se encuentra.', 'b'],
    ['Depende de la situaci�n.', 'c']
]).

pregunta_respuesta(3, '�Est�s satisfecho contigo mismo?', [
    ['S�, muy satisfecho.', 'a'],
    ['No del todo.', 'b'],
    ['Estoy trabajando en ello.', 'c']
]).

pregunta_respuesta(4, 'Si pudieras volver atr�s en el tiempo, �cambiar�as algo o lo dejar�as todo como est�?', [
    ['Cambiar�a algo.', 'a'],
    ['Lo dejar�a todo como est�.', 'b'],
    ['No estoy seguro.', 'c']
]).

pregunta_respuesta(5, '�Existe un destino ya escrito o lo creamos nosotros con nuestros actos?', [
    ['Existe un destino ya escrito.', 'a'],
    ['Lo creamos nosotros con nuestros actos.', 'b'],
    ['Es una combinaci�n de ambos.', 'c']
]).

pregunta_respuesta(6, '�Soy feliz?', [
    ['S�, muy feliz.', 'a'],
    ['No del todo.', 'b'],
    ['En ocasiones.', 'c']
]).

pregunta_respuesta(7, '�De verdad valoro a quienes me rodean?', [
    ['S�, mucho.', 'a'],
    ['A veces me cuesta.', 'b'],
    ['No tanto como deber�a.', 'c']
]).

pregunta_respuesta(8, '�He hecho las paces con mis emociones?', [
    ['S�, completamente.', 'a'],
    ['Todav�a estoy trabajando en ello.', 'b'],
    ['No, tengo dificultades para manejarlas.', 'c']
]).

pregunta_respuesta(9, '�Qu� tanto influye tu pasado en tus decisiones presentes?', [
    ['Mucho, mi pasado afecta significativamente mis decisiones.', 'a'],
    ['Algo, pero trato de no dejar que influya demasiado.', 'b'],
    ['No mucho, prefiero enfocarme en el presente y el futuro.', 'c']
]).

pregunta_respuesta(10, '�Qu� es m�s importante para ti: la realizaci�n personal o el reconocimiento externo?', [
    ['La realizaci�n personal.', 'a'],
    ['El reconocimiento externo.', 'b'],
    ['Ambos son importantes, pero priorizo la realizaci�n personal.', 'c']
]).

pregunta_respuesta(11, '�Con qu� frecuencia experimentas emociones positivas?', [
    ['Todo el tiempo.', 'a'],
    ['De vez en cuando.', 'b'],
    ['Raramente.', 'c']
]).

pregunta_respuesta(12, '�C�mo sueles reaccionar ante situaciones estresantes?', [
    ['Mantengo la calma y busco soluciones.', 'a'],
    ['Me siento abrumado pero eventualmente encuentro una soluci�n.', 'b'],
    ['Me resulta dif�cil manejar el estr�s y tiendo a sentirme desbordado.', 'c']
]).

pregunta_respuesta(13, '�Te sientes motivado y entusiasmado con tus metas y aspiraciones?', [
    ['S�, siempre estoy motivado y entusiasmado.', 'a'],
    ['A veces, depende de la situaci�n.', 'b'],
    ['No tanto como me gustar�a.', 'c']
]).

pregunta_respuesta(14, '�Qu� tan satisfecho est�s con tus relaciones personales?', [
    ['Muy satisfecho, tengo relaciones s�lidas y significativas.', 'a'],
    ['Algo satisfecho, hay �reas en las que me gustar�a mejorar.', 'b'],
    ['No muy satisfecho, me siento desconectado o insatisfecho con mis relaciones.', 'c']
]).

pregunta_respuesta(15, '�Cu�nto tiempo dedicas al autocuidado y la relajaci�n?', [
    ['Regularmente, priorizo mi bienestar emocional y f�sico.', 'a'],
    ['A veces, pero a menudo me descuido.', 'b'],
    ['No dedico suficiente tiempo para m� mismo/a.', 'c']
]).

% Predicado principal para iniciar el cuestionario
iniciar_cuestionario :-
    asserta(respuestas([])),
    mostrar_pregunta(1).

% Predicado para mostrar una pregunta espec�fica
mostrar_pregunta(N) :-
    pregunta_respuesta(N, Pregunta, Respuestas),
    new(D, dialog('Cuestionario')),
    send(D, append, label(question_label, Pregunta), right),
    forall(member([Texto, _], Respuestas),
           agregar_respuesta(D, Texto)),
    send(D, open).

% Predicado para agregar una respuesta (bot�n) al di�logo
agregar_respuesta(D, Texto) :-
    send(D, append, button(Texto,
                    message(@prolog, procesar_respuesta, Texto))).

% Predicado para procesar la respuesta seleccionada
procesar_respuesta(Texto) :-
    agregar_respuesta_seleccionada(Texto),
    siguiente_pregunta.

% Predicado para agregar la respuesta seleccionada a la lista
agregar_respuesta_seleccionada(Texto) :-
    respuestas(Respuestas),
    retract(respuestas(Respuestas)),
    append(Respuestas, [Texto], NuevasRespuestas),
    asserta(respuestas(NuevasRespuestas)).

% Predicado para mostrar la siguiente pregunta o resultados
siguiente_pregunta :-
    length_respuestas(Length),
    NuevaPregunta is Length + 1,
    (NuevaPregunta =< 15 -> mostrar_pregunta(NuevaPregunta); mostrar_resultados).

% Predicado para mostrar los resultados
mostrar_resultados :-
    respuestas(Respuestas),
    new(D, dialog('Resultados')),
    send(D, append, text('Resultados:'), below),
    forall(member(Respuesta, Respuestas),
           send(D, append, text(Respuesta), below)),
    send(D, append, button('Cerrar', message(D, destroy)), below),
    send(D, open).

% Predicado para obtener la longitud de la lista de respuestas
length_respuestas(Length) :-
    respuestas(Respuestas),
    length(Respuestas, Length).

% Iniciar el cuestionario al cargar el archivo
:- iniciar_cuestionario.
