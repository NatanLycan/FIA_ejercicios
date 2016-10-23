;;;Código de ejemplo. Laberintos en 3D.
;;;Este código muestra como deben usarse las funciones de la biblioteca
;;;maze-lib.lisp para que pueda ser usado en la página de internet. Este código
;;;sólo funciona para el laberinto Nivel 1, ya que la solución ya está escrita
;;;dentro del código. Si se usa en los demás laberintos regresará la misma
;;;solución.

;La única diferencia entre los laberintos 2D y los 3D es que en estos últimos
;aparecen las dos celdas para los puentes: la número 16 y la número 17. Ambas
;funcionan exactamente igual, la única diferencia es en su apariencia.

;Primero debe cargarse la biblioteca de la siguiente forma.
(load "maze_lib.lisp")

;Para añadir un algoritmo al menú de la página es necesario usar la función
;add-algrithm, como se muestra a continuación. No importa en que lugar
;del archivo se use, pero en de preferencia que sea al inicio del código.
;Si están haciendo pruebas en su computadora entonces no hay problema si se
;omiten.
(add-algorithm 'breadth-first)
(add-algorithm 'depth-first)
(add-algorithm 'error-example)

;Función de muestra. Regresa el resultado de un algoritmo de búsqueda a lo
;ancho. Esta función no debe llevar argumentos.
(defun breadth-first ()
  ;La posición inicial y la posición meta del laberinto están almacenadas en las
  ;siguientes variables *start* y *goal*. Estas son arreglos de dos elementos, el
  ;primero es la fila y la segunda la columna. La posición #(0 0) está ubicada en
  ;la celda superior izquierda.
  (format t "Posición inicial: ~S~%" *start*) (format t "Posición meta: ~S~%" *goal*)
  ;Para saber cuantas fila y cuantas columnas tiene el laberinto en total, es
  ;necesario usar las funciones get-maze-rows y get-maze-cols
  (format t "Número total de filas: ~S~%" (get-maze-rows))
  (format t "Número total de columnas ~S~%" (get-maze-cols))
  ;Para pedir las paredes de una celda determinada es necesario usar la función
  ;get-cell-walls. Recibe dos argumentos: el número de la fila y el número de la
  ;columna.
  (format t "Paredes de la celda #(0 0): ~S~%" (get-cell-walls 0 0))
  ;Si sienten la necesidad de ver dibujadas las paredes de una celda, pueden
  ;usar la función draw-cell-walls.
  (format t "Dibujo de las paredes de la celda #(0 0):~%")
  (draw-cell-walls 0 0)
  ;Si desean obtener la información de todas las paredes del laberinto pueden
  ;usar la función get-maze-data
  (format t "Datos del laberinto: ~%~S~%" (get-maze-data))
  ;La solución debe almacenarse en la variable global *solution*.
  (setq *solution* '(4 4 3 0 0 0 3 4 2 2 3 4 4)))
  ;La solución debe ser expresada como una lista conteniendo las direcciones de
  ;los desplazamientos. Cada dirección está representada por un número, empezando
  ;desde arriba con el 0 y después en sentido horario. De esta forma, los 8
  ;movimientos posibles son:
  ;Arriba (N): 0
  ;Arriba-derecha (NE): 1
  ;Derecha (E): 2
  ;Abajo-derecha (SE): 3
  ;Abajo (S): 4
  ;Abajo-izquierda (SW): 5
  ;Izquierda (W): 6
  ;Arriba-izquierda (NW): 7

;Función de muestra. Regresa el resultado de un algoritmo de búsqueda a lo
;profundo. Esta función no debe llevar argumentos.
(defun depth-first ()
 (setq *solution* '(4 4 4 4 4 1 2 2 2 7 6 4 3 2 2)))

;Función defectuosa. Esta función genera un error al tratar de obtener las
;paredes de una celda fuera de las fronteras del laberinto. Esto es para que
;puedan ver como se despliegan los errores de ejecución dentro de la página.
(defun error-example () (get-cell-walls 1000 1000))

;La última línea ejecutable del código debe ser la siguiente. Es la que se
;encarga de enviar la solución a la página de Internet para que pueda ser
;dibujada. Si están haciendo pruebas en su computadora entonces pueden omitirla
;o comentarla.
(start-maze)
