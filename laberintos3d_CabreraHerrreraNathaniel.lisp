;;Primero debe cargarse la biblioteca de la siguiente forma.
(load "maze_lib.lisp")

(add-algorithm 'bestfs)
(add-algorithm 'astar)
(add-algorithm 'breath-first)
(add-algorithm 'depth-first)

(defparameter  *open* '())       ;; Frontera de busqueda...                                              

;;************      variables a utilizar        ************
(defparameter  *memory* '())   ;; Memoria de intentos previos
(defparameter  *ops*  '( 
             (:up          0 )   
             (:up-right    1 )   
			 (:right       2 )   
             (:down-right  3 )   
			 (:down        4 )
             (:down-left   5 )   
             (:left        6 )
             (:up-left     7 )   
                        
            )
  ;movimientos posibles son:
  ;Arriba (N): 0
  ;Arriba-derecha (NE): 1
  ;Derecha (E): 2
  ;Abajo-derecha (SE): 3
  ;Abajo (S): 4
  ;Abajo-izquierda (SW): 5
  ;Izquierda (W): 6
  ;Arriba-izquierda (NW): 7
)
(defparameter  *id*  0)  ;; Identificador del ultimo nodo creado, cada vez que se cree un nodo se debe incrementar
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria
(defparameter  *current-prof*  nil)  ;;almacena la profundidad del ancestro comun
;;(defparameter *floor* nil) ;;ayuda a detectar el piso en caso de una celda especial de dos pisos

;;************      fin-variables       ************

;;************      funciones de apoyo      *************
(defun reset-all () 
    "Reinicia todas las variables globales para iniciar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil)
     (setq  *solution*  nil)
     (setq  *current-prof*  nil)
)

;;=======================================================================
;;  CREATE-NODE [posicion  op]  
;;      pos - Un pos del problema a resolver (sistema)...
;;             op - El operador cuya aplicación generó el [pos]...
;;=======================================================================
(defun  create-node (pos  op &optional (heur ()) (prof ()) (val ()))
    "Construye y regresa un nuevo nodo de búsqueda que contiene al pos y operador recibidos como parámetro "
    (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
    (if (equal heur nil)
        (list  (1-  *id*)  pos  *current-ancestor*  (floor? op pos) op) 
        (if (equal prof nil)
            (list  (1-  *id*)  pos  *current-ancestor*  (floor? op pos) op heur) 
            (list  (1-  *id*)  pos  *current-ancestor*  (floor? op pos) op heur prof val) 
        )    
    )
)  ;;los nodos generados son descendientes de *current-ancestor*


(defun floor? (op pos)
    (let (
          (paux ())
          (operador (first op))
          (y (first pos))
          (x (second pos))
         )
        ;;para 16
        (if (equal 16 (get-cell-walls y x))
            (case operador
                (:up
                    2
                )
                (:up-right 
                    (setq paux (get-cell-walls y (1- x)))
                    (if (or (equal 4 paux) (equal 5 paux) (equal 6 paux) (equal 7 paux) (equal 12 paux) (equal 13 paux) (equal 14 paux) (equal 15 paux))
                        2  
                        1
                    )
                ) 
                (:right
                    1
                )
                (:down-right
                    (setq paux (get-cell-walls y (1- x)))
                    (if (or (equal 1 paux) (equal 3 paux) (equal 5 paux) (equal 7 paux) (equal 9 paux) (equal 11 paux) (equal 13 paux) (equal 15 paux))
                        2  
                        1
                    )
                )
                (:down
                    2
                )
                (:down-left
                    (setq paux (get-cell-walls y (1+ x)))
                    (if (or (equal 1 paux) (equal 3 paux) (equal 5 paux) (equal 7 paux) (equal 9 paux) (equal 11 paux) (equal 13 paux) (equal 15 paux))
                        2  
                        1
                    )                
                )
                (:left
                    1
                )
                (:up-left
                    (setq paux (get-cell-walls y (1+ x)))
                    (if (or (equal 4 paux) (equal 5 paux) (equal 6 paux) (equal 7 paux) (equal 12 paux) (equal 13 paux) (equal 14 paux) (equal 15 paux))
                        2  
                        1
                    )
                )
            )
                ;;else... para 17
             (if (equal 17 (get-cell-walls y x))
                (case operador
                    (:up
                        1
                    )
                    (:up-right 
                        (setq paux (get-cell-walls y (1- x)))
                        (if (or (equal 4 paux) (equal 5 paux) (equal 6 paux) (equal 7 paux) (equal 12 paux) (equal 13 paux) (equal 14 paux) (equal 15 paux))
                            1  
                            2
                        )
                    ) 
                    (:right
                        2
                    )
                    (:down-right
                        (setq paux (get-cell-walls y (1- x)))
                        (if (or (equal 1 paux) (equal 3 paux) (equal 5 paux) (equal 7 paux) (equal 9 paux) (equal 11 paux) (equal 13 paux) (equal 15 paux))
                            1  
                            2
                        )
                    )
                    (:down
                        1
                    )
                    (:down-left
                        (setq paux (get-cell-walls y (1+ x)))
                        (if (or (equal 1 paux) (equal 3 paux) (equal 5 paux) (equal 7 paux) (equal 9 paux) (equal 11 paux) (equal 13 paux) (equal 15 paux))
                            1  
                            2
                        )                
                    )
                    (:left
                        2
                    )
                    (:up-left
                        (setq paux (get-cell-walls y (1+ x)))
                        (if (or (equal 4 paux) (equal 5 paux) (equal 6 paux) (equal 7 paux) (equal 12 paux) (equal 13 paux) (equal 14 paux) (equal 15 paux))
                            1  
                            2
                        )
                    )
                )
               0 
            )
        )
    )
)

;;=======================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;=======================================================================


(defun insert-to-open (pos  op  metodo &optional (pos-end ()) (prof ())) 
    "Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
    (let ((nodo  ()) (heur 0))
         (cond 
            ((eql  metodo :depth-first)
                (setq nodo (create-node  pos  op))
	            (push  nodo  *open*)
            )
	        ((eql  metodo :breath-first)
                (setq nodo (create-node  pos  op))
		        (setq *open*  (append  *open*  (list nodo)))
            )
            ((eql  metodo :moves-left) ;;usado en bestfs
                (setq heur (+ (abs (- (first pos) (first pos-end))) (abs (- (second pos) (second pos-end)))))
                (if  (equal prof nil)
                     (setq nodo (create-node  pos  op heur))
                     (setq nodo (create-node  pos  op heur prof (+ heur prof)))
                )
		        (push  nodo  *open*)
                
            )
            (T  Nil)
        )  
    )
)


(defun get-from-open ()
    "Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
    (pop  *Open*)
)

;;=======================================================================
;;  VALID-OPERATOR [op, pos]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [pos] segun los recursos en la orilla donde esta la barca
;;=======================================================================
(defun  valid-operator (op  pos)
    "Predicado. Valida la aplicación de un operador a un pos...
     el pos tiene estructura:  [<y> <x>] ,
     el operador tiene estructura : [<etiqueta-humana> <num>]"  
    (let*
        (
            (operador (first op))
            (y (first pos))
            (x (second pos))
            (paredes (get-cell-walls y x))
            (px ()) ;;auxiliar para operadores en diagonal
            (py ())  ;;auxiliar para operadores en diagonal
            (panterior ())  ;;auxiliar para operadores en diagonal
            (floor (floor? op pos))
        )
        
        (case operador 
            (:up  
               (if (or (equal paredes 4) (equal paredes 5)  (equal paredes 6) (equal paredes 7) (equal paredes 12) (equal paredes 13) (equal paredes 14) (equal paredes 15 ) (and (equal paredes 16) (equal floor 1)) (and (equal paredes 17) (equal floor 2)))
                   NIL
                   ;;else
                   (progn
                        (setq panterior (get-cell-walls (1+ y) x))
                        (setq floor (floor? (fifth (locate-node *current-ancestor* *memory*)) (list (1+ y) x)))
                        (if (or 
                                (and (equal panterior 16) (equal floor 1)) 
                                (and (equal panterior 17) (equal floor 2))
                            ) 
                            NIL
                            T
                        )
                    )
               )
            )
            (:up-right  
                (if (or (equal paredes 12) (equal paredes 13) (equal paredes 14) (equal paredes 15) ())
                    NIL
                    (progn
                        (setq py (get-cell-walls (1+ y) x))
                        (if     
                            (or 
                                (and 
                                    (or (equal paredes 8) (equal paredes 9) (equal paredes 10) (equal paredes 11) (equal paredes 12) (equal paredes 13) (equal paredes 14) (equal paredes 15)) 

                                    (or (equal py 8) (equal py 9) (equal py 10) (equal py 11) (equal py 12) (equal py 13) (equal py 14) (equal py 15)) 
                                )
                                (equal py 17)
                                (equal py 16)
                            )
                            NIL
                            (progn
                                (setq px (get-cell-walls y (1- x)))
                                (if 
                                    (or
                                        (and 
                                            (or (equal paredes 4) (equal paredes 5) (equal paredes 6) (equal paredes 7) (equal paredes 12) (equal paredes 13) (equal paredes 14) (equal paredes 15)) 

                                            (or (equal px 4) (equal px 5) (equal px 6) (equal px 7) (equal px 12) (equal px 13) (equal px 14) (equal px 15))
                                        )
                                        (equal px 17)
                                        (equal px 16)
                                    )
                                    NIL
                                    (progn
                                        (setq panterior (get-cell-walls (1+ y) (1- x)))
                                        (if (or 
                                             (equal panterior 3) (equal panterior 7) (equal panterior 11) (equal panterior 15)
                                             (equal panterior 16) 
                                             (equal panterior 17) 
                                            )
                                            NIL
                                            T 
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (:right  
                (if (or (equal paredes 8) (equal paredes 9)  (equal paredes 10) (equal paredes 11) (equal paredes 12) (equal paredes 13) (equal paredes 14) (equal paredes 15 ) (and (equal paredes 16) (equal floor 2)) (and (equal paredes 17) (equal floor 1)))
                    NIL
                    ;;else
                    (progn
                        (setq panterior (get-cell-walls  y (1- x)))
                        (setq floor (floor? (fifth (locate-node *current-ancestor* *memory*)) (list y (1- x))))
                        (if (or 
                                (and (equal panterior 16) (equal floor 2)) 
                                (and (equal panterior 17) (equal floor 1))
                            ) 
                            NIL
                            T
                        )
                    )
                )
            )
            (:down-right  
                (if (or (equal paredes 9) (equal paredes 11) (equal paredes 13) (equal paredes 15))
                    NIL
                    (progn
                        (setq py (get-cell-walls (1- y) x))
                        (if 
                            (or
                                (and 
                                    (or (equal paredes 8) (equal paredes 9) (equal paredes 10) (equal paredes 11) (equal paredes 12) (equal paredes 13) (equal paredes 14) (equal paredes 15)) 

                                    (or (equal py 8) (equal py 9) (equal py 10) (equal py 11) (equal py 12) (equal py 13) (equal py 14) (equal py 15))
                                )
                                (equal py 16)
                                (equal py 17)
                            )
                                
                            NIL
                            (progn
                                (setq px (get-cell-walls y (1- x)))
                                (if 
                                    (or
                                        (and 
                                            (or (equal paredes 1) (equal paredes 3) (equal paredes 5) (equal paredes 7) (equal paredes 9) (equal paredes 11) (equal paredes 13) (equal paredes 15)) 

                                            (or (equal px 1) (equal px 3) (equal px 5) (equal px 7) (equal px 9) (equal px 11) (equal px 13) (equal px 15))
                                        )
                                        (equal px 17)
                                        (equal px 16)
                                    )                         
                                    NIL
                                    (progn
                                        (setq panterior (get-cell-walls (1- y) (1- x)))
                                        (if (or (equal panterior 6) (equal panterior 7) (equal panterior 14) (equal panterior 15)
                                                (equal panterior 16) 
                                                (equal panterior 17) 
                                            ) 
                                            NIL
                                            T
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (:down 
                (if (or (equal paredes 1) (equal paredes 3)  (equal paredes 5) (equal paredes 7) (equal paredes 9) (equal paredes 11) (equal paredes 13) (equal paredes 15 ) )
                    NIL
                    ;;else
                    (progn
                        (setq panterior (get-cell-walls (1- y) x))
                        (setq floor (floor? (fifth (locate-node *current-ancestor* *memory*)) (list (1- y) x)))
                        (if (or 
                                (and (equal panterior 16) (equal floor 1)) 
                                (and (equal panterior 17) (equal floor 2))
                            ) 
                            NIL
                            T
                        )
                    )
                )
            )
            (:down-left
                (if (or (equal paredes 3) (equal paredes 7) (equal paredes 11) (equal paredes 15))
                    NIL
                    (progn
                        (setq py (get-cell-walls (1- y) x))
                        (if (or
                                (and 
                                     (or (equal paredes 2) (equal paredes 3) (equal paredes 6) (equal paredes 7) (equal paredes 10) (equal paredes 11) (equal paredes 14) (equal paredes 15)) 

                                     (or (equal py 2) (equal py 3) (equal py 6) (equal py 7) (equal py 10) (equal py 11) (equal py 14) (equal py 15))
                                )
                                (equal py 16)
                                (equal py 17)
                            )
                            NIL
                            (progn
                                (setq px (get-cell-walls y (1+ x)))
                                (if 
                                    (or 
                                        (and 
                                             (or (equal paredes 1) (equal paredes 3) (equal paredes 5) (equal paredes 7) (equal paredes 9) (equal paredes 11) (equal paredes 13) (equal paredes 15)) 

                                             (or (equal px 1) (equal px 3) (equal px 5) (equal px 7) (equal px 9) (equal px 11) (equal px 13) (equal px 15))
                                        )
                                        (equal px 16)
                                        (equal px 17)
                                    )
                                    NIL
                                    (progn
                                        (setq panterior (get-cell-walls (1- y) (1+ x)))
                                        (if (or 
                                             (equal panterior 12) (equal panterior 13) (equal panterior 14) (equal panterior 15)
                                             (equal panterior 16)
                                             (equal panterior 17) 
                                            ) 
                                            NIL
                                            T
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (:left 
                (if (or (equal paredes 2) (equal paredes 3)  (equal paredes 6) (equal paredes 7) (equal paredes 10) (equal paredes 11) (equal paredes 14) (equal paredes 15 ) (and (equal paredes 16) (equal floor 2)) (and (equal paredes 17) (equal floor 1)))
                    NIL
                    ;;else
                    (progn
                        (setq panterior (get-cell-walls  y (1+ x)))
                        (setq floor (floor? (fifth (locate-node *current-ancestor* *memory*)) (list y (1+ x))))
                        (if (or 
                                (and (equal panterior 16) (equal floor 2)) 
                                (and (equal panterior 17) (equal floor 1))
                            ) 
                            NIL
                            T
                        )
                    )
                )
            )
            (:up-left  
                (if (or (equal paredes 6) (equal paredes 7) (equal paredes 14) (equal paredes 15))
                    NIL
                    (progn
                        (setq py (get-cell-walls (1+ y) x))
                        (if 
                            (or
                                (and 
                                    (or (equal paredes 2) (equal paredes 3) (equal paredes 6) (equal paredes 7) (equal paredes 10) (equal paredes 11) (equal paredes 14) (equal paredes 15)) 

                                    (or (equal py 2) (equal py 3) (equal py 6) (equal py 7) (equal py 10) (equal py 11) (equal py 14) (equal py 15))
                                )
                                (equal py 16)
                                (equal py 17)
                            )
                            NIL
                            (progn
                                (setq px (get-cell-walls y (1+ x)))
                                (if 
                                    (or 
                                        (and 
                                            (or (equal paredes 4) (equal paredes 5) (equal paredes 6) (equal paredes 7) (equal paredes 12) (equal paredes 13) (equal paredes 14) (equal paredes 15)) 

                                            (or (equal px 4) (equal px 5) (equal px 6) (equal px 7) (equal px 12) (equal px 13) (equal px 14) (equal px 15))
                                        )
                                        (equal px 16)
                                        (equal px 17)
                                    )
                                    NIL
                                    (progn
                                        (setq panterior (get-cell-walls (1+ y) (1+ x)))
                                        (if (or 
                                             (equal panterior 9) (equal panterior 11) (equal panterior 13) (equal panterior 15)
                                             (equal panterior 16) 
                                             (equal panterior 17) 
                                            ) 
                                            NIL
                                            T
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
            (T "error")
        )
     
    )  
)

;;=======================================================================
;;
;;  valid-pos [pos]
;;        Predicado.  Indica si [pos]  esta dentro del laberinto
;;
;;=======================================================================
(defun  valid-pos (pos)
"Predicado. Valida que la posicion no se salga del tamaño del laberinto"
    (let 
        (   
            (y (first pos))
            (x (second pos))
        )
     
        (if (and (> y -1) (> x -1))
            (if (and (< y (get-maze-rows)) (< x (get-maze-cols)))
                T
                NIL
            )
            ;;else
            NIL
        )
    )  
)

;;=======================================================================
;;  APPLY-OPERATOR [op, pos]
;;        Solución simbólica del problema
;;          la posicion tiene estructura:  [<y> <x>]
;;
;;=======================================================================

(defun  apply-operator (op  pos) 
"Obtiene el descendiente de [pos] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  ((y (first pos))     
            (x (second pos))
	        (operador (first op))         ;; este operador es la etiqueta humana del operador...
           )
        (case operador 
            (:up  
                (list (1- y) x)
            )
            (:up-right  
                (list (1- y) (1+ x))
            )
            (:right  
                (list y (1+ x))
            )
            (:down-right  
                (list (1+ y) (1+ x))
            )
            (:down 
                (list (1+ y) x)
            )
            (:down-left
                (list (1+ y) (1- x))
            )
            (:left 
                (list y (1- x))
            )
            (:up-left  
                (list (1- y) (1- x))
            )
            (T "error")
        )
    )
)

;;=======================================================================
;;  EXPAND [posicion]
;;        Construye y regresa una lista con todos los descendientes validos de [pos]
;;=======================================================================
(defun expand (pos)
    "Obtiene todos los descendientes válidos de un pos, aplicando todos los operadores en *ops* en ese mismo órden"
    (let ((descendientes  nil)(nuevo-pos  nil))
        (dolist  (op  *Ops*  descendientes) 
	       (setq  nuevo-pos  (apply-operator  op pos))
		   (when (and (valid-pos  nuevo-pos) (valid-operator  op  nuevo-pos))
	           (setq  descendientes  (cons  (list nuevo-pos op (floor? op nuevo-pos)) descendientes))
           )
        )
    ) 
)

;;=======================================================================
;;  remember-pos?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;=======================================================================
(defun  remember-pos?  (pos  lista-memoria &optional (floor 3))
    "Busca un pos en una lista de nodos que sirve como memoria de intentos previos
     el pos tiene estructura:  [(<l0><o0><w0><b0>) (<l1><o1><w1><b1>)],
     el nodo tiene estructura : [<Id> <pos> <id-ancestro> <operador> ]"  
    (let* (
            (y (first pos))
            (x (second pos))
            (paredes (get-cell-walls y x))
         )
        (if (or (equal paredes 16) (equal paredes 17))
            (cond ((null  lista-memoria)  Nil)
                (
                    (and
                        (equal  pos  (second (first  lista-memoria)))  ;;el pos es igual al que se encuentra en el nodo?
                        (equal  floor (fourth (first  lista-memoria)))
                    )
                    T
                )  
                (T  (remember-pos?  pos  (rest  lista-memoria)))
            )
            ;;else
            (cond ((null  lista-memoria)  Nil)
                (
                    (equal  pos  (second (first  lista-memoria)))  ;;el pos es igual al que se encuentra en el nodo?    
                    T
                )  
                (T  (remember-pos?  pos  (rest  lista-memoria)))
            )
        )
    )
)


(defun  filter-memories (lista-pos-ops-floor memorias) ;;recibe posibles sucesores
    "Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria que se le ingrese
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (let ((memoriasXD memorias))
        (cond 
            ((null  lista-pos-ops-floor)  
                Nil
            )
            ((remember-pos? (first (first  lista-pos-ops-floor)) memoriasXD (third (first lista-pos-ops-floor)))  ;; si se recuerda el primer elemento de la lista, filtrarlo...
                (filter-memories  (rest  lista-pos-ops-floor) memoriasXD)
            )
            (T  ;; de lo contrario, incluirlo en la respuesta
                (cons (first lista-pos-ops-floor) (filter-memories  (rest  lista-pos-ops-floor) memoriasXD)) 
            ) 
        ) 
     )
)  

;;===========================================================
;;
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;
;;       extract-solution   recibe un nodo (el que contiene al pos meta) que ya se encuentra en la memoria y
;;                          rastrea todos sus ancestros hasta llegar  al  nodo que contiene al pos inicial...
;;
;;       display-solution   despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                          el proceso de solución del problema...
;;
;;
;;  Nodo
;;  [<id> <posicion> <id-ancestro> <operador>]
;;
;;===========================================================

(defun locate-node  (id  lista)       ;;busca un nodo por Id  y si lo encuentra regresa el nodo completo
     (cond ((null  lista)  Nil)
           ((eql  id  (first (first  lista))) (first  lista))
           (T  (locate-node  id (rest  lista)))
     )
)


(defun extract-solution (nodo)
    "Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al pos inicial
    el nodo tiene estructura : [<id> <posicion-actual> <id-ancestro> <operador>]"
    (let ((current  (locate-node  (first  nodo)  *memory*)))
        (loop  while  (not (null  current))  do                        
             (push  current  *solucion*)     ;; agregar a la solución el nodo actual
             (setq  current  (locate-node  (third  current) *memory*)) ;; y luego cambiar a su antecesor...
        )   
    )
    *solucion*
)


(defun  display-solution (lista-nodos)
    "Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "Solución con ~A  pasos~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
              (setq  nodo  (nth  i  lista-nodos))
              (if  (= i 0)
                   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este pos inicial
                   ;;else
                   (progn
                        (if (= (length nodo) 6) 
                            (format t "\(~A\) aplicando ~A  se  llega  a  ~A . Heuristica ~A  ~%"  i (second (fifth  nodo))  (second  nodo) (sixth nodo));; imprimir el número de paso, operador y pos...
                            (if (= (length nodo) 8) 
                                (format t "\(~A\) aplicando ~A  se  llega  a  ~A. Heuristica ~A ~%"  i (second (fifth  nodo))  (second  nodo) (eighth nodo));; imprimir el número de paso, operador y pos...
                                (format t "\(~A\) aplicando ~A  se  llega  a  ~A ~%"  i (second (fifth  nodo))  (second  nodo));; imprimir el número de paso, operador y pos...
                            )
                        )
                        (push (second (fifth  nodo)) *solution*)
                   )
              )
          )
          (setq *solution* (reverse *solution*))
          (print *solution*)
     )  
)

;;************      fin funciones de apoyo      **************




;;************       breath-first        ************
(defun breath-first () 
    "Realiza una búsquedacon el metodo :breath-first - búsqueda en anchura"
    
  ;;(format t "Posición inicial: ~S~%" *start*) (format t "Posición meta: ~S~%" *goal*)
  ;;(format t "Número total de filas: ~S~%" (get-maze-rows))
  ;;(format t "Número total de columnas ~S~%" (get-maze-cols))
  ;;(format t "Paredes de la celda #(4 0): ~S~%" (get-cell-walls 4 0))
  ;;(format t "Dibujo de las paredes de la celda #(4 0):~%")
  ;;(draw-cell-walls 4 0)
  ;;(format t "~%Datos del laberinto: ~%~S~%" (get-maze-data))
    
 
  (reset-all)
  (let* (
            (nodo nil)
	        (pos nil)
	        (sucesores  '())
	        (operador  nil)
	        (meta-encontrada  nil)
            (pos-inicial (list (aref *start* 0) (aref *start* 1)))
            (pos-meta   (list (aref *goal* 0) (aref *goal* 1)))
            (metodo :breath-first)
        )
        (format  t  "Posicion Inicial ~A        Posicion meta ~A~%" pos-inicial pos-meta)
        (insert-to-open   pos-inicial  nil  metodo)
        (loop until  (or  meta-encontrada (null *open*))  do
	       (setq  nodo    (get-from-open) pos  (second  nodo) operador  (third  nodo))
           ;;(setq *open* (delete NIL *open*))
	       (push  nodo  *memory*)
	       (cond    
                ((equal  pos-meta  pos)  
                    ;;(format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
                    (display-solution  (extract-solution  nodo))
                    (setq  meta-encontrada  T)
                 
                 )
                 (t 
                     (setq  *current-ancestor*  (first  nodo)) 
                     (setq  sucesores  (expand pos))
                     (setq  sucesores  (filter-memories  sucesores *memory*))
                     (loop for  element  in  sucesores  do
                        (insert-to-open  (first element)  (second element)  metodo)
                     )
                 )
            )
        )
    )  

  ;;(setq *solution* '(3 4 4 4 7 0 7 7 5 3 3 5 7))
)
;;************      fin-breath-first        ************

;;************       depth-first        ************
(defun depth-first ()
    "Realiza una búsquedacon el metodo :depth-first - búsqueda en profundidad"
 
  (reset-all)
  (let* (
            (nodo nil)
	        (pos nil)
	        (sucesores  '())
	        (operador  nil)
	        (meta-encontrada  nil)
            (pos-inicial (list (aref *start* 0) (aref *start* 1)))
            (pos-meta   (list (aref *goal* 0) (aref *goal* 1)))
            (metodo :depth-first)
        )
        (format  t  "Posicion Inicial ~A        Posicion meta ~A~%" pos-inicial pos-meta)
        (insert-to-open   pos-inicial  nil  metodo)
        (loop until  (or  meta-encontrada (null *open*))  do
	       (setq  nodo    (get-from-open) pos  (second  nodo) operador  (third  nodo))
	       (push  nodo  *memory*)
           ;;(setq *open* (delete NIL *open*))
	       (cond    
                ((equal  pos-meta  pos)  
                    ;;(format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
                    (display-solution  (extract-solution  nodo))
                    (setq  meta-encontrada  T)
                 
                 )
                 (t 
                     (setq  *current-ancestor*  (first  nodo)) 
                     (setq  sucesores  (expand pos))
                     (setq  sucesores  (filter-memories  sucesores *memory*))
                     (loop for  element  in  sucesores  do
                        (insert-to-open  (first element)  (second element)  metodo)
                     )
                 )
            )
        )
    )  
    
;;(setq *solution* '(3 4 4 5 0 0 7 7 5 3 3 6 5 0))
)
;;************      fin-depth-first        ************


;;************       best-first-search        ************
(defun  bestfs ()
    "Realiza una búsqueda bestFS, por el método especificado y desde un pos inicial hasta un pos meta
    los métodos posibles son:  :moves-left     - evalua los movimientos necesarios para que cada pieza este en su lugar"
  (reset-all)
  (let (
            (nodo nil)
	        (pos nil)
	        (sucesores  '())
	        (operador  nil)
	        (meta-encontrada  nil)
            (pos-inicial  (list (aref *start* 0) (aref *start* 1)))
            (pos-meta   (list (aref *goal* 0) (aref *goal* 1)))
            (metodo :moves-left)
        )
       (format  t  "Posicion Inicial ~A        Posicion meta ~A~%" pos-inicial pos-meta)
        (insert-to-open   pos-inicial  nil metodo pos-meta)
             
        (loop until  (or  meta-encontrada (null *open*))  do ;;for z from 1 to 10 do 

               (setq  nodo  (get-from-open ) pos  (second  nodo) operador (third  nodo))
                ;;(if (> (length *open*) *max-open*)
                ;;    (setq *max-open* (length *open*))    
                ;;)
               (setq *open* (delete nodo *open*))
               ;;(setq *open* (delete NIL *open*))
               (push  nodo  *memory*)
               (cond    
                    ((equal  pos-meta  pos)  
                        (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
                        (display-solution  (extract-solution  nodo))
                        (setq  meta-encontrada  T)
                     )
                     (t 
                         (setq  *current-ancestor*  (first  nodo)) 
                         (setq  sucesores  (expand pos))
                      ;;(print "sucesores")
                      ;;(print sucesores)
                      ;;(print "memory")
                      ;;(print *memory*)
                      ;;(print "open")
                        ;;(print *open*)
                         (setq  sucesores  (filter-memories  sucesores *memory*))
                         (setq  sucesores  (filter-memories  sucesores *open*))
                         (loop for  element  in  sucesores  do
                            (insert-to-open  (first element)  (second element) metodo pos-meta)
                             ;;(setq *nodo-cre* (1+ *nodo-cre*))
                         )
                        ;;sort *open*
                        (sort *open* '< :key 'sixth)
                     )
                )
        )
            
    )  
)
			     
;;************      fin-best-first-search        ************

;;************       best-a-star       ************
(defun  astar ()
    "Realiza una búsqueda Astar, por el método especificado y desde un pos inicial hasta un pos meta
    los métodos posibles son:  :moves-left     - evalua los movimientos necesarios para llegar a la meta"
  (reset-all)
  (let (
            (nodo nil)
	        (pos nil)
	        (sucesores  '())
	        (operador  nil)
	        (meta-encontrada  nil)
            (pos-inicial  (list (aref *start* 0) (aref *start* 1)))
            (pos-meta   (list (aref *goal* 0) (aref *goal* 1)))
            (metodo :moves-left)
        )
        (insert-to-open   pos-inicial  nil metodo pos-meta -1)
        (format  t  "Posicion Inicial ~A        Posicion meta ~A~%" pos-inicial pos-meta)
        (loop until  (or  meta-encontrada (equal *open* NIL) )  do ;;for z from 1 to 10 do 

               (setq  nodo  (get-from-open ) pos  (second  nodo) operador (third  nodo))
                ;;(if (> (length *open*) *max-open*)
                ;;    (setq *max-open* (length *open*))    
                ;;)
               (setq *open* (delete nodo *open*))
               ;;(setq *open* (delete NIL *open*))
               (push  nodo  *memory*)
               (cond    
                    ((equal  pos-meta  pos)  
                        (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
                        (display-solution  (extract-solution  nodo))
                        (setq  meta-encontrada  T)
                     )
                     (t 
                         (setq  *current-ancestor*  (first  nodo))
                         (setq  *current-prof* (seventh nodo))
                         (setq  sucesores  (expand pos))
                      ;;(print "sucesores")
                      ;;(print sucesores)
                      ;;(print "memory")
                      ;;(print *memory*)
                      ;;(print "open")
                        ;;(print *open*)
                         (setq  sucesores  (filter-memories  sucesores *memory*))
                         (setq  sucesores  (filter-memories  sucesores *open*))
                         (loop for  element  in  sucesores  do
                            (insert-to-open  (first element)  (second element) metodo pos-meta (1+ *current-prof*))
                             ;;(setq *nodo-cre* (1+ *nodo-cre*))
                         )
                        ;;sort *open*
                        (sort *open* '< :key 'eighth)
                     )
                )
        )
            
    )  
)
			     
;;************      fin-a-star        ************
;;(trace expand)
(start-maze)