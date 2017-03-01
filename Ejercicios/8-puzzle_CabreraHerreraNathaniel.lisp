;;;=======================================================================
;;;  8-puzzle.lisp
;;;      Resuelve  el  problema  de  8-puzzle
;;;   
;;;      Codigo original by Cabrera Herrera Nathaniel
;;;  Septiembre, 2016
;;;  
;;;=======================================================================
(defparameter  *open* '())       ;; Frontera de busqueda...                                              
(defparameter  *memory* '())   ;; Memoria de intentos previos

(defparameter  *ops*  '( 
             (:up     )    ;; Operadores para el problema 
			 (:down   )
			 (:right  )
			 (:left   )   
                        )
)

(defparameter  *id*  0)  ;; Identificador del ultimo nodo creado, cada vez que se cree un nodo se debe incrementar
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria
(defparameter *posiciones* '((1 1) (0 0) (1 0) (2 0) (2 1) (2 2) (1 2) (0 2) (0 1))) ;; usado en moves-left
(defparameter *edo-meta* '((1 2 3)(8 0 4)(7 6 5))) ;; no necesita explicacion XD
(defparameter *nodo-expand* 0)
(defparameter *max-open* 0)
(defparameter *nodo-cre* 0)


;;=======================================================================
;;  CREATE-NODE [estado  op]  
;;      estado - Un estado del problema a resolver (sistema)...
;;             op - El operador cuya aplicación generó el [estado]...
;;      Genera nodo con la siguiente estructura:  (<id> <estado> <ancestro> <operacion> <v.heuristico>)
;;=======================================================================
(defun  create-node (estado  op heur)
    "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro "
    (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
    (list  (1-  *id*)  estado  *current-ancestor*  (first op) heur) 
)  ;;los nodos generados son descendientes de *current-ancestor*

;;=======================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista de datos, genera un nodo, obtiene su heuristica y lo inserta en *open*
;;        Insert-to-memory recibe un nodo y lo inserta en memory
;;        Get-from-open  siempre retira el elemento con menor valor heuristico
;;      nodo :  ( id estado ancestro operacion v.heuristico)
;;=======================================================================

(defun posi-dif (num i j);;funcion de apoyo para metrica moves-left
    (let((n  num))
        (if (equal (nth n *posiciones*) (list i j) )
            0
            (+ 
                (abs (- (first (nth n *posiciones*)) i))
                (abs (- (second (nth n *posiciones*)) j))
            )
        )    
    )
)

(defun insert-to-open (estado  op metodo) 
    "Permite insertar nodos de la frontera de busqueda *open*"
    (let ((heur 0)(met metodo))
        (case met ;; se cualcula la heuristica
            (:wrong-pieces
                (if (not (equal 1 (first (first estado))))      (setq heur (1+ heur))   )
                (if (not (equal 2 (second (first estado))))     (setq heur (1+ heur))   )
                (if (not (equal 3 (third (first estado))))      (setq heur (1+ heur))   )
                (if (not (equal 8 (first (second estado))))     (setq heur (1+ heur))   )
                (if (not (equal 4 (third (second estado))))     (setq heur (1+ heur))   )
                (if (not (equal 7 (first (third estado))))      (setq heur (1+ heur))   )
                (if (not (equal 6 (second (third estado))))     (setq heur (1+ heur))   )
                (if (not (equal 5 (third (third estado))))      (setq heur (1+ heur))   )
            )
            (:moves-left
                (do ((j 0 (1+ j))) ((= j 3)) ;;vertical
                    (do ((i 0 (1+ i))) ((= i 3)) ;;horizontal
                        (setq heur (+ (posi-dif (nth i (nth j estado)) i j) heur))
                    )   
                ) 
            )
            (:random-value
                (setq heur (random 15))
            )
            (:custom-value
                (setq heur (mod *id* 23))
            )
            (T (print "error metodo incorrecto"));; XD metod equivocado
        )

        (let ((nodo  (create-node  estado op heur)));;se inserta a open el nodo
             (push  nodo  *open*)
        )
    )  
)


(defun insert-to-memory (nodo) 
    "Permite insertar nodo  a *memory*"
         (push  nodo  *memory*)
)


(defun get-from-open ()
    "Recupera el siguiente elemento a revisar de  frontera de busqueda *open*
     siendo este aquel el primer nodo con valor minimo en  v.heuristico
     el nodo es: (<id> <estado> <ancestro> <operacion> <v.heuristico>)"
    ;;(pop  *Open*)
    (let ((nodo ()) (heur 1000))
        (dolist (n *open* nodo)
            (progn
                (if (< (fifth n) heur)
                    (progn 
                        (setq nodo n)
                        (setq heur (fifth n))
                    )
                )
            )
        )
    )    
)


;;=======================================================================
;;  APPLY-OPERATOR [op, estado]
;;        Solución simbólica del problema  
;;              
;;=======================================================================

;;(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*   (
            (ff   (list (first (first estado))     (list 0 0 )) )       
            (sf   (list (second (first estado))    (list 1 0 )) )
            (tf   (list (third (first estado))     (list 2 0 )) )
            (fs   (list (first (second estado))    (list 0 1 )) )
            (ss   (list (second (second estado))   (list 1 1 )) )
            (ts   (list (third (second estado))    (list 2 1 )) )
            (ft   (list (first (third estado))     (list 0 2 )) )
   	        (st   (list (second (third estado))    (list 1 2 )) )
            (tt   (list (third (third estado))     (list 2 2 )) )
	        (operador (first op))      ;; este operador es la etiqueta humana del operador...
            (lol ())    ;;contendra la posicion de el espacio vacio
            (clonedo (list 
                      (list (first ff) (first sf) (first tf))
                      (list (first fs) (first ss) (first ts))
                      (list (first ft) (first st) (first tt)))
            )    ;;sera la copia del estado que modificare
           )
           ;; *********** encuentra la posision de la casilla vacia representada por 0
           (do ((j 0 (1+ j))) ((= j 3)) ;;vertical
                (do ((i 0 (1+ i))) ((= i 3)) ;;horizontal
                    (if (equal 0 (nth i (nth j estado))) 
                        (setq lol (list i j))
                    )
                )   
            ) 
           
           
        (case operador 
            (:up  ;;sube la pieza de abajo del espacio vacio representado por 0 
                (if (> (second lol) 1)
                    (setq clonedo NIL) ;;si el 0 esta en la primera columna entonces no se puede subir la pieza, por que no hay
                    (let ((aux))    ;;si hay pieza abajo
                        (setq aux (nth (first lol) (nth (1+ (second lol)) clonedo)))    ;;obtengo el valor de la ficha inferior 
                        (setf (nth (first lol) (nth (second lol) clonedo)) aux )        ;;pongo la ficha inferior en el espacio vacio
                        (setf (nth (first lol) (nth (1+ (second lol)) clonedo)) 0 )     ;;pongo 0 en el espacio inferior(que sera el vacio)
                         clonedo
                    )
                )
             
            )
            (:down  ;;baja la pieza de arriba del espacio vacio representado por 0  
                (if (< (second lol) 1)
                    (setq clonedo NIL) ;;si el 0 esta en la ultima columna entonces no se puede bajar la pieza, por que no hay
                    (let ((aux))    ;;si hay pieza arriba
                        (setq aux (nth (first lol) (nth (1- (second lol)) clonedo)))    ;;obtengo el valor de la ficha superior
                        (setf (nth (first lol) (nth (second lol) clonedo)) aux )        ;;pongo la ficha superior en el espacio vacio
                        (setf (nth (first lol) (nth (1- (second lol)) clonedo)) 0 )     ;;pongo 0 en el espacio superior(que sera el vacio)
                         clonedo
                    )
                )
            ) 
            (:right   ;;mueve a la derecha la pieza de la izquierda del espacio vacio representado por 0 
                (if (< (first lol) 1)
                    (setq clonedo NIL) ;;si el 0 esta en la primera fila entonces no se puede mover a la derecha la pieza, por que no hay
                    (let ((aux))    ;;si hay pieza a la izquierda
                        (setq aux (nth (1- (first lol)) (nth (second lol) clonedo)))    ;;obtengo el valor de la ficha izquierda 
                        (setf (nth (first lol) (nth (second lol) clonedo)) aux )        ;;pongo la ficha izquierda en el espacio vacio(derecho)
                        (setf (nth (1- (first lol)) (nth (second lol) clonedo)) 0 )     ;;pongo 0 en el espacio izquierdo(que sera el vacio)
                         clonedo
                    )
                )
            ) 
            (:left   ;;mueve a la izquierda la pieza de la derecha del espacio vacio representado por 0  
                (if (> (first lol) 1)
                    (setq clonedo NIL) ;;si el 0 esta en la primera fila entonces no se puede mover a la derecha la pieza, por que no hay
                    (let ((aux))    ;;si hay pieza a la izquierda
                        (setq aux (nth (1+ (first lol)) (nth (second lol) clonedo)))    ;;obtengo el valor de la ficha derecha
                        (setf (nth (first lol) (nth (second lol) clonedo)) aux )        ;;pongo la ficha derecha en el espacio vacio(izq)
                        (setf (nth (1+ (first lol)) (nth (second lol) clonedo)) 0 )     ;;pongo 0 en el espacio derecho(que sera el vacio)
                         clonedo
                    )
                )
            ) 
            (T (print "error operador incorrecto"));; XD operador equivocado
        )
    )
)


;;=======================================================================
;;  EXPAND [ estado]
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;=======================================================================
(defun expand (estado)
    "Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
    (let ((descendientes  nil)(nuevo-estado  nil))
         (setq *nodo-expand* (1+ *nodo-expand*))
        (dolist  (op  *Ops*  descendientes) 
	       (setq  nuevo-estado  (apply-operator  op estado))
		   (when (not (equal () nuevo-estado))
	           (setq  descendientes  (cons  (list nuevo-estado op) descendientes))
               
           )
        )
    ) 
)


;;=======================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;=======================================================================
(defun  remember-state?  (estado  lista-memoria)    
    "Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [(<ff> <sf> <tf) (<fs><ss><ts>)(<ft><st><tt>)] con f:first s:second t:third,
     el nodo tiene estructura : [<id> <estado> <ancestro> <operacion> <v.heuristico>]"  
    (cond ((null  lista-memoria)  Nil);;si la memoria esta vacia regresa nil
        (
            (equal  estado  (second (first  lista-memoria)))  ;;el estado es igual al que se encuentra en el nodo?
            T
        )  
        (T  (remember-state?  estado  (rest  lista-memoria)))
    ) 
)


(defun  filter-memories (lista-estados-y-ops memorias) ;;recibe posibles sucesores
    "Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria que se le ingrese
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (let ((memoriasXD memorias))
        (cond 
            ((null  lista-estados-y-ops)  
                Nil
            )
            ((remember-state? (first (first  lista-estados-y-ops)) memoriasXD)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
                (filter-memories  (rest  lista-estados-y-ops) memoriasXD)
            )
            (T  ;; de lo contrario, incluirlo en la respuesta
                (cons (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops) memoriasXD)) 
            ) 
        ) 
     )
)  

;;=======================================================================
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;=======================================================================
(defun extract-solution (nodo)
    "Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial
    el nodo tiene estructura : [<id> <estado> <ancestro> <operacion> <v.heuristico>]"
     (labels 
        (
            (locate-node  (id  lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		         (cond ((null  lista)  Nil)
			           ((eql  id  (first (first  lista))) (first  lista))
				       (T  (locate-node  id (rest  lista)))
                 )
            )
        )
	    (let ((current  (locate-node  (first  nodo)  *memory*)))
		    (loop  while  (not (null  current))  do                        
			     (push  current  *solucion*)     ;; agregar a la solución el nodo actual
			     (setq  current  (locate-node  (third  current) *memory*)) ;; y luego cambiar a su antecesor...
            )   
        )
	    *solucion*
     )
)


(defun  display-solution (lista-nodos)
    "Despliega la solución en forma conveniente y numerando los pasos"
    
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
              (setq  nodo  (nth  i  lista-nodos))
              (if  (= i 0)
                   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
                   ;;else
                   (format t "\(~A\) aplicando ~A  se  llega  a  ~A~%"  i (fourth  nodo)  (second  nodo));; imprimir el número de paso, operador y estado...
                   
              )
          )
          (format t "~% Nodos expandidos: ~A nodos~% " *nodo-expand*)
          (format t "Nodos creados: ~A nodos~% " *nodo-cre*)
          (format t "Maxima longitud de la frontera de busqueda: ~A~% " *max-open*)
          (format t "Longitud de la solución: ~A  pasos~%" (1- (length  lista-nodos)))
     )  
)

;;=======================================================================
;;  RESET-ALL  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solucion del problema...
;;=======================================================================
(defun reset-all () 
    "Reinicia todas las variables globales para iniciar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil)
     (setq *nodo-expand* 0)
     (setq *nodo-cre* 0)
     (setq *max-open* 0)
)


(defun  bestFS (edo-inicial  metodo)
    "Realiza una búsqueda bestFS, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :random-value    - valor heuristico random
                               :custom-value    - valor heuristico definido por me
                                :wrong-pieces   - evalua las piezas que estan colocadas de forma incorrecta 
                                :moves-left     - evalua los movimientos necesarios para que cada pieza este en su lugar"
  (reset-all)
  (let (
            (nodo nil)
	        (estado nil)
	        (sucesores  '())
	        (operador  nil)
	        (meta-encontrada  nil)
        )
        (insert-to-open   edo-inicial  nil metodo)
        (let ((tempo1) (tempo2))
            (setq tempo1 (get-internal-real-time))
             
            (loop until  (or  meta-encontrada (null *open*))  do ;;for z from 1 to 10 do 

               (setq  nodo  (get-from-open ) estado  (second  nodo) operador (third  nodo))
                (if (> (length *open*) *max-open*)
                    (setq *max-open* (length *open*))    
                )
               (setq *open* (delete nodo *open*))
               (push  nodo  *memory*)
               (cond    
                    ((equal  *edo-meta*  estado)  
                        (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
                        (display-solution  (extract-solution  nodo))
                        (setq  meta-encontrada  T)
                     )
                     (t 
                         (setq  *current-ancestor*  (first  nodo)) 
                         (setq  sucesores  (expand estado))
                      ;;(print "sucesores")
                      ;;(print sucesores)
                      ;;(print "memory")
                      ;;(print *memory*)
                      ;;(print "open")
                        ;;(print *open*)
                         (setq  sucesores  (filter-memories  sucesores *memory*))
                         (setq  sucesores  (filter-memories  sucesores *open*))
                         (loop for  element  in  sucesores  do
                            (insert-to-open  (first element)  (second element) metodo)
                             (setq *nodo-cre* (1+ *nodo-cre*))
                         )
                     )
                )
            )
            
            (setq tempo2 (get-internal-real-time))
           (format t " Tiempo para encontrar la solución: ~A segundos~%" (/ (- tempo1 tempo2) 1000))
        )
    )  
)
			     
     
;;=======================================================================
;;=======================================================================
