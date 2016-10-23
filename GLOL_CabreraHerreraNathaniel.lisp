;;;=======================================================================
;;;  GLOL.lisp
;;;      Resuelve  el  problema  de  Granjero, Legumbres, Obeja y Lobo  con  búsqueda ciega, a lo profundo y a lo ancho.
;;;   
;;;       Basado en el codigo explicado en clase.  Presentaciones #3 y #4
;;;      Cabrera Herrera Nathaniel
;;;  Septiembre, 2016
;;;=======================================================================
(defparameter  *open* '())       ;; Frontera de busqueda...                                              
(defparameter  *memory* '())   ;; Memoria de intentos previos

(defparameter  *ops*  '( 
             (:legumbres     (1 0 0))    ;; Operadores para el problema GLOL (<legumbres> <obeja> <lobo>)
			 (:obeja         (0 1 0))
			 (:lobo          (0 0 1))
			 (:nada          (0 0 0))    ;;cuando no carga nada en la barca, solo cambia de orilla
                        )
)

(defparameter  *id*  0)  ;; Identificador del ultimo nodo creado, cada vez que se cree un nodo se debe incrementar
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

;;=======================================================================
;;  CREATE-NODE [estado  op]  
;;      estado - Un estado del problema a resolver (sistema)...
;;             op - El operador cuya aplicación generó el [estado]...
;;=======================================================================
(defun  create-node (estado  op)
    "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro "
    (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
    (list  (1-  *id*)  estado  *current-ancestor*  (first op)) 
)  ;;los nodos generados son descendientes de *current-ancestor*

;;=======================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;=======================================================================
(defun insert-to-open (estado  op  metodo) 
    "Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
    (let ((nodo  (create-node  estado  op)))
         (cond 
            ((eql  metodo :depth-first)
	                  (push  nodo  *open*))
	        ((eql  metodo :breath-first)
		          (setq *open*  (append  *open*  (list nodo))))
	        (T  Nil)
         )
    )  
)


(defun get-from-open ()
    "Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
    (pop  *Open*)
)

;;=======================================================================
;;  BARGE-SHORE [estado]
;;        Regresa la orilla del rio en la que se encuentra la barca en  [estado]
;;           0 - Orilla origen (primer sublista del estado)
;;           1 - Orilla destino (segunda sublista del estado)
;;=======================================================================
(defun  barge-shore (estado)
    "Regresa la orilla del río en la que se encuentra la barca en el estado recibido como parámetro:  0 - origen  1 - destino"
    (if  (= 1 (fourth (first  estado)))  
         0  
         1
    )
)


;;=======================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos en la orilla donde esta la barca
;;=======================================================================
(defun  valid-operator (op  estado)
    "Predicado. Valida la aplicación de un operador a un estado...
     el estado tiene estructura:  [(<l0><o0><w0><b0>) (<l1><o1><w1><b1>)] ,
     el operador tiene estructura : [<etiqueta-humana> <lista operador con (<bol legumbres><bol obeja><bol lobo)>]"  
    (let*  
        ((orilla  (barge-shore  estado)) (legumbres  (first  (nth  orilla  estado))) (obeja (second  (nth  orilla  estado))) (lobo (third  (nth  orilla  estado))))
     
        (and  (>=  legumbres  (first (second op)))             ;;legumbres a mover, primer elemento de la lista-operador
              (>=  obeja   (second (second op)))            ;;obeja a mover, segundo elemento de la lista-operador
              (>=  lobo   (third (second op)))            ;;lobo a mover, tercer elemento de la lista-operador
        )
    )  
)

;;=======================================================================
;;  VALID-STATE [estado]
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;               1. La obeja no puede estar en la misma orilla que las legumbres
;;               2. El lobo no puede estar con la obeja en la misma orilla
;;=======================================================================
(defun  valid-state (estado)
"Predicado. Valida  un estado según las restricciones generales del problema...
       el estado tiene estructura:  [(<l0><o0><w0><b0>) (<l1><o1><w1><b1>)]"
    (let 
        (   
            (l0  (first (first estado))) (o0  (second (first estado))) (w0  (third (first estado))) (b0  (fourth (first estado))) 
            (l1  (first (second estado))) (o1  (second (second estado))) (w1  (third (second estado))) 
        )
     
        (if (= 0 b0);;si granjero no esta en la orilla1
            (if (= 1 o0)
                (cond
                    ((or (= 1 w0) (= 1 l0)) NIL)
                    (T T)
                )
                T
            )
            (if (= 1 o1)
                (cond
                    ((or (= 1 w1) (= 1 l1)) NIL)
                    (T T)
                )
                T
            )
        )
        
    )  
)

    
;;=======================================================================
;;  APPLY-OPERATOR [op, estado]
;;        Solución simbólica del problema
;;          el estado tiene estructura:  [(<l0><o0><L0><b0>) (<l1><o1><L1><b1>)]
;;          Donde:
;;              * l : son las legumbres
;;              * o : es la obeja
;;              * L : es el lobo
;;              * b : es la barca
;;=======================================================================

(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  ((orilla1  (first  estado))
	       (orilla2  (second  estado))
	       (l0   (first orilla1))       ;; obtengo los valores de la orilla1
   	       (o0   (second orilla1))
	       (w0   (third  orilla1))
           (b0   (fourth  orilla1))
	       (l1   (first  orilla2))      ;; obtengo los valores de la orilla2
	       (o1   (second  orilla2))
	       (w1   (third   orilla2))
           (b1   (fourth   orilla2))
	       ;;(orilla-barca  (barge-shore estado))    ;;identifica en que orilla esta la barca 
	       (operador (first op)))      ;; este operador es la etiqueta humana del operador...
        (case operador 
            (:legumbres  ;; cambia las legumbres y la barca de orilla 
                (list  
                    (list  (flip l0) o0 w0 (flip b0))   
                    (list  (flip l1) o1 w1 (flip b1))
                )
            )
            (:obeja      ;; cambia la obeja y la barca de orilla
               (list  
                    (list  l0 (flip o0) w0 (flip b0))   
                    (list  l1 (flip o1) w1 (flip b1))
               )
            ) 
            (:lobo      ;; cambia la obeja y la barca de orilla
               (list  
                    (list  l0 o0 (flip w0) (flip b0))   
                    (list  l1 o1 (flip w1) (flip b1))
               )
            ) 
            (:nada      ;; solo cambia la barca de orilla
               (list  
                    (list  l0 o0 w0 (flip b0))   
                    (list  l1 o1 w1 (flip b1))
               )
            ) 
            (T "error")
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
        (dolist  (op  *Ops*  descendientes) 
	       (setq  nuevo-estado  (apply-operator  op estado))
		   (when (and (valid-operator  op  estado) (valid-state  nuevo-estado))
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
     el estado tiene estructura:  [(<l0><o0><w0><b0>) (<l1><o1><w1><b1>)],
     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"  
    (cond ((null  lista-memoria)  Nil)
        (
            (equal  estado  (second (first  lista-memoria)))  ;;el estado es igual al que se encuentra en el nodo?
            T
        )  
        (T  (remember-state?  estado  (rest  lista-memoria)))
    ) 
)


(defun  filter-memories (lista-estados-y-ops) 
    "Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond 
        ((null  lista-estados-y-ops)  
            Nil
        )
        ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
            (filter-memories  (rest  lista-estados-y-ops))
        )
		(T  ;; de lo contrario, incluirlo en la respuesta
            (cons (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))) 
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
    el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"
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
    (format  t  "Solución con ~A  pasos~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
              (setq  nodo  (nth  i  lista-nodos))
              (if  (= i 0)
                   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
                   ;;else
                   (format t "\(~A\) aplicando ~A  se  llega  a  ~A~%"  i (fourth  nodo)  (second  nodo));; imprimir el número de paso, operador y estado...
              )
          )
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
)


(defun  blind-search (edo-inicial  edo-meta  metodo)
    "Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                               :breath-first - búsqueda en anchura"
  (reset-all)
  (let (
            (nodo nil)
	        (estado nil)
	        (sucesores  '())
	        (operador  nil)
	        (meta-encontrada  nil)
        )
        (insert-to-open   edo-inicial  nil  metodo)
        (let (tempo1))
        (setq tempo1 (get-internal-real-time))
        (loop until  (or  meta-encontrada (null *open*))  do
	       (setq  nodo    (get-from-open) estado  (second  nodo) operador  (third  nodo))
	       (push  nodo  *memory*)
	       (cond    
                ((equal  edo-meta  estado)  
                    (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
                    (display-solution  (extract-solution  nodo))
                    (setq  meta-encontrada  T)
                 )
                 (t 
                     (setq  *current-ancestor*  (first  nodo)) 
                     (setq  sucesores  (expand estado))
                     (setq  sucesores  (filter-memories  sucesores))
                     (loop for  element  in  sucesores  do
                        (insert-to-open  (first element)  (second element)  metodo)
                     )
                 )
            )
        )
        (let (tempo2))
        (setq tempo2 (get-internal-real-time))
       (format t "Tiempo para encontrar la solución: ~A segundos~%" (/ (- tempo1 tempo2) 1000))
    )  
)
			     
     
;;=======================================================================
;;=======================================================================
