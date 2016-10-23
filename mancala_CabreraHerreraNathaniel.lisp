;;==========================================================
;;
;;      MANCALA
;;
;;      Reglas
;;
;;  1) MOVIMIENTOS: En  su  turno,  cada  jugador  elige  alguna  de  sus  casillas,
;;     remueve  TODAS  las fichas en  esa  casilla y  las  reparte, una  en  
;;     cada  una  de  las  siguientes  casillas,    excepto  en  la  base  del  oponente. 
;;
;;  2) TURNOS: Si  la  última  ficha  movida  cae  en  la  base  del  jugador  en  turno,
;;     entonces  ese  mismo  jugador  vuelve  a  tirar,  de  lo  
;;     contrario,  será  turno  del  siguiente  jugador... 
;;
;;  3) El  primer  jugador  que  vacíe  todas  sus  casillas,  captura  todas  las  fichas  
;;     restantes  de  su  oponente  y   las  agrega  a  su  base. 
;;
;;  FIN DE JUEGO: El  jugador  con  más  puntos  gana  la  partida... 
;;      
;;  Elaborado por: Nathaniel Cabrera Herrera
;;
;;  Estado tipo:
;;
;;      [   
;;          (<scoreA> <1a> <2a> <3a> <4a> <5a> <6a>)
;;          (<scoreB> <1b> <2b> <3b> <4b> <5b> <6b>)
;;      ]
;;
;;      ---------------------------------------------------------------------------------
;;      |            |  <6a>  |  <5a>  |  <4a>  |  <3a>  |  <2a>  |  <1a>  |            |
;;      -  <scoreA>  -------------------------------------------------------  <scoreB>  |
;;      |            |  <1b>  |  <2b>  |  <3b>  |  <4b>  |  <5b>  |  <6b>  |            |
;;      ---------------------------------------------------------------------------------
;;
;;      cada casilla tiene la siguiente informacion
;;          
;;          [ <NumeroAmarillas> <NumeroVerdes> <NumeroRojas> < NumeroTotalCanicas> <ValorTotal> ]
;;
;;      turno:  1 humano
;;              0 PC
;;
;;==========================================================
(defparameter  *score* '()) ;; [ <scoreA> <scoreB> ]
(defparameter  *turno* 0)

;;============================================
;;
;;  Mancala
;;      Funcion principal de juego
;;      
;;      
;;      turno : 1 humano
;;              0 pc
;;
;;============================================

;;------evaluacion
;;      
;;      aqui van las heuristicas para ganar y selecccionar el mejor movimiento
;;
;;----------------------------------------------



;;------fin-juego
;;      
;;      Verifica si el estado es un estado final
;;
;;----------------------------------------------

;;============================================
;;
;;  Mancala
;;      Funcion principal de juego
;;      
;;      
;;      turno : 1 humano
;;              0 pc
;;
;;============================================

(defun mancala ()
    (loop until  fin-juego)  do
        
    )
)

;;============================================
;;
;;  NegaMax-AlfaBeta
;;      Evalua los operadores que se pueden aplicar al estado recibido
;;      
;;      Retorna: 
;;              [ <***:movimiento> <ventaja:valor> ]    si llega  la maxima profundidad o estado final
;;
;;              [ <op:movimiento>  <mejorvalor:valor> ]     en caso contrario
;;      turno : 1 humano
;;              0 pc
;;
;;============================================
(defun negamax-alfabeta (estado profundidad *maxprof* alfa beta &optional (turno 0))  
    (if (or (fin-juego estado) (equal profundidad *maxprof*))
        (evaluacion estado)
        ;;else
        (let(
                (mejormov ())
                (mejorvalor -100000)
                (nuevoedo ())
                (valor 0)
            )
            (dolist  (op  *Ops* ) 
                (setq nuevoedo (aplica estado op))
                
                (if (equal turno 1 )
                    (setq valor (negamax-alfabeta nuevoedo (1+ profundidad) *maxprof* (max alfa mejorvalor) beta (third nuevoedo)))
                    ;;Else
                    (progn
                        (setq valor (negamax-alfabeta nuevoedo (1+ profundidad) *maxprof* (- beta) (- (max alfa mejorvalor)) (third nuevoedo)))
                        (setq valor (- valor))
                    )
                )
                                
                (if (> valor mejorvalor)
                    (setq mejorvalor valor)
                    (setq mejormov op)
                )
                (if (>= mejorvalor beta ) (return))
            )
            (list mejormov mejorvalor)
        )
    )
    ;;============ pseudocodigo ===========================
    ;;Si  (FinDeJuego()  o  profundidad = *maxprof*)           
    ;;    regresar  evaluacción(estado) 
    ;;de lo contrario 
    ;;    MejorMov = NIL           MejorValor =  -INFINITY             
    ;;    Para cada  operador                  
    ;;        nuevoEdo =  Aplica(estado, operador)                  
    ;;        valor = NegaMax - AlfaBeta (nuevoEdo, profundidad+1, *maxprof*-beta, -Max(alfa, MejorValor))                  
    ;;        valor =  -valor                  
    ;;        Cuando (valor > MejorValor)                           
    ;;            MejorValor ← valor                           
    ;;            MejorMov ← operador                            
    ;;        Cuando  MejorValor >=  beta
    ;;            terminar 
    ;;regresar   [MejorMov, MejorValor]     
    ;;================= pesudocodigo fin===========================    
)