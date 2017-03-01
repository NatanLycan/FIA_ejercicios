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
;;      A: humano
;;      B: PC
;;
;;      ---------------------------------------------------------------------------------------------
;;      |             |  <6a>13  |  <5a>12  |  <4a>11  |  <3a>10  |  <2a>9  |  <1a>8  |             |
;;      -  <scoreA>14  ----------------------------------------------------------------  <scoreB>7  |
;;      |             |  <1b>1   |  <2b>2   |  <3b>3   |  <4b>4   |  <5b>5  |  <6b>6  |             |
;;      ---------------------------------------------------------------------------------------------
;;
;;      cada casilla tiene la siguiente informacion
;;          
;;          [ <nombre> <NumeroAmarillas> <NumeroVerdes> <NumeroRojas> < NumeroTotalCanicas> <ValorTotal> ]
;;
;;      turno:  1 humano
;;              0 PC
;;
;;==========================================================

;;*/*/*/*/*/*/*/    Definicion de variables globales        /*/*/*/*/*/*/*/*/*
(defparameter  *score* '(0 0)) ;; [ <scoreHumano> <scorePC> ]
(defparameter  *turno* 1);; inicia con turno humano 
(defparameter  *estado* '((1 1 1 1 3 16) (2 1 1 1 3 16) (3 1 1 1 3 16) (4 1 1 1 3 16) (5 1 1 1 3 16) (6 1 1 1 3 16) (7 0 0 0 0 0) 
                          (8 1 1 1 3 16) (9 1 1 1 3 16) (10 1 1 1 3 16) (11 1 1 1 3 16) (12 1 1 1 3 16) (13 1 1 1 3 16) (14 0 0 0 0 0)) 
);; es el estado del tablero, iniciado
(defparameter *tableropc* '(1 2 3 4 5 6 7 8 9 10 11 12 13));; casillas donde puede circular pc
(defparameter *tablerouser* '(8 9 10 11 12 13 14 1 2 3 4 5 6));; ;; casillas donde puede circular pc
(defparameter *lastdeposito* 0);; es el lugar de la ultima pieza depositada
(defparameter *mano* '());; contiene las canicas cuando se vacia la casilla y se insertan en las demás
;;*/*/*/*/*/*/*/                                            /*/*/*/*/*/*/*/*/*

;;------ reset-all
;;      
;;      Reinicia las variables a su valor original
;;
;;----------------------------------------------

(defun reset-all ()
    (setq *score* '(0 0))
    (setq *lastdeposito* 0)
    (setq *turno* 1)
    (setq *mano* '())
    (setq *estado* '((1 1 1 1 3 16) (2 1 1 1 3 16) (3 1 1 1 3 16) (4 1 1 1 3 16) (5 1 1 1 3 16) (6 1 1 1 3 16) (7 0 0 0 0 0) 
                          (8 1 1 1 3 16) (9 1 1 1 3 16) (10 1 1 1 3 16) (11 1 1 1 3 16) (12 1 1 1 3 16) (13 1 1 1 3 16) (14 0 0 0 0 0))
    )
    
)

;;------ best-casilla
;;      
;;      Selecciona la mejor casilla para hacer el tiro de tres formas, una despues de la otra
;;      si no encuentra una buena
;;
;;      * mas-turnos:   casillas que generen mas tiros
;;      * bloquea:      bloquea tiros que den mas turnos al contrincante
;;      * no-ventaja:   tiro generado por no dar ventaja al oponente   
;;      * aleatorio:    si lo demas falla sera aleatorio
;;
;;----------------------------------------------

(defun mas-turnos () ;; ubica una casilla que tenga el numero de canicas exactas para que la ultima canica caiga en el dock mio
    (if (equal (fifth (nth 5 *estado*)) 1)
        6
        (if (equal (fifth (nth 4 *estado*)) 2)
            5
            (if (equal (fifth (nth 3 *estado*)) 3)
                4
                (if (equal (fifth (nth 2 *estado*)) 4)
                    3
                    (if (equal (fifth (nth 1 *estado*)) 5)
                        2
                        (if (equal (fifth (nth 0 *estado*)) 6)
                            1
                            0
                        )
                    )
                )
            )
        )
    )
)

(defun bloquea ()
    (let*   (
                (totales-pc (list (fifth (nth 0 *estado*)) (fifth (nth 1 *estado*)) (fifth (nth 2 *estado*)) (fifth (nth 3 *estado*)) (fifth (nth 4 *estado*)) (fifth (nth 5 *estado*))));;contiene el numero total de canicas de cada casilla pc
             
                (totales-h (list (fifth (nth 7 *estado*)) (fifth (nth 8 *estado*)) (fifth (nth 9 *estado*)) (fifth (nth 10 *estado*)) (fifth (nth 11 *estado*)) (fifth (nth 12 *estado*))));;contiene el numero total de canicas de cada casilla humano  
             
                (retorno 0)
            )
        (setq retorno 0)
        (setq totales-pc (reverse totales-pc))
        (cond 
            (;;primer posicion humano
                (equal  6  (first totales-h))  ;;puede repetir, usando esta posicion
                (let ((i 2)   )
                    (dolist  (aux  totales-pc)
                        (if (equal aux i)
                                (progn (setq retorno aux) (return))
                                (setq i (1+ i))
                        )               
                    )
                )
            )  
            (;;segunda posicion humano
                (and (equal  5  (second totales-h)) (or (< (first totales-h) 5) (> (first totales-h) 6)))  ;;puede repetir, usando esta posicion  y no lo beneficio en la posicion anterior
                (let ((i 3))   
                    (dolist  (aux  totales-pc)
                        (if (equal aux i)
                            (progn (setq retorno aux) (return))
                            (setq i (1+ i))
                        )               
                    )
                )
            ) 
            (;;tercer posicion humano
                (and (equal  4  (third totales-h)) (or (< (second totales-h) 4) (> (second totales-h) 5)) (or (< (first totales-h) 5) (> (first totales-h) 6)))  ;;puede repetir, usando esta posicion  y no lo beneficio en las posiciones anteriores
                (let ((i 4))   
                    (dolist  (aux  totales-pc)
                        (if (equal aux i)
                            (progn (setq retorno aux) (return))
                            (setq i (1+ i))
                        )               
                    )
                )
            )
            (;;cuarta posicion humano
                (and (equal  3  (fourth totales-h)) (or (< (third totales-h) 3) (> (third totales-h) 4)) (or (< (second totales-h) 4) (> (second totales-h) 5)) (or (< (first totales-h) 5) (> (first totales-h) 6)))  ;;puede repetir, usando esta posicion  y no lo beneficio en las posiciones anteriores
                (let ((i 5))   
                    (dolist  (aux  totales-pc)
                        (if (equal aux i)
                            (progn (setq retorno aux) (return))
                            (setq i (1+ i))
                        )               
                    )
                )
            )
            (;;quinta posicion humano
                (and (equal  2  (fifth totales-h)) (or (< (fourth totales-h) 2) (> (fourth totales-h) 3)) (or (< (third totales-h) 3) (> (third totales-h) 4)) (or (< (second totales-h) 4) (> (second totales-h) 5)) (or (< (first totales-h) 5) (> (first totales-h) 6)))  ;;puede repetir, usando esta posicion  y no lo beneficio en las posiciones anteriores
                (let ((i 6))   
                     (dolist  (aux  totales-pc)
                        (if (equal aux i)
                            (progn (setq retorno aux) (return))
                            (setq i (1+ i))
                        )               
                    )
                )
            )
            (;;sexta posicion humano
                (and (equal  1  (sixth totales-h)) (or (< (fourth totales-h) 1) (> (fourth totales-h) 2)) (or (< (fourth totales-h) 2) (> (fourth totales-h) 3)) (or (< (third totales-h) 3) (> (third totales-h) 4)) (or (< (second totales-h) 4) (> (second totales-h) 5)) (or (< (first totales-h) 5) (> (first totales-h) 6)))  ;;puede repetir, usando esta posicion  y no lo beneficio en las posiciones anteriores
                (let ((i 7))   
                    (dolist  (aux  totales-pc)
                        (if (equal aux i)
                            (progn (setq retorno aux) (return))
                            (setq i (1+ i))
                        )               
                    )
                )
            )
            (T  0)
        )      
    )
)

(defun no-ventaja ()

    (let* (
            (i 1);; contador
          
            (totales-pc (list (fifth (nth 0 *estado*)) (fifth (nth 1 *estado*)) (fifth (nth 2 *estado*)) (fifth (nth 3 *estado*)) (fifth (nth 4 *estado*)) (fifth (nth 5 *estado*))));;contiene el numero total de canicas de cada casilla pc
          
            (retorno 0)
         )
         (setq totales-pc (reverse totales-pc))
         (dotimes  (j (1- (length  totales-pc)))
              (if (equal i (nth i totales-pc))
                  (progn (setq retorno (- 6 i)) (return))
              )
         )         
         retorno
    )
)

(defun aleatorio ()
    (let* (
            (totales-pc (list (fifth (nth 0 *estado*)) (fifth (nth 1 *estado*)) (fifth (nth 2 *estado*)) (fifth (nth 3 *estado*)) (fifth (nth 4 *estado*)) (fifth (nth 5 *estado*))));;contiene el numero total de canicas de cada casilla pc
            (al 0);;almacenara variable aleatoria
            (exito 0);; verifica si la casilla es valida
         )
         
         (loop while (not (equal exito 1)) do
            (setq al (random 6))
            (setq al (1+ al))
            (if (equal 0 (nth al totales-pc))
                ()
                ;;else
                (setq exito 1)                    
            )
         )
         al
    )    
)


(defun best-casilla ();; retorna el id de la casilla
    (let (
            (casilla 0);; contendra la mejor casilla
         )
         (setq casilla (mas-turnos))
        (if (equal casilla 0)
            (progn
                (setq casilla (bloquea))
                (if (equal casilla 0)
                    (progn
                        (setq casilla (no-ventaja))
                        (if (equal casilla 0)
                            (progn
                                (setq casilla (aleatorio))
                                (format t "~%Casilla elegida ~A~%" casilla) 
                                casilla
                            )
                            (progn (format t "~%Casilla elegida ~A~%" casilla) casilla)                            
                        )
                    )
                    (progn (format t "~%Casilla elegida ~A~%" casilla) casilla)
                )                
            )
            (progn (format t "~%Casilla elegida ~A~%" casilla) casilla)
        )
    )
)


;;------best-orden
;;      
;;      Ordenara las piezas obtenidas de la casilla de la mejor forma posible
;;
;;      bajo valor en aquellas casillas con piezas de igual o mejor valor
;;      y piezas de alto valor en casillas sin piezas de alto valor
;;      
;;      si alcanza casilla de score otorgara la de mejor valor
;;
;;      cada casilla tiene la siguiente informacion
;;          
;;          [ <nombre> <NumeroAmarillas> <NumeroVerdes> <NumeroRojas> < NumeroTotalCanicas> <ValorTotal> ]
;;
;;  
;;----------------------------------------------

(defun best-orden (casilla)
    (let* (
            (c (nth (1- casilla) *estado*));;obtengo la casilla
            (ac (second c));;obtengo el numero de piezas amarillas
            (vc (third c));;obtengo el numero de piezas rojas
            (rc (fourth c));;obtengo en numero total de piezas rojas
            (tc (fifth c));;obtengo el numero total de piezas 
            (nsco (- 7 casilla));; casillas para mi llegar a mi score
            ;;(needed (- nsco tc));;define los casos
            (answ '())
            (i 0)
        )
        (cond
            ;;caso1 tengo piezas para llegara mi casilla de score
            ((equal nsco tc)  
                (loop while (not (equal tc 0)) do
                    (if (equal 0 i)
                        (if (not (equal 0 rc));;enviara la de mayor valor existente a la casilla score
                            (progn (push :R answ) (setq rc (1- rc)) (setq tc (1- tc)))
                            ;;else
                            (if (not (equal 0 vc))
                                (progn (push :V answ) (setq vc (1- vc)) (setq tc (1- tc)))
                                ;;si no, solo han de quedar amarillas XD
                                (progn (push :A answ) (setq ac (1- ac)) (setq tc (1- tc)))
                            )
                        )
                        ;;despues de score
                        (if (> (fourth (nth (- 6 i) *estado*)) 0);;verifica si tiene rojas esa casilla---id-1:posicion de casilla a examinar
                            (if (> vc 0);;le intentare dar de todas menos rojas
                                (progn (push :V answ) (setq vc (1- vc)) (setq tc (1- tc)));;le doy una roja
                                (if (> ac 0);;si no, le intento poner amarillas
                                    (progn (push :A answ) (setq ac (1- ac)) (setq tc (1- tc)))
                                    ;;si no, solo han de quedar roja XD
                                    (progn (push :R answ) (setq rc (1- rc)) (setq tc (1- tc)))
                                )
                            )
                            ;;si no hay rojas en esa casilla
                            (if (> rc 0);;si tengo rojas para dar
                                (progn (push :R answ) (setq rc (1- rc)) (setq tc (1- tc)));;le doy una roja
                                (if (> vc 0);;si no, le intento poner verdes
                                    (progn (push :V answ) (setq vc (1- vc)) (setq tc (1- tc)))
                                    ;;si no,solo han de quedar amarillas XD
                                    (progn (push :A answ) (setq ac (1- ac)) (setq tc (1- tc)))
                                )
                            )
                        )
                    )
                      (setq i (1+ i))
                 )   
            )
            ;;caso2 tengo mas piezas que las que necesito para llegar a la casilla score
		    ((< nsco tc ) 
                (let ((posact 0))
                    (setq posact (+ casilla tc))
                    (loop while (not (equal posact casilla)) do
                        (if (= posact 7);;estoy en mi casilla de score, le doy la mas alta que tenga
                            (if (> rc 0);;si tengo rojas para dar
                                (progn (push :R answ) (setq rc (1- rc)) (setq tc (1- tc)));;le doy una roja
                                (if (> vc 0);;si no, le intento poner verdes
                                    (progn (push :V answ) (setq vc (1- vc)) (setq tc (1- tc)))
                                    ;;si no, solo han de quedar amarillas XD
                                    (progn (push :A answ) (setq ac (1- ac)) (setq tc (1- tc)))
                                )
                            )
                            ;;else
                            (if (and (> posact 7) (< posact 14));;cuando estoy en las casillas del enemigo
                                (if (> vc 0);;le intentare dar de todas menos rojas
                                    (progn (push :V answ) (setq vc (1- vc)) (setq tc (1- tc)));;le doy una roja
                                    (if (> ac 0);;si no, le intento poner amarillas
                                        (progn (push :A answ) (setq ac (1- ac)) (setq tc (1- tc)))
                                        ;;si no, solo han de quedar roja XD
                                        (progn (push :R answ) (setq rc (1- rc)) (setq tc (1- tc)))
                                    )
                                )
                                ;;else en mis casillas
                                (if (> (fourth (nth (1- posact) *estado*)) 0);;verifica si tiene rojas esa casilla---id-1:posicion de casilla a examinar
                                    (if (> vc 0);;le intentare dar de todas menos rojas
                                        (progn (push :V answ) (setq vc (1- vc)) (setq tc (1- tc)));;le doy una roja
                                        (if (> ac 0);;si no, le intento poner amarillas
                                            (progn (push :A answ) (setq ac (1- ac)) (setq tc (1- tc)))
                                            ;;si no, solo han de quedar roja XD
                                            (progn (push :R answ) (setq rc (1- rc)) (setq tc (1- tc)))
                                        )
                                    )
                                    ;;si no hay rojas en esa casilla
                                    (if (> rc 0);;si tengo rojas para dar
                                        (progn (push :R answ) (setq rc (1- rc)) (setq tc (1- tc)));;le doy una roja
                                        (if (> vc 0);;si no, le intento poner verdes
                                            (progn (push :V answ) (setq vc (1- vc)) (setq tc (1- tc)))
                                            ;;si no, solo han de quedar amarillas XD
                                            (progn (push :A answ) (setq ac (1- ac)) (setq tc (1- tc)))
                                        )
                                    )
                                )
                            )
                        )
                        (setq posact (1- posact))
                    )                     
                )
            )
            ;;caso3 tengo menos piezas que las que necesito para llegar a la casilla score 
			(T  
                (let ((posact 0))
                    (setq posact (+ casilla tc))
                    (loop while (not (equal posact casilla)) do 
                        (if (> (fourth (nth (1- posact) *estado*)) 0);;verifica si tiene rojas esa casilla---id-1:posicion de casilla a examinar
                            (if (> vc 0);;le intentare dar de todas menos rojas
                                (progn (push :V answ) (setq vc (1- vc)) (setq tc (1- tc)));;le doy una roja
                                (if (> ac 0);;si no, le intento poner amarillas
                                    (progn (push :A answ) (setq ac (1- ac)) (setq tc (1- tc)))
                                    ;;si no, solo han de quedar roja XD
                                    (progn (push :R answ) (setq rc (1- rc)) (setq tc (1- tc)))
                                )
                            )
                            ;;si no hay rojas en esa casilla
                            (if (> rc 0);;si tengo rojas para dar
                                (progn (push :R answ) (setq rc (1- rc)) (setq tc (1- tc)));;le doy una roja
                                (if (> vc 0);;si no, le intento poner verdes
                                    (progn (push :V answ) (setq vc (1- vc)) (setq tc (1- tc)))
                                    ;;si no, solo han de quedar amarillas XD
                                    (progn (push :A answ) (setq ac (1- ac)) (setq tc (1- tc)))
                                )
                            )
                        )
                        (setq posact (1- posact))
                    ) 
                )
            )
        )
          (format t "~%Orden elegido elegida ~A~%" answ) 
        answ
    )
)


;;------inserta-canicas
;;      
;;      Inserta las canicas de la mano en las casillas consecuentes
;;      
;;      tambien guarda la posicion donde se hizo el ultimo deposito
;;
;;      cada casilla tiene la siguiente informacion
;;          
;;          [ <nombre> <NumeroAmarillas> <NumeroVerdes> <NumeroRojas> < NumeroTotalCanicas> <ValorTotal> ]
;;
;;----------------------------------------------

(defun inserta-canicas (bc) ;;[ <id:casilla> <lista:orden-canicas> ]
    (let* (
            (casilla (first bc))
            (posact (1+ casilla))
            (cd (nth (1- casilla) *estado*))
            (orden (second bc))
            (neoestado *estado*)
            (aux '())
            
         )
         
        ;;elimino la casilla--por que se que al final va a estar vacia
        (setq neoestado (delete cd neoestado))
        ;;agrego la casilla pero vacia
        (setq aux (list casilla 0 0 0 0 0))
        (push aux neoestado)
        (setq neoestado (sort neoestado '< :key 'first))
        (setq *lastdeposito* (+ casilla (length orden)));;indica la ultima casilla depositada  
          ;;(print "neoestadp")
          ;;(print neoestado)
        ;;ahora modifico las siguientes
        (dolist  (h  orden) 
            (if (and (equal *turno* 1) (equal posact 7));; permito que se salten las casillas que no le corresponde a cada turno
                (setq posact 8)
                ;;else
                (if (and (equal *turno* 0) (equal posact 14))
                    (setq posact 1)
                )
            )
            (print posact)
            (print h)
            ;;cambio los valores con lo nuevo que se le agrege
            ;;(print (nth (1- posact) *estado*))
	        (setq  cd (list (nth (1- posact) neoestado)));;obtengo la casilla a modificar
            (setq cd (first cd))
            (print "cd")
            (print cd)
            (case h
                ( :A
                    (let* (
                           (cdc (list (first cd) (second cd) (third cd) (fourth cd) (fifth cd) (sixth cd) ))
                           )
                        (setq aux (list (first cdc) (1+ (second cdc)) (third cdc) (fourth cdc) (1+ (fifth cdc)) (1+ (sixth cdc))))
                        (setq neoestado (delete cd neoestado))
                        (push aux neoestado)
                        ;;(print aux)
                    )
                )
                
                (:V
                    (let* (
                           (cdc (list (first cd) (second cd) (third cd) (fourth cd) (fifth cd) (sixth cd) ))
                           )
                        (setq aux (list (first cdc) (second cdc) (1+ (third cdc)) (fourth cdc) (1+ (fifth cdc)) (+ 5 (sixth cdc))))
                        (setq neoestado (delete cd neoestado))
                        (push aux neoestado)
                          ;;(print aux)
                    )
                )
                
                (:R
                    (let* (
                           (cdc (list (first cd) (second cd) (third cd) (fourth cd) (fifth cd) (sixth cd) ))
                            )
                        (setq aux (list (first cdc) (second cdc) (third cdc) (1+ (fourth cdc)) (1+ (fifth cdc)) (+ 10 (sixth cdc))))
                        (setq neoestado (delete cd neoestado))
                        (push aux neoestado)
                          ;;(print aux)
                    )
                )
                (T (print "error canica no valaida [INSERTA-CANICAS]"))
            )
                    
            
            (setq neoestado (sort neoestado '< :key 'first));;acomo las casilla a su orden correcto
            (setq posact (1+ posact));;incremento posact
            (if (> posact 14);; si la posicion se sale del tablero lo reinicio
                (setq posact 1)
            )
        )      
        
        (setq *estado* neoestado)
    )
)


;;------fin-juego
;;      
;;      Verifica si el estado es un estado final
;;
;;      cada casilla tiene la siguiente informacion
;;          
;;          [ <nombre> <NumeroAmarillas> <NumeroVerdes> <NumeroRojas> < NumeroTotalCanicas> <ValorTotal> ]
;;
;;
;;----------------------------------------------

(defun fin-juego ()
    (let* (
          (user (list (nth 7 *estado*) (nth 8 *estado*) (nth 9 *estado*) (nth 10 *estado*) (nth 11 *estado*) (nth 12 *estado*)))
          (pc (list (nth 0 *estado*) (nth 1 *estado*) (nth 2 *estado*) (nth 3 *estado*) (nth 4 *estado*) (nth 5 *estado*)))
           (robo 0);;almacena el valor de las canicas robadas
         )
        (if (and (equal (fifth (first user)) 0) (equal (fifth (second user)) 0) (equal (fifth (third user)) 0) (equal (fifth (fourth user)) 0) (equal (fifth (fifth user)) 0) (equal (fifth (sixth user)) 0));;si todas las casillas del usurio estan vacias, entonces roba
            (progn 
                (setq robo (+ (sixth (nth 7 *estado*)) (sixth (nth 8 *estado*)) (sixth (nth 9 *estado*)) (sixth (nth 10 *estado*))(sixth (nth 11 *estado*)) (sixth (nth 12 *estado*))))
                (setq *score* (list (sixth (nth 13 *estado*))  (+ robo (sixth (nth 6 *estado*)))))
                T
            )
            ;;Else
            (if (and (equal (fifth (first pc)) 0) (equal (fifth (second pc)) 0) (equal (fifth (third pc)) 0) (equal (fifth (fourth pc)) 0) (equal (fifth (fifth pc)) 0) (equal (fifth (sixth pc)) 0))
                (progn 
                    (setq robo (+ (sixth (nth 0 *estado*)) (sixth (nth 1 *estado*)) (sixth (nth 2 *estado*)) (sixth (nth 3 *estado*))(sixth (nth 4 *estado*)) (sixth (nth 5 *estado*))))
                    (setq *score* (list   (+ robo (sixth (nth 13 *estado*)) (sixth (nth 6 *estado*)) )))
                    T
                )
                NIL
            )            
        )
             
    )
)


;;------turno-user
;;      
;;      Se encarga de la interfaz del usuario y su juego
;;      cada casilla tiene la siguiente informacion
;;          
;;          [ <nombre> <NumeroAmarillas> <NumeroVerdes> <NumeroRojas> < NumeroTotalCanicas> <ValorTotal> ]
;;
;;----------------------------------------------

(defun turno-user ()
    (let(
            (casilla 0)
            (orden '())
            (c '())
            (exito1 0)
            (exito2 0)
        )
        (format t "Al ingresar casilla recuerda que solo puedes seleccionar entre la 8 y la 13~%" )
        (loop until  (equal 1 exito1)  do    
            (princ "Igresa la casilla:")
            (setq casilla (read))
            (if (and (> casilla 7) (< casilla 14))
                (setq exito1 1)
            )  
        )
        (setq c (nth (1- casilla) *estado*))
        (format t "La casilla contiene lo siguiente:~% ~A canicas rojas ~% ~A canicas verdes ~% ~A canicas amarillas~%" (fourth  c) (third c) (second c))
        (loop until  (equal 1 exito2)  do
            (format t "El orden para acomodar las canicas se ingresa de la siguiente manera: (:R :V :A)~%En una lista ordenada, donde el primer elemento se acomodara en la casilla siguiente a la elegida y asi hasta que se terminen~%~%")
            (princ "Igrese el orden:")
            (setq orden (read))
            (if (equal (length orden) (fifth c))
                (setq exito2 1)
            )
        )
        (inserta-canicas (list casilla orden))
        (turno?)
    )
)

;;------turno-pc
;;      
;;      Se encarga del juego en el turno del pc
;;
;;----------------------------------------------

(defun turno-pc ()
    (let (
            (best-choice '());; quedara de la sig. forma [ <id:mejor-casilla> <orden-canicas> ]
            (mcasilla 0)
            (morden '())
        ) 
        (setq mcasilla (best-casilla));;id de la casilla
        (setq morden (best-orden mcasilla));; lista con las letras ordenadas
        (setq best-choice (list  mcasilla morden));; solo obtiene la mejor opcion de casilla y orden
        (inserta-canicas best-choice)
        (turno?)         
   ) 
)



;;------turno?
;;      
;;      Se encarga de verificar de quien es el turno siguiente
;;
;;      Si turno= 1 &  lastdeposito=14   --> repite
;;      Si turno= 0 &  lastdeposito=7    --> repite
;;      en otro caso el turno cambia
;;
;;----------------------------------------------

(defun turno? ()
    (if (and (equal *turno* 1) (equal *lastdeposito* 14))
        ();;si la ultima canica se coloco en el dock del humano, no cambia el turno
        (if (and (equal *turno* 0) (equal *lastdeposito* 7))
            ();;si la ultima canica se coloco en el dock de la pc, no cambia el turno
            (setq *turno* (- 1 *turno*));; si no, entonces cambia el turno
        )
    )
)

;;------printn
;;      
;;      Imprime el tablero con su info
;;          
;;          cada casilla tiene la siguiente informacion
;;
;;          [ <nombre> <NumeroAmarillas> <NumeroVerdes> <NumeroRojas> < NumeroTotalCanicas> <ValorTotal> ]
;;
;;
;;----------------------------------------------

(defun printn ()
    (format t "~%~%~%~%--------------------------------------------------------------------------------------------- ~%")
    (format t "|               |  id:~A  |  id:~A  |  id:~A  |  id:~A  |  id:~A   |  id:~A   |               |~%" (first (nth 12 *estado*)) (first (nth 11 *estado*)) (first (nth 10 *estado*)) (first (nth 9 *estado*)) (first (nth 8 *estado*)) (first (nth 7 *estado*)))
    (format t "|     id:~A     |  A:~A    |  A:~A    |  A:~A    |  A:~A    |  A:~A    |   A:~A   |     id:~A      |~%" 
        (first (nth 13 *estado*)) (second (nth 12 *estado*)) (second (nth 11 *estado*)) (second (nth 10 *estado*)) (second (nth 9 *estado*))
        (second (nth 8 *estado*)) (second (nth 7 *estado*))(first (nth 6 *estado*))
    )
    (format t "|     A:~A       |  V:~A    |  V:~A    |  V:~A    |  V:~A    |  V:~A    |  V:~A    |     A:~A       |~%" 
        (second (nth 13 *estado*)) (third (nth 12 *estado*)) (third (nth 11 *estado*)) (third (nth 10 *estado*)) (third (nth 9 *estado*)) (third (nth 8 *estado*)) (third (nth 7 *estado*)) (second (nth 6 *estado*))
    )
    (format t "|     V:~A       |  R:~A    |  R:~A    |  R:~A    |  R:~A    |  R:~A    |  R:~A    |     V:~A       |~%" 
        (third (nth 13 *estado*)) (fourth (nth 12 *estado*)) (fourth (nth 11 *estado*)) (fourth (nth 10 *estado*)) (fourth (nth 9 *estado*)) (fourth (nth 8 *estado*)) (fourth (nth 7 *estado*)) (third (nth 6 *estado*))
    )
    (format t "|     R:~A       |  T:~A   |  T:~A   |  T:~A   |  T:~A   |  T:~A   |  T:~A   |     R:~A       |~%" 
        (fourth (nth 13 *estado*)) (sixth (nth 12 *estado*)) (sixth (nth 11 *estado*)) (sixth (nth 10 *estado*)) (sixth (nth 9 *estado*)) (sixth (nth 8 *estado*)) (sixth (nth 7 *estado*)) (fourth (nth 6 *estado*))
    )
    (format t "|     T:~A       |---------|---------|---------|---------|---------|---------|     T:~A       |~%" 
        (sixth (nth 13 *estado*)) (sixth (nth 6 *estado*))
    )
    (format t "|               |  id:~A   |  id:~A   |  id:~A   |  id:~A   |  id:~A    |  id:~A    |               |~%" (first (nth 0 *estado*)) (first (nth 1 *estado*)) (first (nth 2 *estado*)) (first (nth 3 *estado*)) (first (nth 4 *estado*)) (first (nth 5 *estado*)))
    (format t "|               |  A:~A    |  A:~A    |  A:~A    |  A:~A    |  A:~A    |  A:~A    |               |~%" 
        (second (nth 0 *estado*)) (second (nth 1 *estado*)) (second (nth 2 *estado*)) (second (nth 3 *estado*))
        (second (nth 4 *estado*)) (second (nth 5 *estado*))
    )
    (format t "|               |  V:~A    |  V:~A    |  V:~A    |  V:~A    |  V:~A    |  V:~A    |               |~%" 
        (third (nth 0 *estado*)) (third (nth 1 *estado*)) (third (nth 2 *estado*)) (third (nth 3 *estado*)) (third (nth 4 *estado*)) (third (nth 5 *estado*)) 
    )
    (format t "|               |  R:~A    |  R:~A    |  R:~A    |  R:~A    |  R:~A    |  R:~A    |               |~%" 
        (fourth (nth 0 *estado*)) (fourth (nth 1 *estado*)) (fourth (nth 2 *estado*)) (fourth (nth 3 *estado*)) (fourth (nth 4 *estado*)) (fourth (nth 5 *estado*)) 
    )
    (format t "|               |  T:~A   |  T:~A   |  T:~A   |  T:~A   |  T:~A   |  T:~A   |               |~%" 
        (sixth (nth 0 *estado*)) (sixth (nth 1 *estado*)) (sixth (nth 2 *estado*)) (sixth (nth 3 *estado*)) (sixth (nth 4 *estado*)) (sixth (nth 5 *estado*))
    )
    (format t "--------------------------------------------------------------------------------------------- ~%")
    (format t "~%Donde: ~% A: Número de Azules (1 pto)  V: Número de verdes (5 ptos)   R: Número de rojas (10 ptos)   T: Valor total de la casilla  ~%" ) 
)

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
    (reset-all)
    (loop until  (fin-juego)  do ;; empieza el juego
        (if (equal *turno* 1)
        ;;turno humano
            (progn
                (printn)
                (turno-user)
            )
        ;;else ... turno pc
            (progn
                (printn)  
                (turno-pc)
            )
        )
    )
    ;;aqui ya termino el juego
    (if (> (first *score*) (second *score*))
    ;;si gana humano
        (format t "Felicidades, has ganado ~% TU PUNTAJE: ~A ~%MI PUNTAJE ~A ~% " (first *score*) (second *score*)) 
    ;;else
        (if (> (second *score*) (first *score*))
            ;;si gana pc
            (format t "Mala suerte, te he ganado ~% TU PUNTAJE: ~A ~%MI PUNTAJE ~A ~% " (first *score*) (second *score*))
            ;;si empatan
            (format t "Wow, somos muy pros, empatamos!. ~% NUESTRO PUNTAJE FUE ~A ~%" (first *score*))
        )
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
;;(defun negamax-alfabeta (estado profundidad *maxprof* alfa beta &optional (turno 0))  
  ;;  (if (or (fin-juego estado) (equal profundidad *maxprof*))
    ;;    (evaluacion estado)
        ;;else
      ;;  (let(
        ;;        (mejormov ())
          ;;      (mejorvalor -100000)
            ;;    (nuevoedo ())
              ;;  (valor 0)
            ;;)
            ;;(dolist  (op  *Ops* ) 
              ;;  (setq nuevoedo (aplica estado op))
                
            ;;    (if (equal turno 1 )
              ;;      (setq valor (negamax-alfabeta nuevoedo (1+ profundidad) *maxprof* (max alfa mejorvalor) beta (third nuevoedo)))
                    ;;Else
                ;;    (progn
                  ;;      (setq valor (negamax-alfabeta nuevoedo (1+ profundidad) *maxprof* (- beta) (- (max alfa mejorvalor)) (third nuevoedo)))
                    ;;    (setq valor (- valor))
;;                    )
;;                )
                                
;;                (if (> valor mejorvalor)
;;                    (setq mejorvalor valor)
;;                    (setq mejormov op)
  ;;              )
           ;;     (if (>= mejorvalor beta ) (return))
    ;;        )
      ;;      (list mejormov mejorvalor)
        ;;)
    ;;)
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
;;)