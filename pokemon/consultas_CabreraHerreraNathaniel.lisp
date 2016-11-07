;;==========================================================
;;
;;      Consultas a base de conocimieento
;;
;;      POKEMON
;;
;;      Funciones de consulta valida:
;;              + exist
;;              - d-exist
;;              * 4all
;;              / not-4all
;;
;;       Tipos de Consulta:
;;              1. 
;;
;;==========================================================
(defparameter *k* ()) ;;contendra el conocimiento
(defparameter *numrows* ());;variable que almacena el numero de tuplas en el archivo de texto de la base de conocimiento
(defparameter *answ* ());;variable que contiene las tuplas de respuesta
(defvar fa());;variable auxiliar
(defvar ra());;variable auxiliar
(defvar list-actual());;variable auxiliar para la busqueda en la base de conocimiento
(defvar tuplas ());;almacena las tuplas encontradas
(defvar ntuplas 0);;almacena el numero de tuplas encontradas
(defvar c ());;Contiene la consulta de entrada

(defun read-k (filename)
    (with-open-file (stream filename)
        (progn
            (setq *numrows* (read stream))
            
            (read-line stream nil nil)
            (dotimes (row *numrows*)
                (setq *k* (push (read stream nil nil) *k*))    
            )
            (setq *k* (reverse *k*))
        )
    )
)
;;(read-k "C:/Users/Nathaniel/Documents/GitHub/FIA_ejercicios/pokemon/knowledge.txt")

;;============================================================
;;
;;  ShowRes    
;;
;;  Esta funcion muestra el resultado de acuerdo a la funcion de consulta usada
;;
;;============================================================


(defun showres (funcion)
    (case funcion
        (exist
            (if (equal tuplas ())
                (progn
                    (print "FALSE")
                    (format t "~%~%"))
                (progn
                    (print "TRUE")
                    tuplas
                )
            )
        )
        (d-exist
            (if (equal tuplas ())
                (progn 
                    (print "TRUE")
                    (format t "~%~%"))
                (progn
                    (print "FALSE")
                    ;;*k*-tuplas
                    (dolist (i tuplas)
                        (setq *k* (delete i *k*))
                    )
                    (setq tuplas *k*)
                    tuplas
                )
            )
        )
        (4all
            (if (equal (length tuplas) (length *k*));;tuplas y base de conocimiento igual
                ;;verdadero
                (progn 
                    (print "TRUE")
                    (format t "~%~%"))
                ;;falso
                (progn
                    (print "FALSE")
                    (dolist (i tuplas)
                        (setq *k* (delete i *k*))
                    )
                    (setq tuplas *k*)
                    tuplas
                )
            )
        )
        (not-4all
            (if (equal tuplas ())
            ;;falso
                (progn 
                    (print "FALSE")
                    tuplas    
                )
            ;,verdadero
                (progn
                    (print "TRUE")
                    (format t "~%~%"))
            )
         
        )
    )    
)


;;============================================================
;;
;;  Finder 
;;
;;  Encuentra todas las tuplas que concuerden con las condiciones
;;
;;============================================================

(defun finder (c)
    ;;(format t "~%finder ~%")
    ;;(print c)
    (dolist (i *k*)
        (setq list-actual i)
        (if (eval c)
            (progn (push list-actual *answ*) (setq ntuplas (1+ ntuplas)))
        )
    )
    (setq tuplas *answ*)
    (print *answ*)
)

;;============================================================
;;
;;  Parser - Recursiva
;;
;;  Cambia la estructura de las condiciones de humano a como se van a procesar
;;
;;============================================================
(defun parser (c &optional (n 0) (w t))
    (let* ((caux ())(andy (list 'and ))(ordy (list 'or )) (aux ())(auxstr ()) (answ ()));;depende para cada caso
        (if (not (equal c NIL))
            (case (first c)
                ('and ;;consulta compuesta
                    (format t "Entro al parser opcion AND~%")
                    (setq caux (rest c))
                    (setq answ andy)
                    (if (listp (first caux))
                        (setq answ  (append answ  (parser caux (1+ n) :and)))
                    )
                    
                )
                ('or ;;consulta compuesta
                    (format t "Entro al parser opcion OR~%")
                    (setq caux (rest c))
                    (setq answ ordy)
                    (if (listp (first caux))
                        (setq answ  (append answ  (parser caux (1+ n) :or)))
                    )
                    
                )
                (t ;;consulta simple
                 
                    (if (equal (first (first c)) 'or) 
                        (progn
                            (setq answ  (append answ  (list (parser (first c) (1+ n))) (parser (rest c) (1+ n) w)))
                        )
                        ;;else
                        (if (equal (first (first c)) 'and)
                            (progn
                                (setq answ  (append answ  (list (parser (first c) (1+ n))) (parser (rest c) (1+ n) w)))
                            )
                            ;;else 
                            (progn
                                (format t "Entro al parser opcion T~%")
                                (if (equal n 0)
                                    (setq answ andy)
                                )
                                (setq aux (first c));;obtengo la primer consulta
                                (setq caux (rest c));;modifico la variable que contiene las consultas
                                (setq fa (first aux));;identificador de la consulta
                                (setq ra (rest aux)) ;; valor de consulta
                                (if (numberp ra);;verifico si condicion es numerico
                                    (setq answ (append answ (list(list 'equal (list 'rest (list 'assoc (list 'quote fa) 'list-actual)) ra )) ))
                                    (progn 
                                        (setq auxstr (string (rest  aux)));;guardo en cadena la condicion                        
                                        (if (equal #\[ (char auxstr 0));;verifico si condicion tiene mas condiciones
                                            ();;algo
                                            ;;else
                                            (progn    
                                            (setq answ (append answ (list (list 'equal (list 'rest (list 'assoc (list 'quote fa) 'list-actual)) (list 'quote ra) )) ))    

                                            )
                                        )
                                    )
                               )
                                (if (equal n 0)
                                    (setq answ  (append answ  (parser caux (1+ n) :and)))
                                    (setq answ  (append answ  (parser caux (1+ n) w)))
                                )

                                ;;(print answ)    
                            )
                        )                           
                    )
                 
                 
                    
                )
            )
            (if (equal w :and)
                (list T)
                (list NIL)
            )
            
        )
        ;;(print answ)
        ;;(format t " ~%antes de eval~%")
        ;;(eval answ)
        ;;(format t " despues de eval")
    )
)


;;============================================================
;;
;;  Fase1
;;
;;  Separa la consulta de entrada para su posterior analisis 
;;
;;============================================================

(defun fase1 (c)
    (let ((funcion (first c)) (clase (second c)) (condiciones (third c)) (condi ()))
        (format t "~% Funcion usada ~A ~%" funcion)
        (format t "Clase solicitada ~A ~%" (rest clase));;DONE
        (format t "Condiciones ~A ~% ~%" condiciones);;DONE
        (setq condi (parser condiciones))
        (format t "~%Condicion ~A ~%" condi)
        ;;(setq list-actual (first *k*))
        ;;(print list-actual)
        ;;(eval condi)
        (setq tuplas (finder (list 'and  (list 'equal (list 'rest (list 'assoc (list 'quote 'clase) 'list-actual)) (list 'quote (rest clase))) condi) ))
        ;;(showres funcion);; le paso la funcion de entrada
         ;;(print "fase1 done")
         ;;tuplas
    )   
)

;;============================================================
;;
;;  reset-all
;;
;;  Reincia las variables
;;
;;============================================================

(defun reset-all ()
    (setq list-actual ())
    (setq tuplas ())
    (setq *answ* ())
    (setq fa())
    (setq ra())
    (setq c ())
    (setq ntuplas 0)
    (setq *k* ())
    )


;;============================================================
;;
;;  Consult
;;
;;  Esta funcion hace consultas a la base de conocimiento
;;
;;============================================================

(defun consult ()
    (let ((fin 0))
        (loop while (eq fin 0) do
            (reset-all)
            (read-k "C:/Users/Nathaniel/Documents/GitHub/FIA_ejercicios/pokemon/knowledge.txt")
            (princ "Igrese consulta:")
            (setq c (read))
            (if (equal c nil)
                (format t "No ha ingresado nada, por favor ingrese una consulta")
                ;;Else
                (progn
                    ;;(print c)
                    (fase1 c)
                    (format t "~% Numero de resultados ~A ~%" ntuplas)
                    
                )
            ) 
            (format t "~% Consulta ~A ~% ~%" c)
        )
    )
)

;Ejemplo de consulta simple
;(exist (clase . pokemon) ((tipo1 . hierva)))
;(d-exist (clase . pokemon) ((num . [>700])))
;(4all (clase . pokemon) ((num . [>700])))
;(not-4all (clase . pokemon) ((num . [>700])))
;Ejemplo de consulta compleja
;(existe (clase . pokemon) (AND (TIPO1 . ACERO) (OR (TIPO2 . HADA) (NUM . [<600]))))
;(paratodo (clase . pokemon) (AND (TIPO1 . ACERO) (OR (TIPO2 . HADA) (NUM . [<600]))))
;(no-existe (clase . pokemon) (AND (TIPO1 . ACERO) (OR (TIPO2 . HADA) (NUM . [<600]))))
;(no-paratodo (clase . pokemon) (AND (TIPO1 . ACERO) (OR (TIPO2 . HADA) (NUM . [<600]))))