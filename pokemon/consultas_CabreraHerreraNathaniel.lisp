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
(defparameter *numrows* ())
(defparameter *answ* ())
(defvar fa())
(defvar ra())
(defvar list-actual())
(defvar tuplas ())

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
            (push list-actual *answ*)
        )
    )
    *answ*
    ;;(print *answ*)
)

;;============================================================
;;
;;  Parser - Recursiva
;;
;;  Cambia la estructura de las condiciones de humano a como se van a procesar
;;
;;============================================================
(defun parser (c &optional (n 0))
    (let* ((caux ())(andy (list 'and ))(ordy (list 'or )) (aux ())(auxstr ()) (answ ())(a 'and)(o 'or));;depende para cada caso
        (if (not (equal c NIL))
            (case (first c)
                (a ;;consulta compuesta
                    (setq caux (rest c))
                    (if (listp (first caux))
                        (setq andy (append andy (list (parser caux (1+ n)))))
                    )
                    (setq a ())
                )
                (o ;;consulta compuesta
                    (setq caux (rest c))
                    (if (listp (first caux))
                        (setq ordy (append ordy (list (parser caux (1+ n)))))
                    )
                    (setq o ())
                )
                (t ;;consulta simple
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
                    (print answ)
                   (setq answ  (append answ  (parser caux (1+ n))))
                  
                )


            )
            (list T)
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
        ;;(setq list-actual (first *k*))
        ;;(print list-actual)
        ;;(eval condi)
        (setq tuplas (finder (list 'and  (list 'equal (list 'rest (list 'assoc (list 'quote 'clase) 'list-actual)) (list 'quote (rest clase))) condi) ))
         
    )   
)


;;============================================================
;;
;;  Consult
;;
;;  Esta funcion hace consultas a la base de conocimiento
;;
;;============================================================

(defun consult ()
    (let ((fin 0) (c ()))
         (read-k "C:/Users/Nathaniel/Documents/GitHub/FIA_ejercicios/pokemon/knowledge.txt")
        (loop while (eq fin 0) do
            (setq list-actual ())
            (setq tuplas ())
            (setq *answ* ())
            (setq fa())
            (setq ra())
            (princ "Igrese el orden:")
            (setq c (read))
            (if (equal c nil)
                ()
                ;;Else
                (progn
                    ;;(print c)
                    (fase1 c)
                    ;;(format t "~%")
                )
            )  
        )
    )
)

;Ejemplo de consulta simple
;(existe (clase . pokemon) ((num . [>700])))
;(no-existe (clase . pokemon) ((num . [>700])))
;(paratodo (clase . pokemon) ((num . [>700])))
;(no-paratodo (clase . pokemon) ((num . [>700])))
;Ejemplo de consulta compleja
;(existe (clase . pokemon) (AND (TIPO1 . ACERO) (OR (TIPO2 . HADA) (NUM . [<600]))))
;(paratodo (clase . pokemon) (AND (TIPO1 . ACERO) (OR (TIPO2 . HADA) (NUM . [<600]))))
;(no-existe (clase . pokemon) (AND (TIPO1 . ACERO) (OR (TIPO2 . HADA) (NUM . [<600]))))
;(no-paratodo (clase . pokemon) (AND (TIPO1 . ACERO) (OR (TIPO2 . HADA) (NUM . [<600]))))