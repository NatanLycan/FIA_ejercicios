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

(defun read-k (filename)
    (with-open-file (stream filename)
        (let (
              (numrows (read stream))
              )
            (read-line stream nil nil)
            (dotimes (row numrows)
                (setq *k* (push (read stream nil nil) *k*))    
            )
            (setq *k* (reverse *k*))
        )
    )
)
;;(read-k "C:/Users/Nathaniel/Documents/GitHub/FIA_ejercicios/pokemon/knowledge.txt")

;;============================================================
;;
;;  Parser - Recursiva
;;
;;  Cambia la estructura de las condiciones de humano a como se van a procesar
;;
;;============================================================
(defun parser (c)
    (let((caux ())(andy '(and ))(ordy '(or )) (aux ())(auxstr ()) (answ ()));;depende para cada caso
        (case (first c)
            ('and ;;consulta compuesta
                (setq caux (rest c))
                (if (listp (first caux))
                    (setq andy (append andy (list (parser caux))))
                )
            )
            ('or ;;consulta compuesta
                (setq caux (rest c))
                (if (listp (first caux))
                    (setq ordy (append ordy (list (parser caux))))
                )
            )
            (NIL ;;si ya se terminaron los casos termino
                
            )
            (t ;;consulta simple
                (setq aux (first c));;obtengo la primer consulta
                (setq c (rest c));;modifico la variable que contiene las consultas
                (setq auxstr (string (rest  aux)));;guardo en cadena la condicion
                (if (equal #\[ (char auxtr 0))
                    ();;algo
                    ;;else
                    (progn 
                        (setq answ (list 'equal (list 'rest (list 'assoc (first aux) 'aux)) (rest aux) ))    
                    )
                )
            )
            
        )
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
        (format t "Clase solicitada ~A ~%" (rest clase))
        (format t "Condiciones ~A ~% ~%" condiciones)
        (setq condi (parser c))
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
        (loop while (eq fin 0) do
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