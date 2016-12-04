(defvar *zebra-bc* ())
(setq *zebra-bc*
    '(  ;;REGLAS
      ;;si existe casa *, la inserto en la calle
      #|(<- (calle (casa 1 Noruego ?c ?d ?e ?f) ?2 ?3 ?4 ?5) (casa 1 Noruego ?c ?d ?e ?f))
      (<- (calle  ?1 (casa 2 ?b ?c ?d ?e ?f) ?3 ?4 ?5) (casa 2 ?b ?c ?d ?e Azul))
      (<- (calle ?1 ?2 (casa 3 ?b ?c ?d Leche ?f) ?4 ?5) (casa 3 ?b ?c ?d Leche ?f))
      (<- (calle ?1 ?2 ?3 (casa 4 ?b ?c ?d ?e ?f) ?5) (casa 4 ?b ?c ?d ?e ?f))
      (<- (calle ?1 ?2 ?3 ?4 (casa 5 ?b ?c ?d ?e ?f)) (casa 5 ?b ?c ?d ?e ?f))|#
      ;;(<-  (junto ?x ?y) (Der ?y ?x) (Izq ?x ?y))
      
      
        ;;HECHOS(casa pos nacionalidad mascota cigarrillo bebida color)
      (<- (posicion 1))
      (<- (posicion 2))
      (<- (posicion 3))
      (<- (posicion 4))
      (<- (posicion 5))
      (<- (junto 1 2));;1.5
      (<- (junto 2 1));;1.5
      (<- (junto 2 3));;1.5
      (<- (junto 3 2));;1.5
      (<- (junto 4 3));;1.5
      (<- (junto 3 4));;1.5
      (<- (junto 5 4));;1.5
      (<- (junto 4 5));;1.5
      (<- (casa 1 Noruego ?c1 ?d1 ?e1 ?f1));;1
      (<- (casa 2 ?b2 ?c2 ?d2 ?e2 Azul));;1
      (<- (casa 3 ?b3 ?c3 ?d3 Leche ?f3));;1
      (<- (casa ?a ?b ?c ?d ?e ?f)
          (junto ?a ?p)
          (posicion ?a));;cada casa tiene una posicion y esta existe junto a otra
      #|(<- (casa ?a1 ?b1 ?c1 ?d1 ?e1 ?f1)
          (posicion ?a1)
          (junto ?a1 ?a2)
          (casa ?a2 ?b2 ?c2 ?d2 ?e2 ?f2)
          (posicion ?a2));;si existe una posicion, una casa con esa posicion y esta junto a otra posicion que tambien existe -> una casa tiene una posicions|#
      
      #|(<- (casa 4 ?b4 ?c4 ?d4 ?e4 ?f4));;1
      (<- (casa 5 ?b5 ?c5 ?d5 ?e5 ?f5));;1|#
      
      #|(<- (calle (casa 1 Noruego ?c ?d ?e ?f) 
                 (casa 2 ?b2 ?c2 ?d2 ?e2 Azul) 
                 (casa 3 ?b ?c ?d Leche ?f) 
                 (casa 4 ?b4 ?c4 ?d4 ?e4 ?f4) 
                 (casa 5 ?b5 ?c5 ?d5 ?e5 ?f5)));;1-10-9|#
      
      (<- (casa ?a Ingles ?c ?d ?e Rojo));;2
      (<- (casa ?a Español Perro ?d ?e ?f));;3
      (<- (casa ?a ?b ?c ?d Café Verde));;4
      (<- (casa ?a Ukraniano ?c ?d Té ?f));;5
      (<- (casa ?a1 ?b1 ?c1 ?d1 ?e1 verde)
          (casa ?a2 ?b2 ?c2 ?d2 ?e2 blanco)
          (junto ?a1 ?a2)
          (posicion ?a1)
          (posicion ?a2));;6
      (<- (casa ?a ?b Caracol OldGold ?e ?f));;7
      (<- (casa ?a ?b ?c Kools ?e Amarillo));;8
      (<- (casa ?a1 ?b1 ?c1 Chesterfields ?e1 ?f1) 
          (casa ?a2 ?b2 Zorro ?d2 ?e2 ?f2)
          (junto ?a1 ?a2)
          (posicion ?a1)
          (posicion ?a2));;11
      (<- (casa ?a1 ?b1 ?c1 Kools ?e1 ?f1) 
          (casa ?a2 ?b2 Caballo ?d2 ?e2 ?f2)
          (junto ?a1 ?a2)
          (posicion ?a1)
          (posicion ?a2));;12
      (<- (casa ?a ?b ?c LuckyStrike Jugo ?f));;13
      (<- (casa ?a Japones ?c Parliaments ?e ?f));;14
      #|(<- (casa ?a1 ?b1 ?c1 ?d1 ?e1 Azul) 
          (casa ?a2 Noruego ?c2 ?d2 ?e2 ?f2)
          (junto ?a1 ?a2));;15|#
      
      ;;Pregunta
      (<- (casa ?a ?b ?c ?d Agua ?f));;agua
      (<- (casa ?a ?b Zebra ?d ?e ?f));;zebra
      ))

;;(casa pos nacionalidad mascota cigarrillo bebida color)
(WITH-KB *zebra-bc* (ASK '(casa ?a ?b ?c ?d ?e ?f)))
