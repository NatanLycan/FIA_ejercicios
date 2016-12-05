(defvar *zebra-bc* ())
(setq *zebra-bc*
    '(  ;;REGLAS
      
      (<- (casa ?id ?b ?a ?d ?e ?f) (posicion ?id))
      (<- (casa ?idn ?an ?c ?d ?e ?f) (nacionalidad ?idn ?an))
      (<- (casa ?idm ?b ?am ?d ?e ?f) (mascota ?idm ?am))
      (<- (casa ?idc ?b ?a ?dc ?e ?f) (cigarro ?idc ?ac))
      (<- (casa ?idb ?b ?a ?d ?eb ?f) (bebida ?idb ?ab))
      (<- (casa ?idco ?b ?a ?d ?e ?aco) (color ?idco ?aco))
        ;;HECHOS(casa pos nacionalidad mascota cigarro bebida color)
      ;;----existen 5 casas
      (<- (posicion 1));;1
      (<- (posicion 2));;1
      (<- (posicion 3));;1
      (<- (posicion 4));;1
      (<- (posicion 5));;1
      ;;----las casas son contiguas
      (<- (junto 1 2));;1.5
      (<- (junto 2 1));;1.5
      (<- (junto 2 3));;1.5
      (<- (junto 3 2));;1.5
      (<- (junto 4 3));;1.5
      (<- (junto 3 4));;1.5
      (<- (junto 5 4));;1.5
      (<- (junto 4 5));;1.5
      (<- (nacionalidad ?id Ingles) (color ?id Rojo));;2
      (<- (nacionalidad ?id Español) (mascota ?id Perro));;3
      (<- (color ?id Verde) (bebida ?id Cafe));;4
      (<- (nacionalidad ?id Ukraniano) (bebida ?id Te));;5
      (<- (color ?id1 Verde) (color ?id2 Blanco) (junto ?id1 ?id2));;6
      (<- (cigarro ?id OldGold) (mascota ?id Caracol));;7
      (<- (cigarro ?id Kools) (color ?id Amarillo));;8
      (<- (bebida 3 Leche));;9
      (<- (nacionalidad 1 Noruego));;10
      (<- (cigarro ?c1 Chesterfields) (mascota ?c2 Zorro) (junto ?c1 ?c2));;11
      (<- (cigarro ?c Kools) (mascota ?m Caballo) (junto ?c ?m));;12
      (<- (cigarro ?id LuckyStrike) (bebida ?id Jugo));;13
      (<- (nacionalidad ?id Japones) (cigarro ?id Parliaments));;14
      (<- (nacionalidad ?n Noruego) (color ?c Azul) (junto ?n ?c));;15
      
      ))

;;(casa pos nacionalidad mascota cigarrillo bebida color)
;;(WITH-KB *zebra-bc* (ASK '(casa ?a ?b ?c ?d ?e ?f)))
