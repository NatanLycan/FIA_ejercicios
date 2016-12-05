(defvar *zebra-bc* ())
(setq *zebra-bc*
    '(
      (<- (posicion 1))
      (<- (posicion 2))
      (<- (posicion 3))
      (<- (posicion 4))
      (<- (posicion 5))
      
      ;;----------Colores conocidos
      (<- (colort Azul))
      (<- (colort Verde))
      (<- (colort Amarillo))
      (<- (colort Blanco))
      (<- (colort Rojo))
      ;;----------Mascotas conocidas
      (<- (mascot Perro))
      (<- (mascot Caballo))
      (<- (mascot Zorro))
      (<- (mascot Caracol))
      (<- (mascot ?x))
      ;;----------Nacionalidades conocidas
      (<- (naciot Español))
      (<- (naciot Ingles))
      (<- (naciot Ukraniano))
      (<- (naciot Japones))
      (<- (naciot Noruego))
      ;;----------Bebidas conocidas
      (<- (bebidt Jugo))
      (<- (bebidt Te))
      (<- (bebidt Cafe))
      (<- (bebidt Leche))
      ;;---------Cigarros conocidos
      (<- (cigart OldGold))
      (<- (cigart Kools))
      (<- (cigart Chestefields))
      (<- (cigart LuckyStrike))
      (<- (cigart Parliaments))
      
      
      (<- (junto 1 2));;1.5
      (<- (junto 2 1));;1.5
      (<- (junto 2 3));;1.5
      (<- (junto 3 2));;1.5
      (<- (junto 4 3));;1.5
      (<- (junto 3 4));;1.5
      (<- (junto 5 4));;1.5
      (<- (junto 4 5));;1.5
      (<- (casa 1 Noruego ?c ?d ?e ?f))
      (<- (casa 2 ?b2 ?c2 ?d2 ?e2 Azul))
      (<- (casa 3 ?b ?c ?d Leche ?f))
      ;;(<- (casa 4 ?b ?c ?d ?e ?f))
      ;;(<- (casa 5 ?b ?c ?d ?e ?f))
      (<- (casa ?a Ingles ?c ?d ?e Rojo));;2
      (<- (casa ?a Español Perro ?d ?e ?f));;3
      (<- (casa ?a ?b ?c ?d Café Verde));;4
      (<- (casa ?a Ukraniano ?c ?d Té ?f));;5
      (<- (junto ?a1 ?a2)
          (casa ?a1 ?b1 ?c1 ?d1 ?e1 Verde)
          (casa ?a2 ?b2 ?c2 ?d2 ?e2 Blanco)
          (posicion ?a2));;6
      (<- (casa ?a ?b Caracol OldGold ?e ?f));;7
      (<- (casa ?a ?b ?c Kools ?e Amarillo));;8
      (<- (junto ?a1 ?a2)
          (casa ?a1 ?b1 ?c1 Chesterfields ?e1 ?f1) 
          (casa ?a2 ?b2 Zorro ?d2 ?e2 ?f2)
          (posicion ?a2));;11
      (<- (junto ?a1 ?a2)
          (casa ?a1 ?b1 ?c1 Kools ?e1 ?f1) 
          (casa ?a2 ?b2 Caballo ?d2 ?e2 ?f2)
          (posicion ?a2));;12
      (<- (casa ?a ?b ?c LuckyStrike Jugo ?f));;13
      (<- (casa ?a Japones ?c Parliaments ?e ?f));;14
      
      #|(<- (casa ?a ?n ?c ?d ?e ?f) (naciot ?n))
      (<- (casa ?a ?b ?n ?d ?e ?f) (mascot ?n))
      (<- (casa ?a ?b ?c ?n ?e ?f) (cigart ?n))
      (<- (casa ?a ?b ?c ?d ?n ?f) (bebidt ?n))
      (<- (casa ?a ?b ?c ?d ?e ?n) (colort ?n))|#
      
      ;;(<- (casa ?a ?b ?c ?d ?e ?f) (posicion ?a) (naciot ?b) (mascot ?c) (cigart ?d) (bebidt ?e) (colort ?f))
      
      
      ;;Pregunta
      ;;(<- (casa ?a ?b ?c ?d Agua ?f));;agua
      ;;(<- (casa ?a ?b Zebra ?d ?e ?f));;zebra
      ))

(WITH-KB *zebra-bc* (ASK '(casa ?a ?b ?c ?d ?e ?f)))