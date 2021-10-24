;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Tetris) (read-case-sensitive #t) (teachpacks ((lib "world.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "world.rkt" "teachpack" "htdp")) #f)))
;; Tetrís
;; 1. Elaborar los bloques del tetris, estos se deben generar de forma aleatoria una vez este allá llegado a su "limite"
;; 2. Cuando los bloques hagan una linea estos deben desaparecer (los que hagan parte de la linea) una linea puede ser horizontal o vertical
;; 3. Dibujar una pieza del tetris



;; *******************************************************************************************
;; DEFINICIÓN DE ESTRUCTURAS
;; *******************************************************************************************

;; bloque puede ser cualquier bloque del juego: 
(define-struct bloque  (pos-x pos-y color))

;; pieza puede ser cualquier pieza del juego 
(define-struct pieza (centro bloques))

;; mundo es una estructura de la forma: 
(define-struct mundo (pieza))


;; *******************************************************************************************
;;  DECLARACIÓN DE CONSTANTES GLOBALES
;; *******************************************************************************************

;; Constantes asociadas al CANVAS:

;; Alto del CANVAS
(define CANVAS-ALTO  600)

;; Ancho del CANVAS
(define CANVAS-ANCHO 500)   

(define ESCENA-INICIAL  (empty-scene CANVAS-ANCHO CANVAS-ALTO))



(define (actualizar-mundo mundo)
  (mover-pieza-abajo-mundo
   mundo))
              
(define (remplazar-pieza pieza mundo)
  (make-mundo
   pieza))

(define (remplazar-bloques bloques mundo)
  (make-mundo
   (make-pieza
   (pieza-centro (mundo-pieza mundo))
   bloques)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                   PIEZAS                  ;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define O 
;  (list (make-bloque  "yellow")
;        (make-bloque  "yellow")
;        (make-bloque  "yellow")
;        (make-bloque  "yellow")))

;(define I 
;  (list (make-bloque  "turquoise")
;        (make-bloque  "turquoise")
;        (make-bloque  "turquoise")
;        (make-bloque  "turquoise")))
;

;(define L 
;  (list (make-bloque  "orange")
;        (make-bloque  "orange")
;        (make-bloque  "orange")
;        (make-bloque  "orange")))
;

;(define J 
;  (list (make-bloque   "pink")
;        (make-bloque   "pink")
;        (make-bloque   "pink")
;        (make-bloque   "pink")))
;

(define T (make-pieza
           (make-posn 14 18)
  (list (make-bloque 14 28 "purple")
        (make-bloque 4 18 "purple")
        (make-bloque 14 18 "purple")
        (make-bloque 24 18 "purple"))))

;(define Z 
;  (list (make-bloque   "green")
;        (make-bloque   "green")
;        (make-bloque   "green")
;        (make-bloque   "green")))
;

;(define S 
;  (list (make-bloque   "red")
;        (make-bloque  "red")
;        (make-bloque   "red")
;        (make-bloque   "red")))


;; entrada: bloque
;; salida: imagen 
(define (dibujar-bloque bloque)
  (rectangle 10 10  "solid" (bloque-color bloque)))

;; entrada: pieza escena
;; salida: escena 
(define (dibujar-pieza pieza escena)
  (cond [(empty? pieza) escena]
        [else  ( dibujar-imagen
                (dibujar-bloque (first pieza))
                (bloque-pos-x (first pieza))
                (bloque-pos-y (first pieza))
                (dibujar-pieza (rest pieza) escena))]))

;; Entrada: imagen x y escena
;; Salida: escena 


(define (dibujar-imagen imagen x y escena)
(place-image imagen x y escena))


(define (dibujar-mundo mundo)
  (dibujar-pieza1 (mundo-pieza mundo)
   ESCENA-INICIAL))

(define (dibujar-pieza1 pieza escena)
        (dibujar-pieza (pieza-bloques pieza)
                       escena))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   MOVIMIENTO BLOQUES              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (procesar-teclado mundo tecla)
  {cond
    
    [(key=? tecla 'left)
     (if (bloque-al-limite-izq?  (pos-x-bloque-mas-a-la-izquierda(pieza-bloques(mundo-pieza mundo))))
        mundo
        (remplazar-pieza
         (mover-pieza (mundo-pieza mundo) -10)
         mundo))]

    [(key=? tecla 'right)
     (if (bloque-al-limite-der?  (pos-x-bloque-mas-a-la-derecha (pieza-bloques(mundo-pieza mundo))))
        mundo
         (remplazar-pieza       
          (mover-pieza  (mundo-pieza mundo) 10)
          mundo))]
    
    [(key=? tecla #\z)
     (remplazar-bloques
      (girar-derecha-bloques (pieza-centro (mundo-pieza mundo)) (pieza-bloques (mundo-pieza mundo)))
     mundo)]

    [(key=? tecla #\x)
     (remplazar-bloques
      (girar-izquierda-bloques (pieza-centro (mundo-pieza mundo)) (pieza-bloques (mundo-pieza mundo)))
      mundo)]
    
     [(key=? tecla 'up)
     (remplazar-bloques
       (girar-izquierda-bloques (pieza-centro (mundo-pieza mundo)) (pieza-bloques (mundo-pieza mundo)))
      mundo)]

     [(key=? tecla 'down)
      (remplazar-pieza       
       (mover-pieza-abajo (mundo-pieza mundo) 10)
       mundo)]
     
     [else mundo]})


;; esta función mueve un bloque
;; Entrada: bloque desplazamiento
;; Salida: bloque
(define (mover-bloque bloque desplazamiento)
(make-bloque
 (+ (bloque-pos-x bloque) desplazamiento)
 (bloque-pos-y bloque)
 (bloque-color bloque)))

(define (mover-bloques bloques desplazamiento)
  (if (empty? bloques)
      '()
      (cons (mover-bloque (first bloques) desplazamiento)
            (mover-bloques (rest bloques) desplazamiento))))

(define (mover-pieza pieza desplazamiento)
  (make-pieza
   (make-posn (+ desplazamiento (posn-x (pieza-centro pieza))) (posn-y (pieza-centro pieza)))
   (mover-bloques (pieza-bloques pieza) desplazamiento)))


(define (mover-bloque-abajo bloque desplazamiento)
(make-bloque
 (bloque-pos-x bloque)
 (+ (bloque-pos-y bloque)desplazamiento)
 (bloque-color bloque)))

(define (mover-bloques-abajo bloques desplazamiento)
  (if (empty? bloques)
      '()
      (cons (mover-bloque-abajo (first bloques)desplazamiento)
            (mover-bloques-abajo (rest bloques) desplazamiento))))

(define (mover-pieza-abajo pieza desplazamiento)
  (make-pieza
   (make-posn (posn-x (pieza-centro pieza)) (+ desplazamiento (posn-y (pieza-centro pieza))))
   (mover-bloques-abajo (pieza-bloques pieza) desplazamiento)))

(define (mover-pieza-abajo-mundo mundo)
  (remplazar-pieza
   (mover-pieza-abajo (mundo-pieza mundo) 0.1)
  mundo))
                         

                             

;; girar-izquierda-bloque : posn bloque -> bloque
;; Rota un bloque 90 grados a la izquierda al rededor de un centro 
(define (girar-izquierda-bloque centro bloque)
  (make-bloque (+ (posn-x centro) (- (posn-y centro) (bloque-pos-y bloque)))
              (+ (posn-y centro) (- (bloque-pos-x bloque) (posn-x centro)))
              (bloque-color bloque)))

(define (girar-izquierda-bloques  centro bloques)
  (local 
    [(define (girar-izquierda bloque)
       (girar-izquierda-bloque centro bloque))]
    (map girar-izquierda bloques)))


;; girar-izquierda-bloque : posn bloque -> bloque
;; tres rotaciones a la izquierda = rotación a la derecha. 
(define (girar-derecha-bloque centro bloque)
  (girar-izquierda-bloque centro (girar-izquierda-bloque centro (girar-izquierda-bloque centro bloque))))


(define (girar-derecha-bloques  centro bloques)
  (local 
    [(define (girar-derecha bloque)
       (girar-derecha-bloque centro bloque))]
    (map girar-derecha bloques)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   LIMITES BLOQUES                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bloque-al-limite-izq? x)
  (< x 10))

(define (bloque-al-limite-der? x)
  (> x (- CANVAS-ANCHO 10)))

(define (pos-x-bloque-mas-a-la-izquierda bloques)
  (cond
    [(empty? bloques) 0]
    [(empty? (rest bloques)) (bloque-pos-x (first bloques))]
    (else
     (min (bloque-pos-x (first bloques))
          (pos-x-bloque-mas-a-la-izquierda (rest bloques))))))


(define (pos-x-bloque-mas-a-la-derecha bloques)
  (cond
    [(empty? bloques) 0]
    [(empty? (rest bloques)) (bloque-pos-x (first bloques))]
    (else
     (max (bloque-pos-x (first bloques))
          (pos-x-bloque-mas-a-la-derecha (rest bloques))))))








;; ###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-###-### ;; 
(big-bang CANVAS-ANCHO CANVAS-ALTO (/ 1 60) (make-mundo
                                             T))
(on-redraw dibujar-mundo)
(on-key-event procesar-teclado)
(on-tick-event actualizar-mundo)

