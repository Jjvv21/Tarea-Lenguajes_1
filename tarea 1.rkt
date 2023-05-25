#lang racket


(define (solucion n posicion)
  (cond
    ((<= n 4) (displayln "No se puede crear la matriz"))
    (else (crear_matriz n posicion))))


(define contador 1) ; Variable global para almacenar el contador

(define (incrementar-contador)
  (set! contador (+ contador 1)))

(define (usar-contador)
  (incrementar-contador)
  (displayln contador))


;; Aqui se crea la matriz y se envia a llenar
(define (crear_matriz n posicion)
  (define (crear-matriz-columnas i j)
    (cond
      ((= j n) '())
      ((equal? (list i j) posicion) (cons contador (crear-matriz-columnas i (+ j 1))))
      (else (cons (Dar_Valor_Casillas n i j) (crear-matriz-columnas i (+ j 1))))))
  
  (define (crear-matriz-filas i)
    (cond
      ((= i n) '())
      (else (cons (crear-matriz-columnas i 0)(crear-matriz-filas (+ i 1))))))
  
  (crear-matriz-filas 0))

(define(Dar_Valor_Casillas n i j)
  (cond
    ((= i (- n 1)) (P_U_fila n i j))
    ((= i (- n 2)) (S_P_fila n i j))
    ((= i 1) (S_P_fila n i j))
    ((= i 0) (P_U_fila n i j))
    (else  (llenar_medio n i j))))


(define (P_U_fila n i j)
  (cond
    ((= j 0) 2)
    ((= j 1) 3)
    ((= j (- n 1)) 2)
    ((= j (- n 2)) 3)
    (else 4)))

(define (S_P_fila n i j)
  (cond
    ((= j 0) 3)
    ((= j 1) 4)
    ((= j (- n 1)) 3)
    ((= j (- n 2)) 4)
    (else 6)))

(define (llenar_medio n i j)
  (cond
    ((= j 0) 4)
    ((= j 1) 6)
    ((= j (- n 1)) 4)
    ((= j (- n 2)) 6)
    (else 8)))
   

(define matriz (solucion 6 '(1 0)))


;;Codigo con la funcion para movimientos del caballo
(define (posiciones_caballo tamano ficha)
  (define fila (car ficha))
  (define columna (cadr ficha))

  (define (es_posicion_valida? x y)
    (and (>= x 0) (< x tamano) (>= y 0) (< y tamano)))

  (define (mover_caballo dx dy)
    (if (es_posicion_valida? (+ fila dx) (+ columna dy))
        (list (+ fila dx) (+ columna dy))
        #f))

  (define (calcular_posiciones movimientos)
    (cond
      ((null? movimientos) '())
      (else
       (let* ((movimiento (car movimientos))
              (dx (car movimiento))
              (dy (cadr movimiento))
              (siguientes_posiciones (calcular_posiciones (cdr movimientos))))
         (if (mover_caballo dx dy)
             (cons (mover_caballo dx dy) siguientes_posiciones)
             siguientes_posiciones)))))

  (define movimientos '((2 1) (1 2) (-2 1) (-1 2) (2 -1) (1 -2) (-2 -1) (-1 -2))) ; Posibles movimientos del caballo
  (calcular_posiciones movimientos))



;;Funciones que estoy trabajando
(define (restar-indices lista1 lista2)
  (restar-indices-aux lista1 lista2))

(define matriz-modificada '()) ; Variable externa para almacenar la matriz modificada

(define (restar-indices-aux lista1 lista2)
  (set! matriz-modificada lista1) ; Inicializar la matriz modificada
  (cond
    ((null? lista2) (displayln "listavacia"))
    (else
     (let ((sublista (car lista2)))
       (set! matriz-modificada (encontrar-tablero matriz-modificada sublista))
       (restar-indices-aux2 matriz-modificada (cdr lista2) sublista)))))

(define (restar-indices-aux2 lista1 lista2 sublista)
  (set! matriz-modificada lista1) ; Inicializar la matriz modificada
  (cond
    ((null? lista2) (displayln ""))
    (else
     (let ((sublista (car lista2)))
       (set! matriz-modificada (encontrar-tablero matriz-modificada sublista))
       (restar-indices-aux2 matriz-modificada (cdr lista2) sublista)))))

 (define (encontrar-tablero lista1 sublista)
  (let ((i (car sublista))
        (j (cadr sublista)))
    (let ((fila (list-ref lista1 i)))
      (let ((nueva-fila (append (take fila j) (list (- (list-ref fila j) 1)) (drop fila (+ j 1)))))
        (set! lista1 (list-set lista1 i nueva-fila)))
    lista1)))

(define (imprimir-matriz matriz)
  (displayln "Matriz modificada:")
  (displayln matriz))
      
(define (realizar-movs matriz-modificada lista2)
  (displayln (car lista2)))


(define (PDC-Sol n lista)
  (define matriz1 (solucion n lista))
  (define matriz2 (posiciones_caballo n lista))
  (restar-indices matriz1 matriz2)
  (imprimir-matriz matriz-modificada)
  (realizar-movs matriz-modificada matriz2)
  (list matriz1 matriz2))

(PDC-Sol 6 '(0 0))
