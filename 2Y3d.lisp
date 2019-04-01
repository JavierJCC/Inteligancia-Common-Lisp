;;;=======================================================================
;;;  Prueba3D.lisp
;;;      Resuelve un laberinto en 2D
;;;
;;;      Chávez Chávez Javier
;;;=======================================================================
(load "maze_lib.lisp")

;;========================================================================
;;  Varibales Globales
;;========================================================================

(defparameter  *open* '())       ;; Frontera de busqueda...                                              
(defparameter  *memory* '())   ;; Memoria de intentos previos
(defparameter *edoMeta* nil)  ;; Estado meta
(defparameter *limiteEnX* 0)
(defparameter *limiteEnY* 0)
;;
;;(defparameter  *ops*  '((:Arriba-derecha  (-1 1) 1)
;;                      (:Abajo-derecha   (1 1) 3) 
;;                      (:Abajo-izquierda (1 -1) 5)   
;;                      (:Arriba-izquierda  (-1 -1) 7)
;;                      (:Arriba      (-1 0) 0) 
;;                      (:Derecha       (0 1) 2 )
;;                      (:Abajo       (1 0) 4 )
;;                     (:Izquierda     (0 -1) 6)
;;                      (:Arriba-Puente    (-2 0) 0)
;;                      (:Derecha-Puente  (0 2)  2)
;;                      (:Abajo-Puente    (2 0)  4)
;;                      (:Izquierda-Puente  (0 -2) 6)
;;                      ))

(defparameter  *ops*  '(
                      (:Arriba      (-1 0) 0) 
                      (:Derecha       (0 1) 2 )
                      (:Abajo       (1 0) 4 )
                      (:Izquierda     (0 -1) 6)
                      (:Arriba-Puente    (-2 0) 0)
                      (:Derecha-Puente  (0 2)  2)
                      (:Abajo-Puente    (2 0)  4)
                      (:Izquierda-Puente  (0 -2) 6)
                      ))



(defparameter  *id*  0)  ;; Identificador del ultimo nodo creado, cada vez que se cree un nodo se debe incrementar
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *profundidad*  -1)  ;;La profundidad del ancestro común a todos los descendientes que se generen

;;=======================================================================
;;  CREATE-NODE [estado  op actitud]  
;;
;;  ESTRUCTURA DE UN NODO
;;    (<id> <estado> <actitud> <ancestro> <operador> <profundidad>)
;;=======================================================================

(defun create-node(estado op aptitud)
"Creamos un nuevo nodo recibiendo como parametros el estado, operador y la actitud"
  (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
    (list  *id*  estado aptitud *current-ancestor*  (third op) (1+ *profundidad*))
    )  ;;los nodos generados son descendientes de *current-ancestor*

;;=======================================================================
;;  FUNCIONES DE APTITUD
;;    * Distancia Manhatan  f(x)= h(x) donde h(x)= |x1 - x0| + |y1 - y0|
;;    * Aptitudes
;;      BFS = h(x)
;;      A* = g(x) + h(x) donde h(x) es la profundidad
;;=======================================================================

(defun DistanciaManhatan (x0 y0 x1 y1)
  "Regresa la Distancia Manhathan entre dos coordenadas"
  (+ (abs (- x1 x0)) (abs (- y1 y0))))


(defun insert-aptitud (estado metodo)
    (cond ((eql  metodo :A*) (+ (1+ *profundidad*) (DistanciaManhatan (first estado) (second estado) (first *edoMeta*) (second *edoMeta*))))
    ((eql metodo :BFS) (DistanciaManhatan (first estado) (second estado) (first *edoMeta*) (second *edoMeta*)))
    (T 1))
  )

;;=======================================================================
;;  INSERT-TO-OPEN
;;      * Decide el metodo de inserción que vamos a utilizar ya sea A*, BFS, depth-first y breath-first
;;  INSERT-TO-ORDER
;;      * Inserta en la frontera especificamente haciendo uso del algottimo BFS 
;;
;;
;;=======================================================================
(defun insert-in-order (nodo aux-open) 
"Inserta el nodo recibido en *OPEN* segun su valor de aptitud"
  ;(format t "Nodo ~a y la lista ~a ~%"nodo aux-open)
  (cond ((null aux-open) (list nodo))
    ((<= (third nodo) (third (first aux-open))) (cons nodo aux-open)) ;;Comparamos las aptitudes que existen en nuestra frontera
    (T (cons (first aux-open) (insert-in-order nodo (rest aux-open)))))
  )



(defun encuentra-estado (estado aptitud lista)
  (cond ((null lista) (list nil nil))
    ((equal estado (third (first lista))) 
      (if (< aptitud (second (first lista))) (list T T) (list T nil)))
    (T (encuentra-estado estado aptitud (rest lista)))
    )
  )

(defun elimina-estado(estado lista)
  (cond ((equal estado (third (first lista))) (rest lista))
    (T (cons (first lista) (elimina-estado estado (rest lista))))
    )
  )

(defun insert-A* (nodo)
  (let ((res (encuentra-estado (second nodo) (third nodo) *open*)))
    ;(format t "Nodo ~a encontro ~a ~%" nodo res)
    (cond ((and (first res) (second res))
      (setq *open* (elimina-estado (second nodo) *open*)
          *open* (insert-in-order nodo *open*)))
      ((and (null (first res)) (null (second res)))
      (setq *open* (insert-in-order nodo *open*))
        )
      )
    )
  )

(defun insert-to-open (estado  op metodo)  
"Funcion que determina la forma de insercion respecto al metodo"
    (let* ((aptitud (insert-aptitud estado metodo)) (nodo  (create-node estado op aptitud)))
      (cond ((eql  metodo :A*) (insert-A* nodo))
          ((eql  metodo :BFS) (setq *open* (insert-in-order nodo *open*)))
          ((eql  metodo :depth-first) (push  nodo  *open*))
            ((eql  metodo :breath-first) (setq *open*  (append  *open*  (list nodo))))
      )
    ) 
)

;;=======================================================================
;;  GET-FROM-OPEN
;;    *Regresa la solución
;;=======================================================================

(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *Open*))
;;=======================================================================
;;    Funciones que cuentan los puentes consecutivamente dependiendo de 
;;    su operador
;;=======================================================================
(defun sube (estado avance contador)
    (cond ((< avance 15) contador)
    (T (sube (list (1- (first estado)) (second estado)) (get-cell-walls (1- (first estado)) (second estado)) (1+ contador)))
    )
  )
(defun baja (estado avance contador)
    (cond ((< avance 15) contador)
    (T (baja (list (1+ (first estado)) (second estado)) (get-cell-walls (1+ (first estado)) (second estado)) (1+ contador)))
    )
  )
(defun der (estado avance contador)
    (cond ((< avance 15) contador)
          (T (der (list (first estado) (1+ (second estado))) (get-cell-walls (first estado) (1+ (second estado))) (1+ contador)))
    )
  )
(defun izq (estado avance contador)
    (cond ((< avance 15) contador)
    (T (izq (list (first estado) (1- (second estado))) (get-cell-walls (first estado) (1- (second estado))) (1+ contador)))
    )
  )

;;=======================================================================
;;    Cuntan el numero de puentes que existen consecutivamente
;;
;;=======================================================================
(defun cuenta(estado op)
  "Cuenta cuantos puentes existen"
    (let* ((total 0) (avance 17))
      ;(format t "El estado es: ~a ~%" estado)
      (cond 
      ((eql (first op) :Arriba-Puente) 
          (setq total (sube estado avance total))
        )
      ((eql (first op) :Abajo-Puente) 
          (setq total (baja estado avance total))
        )
      ((eql (first op) :Derecha-Puente) 
          (setq total (der estado avance total))
        )
      ((eql (first op) :Izquierda-Puente)
          (setq total (izq estado avance total))
        )
      (T -1)
      )
    )
  )

;;=======================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado]
;;=======================================================================

(defun valid-operator(op estado)
"Predicado. Valida la aplicación de un operador a un estado"
  (let*  ((newX (+ (first estado) (first (second op)))) (newY (+ (second estado) (second (second op))))
         (p nil) (d nil)) 

    ;(format t "Las coordenadas son: ~a ~a ~a~%" newX newY (first op))
    ;;Validamos primero que exista en el mapa
    ;(format t "~a ~a ~a ~a~%" estado op newX newY)
    (cond ((not (and (>= newX 0) (< newX *limiteEnX*) (>= newY 0) (< newY *limiteEnY*) )) nil)
      (T (setq p (get-cell-walls (first estado) (second estado)) d (get-cell-walls newX newY))
        ;(format t "el operador es ~a son ~a  y ~a ~%"(first op) p d)
        ;;validamos que se pueda avanzar en la direccion
        (cond 
        ((eql  (first op) :Arriba)
        (and (/= (logand d 16) 16) (/= (logand d 17) 17) (/= (logand p 1) 1))
          )
        ((eql  (first op) :Arriba-Puente)
          (setq d (get-cell-walls (1+ newX) newY))
          (print d)
          (return-from valid-operator (or (= (logand d 16) 16) (= (logand d 17) 17)))
          )
        ((eql  (first op) :Derecha)
            (and (/= (logand d 16) 16) (/= (logand d 17) 17) (/= (logand p 2) 2))

            )
          ((eql  (first op) :Derecha-Puente)
            (setq d (get-cell-walls newX (1- newY)))
            (or (= (logand d 16) 16) (= (logand d 17) 17))
            )
          ((eql  (first op) :Abajo)
          (and (/= (logand d 16) 16) (/= (logand d 17) 17) (/= (logand p 4) 4))

            )
          ((eql  (first op) :Abajo-Puente)
            (setq d (get-cell-walls (1- newX) newY))
            (or (= (logand d 16) 16) (= (logand d 17) 17))
            )
          ((eql  (first op) :Izquierda)
            (and (/= (logand d 16) 16) (/= (logand d 17) 17) (/= (logand p 8) 8))
            )
          ((eql  (first op) :Izquierda-Puente)
            (setq d (get-cell-walls newX (1+ newY)))
            (or (= (logand d 16) 16) (= (logand d 17) 17))
            )
          ((eql  (first op) :Arriba-derecha)
              (cond ((= (+ (logand p 2) (logand d 8)) 10) Nil)
            ((= (+ (logand p 1) (logand d 4)) 5) Nil)
            ((= (logand p 3) 3) Nil)
            ((= (logand d 12) 12) Nil)
            ((or (= (logand d 16) 16) (= (logand d 17) 17)) Nil)
            ((or (= (get-cell-walls (- (first estado) 1) (second estado)) 17) (= (get-cell-walls (- (first estado) 1) (second estado)) 16)) Nil)
            ((or (= (get-cell-walls (first estado) (+ (second estado) 1)) 17) (= (get-cell-walls (first estado) (+ (second estado) 1)) 16)) Nil)
            (T T)
            )
            )
          ((eql  (first op) :Abajo-derecha)
              (cond ((= (+ (logand p 2) (logand d 8) ) 10) Nil)
            ((= (+ (logand p 4) (logand d 1) ) 5) Nil)
            ((= (logand p 6) 6) Nil)
            ((= (logand d 9) 9) Nil)
            ((or (= (logand d 16) 16) (= (logand d 17) 17)) Nil)
            ((or (= (get-cell-walls (+ (first estado) 1) (second estado)) 17) (= (get-cell-walls (+ (first estado) 1) (second estado)) 16)) Nil)
            ((or (= (get-cell-walls (first estado) (+ (second estado) 1)) 17) (= (get-cell-walls (first estado) (+ (second estado) 1)) 16)) Nil)
            (T T)
            )
            )
          ((eql  (first op) :Abajo-izquierda)
              (cond ((= (+ (logand p 4) (logand d 1)) 5) Nil)
            ((= (+ (logand p 8) (logand d 2)) 10) Nil)
            ((= (logand p 12) 12) Nil)
            ((= (logand d 3) 3) Nil)
            ((or (= (logand d 16) 16) (= (logand d 17) 17)) Nil)
            ((or (= (get-cell-walls (+ (first estado) 1) (second estado)) 17) (= (get-cell-walls (+ (first estado) 1) (second estado)) 16)) Nil)
            ((or (= (get-cell-walls (first estado) (- (second estado) 1)) 17) (= (get-cell-walls (first estado) (- (second estado) 1)) 16)) Nil)
            (T T)
            )
            )
          ((eql  (first op) :Arriba-izquierda)
              (cond ((= (+ (logand p 8) (logand d 2)) 10) Nil)
            ((= (+ (logand p 1) (logand d 4)) 5) Nil)
            ((= (logand p 9) 9) Nil)
            ((= (logand d 6) 6) Nil)
            ((or (= (logand d 16) 16) (= (logand d 17) 17)) Nil)
            ((or (= (get-cell-walls (- (first estado) 1) (second estado)) 17) (= (get-cell-walls (- (first estado) 1) (second estado)) 16)) Nil)
            ((or (= (get-cell-walls (first estado) (- (second estado) 1)) 17) (= (get-cell-walls (first estado) (- (second estado) 1)) 16)) Nil)
            (T T)
            )
            )

        (T  Nil)) 
    ) )
  )
)
;;=======================================================================
;;  APPLY-OPERATOR [op, estado]
;;        Solución simbólica del problema 
;;=======================================================================
(defun  apply-operator (op  estado repetir) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  CON VALIDACIONES EN LOS ESTADOS"
  (let  ( (nuevo nil) )
      (cond ((= repetir -1)      
        (setq nuevo (list (+ (first estado) (first (second op))) (+ (second estado) (second (second op)))))
          )
        ((/= repetir -1) 
           (cond 
              ((eql (first op) :Arriba-Puente) 
                  (setq nuevo (list (+ (first estado) (* repetir -1)) (+ (second estado) (second (second op)))))
                )
              ((eql (first op) :Abajo-Puente) 
                  (setq nuevo (list (+ (first estado) repetir) (+ (second estado) (second (second op)))))
                )
              ((eql (first op) :Derecha-Puente) 
                  (setq nuevo (list (+ (first estado) (first (second op))) (+ (second estado) repetir)))
                )
              ((eql (first op) :Izquierda-Puente)
                  (setq nuevo (list (+ (first estado) (first (second op))) (+  (second estado) (* repetir -1))))
                )
            )
          )
      ) 
   )
)
;;=======================================================================
;;  EXPAND [ estado]
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;=======================================================================
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
    (let* ((descendientes  nil)
        (nuevo-estado  nil) (repetir 0) (primedos nil)(segundos nil))
      (print estado)
        (dolist  (op  *Ops*  descendientes) 
           ;(print op)
           
        (when (valid-operator  op  estado)
          (format t "~a ~a ~%"(valid-operator  op  estado) (first op))
          (setq repetir (cuenta estado op))
          (setq primedos (list (first op) (second op)))
          (setq segundos (list (first op) (second op) (list repetir (third op))))
          ;(format t "Repeticion es igual a: ~a ~%" repetir)
          ;(format t "Primeros es igual a: ~a ~%" segundos)
          (setq  nuevo-estado  (apply-operator  op estado repetir))
          ;(format t "El nuevo estado es: ~a ~%" nuevo-estado)
              (cond ((= repetir -1) (setq  descendientes  (cons  (list nuevo-estado op) descendientes)) )
                (T (setq  descendientes  (cons  (list nuevo-estado  segundos)  descendientes)) )
                )
              ))))
    
;;=======================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;=======================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos"
     (cond ((null  lista-memoria)  Nil)
          ((equalp  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
    (T  (remember-state?  estado  (rest  lista-memoria))))  )

(defun  filter-memories (lista-estados-y-ops metodo) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory* la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
    (cond ((null  lista-estados-y-ops)  Nil)
      ( (or (remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda en la memoria, filtrarlo...
        (if (eql metodo :BFS) (remember-state? (first (first  lista-estados-y-ops)) *open*) nil));; si se encuentra en la frontera de busqueda, filtrarlo...
         (filter-memories  (rest  lista-estados-y-ops) metodo))
  (T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops) metodo)))) )  ;; de lo contrario, incluirlo en la respuesta

;;=======================================================================
;;  Aplana la solución de la lista  
;;  Ejem (1 4 (2 2 2 2 2)) se transforma a (1 4 2 2 2 2 2)
;;=======================================================================
(defun aplana(lista)
  (cond ((null lista) lista)
  ((atom (first lista)) (cons (first lista) (aplana (rest lista))))
  (T (append (aplana (first lista)) (aplana (rest lista)))))

  )
;;=======================================================================
;;  EXTRACT-SOLUTION  
;;  Extrae la solucion de la memoria
;;=======================================================================
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)
         (cond ((null  lista)  Nil)
         ((eql  id  (first (first  lista))) (first  lista))
    (T  (locate-node  id (rest  lista))))))


     (let ((current  (locate-node  (first  nodo)  *memory*)))
       (loop  while  (not (null  current))  do                        
   (push  (nth 4 current)  *solution*)     
   (setq  current  (locate-node  (fourth  current) *memory*))))
        (pop *solution*)))
;;=======================================================================
;;  RESET-ALL  ,  MAZE-SEARCH   y   MESURE-SEARCH
;;
;;=======================================================================

(defun reset-all () 
"Reinicia todas las variables globales para iniciar una nueva búsqueda..."
    (setq  *open*  nil)
    (setq  *memory*  nil)
    (setq  *id*  0)
    (setq  *current-ancestor*  nil)
    (setq  *solution*  nil)
    (setq  *edoMeta* nil) 
    (setq *profundidad* -1)
    (setq *limiteEnX* (get-maze-rows)) 
    (setq *limiteEnY* (get-maze-cols))
    )
;;=======================================================================
;;  crealista 
;;  Crea una lista dependiendo del numero de puentes consecutivos que existieron
;;  Ejem (5 2) se transforma a (2 2 2 2 2)
;;=======================================================================
(defun crealista(repetir num)
  "Crea una lista con n numeros repetidos"
  (let ((lista nil))
    (dotimes (i repetir lista)
      (setq lista (cons num lista))
      )
    )
    
  )

;;=======================================================================
;;  recorre 
;;  recorre la solucion y si se encuentra una lista la transforma 
;;  Ejem ( 1 4 (5 2)) se transforma a (1 4 (2 2 2 2 2))
;;=======================================================================
(defun recorre (lista)
  (cond ((null lista) lista)
      ((listp (first lista)) (cons (crealista (first (first lista)) (second (first lista))) (recorre (rest lista))))
      ((atom (first lista)) (cons (first lista) (recorre (rest lista))))
    )
  )

(defun Maze2d-search (edo-inicial  edo-meta  metodo)
"Realiza una busqueda para un laberinto los métodos posibles para evaluar la aptitud son: :A*, :BFS, depth-first, breath-first"
  (reset-all)
  (let ((nodo nil)
    (estado nil)
    (sucesores  '())
    (operador  nil)
    (meta-encontrada  nil))
      (setq *edoMeta* edo-meta )
      (insert-to-open  edo-inicial nil metodo)
      (loop until (or  meta-encontrada (null *open*))  do
      (setq  nodo (get-from-open)   
         estado  (second  nodo)
         operador  (nth 4 nodo))
      (cond ((not (remember-state? estado *memory*)) 
      (push  nodo  *memory*)
      ;(format t "Memoria ~%~a~%" *memory*)
      (cond ((equal  edo-meta  estado)
              (extract-solution  nodo)
                (setq  meta-encontrada  T))
            (t 
              (setq  *current-ancestor*  (first  nodo)) 
              (setq  *profundidad*  (nth 5 nodo))
          (setq  sucesores  (expand estado))
          (format t "sucesores ~a~%" sucesores)
          (setq  sucesores  (filter-memories  sucesores metodo))
          (format t "sucesores filtrados~a~%" sucesores)
          (loop for  element  in  sucesores  do
            (insert-to-open  (first element)  (second element)  metodo))))))  ))

  (setq *solution* (recorre *solution*))
  (print *solution*)
  (setq *solution* (aplana *solution*))
  (print *solution*)
  )

(defun A*-Search ()
  ;;Obtenemos la posiciób inicial del arreglo guardado en la variable *start*
  ;;Obtenemos la posiciób final del arreglo guardado en la variable *goal*
  ;;Para finalizar agregamos el metodo que deseamos utilizar
  (Maze2d-Search  (list (aref *start* 0) (aref *start* 1))  
          (list (aref *goal* 0) (aref *goal* 1))   ':A*)
  )
(defun Best-First-Search()
  ;;Obtenemos la posiciób inicial del arreglo guardado en la variable *start*
  ;;Obtenemos la posiciób final del arreglo guardado en la variable *goal*
  ;;Para finalizar agregamos el metodo que deseamos utilizar
  (Maze2d-Search  (list (aref *start* 0) (aref *start* 1))  
          (list (aref *goal* 0) (aref *goal* 1))   ':BFS)
  )
(defun Depth-First-Search()
  ;;Obtenemos la posiciób inicial del arreglo guardado en la variable *start*
  ;;Obtenemos la posiciób final del arreglo guardado en la variable *goal*
  ;;Para finalizar agregamos el metodo que deseamos utilizar
  (Maze2d-Search  (list (aref *start* 0) (aref *start* 1))  
          (list (aref *goal* 0) (aref *goal* 1))   ':depth-first)
  )
(defun Breath-First-Search()
  ;;Obtenemos la posiciób inicial del arreglo guardado en la variable *start*
  ;;Obtenemos la posiciób final del arreglo guardado en la variable *goal*
  ;;Para finalizar agregamos el metodo que deseamos utilizar  
  (Maze2d-Search  (list (aref *start* 0) (aref *start* 1))  
          (list (aref *goal* 0) (aref *goal* 1))   ':breath-first)
  )

(add-algorithm 'A*-Search)
(add-algorithm 'Best-First-Search)
(add-algorithm 'Depth-First-Search)
(add-algorithm 'Breath-First-Search)
(start-maze)