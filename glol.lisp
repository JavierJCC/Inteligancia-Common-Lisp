;;;=======================================================================
;;;  GLOL.lisp
;;;      Resuelve el problema GLOL con búsqueda ciega, a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados: 
;;;         En cada orilla, número de Lovo(M), Lechuga(C), Oveja(B) y Granjero(G).
;;;
;;;             L L O G   L L O G 
;;;           [(1 1 1 1) (0 0 0 0)]   Estado inicial del problema
;;;
;;;		Entradas 
;;;(blind-search2 '((1 1 1 1) (0 0 0 0)) '((0 0 0 0) (1 1 1 1)) :depth-first)
;;;
;;;(blind-search2 '((1 1 1 1) (0 0 0 0)) '((0 0 0 0) (1 1 1 1)) :breath-first)
;;;
;;;      Chavez Chavez Javier
;;;=======================================================================
(defparameter  *open* '())       ;; Frontera de busqueda...                                              
(defparameter  *memory* '())   ;; Memoria de intentos previos

(defparameter  *ops*  '( (:Pasa-Lovo       		(1 0 0))     ;; Operadores para el problema de Lovo, Lechuga, Oveja y Granjero
						 (:Pasa-Lechuga         (0 1 0))
						 (:Pasa-Oveja       	(0 0 1))
						 (:Pasa-Granjero   		(0 0 0)) ) )

(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

;;=======================================================================
;;  CREATE-NODE [estado  op]  
;;      estado - Un estado del problema a resolver (sistema)...
;;          op - El operador cuya aplicación generó el [estado]...
;;=======================================================================
(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro "
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  *id*  estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;=======================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;=======================================================================
(defun insert-to-open (estado  op  metodo) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
     (let ((nodo  (create-node  estado  op)))
         (cond ((eql  metodo :depth-first)
	                  (push  nodo  *open*))
	           ((eql  metodo :breath-first)
		          (setq *open*  (append  *open*  (list nodo))))
	   	   (T  Nil)))  )


(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *Open*))

;;=======================================================================
;;  BARGE-SHORE [estado]
;;        Regresa la orilla del rio en la que se encuentra la barca en  [estado]
;;           0 - Orilla origen (primer sublista del estado)
;;           1 - Orilla destino (segunda sublista del estado)
;;=======================================================================
(defun  barge-shore (estado)
"Regresa la orilla del río en la que se encuentra la barca en el estado recibido como parámetro:  0 - origen  1 - destino"
     (if  (= 1 (fourth (first  estado)))  0  1))	;;El granjero se vuelve ahora el indicador de la orilla del rio

;;=======================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos 
;; El granjero nos servira ahora para saber en que orilla estamos 
;;=======================================================================
(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado...
     el estado tiene estructura:  [(<Lobo0><Lechu0><Oveja0><Gran0>) (<Lobo1><Lechu1><Oveja1><Gran1>)],
     el operador tiene estructura : [<etiqueta-humana> <lista operador con (<num Lobo1><num Lechu1><num Oveja1)>]"  
  (let*  ((orilla  (barge-shore  estado))                         
	    (lovo  		(first  (nth  orilla  estado)))   ;;Obtenemos los valores de cada uno de nuestros actoresa excepcion del valor del granjero
	    (lechuga    (second  (nth  orilla  estado)))
  		(oveja 		(third (nth orilla estado))))
    (and  (>=  lovo  (first (second op)))              
               (>=  lechuga   (second (second op)))
               (>= Oveja 	(third (second op)))
               ))	)  ;;Comparamos si es posible realizar la acción 


;;=======================================================================
;;  VALID-STATE [estado]
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;                            Es decir, si en c/orilla hay igual o mayor numero de misioneros que de canibales
;;=======================================================================
(defun  valid-state? (estado)
"Predicado. Valida  un estado según las restricciones generales del problema...
       el estado tiene estructura:  [(<lov0><lec0><ove0>) (<m1><c1><b1>)]"

    (let* ((ori  (flip (barge-shore  estado))) 	;;Verificamos el estado en el cual nno estamos
    	(lov0  (first (nth ori estado)))        ;;el estado tiene estructura ((<lov0><lec0><ove0><gra0>) (<lov0><lec0><ove0><gra0>)) ...
	    (lec0  (second (nth ori estado)))
	    (ove0  (third (nth ori estado))))
      (and  (or  (> lec0 ove0) (zerop lec0))  ;; Debe existir un mayor numero de lovos que ovejas lov=0 ove=0
    	    (or  (> ove0 lov0) (zerop ove0)))	;;Debe existir un mayor numedo de ovejas que lechugas lec0=0 ove0=0
      )  )

;;=======================================================================
;;  APPLY-OPERATOR [op, estado]
;;        Solución simbólica del problema
;;=======================================================================

(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  ((orilla1  (first  estado))
	       (orilla2  (second  estado))
	       (lov0   (first orilla1))
   	       (lec0   (second orilla1))
	       (ove0   (third  orilla1))
	       (gra0 	(fourth orilla1))
	       ;;Guardamos los elementos de cada uno de los datos
	       (lov1   (first  orilla2))
	       (lec1   (second  orilla2))
	       (ove1   (third   orilla2))
	       (gra1 	(fourth orilla2))
	       (orilla-barca  (barge-shore estado)) 
	       (operador (first op)))      ;; este operador es la etiqueta humana del operador...	
	 (case operador 
	    (:Pasa-Lovo ;; El operador a aplicar es (1 0 0)
                (if (= orilla-barca 0)  ;;siempre  restar elementos de la orilla con la barca y sumarlos en la otra orilla...
	                    (list  (list  (- lov0 1) lec0 ove0 (flip gra0))   (list  (+ lov1 1) lec1 ove1 (flip gra1)))
                        (list  (list  (+ lov0 1) lec0 ove0 (flip gra0))   (list (- lov1 1) lec1 ove1 (flip gra1)))
                        ))
	    (:Pasa-Lechuga ;; El operador a aplicar es (0 1 0) 
                (if (= orilla-barca 0)  
	                    (list  (list  lov0 (- lec0 1) ove0 (flip gra0))   (list  lov1 (+ lec1 1) ove1 (flip gra1)))
                        (list  (list  lov0 (+ lec0 1) ove0 (flip gra0))   (list lov1  (- lec1 1) ove1 (flip gra1)))))
	    (:Pasa-Oveja ;;	El operador a aplicar es (0 0 1)    
	            (if (= orilla-barca 0)  
                        (list  (list  lov0 lec0 (- ove0 1) (flip gra0))   (list  lov1 lec1 (+ ove1 1) (flip gra1)))
                        (list  (list  lov0 lec0 (+ ove0 1) (flip gra0))   (list lov1  lec1 (- ove1 1) (flip gra1)))))
	    (:Pasa-Granjero  ;; El operador a aplicar es (0 0 0)
                (if (= orilla-barca 0)  
                    (list  (list  lov0 lec0 ove0 (flip gra0))   (list  lov1 lec1 ove1 (flip gra1)))
                    (list  (list  lov0 lec0 ove0 (flip gra0))   (list  lov1 lec1 ove1 (flip gra1)))
                    ))
	    (T "error"))))

(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let* ((descendientes  nil)
	     (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes) 
           	(print (second op))
	         (setq  nuevo-estado  (apply-operator  op estado))
	         (print nuevo-estado)
		 (when (and (valid-operator?  op  estado) 
			    (valid-state?  nuevo-estado))
		 		(print (valid-operator?  op  estado))
		 		(print (valid-state? nuevo-estado))
	                (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))) )

;;=======================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;=======================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [(<lov0><lec0><ove0><gra0>) (<lov1><lec1><ove1><gra1>)]
     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"  
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )


(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
		       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

;;=======================================================================
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;=======================================================================
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current  (locate-node  (first  nodo)  *memory*)))
	     (loop  while  (not (null  current))  do                        ;; Memoria de intentos previos...
		 (push  current  *solucion*)     ;; agregar a la solución el nodo actual
		 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor...
	     *solucion*))


(defun  display-solution (lista-nodos)
"Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "Solucion con ~A  pasos:~%~%" (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
	       ;;else
		   (format t "\(~2A\)  aplicando ~20A se llega a ~A~%"  i (fourth  nodo)  (second  nodo)))))  )  ;; imprimir el número de paso, operador y estado...

;;=======================================================================
;;  RESET-ALL  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solucion del problema...
;;=======================================================================
(defun reset-all () 
"Reinicia todas las variables globales para iniciar una nueva búsqueda..."
     (setq  *open*  nil)
     (setq  *memory*  nil)
     (setq  *id*  0)
     (setq  *current-ancestor*  nil)
     (setq  *solucion*  nil))


(defun  blind-search2 (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                              :breath-first - búsqueda en anchura"

  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil)
	  (tiempo1	(get-internal-run-time))
	  (tiempo2 0)
	  (tiempoTotal 0)
	  )

      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada
			        (null *open*))  do
	   (setq  nodo    (get-from-open)   
		     estado  (second  nodo)
		     operador  (third  nodo))
	   		(push  nodo  *memory*)
	   (cond    ((equal  edo-meta  estado)  
		                (format  t  "Exito. Meta encontrada en ~A  intentos~%" (first  nodo))
		                (display-solution  (extract-solution  nodo))
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expand estado))
			     (setq  sucesores  (filter-memories  sucesores))
			      (loop for  element  in  sucesores  do
				    (insert-to-open  (first element)  (second element)  metodo)))))

      (setq tiempo2 (get-internal-run-time))
      (print tiempo2)
      (print tiempo1)
      (setq tiempoTotal (/ (- tiempo2 tiempo1) 1000))
      (print "El tiempo total es: ")
      (print tiempoTotal)
      )  )
			     
     
;;=======================================================================
;;=======================================================================
