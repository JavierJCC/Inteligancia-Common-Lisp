;;;=======================================================================
;;;  Mancala.lisp
;;;      Agente artificial para el video juego de mancala
;;;
;;;      Chávez Chávez Javier
;;;=======================================================================

(defparameter *TableroH* nil) 	;;El tablero del humano va ser modelado por una lista
(defparameter *TableroAI* nil)	;;El tablero del agente va ser modelado por una lista
(defparameter *PuntuacionAI* 0) ;;La puntuación va a ser modelada por un entero
(defparameter *PuntuacionH* 0)	;;La puntuación va ser modelada por un entero
(defparameter *MaxProfundidad* 5)	;;Profundidad maxima a realizar en el arbol
(defparameter *ALFA* 9999999)	;;Numero minimo usado en negamaxalfabeta
(defparameter *BETA* -9999999)		;;Numero maximo usado en negamaxalfabeta
(defparameter *EXTRA* nil)

(defun tablero ()
	(format t "=================== MANCALA ============================~%")
	(format t "AI ~a ~a~% ~a HM ~a~%" *PuntuacionAI* *TableroAI* *TableroH* *PuntuacionH* )
	(format t "========================================================~%")
	)
(defun elementos-cuenca (cuenca)
	(+ (first cuenca) (second cuenca) (third cuenca))
	)

(defun revisacuenca(cuenca)
	"Revisamos si tanto la cuenca esta vacia"
	(and (= (nth 0 cuenca) 0) (= (nth 1 cuenca) 0) (= (nth 2 cuenca) 0))
	)

(defun leer-forma-reparto (cuenca)
	"Leemos en la forma que vamos a repartir la cuenca"
	(let ((salida nil) (bandera nil) (numelementos (elementos-cuenca cuenca)) (auxCuenca '(0 0 0))) 
		(format t "Numero de eleentos: ~a ~%" numelementos)
		(loop until bandera do
			(loop for i from 1 to numelementos do
				(push (read) salida)
				(format t "La salida es: ~a y la nueva cuenca es: ~a~%" salida auxCuenca)
					(cond ((= (first salida) 1) (setq auxCuenca (list (1+ (first auxCuenca)) (second auxCuenca) (third auxCuenca))))
						  ((= (first salida) 5) (setq auxCuenca (list (first auxCuenca) (1+ (second auxCuenca)) (third auxCuenca))))
						  ((= (first salida) 10)(setq auxCuenca (list (first auxCuenca) (second auxCuenca) (1+ (third auxCuenca)))))
						  )
					)
					(cond ((equalp cuenca auxCuenca) (setq bandera T))
						  (T (format t "No es valida la cuenca~%") (setq salida nil auxCuenca '(0 0 0)))
						  )
			)
		(reverse salida)
		)
	)
(defun Aplicar-jugada (reparto cuenca jugador)
	"Realizamos la jugada dependiendo del jugador que la realice"
	(format t "~a ~a ~a ~%"reparto cuenca jugador)
	(let ((aux 0) (auxCuenca nil) (salto 0) (casilla cuenca))
		;;Dejamos en blanco la casilla que seleccionamos ya sea para la inteligencia o para el humano
		(if (eql jugador :HM) (setf (nth (1- cuenca) *TableroH*) '(0 0 0)) (setf (nth (1- cuenca) *TableroAI*) '(0 0 0)))
			(loop for i from 0 to (1- (length reparto)) do
					(setq aux (nth i reparto)) ;;Obtenemos el valor para repartir de la lista 1 1 5 10 10
					(format t "Salto es: ~a casilla es: ~a jugador es: ~a y reperto es: ~a ~%" salto casilla jugador aux)
					(cond ((and (eql jugador :HM) (= salto 0) (= casilla 6)) (setq salto 1 *PuntuacionH* (+ *PuntuacionH* aux) casilla 5))
						  ((and (eql jugador :HM) (= salto 1) (= casilla -1)) (setq salto 0 casilla 0))
						  ((and (eql jugador :AI) (= salto 1) (= casilla -1)) (setq salto 0 *PuntuacionAI* (+ *PuntuacionAI* aux) casilla 0))
						  ((and (eql jugador :AI) (= salto 0) (= casilla 6)) (setq salto 1 casilla 5))
						  (T (cond ((= salto 0)
						  				(setq auxCuenca (nth casilla *TableroH*)) ;;Obtenemos la cuenca siguiente para agrgar el valor a repatir
						  			(case aux 
						  				(1 (setf (nth casilla *TableroH*) (list (1+ (first auxCuenca)) (second auxCuenca) (third auxCuenca)) ))
						  				(5 (setf (nth casilla *TableroH*) (list (first auxCuenca) (1+ (second auxCuenca)) (third auxCuenca)) ))
						  				(10(setf (nth casilla *TableroH*) (list (first auxCuenca) (second auxCuenca) (1+ (third auxCuenca))) ))
						  				)
						  			(incf casilla))
						  		    ((= salto 1) 
						  		    	(setq auxCuenca (nth casilla *TableroAI*))
						  		    	(case aux 
						  		    	(1 (setf (nth casilla *TableroAI*) (list (1+ (first auxCuenca)) (second auxCuenca) (third auxCuenca)) ))
						  		    	(5 (setf (nth casilla *TableroAI*) (list (first auxCuenca) (1+ (second auxCuenca)) (third auxCuenca)) ))
						  		    	(10(setf (nth casilla *TableroAI*) (list (first auxCuenca) (second auxCuenca) (1+ (third auxCuenca))) ))
						  		    	)
						  		    (decf casilla))
						  		    ))
							)
				)
			(format t "casilla es igual ~a ~%" casilla)
		)
	)

(defun turnoH ()
	(let ((cuenca 0) (mov 0) (reparto nil))
		(format t "Ingresa la cuenca que desea mover de 1 a 6: ~%")
		(setq mov (read))
		(setq cuenca (nth (1- mov) *TableroH*))
		(format t "Ingrese la forma en que se repartiran por medio de su valor ~%~a de 1, ~a de 5 y ~a de 10~%" (first cuenca) (second cuenca) (third cuenca))
		(setq reparto (leer-forma-reparto cuenca))
		(Aplicar-jugada reparto mov :HM)
		)
	)
(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun game-over-virtual (TableroH TableroAI)
	"Verifica si cualquiera de ambos lados es estado meta"
	(or (and (revisacuenca (nth 0 TableroH))
			 (revisacuenca (nth 1 TableroH))
			 (revisacuenca (nth 2 TableroH))
			 (revisacuenca (nth 3 TableroH))
			 (revisacuenca (nth 4 TableroH))
			 (revisacuenca (nth 5 TableroH)))
		(and (revisacuenca (nth 0 TableroAI))
			 (revisacuenca (nth 1 TableroAI))
			 (revisacuenca (nth 2 TableroAI))
			 (revisacuenca (nth 3 TableroAI))
			 (revisacuenca (nth 4 TableroAI))
			 (revisacuenca (nth 5 TableroAI))
			 ))
	)
(defun evaluacion(estado jugador)
	(let ((TableroAI (first estado)) (TableroH (second estado)) (PuntuacionAI (third estado)) (PuntuacionH (fourth estado)) (acumula 0))
		(loop for i from 0 to 5 do
			(setq acumula (+ acumula (* 1 (nth i TableroAI)) (* 5 (nth i TableroAI)) (* 10 (nth i TableroAI))))
			(setq acumula (+ acumula (* 1 (nth i TableroH)) (* 5 (nth i TableroH)) (* 10 (nth i TableroH))))
			)
		(cond((and (revisacuenca (nth 0 TableroAI)) (revisacuenca (nth 1 TableroAI)) (revisacuenca (nth 2 TableroAI)) (revisacuenca (nth 3 TableroAI))
				   (revisacuenca (nth 4 TableroAI)) (revisacuenca (nth 5 TableroAI))) (setq PuntuacionAI (+ PuntuacionAI acumula)))
			 ((and (revisacuenca (nth 0 TableroH)) (revisacuenca (nth 1 TableroH)) (revisacuenca (nth 2 TableroH)) (revisacuenca (nth 3 TableroH))
			 	   (revisacuenca (nth 4 TableroH)) (revisacuenca (nth 5 TableroH))) (setq PuntuacionH (+ PuntuacionH acumula)))
		)
		(if (= jugador 0) (- PuntuacionAI PuntuacionH) (- PuntuacionH PuntuacionAI))
		)
	)
(defun mejorreparto(cuenca estado jugador)
	"Función que analiza la mejor reparto de las fichas"

	)

(defun	Nuevotablero (estado cuenca jugador)
	"Funcion que genera un numev tablero"
	(format t "~a ~a ~a ~%"reparto cuenca jugador)
	(let ((TableroAI (first estado)) (TableroH (second estado)) (PuntuacionAI (third estado)) (PuntuacionH (fourth estado))
		  (nivel 0) (salto 0) (jugada (mejorreparto cuenca estado jugador)))


		)

)
;;===================================================
;;	Implementación del algoritmo NegaMAX
;;	estado <TableroAI TableroH PuntuacionAI PuntuacionH>
;;
;;===================================================
(defun negamaxAB (estado profundidad maxprofundidad alfa beta jugador)
	"Algoritmo para podar el arbol y regresamos la casilla y el valor"
	(cond ((or (game-over-virtual (first estado) (second estado)) (= profundidad maxprofundidad)) (list (evaluacion estado jugador)))
		  (T (let ((Mejmovimiento nil) (Mejorvalor alfa) (valor 0) (Newestado  nil))
		  		(loop for i from 0 to 5 do 
		  			(setq *EXTRA* nil)
		  			(setq Newestado (Nuevotablero estado estado jugador))
		  			(setq valor (negamaxAB Newestado (1+ profundidad) maxprofundidad alfa beta (flip jugador)))

		  			

		  			(when (> (second valor) Mejorvalor)
		  				(setq Mejorvalor (second valor) Mejmovimiento i)
		  				(if(>= Mejorvalor beta) (return-from negamaxAB (list (Mejmovimiento Mejorvalor))))
		  				)
		  		)
		  	(list Mejmovimiento Mejorvalor)
		  	)
		  )
		)
	)

(defun turnoAI()
	"Realiza el movimiento del agente artificial"
	(let ((jugada 0) (casilla 0) (estado (list *TableroAI* *TableroH* *PuntuacionAI* *PuntuacionH*)))
		(format t "Estado es: ~a ~%" estado)
		(setq casilla (negamaxAB estado 0 *MaxProfundidad* *ALFA* *BETA* 0))


		)
	)


(defun game-over()
	"Verifica si cualquiera de ambos lados es estado meta"
	(or (and (revisacuenca (nth 0 *TableroH*))
			 (revisacuenca (nth 1 *TableroH*))
			 (revisacuenca (nth 2 *TableroH*))
			 (revisacuenca (nth 3 *TableroH*))
			 (revisacuenca (nth 4 *TableroH*))
			 (revisacuenca (nth 5 *TableroH*)))
		(and (revisacuenca (nth 0 *TableroAI*))
			 (revisacuenca (nth 1 *TableroAI*))
			 (revisacuenca (nth 2 *TableroAI*))
			 (revisacuenca (nth 3 *TableroAI*))
			 (revisacuenca (nth 4 *TableroAI*))
			 (revisacuenca (nth 5 *TableroAI*))
			 ))

	)

(defun resetall ()
	(setq *TableroH* '((1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1)))
	(setq *TableroAI* '((1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1) (1 1 1)))
	(setq *PuntuacionH* 0)
	(setq *PuntuacionAI* 0)
	(setq *Maxprofundidad* 5)
	(setq *Minimo* -9999999)
	(setq *Maximo* 9999999)
	)

(defun victoria(jugador)
	"Funcion que imprime el mensaje ganador dependiendo del ganador"
	(cond ((eql jugador :HM) 
			(format t "Felicidades haz ganado le haz ganado a un agente artificial"))
		  (T (format t "Haz perdido el agente artificial supero tus capacidades"))
		)
	)


(defun mancala ()
	(let ((bandera nil))
		(resetall)
		(tablero)
		(loop until bandera do
			(turnoH) ;;Empieza con el turno del humano
			(format t "~a ~%"(game-over))
			(tablero)
			(if (game-over) (setq bandera T) (turnoAI) )	;;Revisamos que el humano no haya ganado si no turmo del agente
			(tablero)
			(if (game-over) (setq bandera T))	;;Revisamos que el agente no haya ganado 
			)
		)
)