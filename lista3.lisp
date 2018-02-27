(defun factorial(num)
  (cond ((= num 0) 1)
	(T (* num (factorial(- num 1))))
    )
)

(defun recita(lista)
        (cond ((null lista) NIL)
	   (T (print (first lista)))
	   (recita(rest lista))
	)
	)

(defun elemento-en-aux(lista  elemenuno elemendos result)
  (cond ((null lista) result)
	((equal elemenuno (first lista)) (elemento-en-aux (rest lista) elemenuno elemendos (cons elemendos result)) )
	(T (elemento-en-aux (rest lista) elemenuno elemendos (cons (first lista)result)))
	)

)

(defun elemento-en(lista elemenuno elemendos)
  (elemento-en-aux lista  elemenuno elemendos nil)
)

(defun cambia(lista elemenuno elemendos)
  (cond ((null lista) NIL)
	((equal elemenuno (first lista)) (cons elemenuno (rest lista))
	(T (cambia (rest lista) elemenuno elemendos))    
  )
	))

(defun invierte(lista)
  (invierte-aux lista nil)
  )

(defun invierte-aux(lista result)
  (cond ((null lista) result)
	(T (invierte-aux(rest lista)(cons (first lista) result)))
	)
)

(defun mieql(lista1 lista2)
  (or (eql lista1 lista2)
      (and (consp lista1)
	   (consp lista2)
	   (mieql (car lista1) (car lista2))
	   (mieql (cdr lista1) (cdr lista2))))

  )


(defun aplana(lista)
  (cond ((null lista) lista)
	((atom (first lista)) (cons (first lista) (aplana (rest lista))))
	(T (append (aplana (first lista)) (aplana (rest lista)))))

  )

(defun verifica (lista result)
  (cond ((consp lista) (cons (first (first lista)) result)))
  )

(defun elimina (lista real)
  (cond ((null lista) nil)
	((and (numberp (first lista)) (< real (first lista))(cons (first lista)(elimina (rest lista) real))))
        (t (elimina (rest lista) real))
	)
  )

(defun PegaYcambia (lista1 lista2 elem1 elem2)
  (cond ((and (null lista1) (null lista2)) nil) 
	((and (equal elem1 (first lista1)) (equal elem1 (first lista2))) (cons elem2 (PegaYcambia (rest lista1) (rest lista2) elem1 elem2)))
	( T (append (cons (first lista))  (PegaYcambia (rest lista1) (rest lista2) elem1 elem2)))
	)
  )

(defun Qsor (lista )
  (cond ((null lista) nil)

	)
    )  
