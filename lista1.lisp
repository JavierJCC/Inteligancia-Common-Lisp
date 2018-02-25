(defun Recombina (lista)
  (let ((valordos '()) (val3 '()) (todo '()) (resultado '()) (resultadof '()))
    (do ((n 0 (+ n 1)))
	((= n (- (length lista) 1)) resultado)
      (setq todo (cons (rest (nth n lista)) todo))
      (setq valordos (rest (nth n lista)))
      (setq val3 (rest (nth (+ n 1) lista)))
      (setq resultado (cons (list valordos val3) resultado))
      )
    (setq todo (cons val3 todo))
    (setq resultadof (list (cons (second resultado) (first (first lista))) (cons (first resultado) (first (third lista))) (cons todo (first (second lista))) ))

    )
  )

(defun RealNoCero (n)
  (and (realp n) (/= n 0))
  )

(defun Analiza (arg)
  (list (atom arg) (numberp arg) (listp arg) (consp arg) (null arg))
  )

