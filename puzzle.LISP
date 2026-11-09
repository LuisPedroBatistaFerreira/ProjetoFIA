(defun estado-teste ()
  (list (list(list 1 1 1 0)
        (list nil 1 1 0)
        (list nil 0 0 0))
        (list (list 'L 1) (list 'P 1) (list 'V 1))))

(defun tabuleiro-teste ()
  (car (estado-teste)))

(defun linha (pos tabuleiro)
   (nth (- pos 1) tabuleiro))

(defun coluna (pos tabuleiro)
  (mapcar #'(lambda (linha) (nth (- pos 1) linha)) tabuleiro))

(defun celula (x y tabuleiro)
  (nth (- x 1) (nth (- y 1) tabuleiro)))

(defun estado-tabuleiro (estado)
  (car estado))

(defun estado-pecas (estado)
  (car (cdr estado)))

(defun quantidade-peca (peca lista)
  (cond 
   ((null lista) 0)
   ((equal (car (car lista)) peca) (nth 1 (car lista)))
   (t (quantidade-peca peca (cdr lista)))
   )
)

(defun celula-validap (x y tabuleiro)
  (cond 
   ((or (< (length (car tabuleiro)) x) (< (length tabuleiro) y)) nil)
   ((null (celula x y tabuleiro)) nil)
   (t T)
   )
)

(defun celula-livrep (x y tabuleiro)
  (cond
   ((not (celula-validap x y tabuleiro)) nil)
   ((= (celula x y tabuleiro) 1) nil)
   (t T)
   )
)
