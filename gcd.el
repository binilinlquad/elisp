;;; gcd.el --- Playing with Greatest Common Divisor
;;
;; Copyright © 2015 Chandra
;;
;; Author: Chandra <otama.ariq@gmail.com>
;; URL:
;; Version: 1.0.0
;; Keywords: Mathematic

;; This file is not part of GNU Emacs.

;;; Commentary:
;;; This is collection for finding Greatest Common Divisior
;;; in any algorithm

;;; Code:
(defun gcd-loop(x y)
  "Find greatest common divisor for (X) and (Y)."
  ;; find the smaller number as divisor
  (let (divisor x)
    (if (> x y)
        (setq divisor y))

    (loop
     (when (= divisor 0) (return 0))
     ;; divide bigger number with divisor number. if remainder
     ;; not zero, repeat it with the divide with divisor-1
     (if (and (= 0 (% x divisor)) (= 0 (% y divisor)))
         (return divisor)
       (setq divisor (- divisor 1)))))
  )

;; use simple euclid gcd that i got from wikipedia
;; (not in the loop, etc) with recursive
(defun gcd-euclid-recursive(x y)
  (cond
   ((and (<= x 0) (<= y 0)) 0)
   ((= x y) x)
   ((> x y) (gcd-euclid-recursive (- x y) y))
   ((< x y) (gcd-euclid-recursive y (- y x)))))

;; use simple euclid gcd that i got from wikipedia
;; (not in the loop, etc) with looping
(defun gcd-euclid-loop(x y)
  "Find common division for (X) and (Y) with euclid algorithm and looping."
  (loop
   (cond
    ((and (<= x 0) (<= y 0)) (return 0))
    ((= x y) (return x))
    ((> x y) (setq x (- x y)))
    ((< x y) (setq y (- y x))))
   ))

;; (not in the loop, etc) with looping
(defun gcd-euclid-by-mod(x y)
  "Find common division for (X) and (Y) with euclid algorithm recursively."
  (if (= y 0)
      x
    (gcd-euclid-by-mod y (mod x y))))

(defun gcd (x y)
  "Look common divisor for (X) and (Y)."
  (interactive "nfirst: \nnsecond: ")
  (message "Commond Divisor: %d" (gcd-euclid-by-mod x y)))

(provide 'gcd)
;;; gcd.el ends here
