(vl-load-com)

(defun WT_Arrow (p ang / s p1 p2)
  (setq s 1.5)
  (setq p1 (polar p (+ ang (* pi 0.85)) s))
  (setq p2 (polar p (- ang (* pi 0.85)) s))
  (command "_.LINE" p p1 "")
  (command "_.LINE" p p2 "")
)

(defun WT_Draw (p dir txt len h / ang end rot)

  (setq ang (angle p dir))
  (setq end (polar p ang len))
  (setq rot (* 180.0 (/ ang pi)))

  ;; линия
  (command "_.LINE" p end "")

  ;; стрелка
  (WT_Arrow end ang)

  ;; текст
  (command
    "_.TEXT"
    "_J"
    "_MC"
    (polar end ang (+ h 2.5))
    h
    rot
    txt
  )
)

(defun c:WT (/ num p1 p2 d1 d2 len txtH)

  (setq txtH (getvar "TEXTSIZE"))
  (setq len (* txtH 3.2))  ;; размер выноски от высоты текста

  (setq num (getstring "\nВведите номер провода: "))

  (setq p1 (getpoint "\nПервая точка: "))
  (setq d1 (getpoint p1 "\nУкажите направление первой выноски: "))

  (setq p2 (getpoint "\nВторая точка: "))
  (setq d2 (getpoint p2 "\nУкажите направление второй выноски: "))

  (WT_Draw p1 d1 num len txtH)
  (WT_Draw p2 d2 num len txtH)

  (princ)
)