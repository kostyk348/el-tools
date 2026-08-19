(defun c:WireSegAddr ( / ss ent obj divpts i pt pt-ins addr txtheight offset)
  (vl-load-com)
  
  (princ "\n=== ПОДПИСИ ПОД ТОЧКАМИ РАЗБИВКИ ===\n")
  (princ "Выберите один провод (отрезок или полилинию):\n")
  
  (if (not (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE")))))
    (progn (princ "\nНичего не выбрано!") (exit))
  )
  
  (if (> (sslength ss) 1)
    (progn (princ "\nВыберите ТОЛЬКО ОДИН провод!") (exit))
  )
  
  (setq ent (ssname ss 0)
        obj (vlax-ename->vla-object ent)
        txtheight (getvar "TEXTSIZE")
        ;; Отступ: 0.15 от высоты текста (компактный, но не наезжает)
        offset (* txtheight 0.15)) 
  
  (princ "\nУкажите точки разбивки. После последнего — Enter.\n")
  
  (setq divpts '())
  (while (setq pt (getpoint "\nТочка разбивки: "))
    (setq divpts (append divpts 
                         (list (vlax-curve-getClosestPointTo obj (trans pt 1 0) nil))))
  )
  
  ;; ? УБРАНЫ начало и конец провода. Текст ТОЛЬКО под выбранными точками.
  
  ;; Сортировка слева направо (стабильный порядок нумерации)
  (setq divpts (vl-sort divpts 
                        '(lambda (a b)
                           (if (equal (car a) (car b) 0.001)
                             (< (cadr a) (cadr b))
                             (< (car a) (car b))))))
  
  ;; Убираем случайно сдвоенные клики
  (setq divpts (el:unique-fuzz divpts 0.1))
  
  (if (< (length divpts) 1)
    (progn (princ "\nТочки не указаны!") (exit))
  )
  
  (setq i 0)
  (foreach pt divpts
    ;; ? Точка вставки: строго тот же X, Y смещён вниз на offset
    (setq pt-ins (list (car pt) (- (cadr pt) offset) (if (caddr pt) (caddr pt) 0.0)))
    
    (setq addr (getstring (strcat "\nАдрес для точки " (itoa (1+ i)) ": ") t))
    
    (if (and addr (/= addr ""))
      (entmakex (list '(0 . "MTEXT")
                      '(100 . "AcDbEntity")
                      '(100 . "AcDbMText")
                      (cons 10 pt-ins)
                      (cons 40 (/ txtheight 5.0))   ; Высота текста (мелкая)
                      (cons 1 addr)
                      (cons 50 0.0)                 ; ? Угол 0° (переворот физически невозможен)
                      '(71 . 2)                     ; ? Top Center (текст "свисает" ровно под точкой)
                      ))
    )
    (setq i (1+ i))
  )
  
  (princ (strcat "\nГотово! Расставлено " (itoa i) " подписей ровно под точками."))
  (princ)
)

;; Вспомогательная функция удаления дубликатов с допуском
(defun el:unique-fuzz (lst fuzz / res)
  (foreach pt lst
    (if (not (vl-some '(lambda (x) (< (distance pt x) fuzz)) res))
      (setq res (cons pt res))
    )
  )
  (reverse res)
)