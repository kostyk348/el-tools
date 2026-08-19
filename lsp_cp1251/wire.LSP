(vl-load-com)

;; ==========================================================
;; ?? Wire System v2.1: Чистая схема + Скрытые данные + Таблица
;; ==========================================================

;; ?? Запись данных в XData (скрыто внутри полилинии)
(defun set-wire-xdata (ent num dev1 term1 dev2 term2 / xdata)
  (if (not (tblsearch "APPID" "WIRE_DATA"))
    (entmake (list '(0 . "APPID") (cons 2 "WIRE_DATA")))
  )
  (setq xdata (list (cons 1000 (itoa num))
                    (cons 1000 dev1) (cons 1000 term1)
                    (cons 1000 dev2) (cons 1000 term2)))
  (entmod (append (entget ent) (list (list -3 (cons "WIRE_DATA" xdata)))))
)

;; ?? Чтение скрытых данных
(defun get-wire-xdata (ent / xd vals)
  (setq xd (cdr (assoc -3 (entget ent '("WIRE_DATA")))))
  (if xd (setq vals (mapcar 'cdr (vl-remove-if '(lambda (x) (/= (car x) 1000)) (cdr xd)))))
  vals
)

;; ==========================================
;; ?? DrawWire: Создание провода
;; ==========================================
(defun c:DrawWire ( / *error* doc old-vars pts start-pt end-pt wire-num 
                    dev-start term-start dev-end term-end mid-pt txth pl-ent pl-obj mid-param)
  (defun *error* (msg)
    (if msg (princ (strcat "\n? Ошибка: " msg)))
    (foreach v old-vars (setvar (car v) (cdr v)))
    (princ)
  )
  
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq old-vars (list (cons "CMDECHO" (getvar "CMDECHO"))
                       (cons "OSMODE"  (getvar "OSMODE"))
                       (cons "BLIPMODE"(getvar "BLIPMODE"))))
  (foreach v old-vars (setvar (car v) 0))
  
  ;; Слои с правильными цветами для печати
  (foreach lay '(("WIRE" . 256) ("WIRE_NUM" . 30) ("WIRE_NODES" . 8))
    (if (not (tblsearch "LAYER" (car lay)))
      (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord") '(100 . "AcDbLayerTableRecord")
                     (cons 2 (car lay)) '(70 . 0) (cons 62 (cdr lay)) '(6 . "Continuous")))
    )
  )
  
  ;; Счётчик проводов
  (setq wire-num (getenv "CAD_WIRE_CNT"))
  (if (or (not wire-num) (= wire-num "")) (setq wire-num "1")
      (setq wire-num (itoa (1+ (atoi wire-num)))))
  (setq wire-num (atoi wire-num)
        txth (getvar "TEXTSIZE"))
  
  (princ (strcat "\n?? Провод №" (itoa wire-num) "\n"))
  (setq pts '()
        start-pt (getpoint "\n?? Начальная точка: "))
  (if (not start-pt) (progn (foreach v old-vars (setvar (car v) (cdr v))) (vla-EndUndoMark doc) (princ "\n? Отменено.") (exit)))
  (setq pts (list start-pt))
  (princ "?? Промежуточные точки (Enter для завершения): ")
  (while (setq end-pt (getpoint (car pts) "\nСледующая точка: "))
    (setq pts (append pts (list end-pt)))
  )
  (if (< (length pts) 2) (progn (princ "\n? Нужно минимум 2 точки!") (foreach v old-vars (setvar (car v) (cdr v))) (vla-EndUndoMark doc) (exit)))
  
  ;; Данные подключений
  (setq dev-start (getstring "\n?? Откуда (устройство): " t))
  (if (= dev-start "") (setq dev-start "Х"))
  (setq term-start (getstring "?? Клемма от: " t))
  (if (= term-start "") (setq term-start "1"))
  (setq dev-end (getstring "\n?? Куда (устройство): " t))
  (if (= dev-end "") (setq dev-end "Х"))
  (setq term-end (getstring "?? Клемма до: " t))
  (if (= term-end "") (setq term-end "1"))
  
  ;; Рисуем полилинию
  (setq pl-ent (entmakex (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                                       '(100 . "AcDbPolyline") (cons 90 (length pts)) '(70 . 0))
                                 (mapcar '(lambda (p) (cons 10 p)) pts))))
  (vlax-put-property (vlax-ename->vla-object pl-ent) 'Layer "WIRE")
  
  ;; ?? Записываем данные скрыто
  (set-wire-xdata pl-ent wire-num dev-start term-start dev-end term-end)
  
  ;; ?? Один кружок с номером по центру трассы
  (setq pl-obj (vlax-ename->vla-object pl-ent)
        mid-param (* 0.5 (vlax-curve-getEndParam pl-obj))
        mid-pt (vlax-curve-getPointAtParam pl-obj mid-param))
  (place-wire-tag mid-pt wire-num txth)
  
  (setenv "CAD_WIRE_CNT" (itoa (1+ wire-num)))
  (princ (strcat "\n? Провод #" (itoa wire-num) " создан. Данные скрыты в объекте."))
  (vla-EndUndoMark doc)
  (princ)
)

;; Вспомогательная: кружок с номером
(defun place-wire-tag (pt num h / r txt-h)
  (setq r (* h 0.7)        ; Радиус кружка (компактный, но видимый)
        txt-h (* r 0.65))  ; Текст внутри
  (entmake (list '(0 . "CIRCLE") (cons 10 pt) (cons 40 r) '(8 . "WIRE_NUM")))
  (entmake (list '(0 . "MTEXT") (cons 10 pt) (cons 40 txt-h) (cons 1 (itoa num))
                 '(50 . 0.0) '(71 . 5) '(8 . "WIRE_NUM")))
)

;; ==========================================
;; ?? WireTable: Создание таблицы из скрытых данных
;; ==========================================
(defun c:WireTable ( / doc ss i ent data-list row col tbl txth len)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
        txth (getvar "TEXTSIZE"))
  
  (setq ss (ssget "_X" '((0 . "LWPOLYLINE") (-3 ("WIRE_DATA")))))
  (if (not ss) (progn (princ "\n? Проводов с данными не найдено!") (exit)))
  
  (setq data-list '() i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i)
          data (get-wire-xdata ent))
    (if data
      (setq len (/ (vla-get-Length (vlax-ename->vla-object ent)) 1000.0)
            data-list (cons (list (atoi (nth 0 data))
                                  (nth 1 data) (nth 2 data)
                                  (nth 3 data) (nth 4 data)
                                  (rtos len 2 2)) data-list))
    )
    (setq i (1+ i))
  )
  
  (setq data-list (vl-sort data-list '(lambda (a b) (< (car a) (car b)))))
  
  (setq ins (getpoint "\n?? Точка вставки таблицы: "))
  (if (not ins) (exit))
  
  (setq tbl (vlax-invoke (vla-get-ModelSpace doc) 
                         'AddTable ins (1+ (length data-list)) 6 (* txth 2.5) (* txth 12)))
  (vla-SetCellValue tbl 0 0 "№ Провода" txth)
  (vla-SetCellValue tbl 0 1 "Откуда" txth)
  (vla-SetCellValue tbl 0 2 "Клемма" txth)
  (vla-SetCellValue tbl 0 3 "Куда" txth)
  (vla-SetCellValue tbl 0 4 "Клемма" txth)
  (vla-SetCellValue tbl 0 5 "Длина (м)" txth)
  
  (setq row 0)
  (foreach rec data-list
    (setq row (1+ row) col 0)
    (foreach val rec
      (vla-SetCellValue tbl row col (vl-princ-to-string val) txth)
      (vla-SetAlignment tbl row col 1)
      (setq col (1+ col))
    )
  )
  (princ (strcat "\n? Таблица создана! Строк: " (itoa (length data-list))))
  (princ)
)

;; ==========================================
;; ?? WireNodes: Отметить промежуточные точки (по требованию)
;; ==========================================
(defun c:WireNodes ( / ss i ent pts pt)
  (princ "\n?? Выберите провода для маркировки узлов: ")
  (if (setq ss (ssget '((0 . "LWPOLYLINE"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i))
        (if (eq (vlax-get-property (vlax-ename->vla-object ent) 'Layer) "WIRE")
          (progn
            (setq pts (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) (entget ent))))
            (foreach pt pts
              (entmake (list '(0 . "CIRCLE") (cons 10 pt) (cons 40 (* (getvar "TEXTSIZE") 0.15))
                             '(8 . "WIRE_NODES") '(62 . 8))) ; серый цвет, почти не виден на печати
            )
          )
        )
        (setq i (1+ i))
      )
      (princ "\n? Узлы отмечены.")
    )
    (princ "\n? Ничего не выбрано.")
  )
  (princ)
)

(princ "\n?? Загружено: DrawWire / WireTable / WireNodes")
(princ)