;; el-table.lsp — генератор таблицы соединений из топологии
;; Зависит от: el-core.lsp
;; Команда: EL-TABLE

(if (not *el:tolerance*) (load "el-core.lsp"))

;; ============================================================
;; Генерация таблицы соединений
;; ============================================================

(defun C:EL-TABLE (/ lines graph chains i terminals t1 t2 texts1 texts2
                    table-x table-y linespacing row)
  (setq lines (el:all-lines))
  (if (< (length lines) 2) (progn (princ "\n! Мало LINE") (exit)))

  (setq graph (el:build-graph lines)
        chains (el:all-chains graph))

  ;; Позиция таблицы — клик
  (setq ins (getpoint "\n> Кликни точку вставки таблицы: "))
  (if (null ins) (exit))

  (setq table-x (car ins) table-y (cadr ins) linespacing 3.0 row 0)

  (princ "\n=== EL-TABLE: Генерация таблицы соединений ===\n")
  (princ "Откуда -> Куда (текст рядом с концами)\n")

  (setq i 0)
  (foreach ch chains
    (setq i (1+ i))
    (setq terminators (el:chain-terminals ch))

    (cond
      ;; цепь с 2 концами — норма
      ((>= (length terminators) 2)
        (setq t1 (car terminators) t2 (cadr terminators)
              texts1 (el:text-near t1 *el:text-radius*)
              texts2 (el:text-near t2 *el:text-radius*))
        (setq froms "" tos "")
        (foreach tp texts1 (setq froms (strcat froms " " (car tp))))
        (foreach tp texts2 (setq tos (strcat tos " " (car tp))))
        (princ (strcat "\n  " froms " > " tos))
      )
      ;; цепь с 1 концом — петля/кольцо
      ((= (length terminators) 1)
        (setq t1 (car terminators)
              texts1 (el:text-near t1 *el:text-radius*)
              froms "")
        (foreach tp texts1 (setq froms (strcat froms " " (car tp))))
        (princ (strcat "\n  [петля] " froms))
      )
      ;; цепь без концов — замкнутое кольцо
      (T
        (princ (strcat "\n  [кольцо #" (itoa i) "]")))
    )
  )

  ;; Рисуем таблицу MTEXT
  (setq tbl-str "ТАБЛИЦА СОЕДИНЕНИЙ\n")
  (setq tbl-str (strcat tbl-str "Цепь\tОткуда\tКуда\n"))
  (setq tbl-str (strcat tbl-str "-----\t------\t----\n"))
  (setq i 0)
  (foreach ch chains
    (setq i (1+ i))
    (setq terminators (el:chain-terminals ch))
    (cond
      ((>= (length terminators) 2)
        (setq t1 (car terminators) t2 (cadr terminators))
        (setq froms "" tos "")
        (foreach tp (el:text-near t1 *el:text-radius*) (setq froms (strcat froms " " (car tp))))
        (foreach tp (el:text-near t2 *el:text-radius*) (setq tos (strcat tos " " (car tp))))
        (setq tbl-str (strcat tbl-str (itoa i) "\t" froms "\t" tos "\n")))
      (T
        (setq tbl-str (strcat tbl-str (itoa i) "\t[petlya/kolco]\n")))))

  (entmake (list '(0 . "MTEXT") '(100 . "AcDbEntity") '(100 . "AcDbMText")
                 (cons 10 ins) (cons 1 tbl-str) '(40 . 2.5)))
  (princ (strcat "\n; Таблица начерчена в точке " (vl-princ-to-string ins)))
  (princ)
)
