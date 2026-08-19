;; el-whatif.lsp — "что если": симуляция отключения линии/цепи
;; Зависит от: el-core.lsp
;; Команда: EL-WHATIF

(if (not *el:tolerance*) (load "el-core.lsp"))

(defun el:graph-remove-edge (graph edge / g adj)
  ;; возвращает новый граф без edge
  (setq g nil)
  (foreach pair graph
    (if (eq (car pair) edge)
      (setq g (cons (cons (car pair) nil) g))  ;; edge теперь без соседей
      (progn
        (setq adj (cdr pair))
        (setq adj (vl-remove edge adj))
        (setq g (cons (cons (car pair) adj) g)))))
  (reverse g))

(defun C:EL-WHATIF (/ lines graph e chain split1 split2 texts1 texts2 g2)
  (princ "\n=== EL-WHATIF: что будет, если разорвать цепь ===")
  (setq lines (el:all-lines))
  (if (< (length lines) 2) (progn (princ "\n! Мало LINE") (exit)))

  (setq graph (el:build-graph lines))

  (princ "\n→ Кликни на линию, которую разрываем: ")
  (setq e (entsel))
  (if (null e) (exit))
  (setq e (car e))
  (if (not (eq "LINE" (cdr (assoc 0 (entget e)))))
    (progn (princ "\n! Это не LINE") (exit)))

  ;; Полная цепь
  (setq chain (el:trace e graph))

  ;; Убираем линию из графа
  (setq g2 (el:graph-remove-edge graph e))

  ;; Два компонента после разрыва
  ;; Находим соседей удалённой линии
  (setq adj (cdr (assoc e graph)))
  (if (< (length adj) 2)
    (progn
      (princ "\n! Эта линия концевая — разрыв не разделит цепь")
      (princ "\n  (она и так на конце, отключится только она сама)")
      (el:hilite (list e))
      (exit)))

  (setq split1 (el:trace (car adj) g2))
  (setq split2 (el:trace (cadr adj) g2))

  (setq texts1 (el:chain-texts split1)
        texts2 (el:chain-texts split2))

  (princ "\n=== РЕЗУЛЬТАТ РАЗРЫВА ===")
  (princ (strcat "\nРазорванная линия: " (vl-princ-to-string (mapcar 'el:2d (el:line-endpoints e)))))

  (princ (strcat "\n\n<<< Часть 1 (" (itoa (length split1)) " линий):"))
  (foreach t texts1 (princ (strcat " [" t "]")))

  (princ (strcat "\n\n<<< Часть 2 (" (itoa (length split2)) " линий):"))
  (foreach t texts2 (princ (strcat " [" t "]")))

  ;; Подсветка
  (el:hilite split1)
  (princ "\n\nЧасть 1 подсвечена жёлтым. ENTER — показать часть 2: ")
  (if (null (grread)) (princ "."))
  (el:unhilite split1)
  (el:hilite split2)
  (princ "\nЧасть 2 подсвечена жёлтым. ENTER — снять подсветку: ")
  (if (null (grread)) (princ "."))
  (el:unhilite split2)

  (princ "\n=== EL-WHATIF завершён ===")
  (princ)
)
