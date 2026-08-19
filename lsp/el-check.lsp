;; el-check.lsp — дефектоскоп электрических схем
;; Проверки: висячие цепи, разрывы, дубликаты, изолированные фрагменты
;; Зависит от: el-core.lsp
;; Команда: EL-CHECK

(if (not *el:tolerance*) (load "el-core.lsp"))

;; ============================================================
;; Проверка: изолированные линии (degree 0)
;; ============================================================

(defun el:check-isolated (graph / items)
  (vl-remove-if '(lambda (pair) (cdr pair)) graph))

(defun el:report-isolated (graph / bad)
  (setq bad (el:check-isolated graph))
  (princ (strcat "\n--- ИЗОЛИРОВАННЫЕ ЛИНИИ: " (itoa (length bad)) " шт ---"))
  (foreach pair bad
    (princ (strcat "\n  LINE " (vl-princ-to-string (mapcar 'el:2d (el:line-endpoints (car pair)))))))
  bad)

;; ============================================================
;; Проверка: near-miss разрывы (концы почти рядом, но не соединены)
;; v2: пространственный индекс (grid) — O(n) вместо O(n²)
;; ============================================================

(defun el:check-gaps (lines / gs end-grid l ep cell entry cand other ol op d gaps)
  (setq gs (* *el:tolerance* 6.0) end-grid nil gaps nil)

  ;; grid по концам: cell -> ((линия . конец) ...)
  (foreach l lines
    (foreach ep (el:line-endpoints l)
      (setq cell (el:get-cell (el:2d ep) gs)
            entry (assoc cell end-grid))
      (if entry
        (setq end-grid (subst (cons cell (cons (cons l ep) (cdr entry))) entry end-grid))
        (setq end-grid (cons (list cell (cons l ep)) end-grid)))))

  ;; ищем близкие концы только в соседних ячейках
  (foreach l lines
    (foreach ep (el:line-endpoints l)
      (setq p (el:2d ep)
            cand (apply 'append
                   (mapcar '(lambda (c) (cdr (assoc c end-grid)))
                           (el:neighbors (el:get-cell p gs) gs))))
      (foreach other cand
        (setq ol (car other) op (el:2d (cdr other)))
        (if (and (not (eq l ol))
                 (not (member (cons l ol) gaps))
                 (not (member (cons ol l) gaps)))
          (progn
            (setq d (el:dist p op))
            (if (and (> d *el:tolerance*) (< d gs))
              (setq gaps (cons (list l ol d) gaps))))))))
  (reverse gaps))

(defun el:report-gaps (lines / gaps)
  (setq gaps (el:check-gaps lines))
  (princ (strcat "\n--- БЛИЗКИЕ РАЗРЫВЫ (gap < " (rtos (* *el:tolerance* 6) 2 1) "mm): " (itoa (length gaps)) " ---"))
  (foreach g gaps
    (princ (strcat "\n  gap " (rtos (caddr g) 2 2) "mm: LINE1 " (vl-princ-to-string (mapcar 'el:2d (el:line-endpoints (car g)))))))
  gaps)

;; ============================================================
;; Проверка: дубликаты текста в разных цепях
;; ============================================================

(defun el:check-duplicates (chains / text-map idx texts txt entry result)
  (setq text-map nil idx 0)
  (foreach ch chains
    (setq idx (1+ idx) texts (el:chain-texts ch))
    (foreach t texts
      (setq txt (vl-string-trim " " t))
      (if (> (strlen txt) 0)
        (progn
          (setq entry (assoc txt text-map))
          (if entry
            (setq text-map (subst (cons txt (cons idx (cdr entry))) entry text-map))
            (setq text-map (cons (list txt idx) text-map)))))))
  (setq result nil)
  (foreach entry text-map
    (if (> (length (cdr entry)) 1)
      (setq result (cons entry result))))
  (reverse result))

(defun el:report-duplicates (chains / dups)
  (setq dups (el:check-duplicates chains))
  (princ (strcat "\n--- ДУБЛИКАТЫ ТЕКСТА В РАЗНЫХ ЦЕПЯХ: " (itoa (length dups)) " ---"))
  (foreach d dups
    (princ (strcat "\n  \"" (car d) "\" в цепях: "))
    (foreach ci (cdr d)
      (princ (strcat "#" (itoa ci) " "))))
  dups)

;; ============================================================
;; Проверка: цепь без текста
;; ============================================================

(defun el:report-textless (chains / i texts)
  (setq i 0)
  (princ "\n--- ЦЕПИ БЕЗ ПОДПИСЕЙ ---")
  (foreach ch chains
    (setq i (1+ i) texts (el:chain-texts ch))
    (if (null texts)
      (princ (strcat "\n  Цепь #" (itoa i) ": " (itoa (length ch)) " линий, без текста"))))
  (princ (strcat "\nВсего: " (itoa i) " цепей"))
)

;; ============================================================
;; Проверка: цепь из 1 линии (подозрительно)
;; ============================================================

(defun el:report-single-lines (chains / i)
  (setq i 0)
  (princ "\n--- ЦЕПИ ИЗ ОДНОЙ ЛИНИИ ---")
  (foreach ch chains
    (setq i (1+ i))
    (if (= (length ch) 1)
      (princ (strcat "\n  Цепь #" (itoa i) ": LINE " (vl-princ-to-string (mapcar 'el:2d (el:line-endpoints (car ch))))))))
)

;; ============================================================
;; ГЛАВНАЯ КОМАНДА: EL-CHECK
;; ============================================================

(defun C:EL-CHECK (/ lines graph chains)
  (princ "\n=== EL-CHECK: дефектоскоп схемы ===")
  (setq lines (el:all-lines))
  (if (< (length lines) 2)
    (progn (princ "\n! Мало LINE в чертеже") (princ) (exit)))

  (setq graph (el:build-graph lines))
  (setq chains (el:all-chains graph))

  (princ (strcat "\nСхема: " (itoa (length lines)) " линий, "
                 (itoa (length chains)) " цепей"))
  (princ "\n---")

  ;; изолированные линии
  (el:report-isolated graph)

  ;; near-miss разрывы
  (el:report-gaps lines)

  ;; дубликаты текста
  (el:report-duplicates chains)

  ;; цепи без подписей
  (el:report-textless chains)

  ;; цепи из 1 линии
  (el:report-single-lines chains)

  (princ "\n=== EL-CHECK завершён ===")
  (princ)
)
