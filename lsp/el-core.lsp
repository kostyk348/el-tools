;; el-core.lsp v2.0 — производительное ядро с кэшем и пространственным индексом
;; Загружать первой: (load "el-core.lsp")

(vl-load-com)

(setq *el:tolerance*    0.5)  ;; мм — стыковка LINE
(setq *el:text-radius*  5.0)  ;; мм — поиск текста

;; ============================================================
;; Геометрия
;; ============================================================
(defun el:2d (p) (list (car p) (cadr p)))

(defun el:dist2 (p1 p2)
  (+ (expt (- (car p1) (car p2)) 2)
     (expt (- (cadr p1) (cadr p2)) 2)))

(defun el:dist  (p1 p2) (sqrt (el:dist2 p1 p2)))
(defun el:approx-p (p1 p2 t2) (<= (el:dist2 p1 p2) t2))

;; ============================================================
;; Cбор сущностей (один раз, только ModelSpace)
;; ============================================================
(defun el:collect-lines (/ ss i e ed out)
  (princ "\n; Сбор LINE...")
  (setq *el:line-ents* nil *el:line-ends* nil)
  (if (setq ss (ssget "_X" '((0 . "LINE") (410 . "Model"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) ed (entget e) i (1+ i))
        (setq *el:line-ents* (cons e *el:line-ents*))
        (setq *el:line-ends*
          (cons (cons e
            (list (el:2d (cdr (assoc 10 ed)))
                  (el:2d (cdr (assoc 11 ed)))))
            *el:line-ends*)))))
  (setq *el:line-ents* (reverse *el:line-ents*))
  (setq *el:line-ends* (reverse *el:line-ends*))
  (princ (strcat " " (itoa (length *el:line-ents*))))
  *el:line-ents*)

(defun el:collect-texts (/ ss i e ed)
  (princ " TEXT...")
  (setq *el:text-cache* nil *el:text-ents* nil *el:text-grid* nil)
  (if (setq ss (ssget "_X" '((0 . "TEXT,MTEXT") (410 . "Model"))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) ed (entget e) i (1+ i))
        (setq *el:text-ents* (cons e *el:text-ents*))
        (setq *el:text-cache*
          (cons (cons (el:2d (cdr (assoc 10 ed)))
                      (cdr (assoc 1 ed)))
                *el:text-cache*)))))
  (setq *el:text-ents* (reverse *el:text-ents*))
  (setq *el:text-cache* (reverse *el:text-cache*))
  (princ (strcat " " (itoa (length *el:text-cache*))))
  *el:text-ents*)

(defun el:init ()
  (el:collect-lines)
  (el:collect-texts)
  (el:init-text-grid)
  (princ " OK")
  (princ))

;; ============================================================
;; Быстрый доступ (кэш)
;; ============================================================
(defun el:ends (ent) (cdr (assoc ent *el:line-ends*)))
(defun el:line-mid (ent / e) (setq e (el:ends ent)) (mapcar '(lambda (a b) (/ (+ a b) 2.0)) (car e) (cadr e)))

;; ============================================================
;; Поиск текста — пространственный индекс (grid) вместо полного скана.
;; Приемлемо: кэш текст = (точка . строка); grid: cell -> список (точка . строка)
;; ============================================================
(defun el:text-grid-size () (max *el:text-radius* 10.0))

(defun el:init-text-grid (/ gs cell entry)
  (setq gs (el:text-grid-size) *el:text-grid* nil)
  (foreach pair *el:text-cache*
    (setq cell (el:get-cell (car pair) gs)
          entry (assoc cell *el:text-grid*))
    (if entry
      (setq *el:text-grid* (subst (cons cell (cons pair (cdr entry))) entry *el:text-grid*))
      (setq *el:text-grid* (cons (list cell pair) *el:text-grid*))))
  *el:text-grid*)

(defun el:text-near (pt rad / gs r2 cells cand result d2)
  (setq gs (el:text-grid-size))
  (if (null *el:text-grid*) (el:init-text-grid))
  (setq r2 (* rad rad)
        cells (el:neighbors (el:get-cell pt gs) gs)
        cand (apply 'append
              (mapcar '(lambda (c) (cdr (assoc c *el:text-grid*))) cells))
        result nil)
  (foreach pair cand
    (setq d2 (el:dist2 (car pair) pt))
    (if (<= d2 r2)
      (setq result (cons (cons (cdr pair) (car pair)) result))))
  result)

;; ============================================================
;; Пространственный индекс для графа
;; ============================================================
(defun el:build-grid (ents endsfn grid-size / grid cell entry)
  (setq grid nil)
  (foreach e ents
    (foreach p (endsfn e)
      (setq cell (list (* (fix (/ (car p) grid-size)) grid-size)
                       (* (fix (/ (cadr p) grid-size)) grid-size))
            entry (assoc cell grid))
      (if entry
        (setq grid (subst (cons cell (cons e (cdr entry))) entry grid))
        (setq grid (cons (list cell e) grid)))))
  grid)

(defun el:get-cell (p gs)
  (list (* (fix (/ (car p) gs)) gs)
        (* (fix (/ (cadr p) gs)) gs)))

(defun el:neighbors (cell gs)
  (list cell
    (list (- (car cell) gs) (cadr cell))
    (list (+ (car cell) gs) (cadr cell))
    (list (car cell) (- (cadr cell) gs))
    (list (car cell) (+ (cadr cell) gs))
    (list (- (car cell) gs) (- (cadr cell) gs))
    (list (+ (car cell) gs) (- (cadr cell) gs))
    (list (- (car cell) gs) (+ (cadr cell) gs))
    (list (+ (car cell) gs) (+ (cadr cell) gs))))

;; ============================================================
;; Построение графа
;; ============================================================
(defun el:build-graph (lines / gs grid graph l1 ends cell adj cand t2 done)
  (setq t2 (* *el:tolerance* *el:tolerance*)
        gs (max *el:tolerance* 50.0))
  (princ "\n; Сетка...")
  (setq grid (el:build-grid lines 'el:ends gs))
  (princ " Граф...")
  (setq graph nil done 0)

  (foreach l1 lines
    (setq ends (el:ends l1) adj nil)
    (setq cand (apply 'append
      (mapcar '(lambda (c) (cdr (assoc c grid)))
              (el:neighbors (el:get-cell (car ends) gs) gs))))
    (foreach l2 cand
      (if (and (not (eq l1 l2))
               (not (member l2 adj))
               (el:approx-p (car (el:ends l1)) (car (el:ends l2)) t2))
        (setq adj (cons l2 adj)))
      (if (and (not (eq l1 l2))
               (not (member l2 adj))
               (el:approx-p (car (el:ends l1)) (cadr (el:ends l2)) t2))
        (setq adj (cons l2 adj)))
      (if (and (not (eq l1 l2))
               (not (member l2 adj))
               (el:approx-p (cadr (el:ends l1)) (car (el:ends l2)) t2))
        (setq adj (cons l2 adj)))
      (if (and (not (eq l1 l2))
               (not (member l2 adj))
               (el:approx-p (cadr (el:ends l1)) (cadr (el:ends l2)) t2))
        (setq adj (cons l2 adj))))
    (setq graph (cons (cons l1 adj) graph))
    (setq done (1+ done))
    (if (= (rem done 200) 0) (princ ".")))

  (princ " done")
  (reverse graph))

;; ============================================================
;; BFS
;; ============================================================
(defun el:trace (start graph / visited q cur adj)
  (setq visited nil q (list start))
  (while q
    (setq cur (car q) q (cdr q))
    (if (not (member cur visited))
      (progn
        (setq visited (cons cur visited)
              adj (cdr (assoc cur graph)))
        (foreach a adj
          (if (not (member a visited))
            (setq q (cons a q)))))))
  (reverse visited))

(defun el:all-chains (graph / visited chains ch)
  (setq visited nil chains nil)
  (foreach pair graph
    (if (not (member (car pair) visited))
      (progn
        (setq ch (el:trace (car pair) graph))
        (setq visited (append ch visited)
              chains (cons ch chains)))))
  (reverse chains))

;; ============================================================
;; Тексты для цепи
;; ============================================================
(defun el:chain-texts (chain / result)
  (setq result nil)
  (foreach l chain
    (foreach p (el:ends l)
      (foreach t (el:text-near p *el:text-radius*)
        (setq result (cons (car t) result)))))
  (reverse result))

;; ============================================================
;; Подсветка
;; ============================================================
(defun el:hilite (ents) (foreach e ents (redraw e 3)))
(defun el:unhilite (ents) (foreach e ents (redraw e 4)))

;; ============================================================
;; Терминалы цепи (из el-table, теперь в ядре)
;; ============================================================
(defun el:chain-terminals (chain / cnts terms ends pts pt found)
  (setq cnts nil)
  (foreach l chain
    (foreach p (el:ends l)
      (setq found nil)
      (foreach c cnts
        (if (el:approx-p (car c) p (* *el:tolerance* *el:tolerance*))
          (setq found c)))
      (if found
        (setq cnts (subst (list (car found) (1+ (cadr found))) found cnts))
        (setq cnts (cons (list p 1) cnts)))))
  (setq terms nil)
  (foreach c cnts
    (if (= (cadr c) 1)
      (setq terms (cons (car c) terms))))
  (reverse terms))

;; ============================================================
;; EL-GRAPH (отладка, теперь быстрый)
;; ============================================================
(defun C:EL-GRAPH (/ lines graph chains i)
  (el:init)
  (setq lines *el:line-ents*)
  (if (< (length lines) 2) (progn (princ "\n! Мало LINE") (princ) (exit)))
  (setq graph (el:build-graph lines)
        chains (el:all-chains graph) i 0)
  (princ (strcat "\n--- EL-GRAPH: " (itoa (length lines)) " line, "
                 (itoa (length chains)) " chain ---"))
  (foreach ch chains
    (setq i (1+ i))
    (princ (strcat "\n  #" (itoa i) ": " (itoa (length ch)) " lines"))
    (foreach t (el:chain-texts ch) (princ (strcat " [" t "]"))))
  (princ)
)

;; ============================================================
;; Backward compat — старые файлы используют эти имена
;; ============================================================
(defun el:all-lines () (or *el:line-ents* (el:init)) *el:line-ents*)
(defun el:all-texts () (or *el:text-ents* (el:init)) *el:text-ents*)
(defun el:line-endpoints (ent) (el:ends ent))

(princ "\n; el-core v2.0 loaded. (el:init) — собрать LINE+TEXT из ModelSpace")
(princ)
