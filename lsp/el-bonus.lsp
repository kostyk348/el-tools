;; el-bonus.lsp — бонусные фишки, которых ни у кого нет
;; Зависит от: el-core.lsp
;; Команды: EL-COLOR-CHAINS, EL-LOOPS, EL-STATS, EL-HOTSPOTS, EL-BOTTLENECK

(if (not *el:tolerance*) (load "el-core.lsp"))

;; ============================================================
;; EL-COLOR-CHAINS — раскрасить каждую цепь в свой цвет
;; ============================================================

(defun C:EL-COLOR-CHAINS (/ lines graph chains colors i col)
  (setq lines (el:all-lines))
  (if (< (length lines) 2) (progn (princ "\n! Мало LINE") (exit)))

  (setq graph (el:build-graph lines)
        chains (el:all-chains graph)
        colors '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 20 30 40 50 60 70 80 90 100 110 120 130 140 150 160 170 180 190 200 210 220 230 240 250)
        i 0)

  (princ (strcat "\n=== EL-COLOR-CHAINS: " (itoa (length chains)) " цепей ==="))

  (foreach ch chains
    (setq col (nth (rem i (length colors)) colors))
    (foreach l ch
      (setq ed (entget l))
      (setq ed (append ed (list (cons 62 col))))
      (entmod ed))
    (setq i (1+ i)))

  (princ (strcat "\n; Раскрашено. Всего цветов: " (itoa (length chains))))
  (princ "\n; REGEN для обновления.")
  (princ)
)

;; ============================================================
;; EL-LOOPS — найти петли в графе (циклы)
;; ============================================================

(defun el:find-loops (graph / visited loops stack current adj)
  ;; Поиск циклов через DFS
  (setq visited nil loops nil)
  (foreach pair graph
    (setq current (car pair) adj (cdr pair))
    (if (not (member current visited))
      (progn
        (setq stack (list (list current)))
        (while stack
          (setq path (car stack) stack (cdr stack))
          (setq current (car path))
          (if (not (member current visited))
            (progn
              (setq visited (cons current visited))
              (foreach neigh (cdr (assoc current graph))
                (if (member neigh (cdr path))
                  ;; нашли цикл
                  (setq loops (cons (cons neigh (reverse path)) loops))
                  (setq stack (cons (cons neigh path) stack))))))))))
  loops)

(defun C:EL-LOOPS (/ lines graph chains has-loops)
  (princ "\n=== EL-LOOPS: поиск петель ===")
  (setq lines (el:all-lines))
  (if (< (length lines) 2) (progn (princ "\n! Мало LINE") (exit)))

  (setq graph (el:build-graph lines))

  ;; Упрощённо: цепь с количеством терминалов = 0 — это кольцо
  (setq chains (el:all-chains graph)
        has-loops nil)

  (foreach ch chains
    (setq terms (el:chain-terminals ch))
    (if (= (length terms) 0)
      (progn
        (setq has-loops (cons ch has-loops))
        (el:hilite ch)
        (princ (strcat "\n! КОЛЬЦО/ПЕТЛЯ: " (itoa (length ch)) " линий"))
        (foreach t (el:chain-texts ch)
          (princ (strcat " [" t "]")))
        (princ "\nПодсвечено жёлтым. ENTER — продолжить: ")
        (if (null (grread)) (princ "."))
        (el:unhilite ch))))

  (if (null has-loops)
    (princ "\n; Петли не найдены")
    (princ (strcat "\n; Найдено петель: " (itoa (length has-loops)))))

  (princ "\n=== EL-LOOPS завершён ===")
  (princ)
)

;; ============================================================
;; EL-STATS — полная статистика чертежа
;; ============================================================

(defun C:EL-STATS (/ lines texts graph chains i lens minl maxl avg)
  (setq lines (el:all-lines))
  (setq texts (el:all-texts))

  (princ "\n=== EL-STATS: статистика схемы ===")
  (princ (strcat "\nLINE:   " (itoa (length lines)) " шт"))
  (princ (strcat "\nTEXT:   " (itoa (length texts)) " шт"))

  (if (>= (length lines) 2)
    (progn
      (setq graph (el:build-graph lines)
            chains (el:all-chains graph))
      (princ (strcat "\nЦЕПЕЙ: " (itoa (length chains)) " шт"))

      (setq lens (mapcar 'length chains))
      (setq minl (apply 'min lens)
            maxl (apply 'max lens)
            avg (/ (apply '+ lens) (float (length lens))))

      (princ (strcat "\nДлина цепи (линий):  мин=" (itoa minl)
                     "  макс=" (itoa maxl)
                     "  сред=" (rtos avg 2 1)))

      ;; цепи с текстом / без
      (setq with-text 0 without-text 0)
      (foreach ch chains
        (if (el:chain-texts ch)
          (setq with-text (1+ with-text))
          (setq without-text (1+ without-text))))
      (princ (strcat "\nЦепи с подписями:   " (itoa with-text)))
      (princ (strcat "\nЦепи без подписей:  " (itoa without-text)))

      ;; распределение
      (princ "\n\nРаспределение по размеру цепи:")
      (setq hist '(0 0 0 0 0 0))
      (foreach ch chains
        (setq n (length ch))
        (cond ((<= n 1) (setq hist (list (1+ (nth 0 hist)) (nth 1 hist) (nth 2 hist) (nth 3 hist) (nth 4 hist) (nth 5 hist))))
              ((<= n 3) (setq hist (list (nth 0 hist) (1+ (nth 1 hist)) (nth 2 hist) (nth 3 hist) (nth 4 hist) (nth 5 hist))))
              ((<= n 10) (setq hist (list (nth 0 hist) (nth 1 hist) (1+ (nth 2 hist)) (nth 3 hist) (nth 4 hist) (nth 5 hist))))
              ((<= n 30) (setq hist (list (nth 0 hist) (nth 1 hist) (nth 2 hist) (1+ (nth 3 hist)) (nth 4 hist) (nth 5 hist))))
              ((<= n 100) (setq hist (list (nth 0 hist) (nth 1 hist) (nth 2 hist) (nth 3 hist) (1+ (nth 4 hist)) (nth 5 hist))))
              (T (setq hist (list (nth 0 hist) (nth 1 hist) (nth 2 hist) (nth 3 hist) (nth 4 hist) (1+ (nth 5 hist)))))))
      (princ (strcat "\n  1 линия:     " (itoa (nth 0 hist))))
      (princ (strcat "\n  2-3 линии:   " (itoa (nth 1 hist))))
      (princ (strcat "\n  4-10 линий:  " (itoa (nth 2 hist))))
      (princ (strcat "\n  11-30 линий: " (itoa (nth 3 hist))))
      (princ (strcat "\n  31-100 л:    " (itoa (nth 4 hist))))
      (princ (strcat "\n  >100 линий:  " (itoa (nth 5 hist))))
    ))

  (princ "\n=== EL-STATS завершён ===")
  (princ)
)

;; ============================================================
;; EL-HOTSPOTS — тепловая карта плотности линий
;; ============================================================

(defun C:EL-HOTSPOTS (/ ll ur grid-size xs ys grid max-val colors i j ent)
  (princ "\n=== EL-HOTSPOTS: тепловая карта ===")

  ;; границы чертежа
  (setq ll (getvar "extmin") ur (getvar "extmax"))
  (setq grid-size 10)  ;; количество ячеек

  (setq xs (/ (- (car ur) (car ll)) grid-size)
        ys (/ (- (cadr ur) (cadr ll)) grid-size))

  (setq grid nil)
  (setq i 0)
  (repeat grid-size
    (setq j 0)
    (repeat grid-size
      (setq grid (cons (list i j 0) grid))
      (setq j (1+ j)))
    (setq i (1+ i)))

  ;; считаем пересечения линий с ячейками
  (setq lines (el:all-lines))
  (foreach l lines
    (setq mid (el:line-mid l))
    (setq ix (fix (/ (- (car mid) (car ll)) xs))
          iy (fix (/ (- (cadr mid) (cadr ll)) ys))
          ix (max 0 (min (1- grid-size) ix))
          iy (max 0 (min (1- grid-size) iy)))
    ;; инкремент ячейки
    (setq grid
      (subst (list ix iy (1+ (caddr (nth (+ (* iy grid-size) ix) grid))))
             (nth (+ (* iy grid-size) ix) grid)
             grid)))

  ;; нормализация и отрисовка
  (setq max-val 1)
  (foreach g grid
    (if (> (caddr g) max-val) (setq max-val (caddr g))))

  (setq colors '(40 30 20 10 1 2 3 4 5 6))
  (setq i 0)
  (repeat grid-size
    (setq j 0)
    (repeat grid-size
      (setq val (caddr (nth (+ (* j grid-size) i) grid)))
      (setq idx (fix (if (> max-val 0)
                       (* (/ val (float max-val)) (1- (length colors)))
                       0)))
      (entmake (list '(0 . "RECTANG")
                     (cons 10 (list (+ (car ll) (* i xs)) (+ (cadr ll) (* j ys))))
                     (cons 11 (list (+ (car ll) (* (1+ i) xs)) (+ (cadr ll) (* (1+ j) ys))))
                     (cons 62 (nth idx colors))))
      (setq j (1+ j)))
    (setq i (1+ i)))

  (princ (strcat "\n; Тепловая карта начерчена. Макс плотность: " (itoa max-val)))
  (princ "\n=== EL-HOTSPOTS завершён ===")
  (princ)
)

;; ============================================================
;; EL-BOTTLENECK — найти точки пересечения многих цепей
;; ============================================================

(defun C:EL-BOTTLENECK (/ lines graph chains i j intersections count)
  (princ "\n=== EL-BOTTLENECK: поиск узких мест ===")
  (setq lines (el:all-lines))
  (if (< (length lines) 2) (progn (princ "\n! Мало LINE") (exit)))

  (setq graph (el:build-graph lines)
        chains (el:all-chains graph))

  ;; считаем, сколько цепей проходит через каждую линию
  (setq chain-counts nil)
  (foreach ch chains
    (foreach l ch
      (setq existing (assoc l chain-counts))
      (if existing
        (setq chain-counts (subst (cons l (1+ (cdr existing))) existing chain-counts))
        (setq chain-counts (cons (cons l 1) chain-counts)))))

  ;; сортируем по убыванию
  (setq chain-counts (vl-sort chain-counts '(lambda (a b) (> (cdr a) (cdr b)))))

  (princ "\nТоп-10 узких мест (линий, через которые проходит много цепей):")
  (setq i 0)
  (while (and chain-counts (< i 10))
    (setq cc (car chain-counts) chain-counts (cdr chain-counts))
    (if (> (cdr cc) 1)
      (progn
        (princ (strcat "\n  LINE через " (itoa (cdr cc)) " цепей: "))
        (setq texts (el:chain-texts (list (car cc))))
        (foreach t texts (princ (strcat "[" t "]" )))))
    (setq i (1+ i)))

  (princ "\n=== EL-BOTTLENECK завершён ===")
  (princ)
)

(princ "\n; el-bonus.lsp loaded — EL-COLOR-CHAINS, EL-LOOPS, EL-STATS, EL-HOTSPOTS, EL-BOTTLENECK")
(princ)
