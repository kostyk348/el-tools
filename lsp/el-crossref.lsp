;; el-crossref.lsp — поиск перекрёстных ссылок между листами
;; Работает только с TEXT/MTEXT, ищет шаблоны "лист N", "sheet N", "→ N"
;; Команда: EL-CROSSREF

(if (not *el:tolerance*) (load "el-core.lsp"))

;; ============================================================
;; Поиск ссылок в одном чертеже
;; v2: единый проход. Ключевые слова: "лист", "sheet", "page", ">", "→"
;; ============================================================

(defun el:crossref-keyword (content / kws kw pos min-len)
  ;; возвращает (позицию ключевого слова . длину слова) или nil
  (setq kws '(("лист" . 4) ("sheet" . 5) ("page" . 4) (">" . 1) ("→" . 1))
        pos nil)
  (foreach kw kws
    (setq p (vl-string-search (car kw) content))
    (if p
      (if (or (null pos) (< p (car pos)))
        (setq pos (cons p (cdr kw))))))
  pos)

(defun el:crossref-in-dwg (/ lower res e content pos tail num ch)
  (setq res nil)
  (foreach e (el:all-texts)
    (setq content (cdr (assoc 1 (entget e))))
    (setq pos (el:crossref-keyword (strcase content T)))
    (if pos
      (progn
        ;; число после ключевого слова (пропуская пробелы)
        (setq tail (substr content (+ (car pos) (cdr pos) 1))
              num "")
        (while (and (/= tail "") (member (ascii (substr tail 1 1)) '(32 9)))
          (setq tail (substr tail 2)))
        (while (and (/= tail "") (<= 48 (ascii (substr tail 1 1)) 57))
          (setq num (strcat num (substr tail 1 1))
                tail (substr tail 2)))
        (if (> (strlen num) 0)
          (setq res (cons (list content num (el:2d (cdr (assoc 10 (entget e))))) res))))))
  (reverse res))

;; ============================================================
;; Команда
;; ============================================================

(defun C:EL-CROSSREF (/ refs)
  (princ "\n=== EL-CROSSREF: поиск ссылок на другие листы ===")
  (setq refs (el:crossref-in-dwg))

  (if (null refs)
    (princ "\n; Ссылок на другие листы не найдено")
    (progn
      (princ (strcat "\n; Найдено ссылок: " (itoa (length refs))))
      (foreach r refs
        (princ (strcat "\n  \"" (car r) "\" → лист " (cadr r) "  поз. " (vl-princ-to-string (caddr r)))))
      ;; подсветить найденные тексты по позициям
      (foreach r refs
        (setq pos (caddr r) found nil i 0)
        (foreach pc *el:text-cache*
          (if (equal (car pc) pos 0.1)
            (setq found (nth i *el:text-ents*)))
          (setq i (1+ i)))
        (if found (redraw found 3)))))
  (princ "\n=== EL-CROSSREF завершён ===")
  (princ)
)

;; ============================================================
;; Полистовой поиск (EL-CROSSREF-ALL): реализован в C#-плагине
;; (autocad-electrical-plugin). В LISP не дублируется — открытие
;; и чтение произвольных DWG вне текущего документа через ActiveX
;; ненадёжно и медленно.
;; ============================================================
