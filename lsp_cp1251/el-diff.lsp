;; el-diff.lsp — топологический дифф между версиями схем
;; Зависит от: el-core.lsp
;; Команды: EL-SAVE-SNAPSHOT, EL-DIFF

(if (not *el:tolerance*) (load "el-core.lsp"))

;; ============================================================
;; Снимок: сохранить топологию текущего чертежа в файл
;; ============================================================

(defun C:EL-SAVE-SNAPSHOT (/ lines graph chains ch texts fname f)
  (setq fname (getfiled "Сохранить снимок топологии" "snapshot" "txt" 1))
  (if (null fname) (exit))

  (setq lines (el:all-lines))
  (if (< (length lines) 2) (progn (princ "\n! Мало LINE") (exit)))

  (setq graph (el:build-graph lines)
        chains (el:all-chains graph))

  (setq f (open fname "w"))
  (write-line ";; EL-DIFF snapshot" f)
  (write-line (strcat ";; File: " (getvar "dwgname")) f)
  (write-line (strcat ";; Date: " (menucmd "M=$(edtime,$(getvar,date),YYYY.MO.DD HH:MM:SS)")) f)
  (write-line (strcat ";; Lines: " (itoa (length lines))) f)
  (write-line (strcat ";; Chains: " (itoa (length chains))) f)

  (foreach ch chains
    (setq texts (el:chain-texts ch))
    (setq line-str "CHAIN: ")
    (foreach t texts
      (setq line-str (strcat line-str "\"" t "\" ")))
    (write-line line-str f))

  (close f)
  (princ (strcat "\n; Снимок сохранён: " fname))
  (princ)
)

;; ============================================================
;; Загрузить снимок (парсер CHAIN-строк)
;; ============================================================

(defun el:snapshot-chains (fname / f chains line texts)
  (setq chains nil f (open fname "r"))
  (while (setq line (read-line f))
    (if (wcmatch line "CHAIN: *")
      (progn
        (setq texts nil
              line (vl-string-left-trim "CHAIN: " line))
        ;; парсим "txt1" "txt2" ...
        (while (wcmatch line "*\"*\"*")
          (setq texts (cons (substr line 2 (vl-string-search "\"" line 2)) texts)
                line (substr line (+ (vl-string-search "\"" line 2) 3))))
        (if (> (strlen line) 2)
          (setq texts (cons (substr line 2 (1- (strlen line))) texts)))
        (if texts (setq chains (cons (reverse texts) chains))))))
  (close f)
  (reverse chains))

(defun el:chain-signature (texts)
  (vl-sort texts '(lambda (a b) (< a b))))

(defun el:match-score (a b)
  (setq common 0)
  (foreach t a
    (if (member t b) (setq common (1+ common))))
  (if (= (max (length a) (length b)) 0) 0
    (/ common (max (length a) (length b)) 1.0)))

(defun C:EL-DIFF (/ fname lines graph chains snap added removed changed)
  (setq fname (getfiled "Загрузить снимок для сравнения" "snapshot" "txt" 0))
  (if (null fname) (exit))

  (setq snap (el:snapshot-chains fname))
  (setq lines (el:all-lines))
  (if (< (length lines) 2) (progn (princ "\n! Мало LINE") (exit)))

  (setq graph (el:build-graph lines)
        chains (el:all-chains graph))

  (princ (strcat "\n=== EL-DIFF ==="))
  (princ (strcat "\nСнимок: " (itoa (length snap)) " цепей"))
  (princ (strcat "\nСейчас: " (itoa (length chains)) " цепей"))

  ;; Для каждой цепи из снимка — ищем совпадение в текущем
  (setq added nil removed nil changed nil)
  (foreach sch snap
    (setq match nil)
    (foreach cch chains
      (if (>= (el:match-score (el:chain-signature sch) (el:chain-signature (el:chain-texts cch))) 0.5)
        (setq match cch)))
    (if (null match)
      (setq removed (cons sch removed))
      (progn
        (if (not (equal (vl-sort sch '<) (vl-sort (el:chain-texts match) '<)))
          (setq changed (cons (list sch (el:chain-texts match)) changed))))))

  ;; Цепи только в текущем чертеже
  (foreach cch chains
    (setq texts (el:chain-texts cch) found nil)
    (foreach sch snap
      (if (>= (el:match-score (el:chain-signature sch) (el:chain-signature texts)) 0.5)
        (setq found T)))
    (if (not found) (setq added (cons texts added))))

  (princ "\n---")
  (princ (strcat "\n<<< УДАЛЕННЫЕ цепи: " (itoa (length removed))))
  (foreach r removed
    (princ "\n  [")
    (foreach t r (princ (strcat " " t)))
    (princ " ]"))

  (princ (strcat "\n>>> НОВЫЕ цепи: " (itoa (length added))))
  (foreach a added
    (princ "\n  [")
    (foreach t a (princ (strcat " " t)))
    (princ " ]"))

  (princ (strcat "\n~~~ ИЗМЕНЁННЫЕ цепи: " (itoa (length changed))))
  (foreach c changed
    (princ "\n  Было: [") (foreach t (car c) (princ (strcat " " t))) (princ " ]")
    (princ "\n  Стало: [") (foreach t (cadr c) (princ (strcat " " t))) (princ " ]"))

  (princ "\n=== EL-DIFF завершён ===")
  (princ)
)
