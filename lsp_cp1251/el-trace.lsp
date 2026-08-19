;; el-trace.lsp — трассировка цепи по клику с подсветкой
;; Зависит от: el-core.lsp
;; Команда: EL-TRACE

(if (not *el:tolerance*) (load "el-core.lsp"))

(defun C:EL-TRACE (/ lines graph e chain texts more)
  (setq lines (el:all-lines))
  (if (< (length lines) 2)
    (progn (princ "\n! Мало LINE в чертеже (нужно >=2)") (princ) (exit)))

  (setq graph (el:build-graph lines))
  (setq more T)
  (princ "\n=== EL-TRACE: кликай на любую LINE цепи ===")
  (princ "\nENTER — выход, клик на линию — трассировка")

  (while more
    (princ "\n> Выбери линию: ")
    (setq e (entsel))
    (if (null e)
      (setq more nil)
      (progn
        (setq e (car e))
        (if (not (eq "LINE" (cdr (assoc 0 (entget e)))))
          (princ "\n! Это не LINE. Выбери линию.")
          (progn
            (setq chain (el:trace e graph))
            (el:hilite chain)
            (princ (strcat "\n=== ЦЕПЬ: " (itoa (length chain)) " отрезков ==="))
            (princ (strcat "\nТексты: "))
            (foreach t (el:chain-texts chain)
              (princ (strcat " [" t "] ")))
            (princ (strcat "\nДлина цепи: "
              (rtos (apply '+
                (mapcar '(lambda (l / ep)
                  (setq ep (el:line-endpoints l))
                  (el:dist (car ep) (cadr ep))) chain)) 2 2) " мм"))
            (princ "\nENTER — снять подсветку, LINE — трассировать другую: ")
            (if (null (entsel))
              (progn (el:unhilite chain) (princ "\nПодсветка снята."))))))))
  (princ)
  (princ "\n; EL-TRACE завершён."))
