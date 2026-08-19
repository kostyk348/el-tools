(vl-load-com)

;;; ------------------------------------------------------------
;;; AutoWires (AW)
;;; Обработка по страницам:
;;; 1. Выделяете ТОЛЬКО одну страницу
;;; 2. Скрипт считает её отдельно
;;; 3. Спрашивает: добавить ещё страницу?
;;; 4. После последней страницы делает ОБЩУЮ финальную сводку
;;; ------------------------------------------------------------

(defun AW-ProcessSelection (selSet summary-wires summary-terms / 
        i ent obj txt minp maxp p1 p2 cx cy rawData
        regex rows anchors cleanAnchors prevY bounds
        currentRow yTop yBot upperTxt
        std-colors color size lengthSum rowQty
        matches match numStr qty cleanTermName exist
        k-c)

  (setq regex (vlax-create-object "VBScript.RegExp"))
  (vlax-put-property regex 'Global -1)
  (vlax-put-property regex 'IgnoreCase -1)

  ;; --- стандартные цвета ---
  (setq std-colors
    '(
      "КРАСН" "СИН" "ЧЕРН" "БЕЛ" "ЖЕЛТ"
      "ЗЕЛ" "СЕР" "КОРИЧ" "ОРАНЖ"
      "ФИОЛЕТ" "РОЗОВ" "ГОЛУБ"
      "Ж/З" "ПРОЗР" "САЛАТ" "БИРЮЗ"
     )
  )

  (setq rawData '())

  ;; ------------------------------------------------------------
  ;; ШАГ 1. Считываем все тексты
  ;; ------------------------------------------------------------

  (setq i 0)
  (repeat (sslength selSet)

    (setq ent (ssname selSet i)
          obj (vlax-ename->vla-object ent))

    (setq txt (vla-get-TextString obj))

    ;; очистка форматирования MTEXT
    (vlax-put-property regex 'Pattern "\\\\[A-Za-z0-9][^;]*;|[{}\\\\]")
    (setq txt (vlax-invoke-method regex 'Replace txt ""))
    (setq txt (vl-string-trim " \t\n\r" txt))

    ;; исправление квадрата
    (while (vl-string-search "?" txt)
      (setq txt (vl-string-subst "2" "?" txt))
    )

    (if (/= txt "")
      (if (not
            (vl-catch-all-error-p
              (vl-catch-all-apply
                'vla-getBoundingBox
                (list obj 'minp 'maxp)
              )
            )
          )
        (progn
          (setq p1 (vlax-safearray->list minp)
                p2 (vlax-safearray->list maxp)
                cy (/ (+ (cadr p1) (cadr p2)) 2.0)
                cx (/ (+ (car p1) (car p2)) 2.0))

          (setq rawData
            (cons (list cy cx txt) rawData)
          )
        )
      )
    )

    (setq i (1+ i))
  )

  ;; ------------------------------------------------------------
  ;; ШАГ 2. Ищем якоря строк (мм2)
  ;; ------------------------------------------------------------

  (setq anchors '())

  (foreach item rawData
    (if (or
          (vl-string-search "ММ2" (strcase (caddr item)))
          (vl-string-search "MM2" (strcase (caddr item)))
        )
      (setq anchors (cons item anchors))
    )
  )

  (if (= (length anchors) 0)
    (progn
      (vlax-release-object regex)
      (list summary-wires summary-terms)
    )
    (progn

      ;; сортировка сверху вниз
      (setq anchors
        (vl-sort anchors '(lambda (a b) (> (car a) (car b))))
      )

      ;; удаление дублей
      (setq cleanAnchors '()
            prevY nil)

      (foreach a anchors
        (if (or
              (not prevY)
              (> (abs (- prevY (car a))) 10.0)
            )
          (progn
            (setq cleanAnchors (cons a cleanAnchors))
            (setq prevY (car a))
          )
        )
      )

      (setq anchors
        (vl-sort cleanAnchors '(lambda (a b) (> (car a) (car b))))
      )

      ;; --------------------------------------------------------
      ;; ШАГ 3. Коридоры строк
      ;; --------------------------------------------------------

      (setq bounds '()
            i 0)

      (while (< i (1- (length anchors)))
        (setq bounds
          (cons
            (/ (+ (car (nth i anchors))
                  (car (nth (1+ i) anchors)))
               2.0)
            bounds
          )
        )
        (setq i (1+ i))
      )

      (setq bounds (reverse bounds))
      (setq rows '()
            i 0)

      (foreach a anchors

        (setq yTop (if (= i 0) 1e99 (nth (1- i) bounds)))
        (setq yBot (if (= i (length bounds)) -1e99 (nth i bounds)))

        (setq currentRow '())

        (foreach item rawData
          (setq cy (car item))
          (if (and (<= cy yTop) (> cy yBot))
            (setq currentRow (cons item currentRow))
          )
        )

        (setq rows (cons currentRow rows))
        (setq i (1+ i))
      )

      ;; --------------------------------------------------------
      ;; ШАГ 4. Обработка строк
      ;; --------------------------------------------------------

      (foreach row rows

        (setq color ""
              size ""
              lengthSum 0.0
              rowQty 1)

        (foreach item row

          (setq txt (caddr item)
                upperTxt (strcase txt))

          ;; количество проводов в строке (N шт / (N шт))
          (if (vl-string-search "ШТ" upperTxt)
            (progn
              (vlax-put-property regex 'Pattern "[0-9]+(?=\\s*[шШ][тТ])")
              (setq matches (vlax-invoke-method regex 'Execute upperTxt))
              (if (> (vlax-get-property matches 'Count) 0)
                (setq rowQty (max rowQty (atoi (vlax-get-property (vlax-invoke-method matches 'Item 0) 'Value)))))))

          (cond

            ;; игнор адресов
            ((vl-string-search ":" upperTxt))

            ;; игнор служебных слов
            ((or
               (vl-string-search "ЛУЖЕН" upperTxt)
               (vl-string-search "ЗАЧИСТ" upperTxt)
             ))

            ;; сечение
            ((or
               (vl-string-search "ММ2" upperTxt)
               (vl-string-search "MM2" upperTxt)
             )
             (setq size txt)
            )

            ;; длина
            ((or
               (vl-string-search "СМ" upperTxt)
               (vl-string-search "CM" upperTxt)
             )

             (vlax-put-property regex 'Pattern "\\d+([\\.,]\\d+)?")
             (setq matches (vlax-invoke-method regex 'Execute txt))

             (if (> (vlax-get-property matches 'Count) 0)
               (vlax-for match matches
                 (setq numStr (vlax-get-property match 'Value))
                 (setq numStr (vl-string-subst "." "," numStr))
                 (setq lengthSum (+ lengthSum (atof numStr)))
               )
             )
            )

            ;; цвет
            ((vl-some
               '(lambda (kw) (vl-string-search kw upperTxt))
               std-colors
             )
             (setq color
               (strcat color (if (= color "") "" " ") txt)
             )
            )

            ;; детали
            (T
             (setq qty 1
                   cleanTermName txt)

             (setq cleanTermName
               (vl-string-trim " -_()[]\t\n\r" cleanTermName)
             )

             (if (/= cleanTermName "")
               (if (setq exist (assoc cleanTermName summary-terms))
                 (setq summary-terms
                   (subst
                     (cons cleanTermName (+ (cdr exist) qty))
                     exist
                     summary-terms
                   )
                 )
                 (setq summary-terms
                   (cons (cons cleanTermName qty) summary-terms)
                 )
               )
             )
            )
          )
        )

        ;; запись провода
        ;; умножение длины на количество проводов в строке
        (if (> rowQty 1) (setq lengthSum (* lengthSum rowQty)))

        (if (> lengthSum 0.0)
          (progn
            (if (= color "") (setq color "Не указан"))
            (if (= size "") (setq size "Не указан"))

            (setq k-c (cons color size))

            (if (setq exist (assoc k-c summary-wires))
              (setq summary-wires
                (subst
                  (cons k-c (+ (cdr exist) lengthSum))
                  exist
                  summary-wires
                )
              )
              (setq summary-wires
                (cons (cons k-c lengthSum) summary-wires)
              )
            )
          )
        )
      )

      (vlax-release-object regex)
      (list summary-wires summary-terms)
    )
  )
)

;;; ------------------------------------------------------------
;;; ГЛАВНАЯ КОМАНДА
;;; ------------------------------------------------------------

(defun c:AW (/ continue selSet result summary-wires summary-terms)

  (setq summary-wires '())
  (setq summary-terms '())
  (setq continue T)

  (while continue

    (prompt "\nВыберите ОДНУ страницу таблицы: ")

    (if (setq selSet (ssget '((0 . "TEXT,MTEXT"))))
      (progn
        (setq result
          (AW-ProcessSelection selSet summary-wires summary-terms)
        )

        (setq summary-wires (car result))
        (setq summary-terms (cadr result))

        (if (/= (getkword "\nДобавить ещё страницу? [Да/Нет] <Да>: ") "Нет")
          (setq continue T)
          (setq continue nil)
        )
      )
      (setq continue nil)
    )
  )

  (prompt "\nВсе выбранные страницы объединены в одну итоговую сводку.")
  (princ)
)
