(vl-load-com)

(defun c:AW33 (/ regex std-colors ignore-words grand-wires grand-terms 
               selSet i ent obj txt minp maxp p1 p2 cx cy rawData 
               anchors cleanAnchors prevY bounds yTop yBot rows currentRow 
               page-wires page-terms lastSize lastColor color size lengthSum 
               rowQty match matches numStr pt space tbl-wires tbl-terms rowIdx 
               pt2 k-c exist cleanTermName qty upperTxt item pageCount)
  
  (setq regex (vlax-create-object "VBScript.RegExp"))
  (vlax-put-property regex 'Global -1)
  (vlax-put-property regex 'IgnoreCase -1)

  ;; --- СЛОВАРИ ---
  (setq std-colors '("КРАСН" "СИН" "ЧЕРН" "БЕЛ" "ЖЕЛТ" "ЗЕЛ" "СЕР" "КОРИЧ" 
                     "ОРАНЖ" "ФИОЛЕТ" "РОЗОВ" "ГОЛУБ" "Ж/З" "ПРОЗР" "САЛАТ" "БИРЮЗ"))
  
  ;; Жесткий игнор-лист (все технические надписи и штампы)
  (setq ignore-words '("ЛУЖЕН" "ЗАЧИС" "ПАЙКА" "ПРИКЛЕПАТЬ" "ВЫВОД" "ВЕНТИЛЯТОР" 
                       "ИЗМ" "ЛИСТ" "ДОКУМ" "ПОДПИСЬ" "ДАТА" "КОП" "ФОРМАТ" "МАССА" 
                       "МАСШТАБ" "РАЗРАБ" "ПРОВ" "Т.КОНТР" "Н.КОНТР" "УТВ" "ПЕРЕГРЕВ" 
                       "СПРАВ" "ПЕРВОЕ ПРИМЕНЕНИЕ" "ТЕПЛОМАШ" "ООО"))

  (setq grand-wires '() grand-terms '())
  (setq pageCount 1)

  ;; ==========================================
  ;; ГЛАВНЫЙ ЦИКЛ ПОСТРАНИЧНОЙ ОБРАБОТКИ
  ;; ==========================================
  (while (progn
           (prompt (strcat "\n--- ЛИСТ " (itoa pageCount) " ---"))
           (prompt "\nВыделите провода НА ОДНОМ ЛИСТЕ (или нажмите Enter для вывода ИТОГА): ")
           (setq selSet (ssget '((0 . "TEXT,MTEXT"))))
         )
    
    (setq rawData '() page-wires '() page-terms '())
    
    ;; 1. Извлекаем тексты текущего листа
    (setq i 0)
    (repeat (sslength selSet)
      (setq ent (ssname selSet i) obj (vlax-ename->vla-object ent))
      (setq txt (vla-get-textstring obj))
      
      (vlax-put-property regex 'Pattern "\\\\[A-Za-z0-9][^;]*;|[{}\\\\]")
      (setq txt (vlax-invoke-method regex 'Replace txt ""))
      (setq txt (vl-string-trim " \t\n\r" txt))
      (while (vl-string-search "?" txt) (setq txt (vl-string-subst "2" "?" txt)))
      (while (vl-string-search "ММ?" (strcase txt)) (setq txt (vl-string-subst "2" "?" txt)))

      (if (/= txt "")
        (if (not (vl-catch-all-error-p (vl-catch-all-apply 'vla-getboundingbox (list obj 'minp 'maxp))))
          (progn
            (setq p1 (vlax-safearray->list minp) p2 (vlax-safearray->list maxp)
                  cy (/ (+ (cadr p1) (cadr p2)) 2.0) cx (/ (+ (car p1) (car p2)) 2.0))
            (setq rawData (cons (list cy cx txt) rawData))
          )
        )
      )
      (setq i (1+ i))
    )

    ;; 2. Поиск якорей (сечений ММ2) для создания КОРИДОРОВ (защита от смешивания)
    (setq anchors '())
    (foreach item rawData
      (if (or (vl-string-search "ММ2" (strcase (caddr item))) (vl-string-search "MM2" (strcase (caddr item))))
        (setq anchors (cons item anchors))
      )
    )

    (if (= (length anchors) 0)
      (princ "\n[ВНИМАНИЕ] На этом листе не найдено текстов 'мм2'. Скрипт не может разделить провода. Пропуск.")
      (progn
        ;; Сортируем якоря сверху вниз
        (setq anchors (vl-sort anchors '(lambda (a b) (> (car a) (car b)))))
        (setq cleanAnchors '() prevY nil)
        (foreach a anchors
          (if (or (not prevY) (> (abs (- prevY (car a))) 10.0))
            (progn (setq cleanAnchors (cons a cleanAnchors)) (setq prevY (car a)))
          )
        )
        (setq anchors (vl-sort cleanAnchors '(lambda (a b) (> (car a) (car b)))))

        ;; Строим границы коридоров
        (setq bounds '() i 0)
        (while (< i (1- (length anchors)))
          (setq bounds (cons (/ (+ (car (nth i anchors)) (car (nth (1+ i) anchors))) 2.0) bounds))
          (setq i (1+ i))
        )
        (setq bounds (reverse bounds))

        ;; Распределяем тексты по коридорам
        (setq rows '() i 0)
        (foreach a anchors
          (setq yTop (if (= i 0) 1e99 (nth (1- i) bounds)))
          (setq yBot (if (= i (length bounds)) -1e99 (nth i bounds)))
          (setq currentRow '())
          (foreach item rawData
            (if (and (<= (car item) yTop) (> (car item) yBot))
              (setq currentRow (cons item currentRow))
            )
          )
          (setq rows (cons currentRow rows))
          (setq i (1+ i))
        )

        ;; 3. Анализ коридоров
        (setq lastSize "Не указан" lastColor "Не указан") ; СБРОС ПАМЯТИ ДЛЯ НОВОГО ЛИСТА

        (foreach row rows
          (setq color "" size "" lengthSum 0.0 rowQty 1)

          (foreach item row
            (setq txt (caddr item) upperTxt (strcase txt))

            ;; Количество проводов в строке: "N шт" / "(N шт)" / "Nшт"
            (if (vl-string-search "ШТ" upperTxt)
              (progn
                (vlax-put-property regex 'Pattern "[0-9]+(?=\\s*[шШ][тТ])")
                (setq matches (vlax-invoke-method regex 'Execute upperTxt))
                (if (> (vlax-get-property matches 'Count) 0)
                  (setq rowQty (max rowQty (atoi (vlax-get-property (vlax-invoke-method matches 'Item 0) 'Value))))))))

            (cond
              ((vl-string-search ":" upperTxt)) ; Адреса
              ((vl-some '(lambda (kw) (vl-string-search kw upperTxt)) ignore-words)) ; Мусор
              
              ((or (vl-string-search "ММ2" upperTxt) (vl-string-search "MM2" upperTxt))
               (setq size txt)
               ;; Множитель в сечении: "2х1,5 мм²" -> qty=2, size="1,5 мм²"
               (vlax-put-property regex 'Pattern "^([0-9]+)\\s*[xхXХ]")
               (setq matches (vlax-invoke-method regex 'Execute upperTxt))
               (if (> (vlax-get-property matches 'Count) 0)
                 (progn
                   (setq rowQty (max rowQty (atoi (vlax-get-property (vlax-invoke-method matches 'Item 0) 'Value))))
                   (vlax-put-property regex 'Pattern "^[0-9]+\\s*[xхXХ]")
                   (setq size (vlax-invoke-method regex 'Replace size ""))))
              
              ((or (vl-string-search "СМ" upperTxt) (vl-string-search "CM" upperTxt))
               (vlax-put-property regex 'Pattern "\\d+([\\.,]\\d+)?")
               (setq matches (vlax-invoke-method regex 'Execute txt))
               (if (> (vlax-get-property matches 'Count) 0)
                 (vlax-for match matches
                   (setq numStr (vlax-invoke-method (progn (vlax-put-property regex 'Pattern ",") regex) 'Replace (vlax-get-property match 'Value) "."))
                   (setq lengthSum (+ lengthSum (atof numStr)))
                   (vlax-put-property regex 'Pattern "\\d+([\\.,]\\d+)?")
                 )
               )
              )

              ((vl-some '(lambda (kw) (vl-string-search kw upperTxt)) std-colors)
               (if (= color "") (setq color txt)) ; Берем только первый цвет
              )
              
              ;; Детали
              (t
               (vlax-put-property regex 'Pattern "[a-zA-Zа-яА-Я]")
               (if (= (vlax-invoke-method regex 'Test txt) :vlax-true)
                 (progn
                   (setq qty 1 cleanTermName txt)
                   (vlax-put-property regex 'Pattern "\\d+(?=\\s*[шШ][тТ])")
                   (setq matches (vlax-invoke-method regex 'Execute cleanTermName))
                   (if (> (vlax-get-property matches 'Count) 0) (setq qty (atoi (vlax-get-property (vlax-invoke-method matches 'Item 0) 'Value))))
                   (vlax-put-property regex 'Pattern "\\(?\\d+\\s*[шШ][тТ]\\.?\\)?")
                   (setq cleanTermName (vlax-invoke-method regex 'Replace cleanTermName ""))
                   (vlax-put-property regex 'Pattern "\\d+(?=\\s*[xхXХ])")
                   (setq matches (vlax-invoke-method regex 'Execute cleanTermName))
                   (if (> (vlax-get-property matches 'Count) 0) (setq qty (atoi (vlax-get-property (vlax-invoke-method matches 'Item 0) 'Value))))
                   (vlax-put-property regex 'Pattern "\\d+\\s*[xхXХ]")
                   (setq cleanTermName (vlax-invoke-method regex 'Replace cleanTermName ""))
                   (setq cleanTermName (vl-string-trim " -_()[]\t\n\r.:" cleanTermName))

                   ;; Защита от слишком длинного мусорного текста (> 25 символов)
                   (if (and (/= cleanTermName "") (< (strlen cleanTermName) 25))
                     (if (setq exist (assoc cleanTermName page-terms))
                       (setq page-terms (subst (cons cleanTermName (+ (cdr exist) qty)) exist page-terms))
                       (setq page-terms (cons (cons cleanTermName qty) page-terms))
                     )
                   )
                 )
               )
              )
            )
          )
          
          ;; Наследование внутри страницы
          (if (/= size "") (setq lastSize size) (setq size lastSize))
          (if (/= color "") (setq lastColor color) (setq color lastColor))

          ;; Умножение длины на количество проводов в строке (N шт / Nх<сечение>)
          (if (> rowQty 1) (setq lengthSum (* lengthSum rowQty)))

          ;; Запись в итог страницы
          (if (> lengthSum 0.0)
            (progn
              (setq k-c (list color size rowQty))
              (if (setq exist (assoc k-c page-wires))
                (setq page-wires (subst (cons k-c (+ (cdr exist) lengthSum)) exist page-wires))
                (setq page-wires (cons (cons k-c lengthSum) page-wires))
              )
            )
          )
        )

        ;; 4. ВСТАВКА ТАБЛИЦ ТЕКУЩЕГО ЛИСТА
        (if (or page-wires page-terms)
          (progn
            (setq pt (getpoint (strcat "\nКликните точку для таблиц ЛИСТА " (itoa pageCount) ": ")))
            (if pt
              (progn
                (setq space (vla-get-Block (vla-get-ActiveLayout (vla-get-ActiveDocument (vlax-get-acad-object)))))
                
(if page-wires
                  (progn
                    (setq tbl-wires (vla-AddTable space (vlax-3d-point pt) (+ (length page-wires) 2) 4 8 50))
                    (vla-SetText tbl-wires 0 0 (strcat "Провода (Лист " (itoa pageCount) ")"))
                    (vla-SetText tbl-wires 1 0 "Марка / Цвет")
                    (vla-SetText tbl-wires 1 1 "Сечение")
                    (vla-SetText tbl-wires 1 2 "Кол-во")
                    (vla-SetText tbl-wires 1 3 "Длина, см")
                    (setq page-wires (vl-sort page-wires '(lambda (a b) (< (caar a) (caar b)))))
                    (setq rowIdx 2)
                    (foreach item page-wires
                      (vla-SetText tbl-wires rowIdx 0 (nth 0 (car item)))
                      (vla-SetText tbl-wires rowIdx 1 (nth 1 (car item)))
                      (vla-SetText tbl-wires rowIdx 2 (itoa (nth 2 (car item))))
                      (vla-SetText tbl-wires rowIdx 3 (rtos (cdr item) 2 1))
(setq rowIdx (1+ rowIdx))
                    )
                  )
                )

                (if page-terms
                  (progn
                    (setq pt2 (list (+ (car pt) 180.0) (cadr pt) (caddr pt)))
                    (setq tbl-terms (vla-AddTable space (vlax-3d-point pt2) (+ (length page-terms) 2) 2 8 80))
                    (vla-SetText tbl-terms 0 0 (strcat "Детали (Лист " (itoa pageCount) ")"))
                    (vla-SetText tbl-terms 1 0 "Наименование")
                    (vla-SetText tbl-terms 1 1 "Кол-во, шт")
                    (setq page-terms (vl-sort page-terms '(lambda (a b) (< (car a) (car b)))))
                    (setq rowIdx 2)
                    (foreach item page-terms
                      (vla-SetText tbl-terms rowIdx 0 (car item))
                      (vla-SetText tbl-terms rowIdx 1 (itoa (cdr item)))
                      (setq rowIdx (1+ rowIdx))
                    )
                  )
                )
              )
            )

            ;; 5. ДОБАВЛЯЕМ ДАННЫЕ В ИТОГОВЫЙ КОТЕЛ
            (foreach item page-wires
              (setq k-c (car item))
              (if (setq exist (assoc k-c grand-wires))
                (setq grand-wires (subst (cons k-c (+ (cdr exist) (cdr item))) exist grand-wires))
                (setq grand-wires (cons item grand-wires))
              )
            )
            (foreach item page-terms
              (setq k-c (car item))
              (if (setq exist (assoc k-c grand-terms))
                (setq grand-terms (subst (cons k-c (+ (cdr exist) (cdr item))) exist grand-terms))
                (setq grand-terms (cons item grand-terms))
              )
            )
            (setq pageCount (1+ pageCount))
          )
        )
      )
    )
  )

  ;; ==========================================
  ;; ГЕНЕРАЦИЯ ИТОГОВОЙ ТАБЛИЦЫ
  ;; ==========================================
  (if (or grand-wires grand-terms)
    (progn
      (setq pt (getpoint "\n=== ВЫДЕЛЕНИЕ ЗАВЕРШЕНО ===\nКликните точку для ГЛАВНЫХ ИТОГОВЫХ ТАБЛИЦ: "))
      (if pt
        (progn
          (setq space (vla-get-Block (vla-get-ActiveLayout (vla-get-ActiveDocument (vlax-get-acad-object)))))
          
          (if grand-wires
            (progn
              (setq tbl-wires (vla-AddTable space (vlax-3d-point pt) (+ (length grand-wires) 2) 4 8 50))
              (vla-SetText tbl-wires 0 0 "ИТОГО: ВСЕ ПРОВОДА")
              (vla-SetText tbl-wires 1 0 "Марка / Цвет")
              (vla-SetText tbl-wires 1 1 "Сечение")
              (vla-SetText tbl-wires 1 2 "Кол-во")
              (vla-SetText tbl-wires 1 3 "Общая длина, см")
              (setq grand-wires (vl-sort grand-wires '(lambda (a b) (< (caar a) (caar b)))))
              (setq rowIdx 2)
              (foreach item grand-wires
                (vla-SetText tbl-wires rowIdx 0 (nth 0 (car item)))
                (vla-SetText tbl-wires rowIdx 1 (nth 1 (car item)))
                (vla-SetText tbl-wires rowIdx 2 (itoa (nth 2 (car item))))
                (vla-SetText tbl-wires rowIdx 3 (rtos (cdr item) 2 1))
                (setq rowIdx (1+ rowIdx))
              )
              (vla-put-color tbl-wires 3) ; Делаем итоговую таблицу зеленой для визуального отличия
            )
          )

          (if grand-terms
            (progn
              (setq pt2 (list (+ (car pt) 180.0) (cadr pt) (caddr pt)))
              (setq tbl-terms (vla-AddTable space (vlax-3d-point pt2) (+ (length grand-terms) 2) 2 8 80))
              (vla-SetText tbl-terms 0 0 "ИТОГО: ВСЕ ДЕТАЛИ")
              (vla-SetText tbl-terms 1 0 "Наименование")
              (vla-SetText tbl-terms 1 1 "Общее кол-во, шт")
              (setq grand-terms (vl-sort grand-terms '(lambda (a b) (< (car a) (car b)))))
              (setq rowIdx 2)
              (foreach item grand-terms
                (vla-SetText tbl-terms rowIdx 0 (car item))
                (vla-SetText tbl-terms rowIdx 1 (itoa (cdr item)))
                (setq rowIdx (1+ rowIdx))
              )
              (vla-put-color tbl-terms 3)
            )
          )
          (princ "\nИтоговые таблицы успешно созданы!")
        )
      )
    )
    (princ "\nДанные не были собраны.")
  )
  
  (vlax-release-object regex)
  (princ)
)