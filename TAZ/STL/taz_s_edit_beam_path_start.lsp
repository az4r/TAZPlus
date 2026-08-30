;; =============================================================
;; taz_s_edit_beam_path_start
;; Otwiera tryb edycji ścieżki wyciągnięcia belki.
;; Rysuje linię sterującą, ustawia reaktor nasłuchujący
;; zmian tej linii (grip, move, rotate, stretch).
;; Zakończenie edycji: c:taz_s_edit_beam_path_stop
;; =============================================================

(setq taz_s_edit_beam_path_reactor nil)

;; -------------------------------------------------------------
;; CALLBACK REAKTORA – wywoływany po każdej zakończonej komendzie
;; -------------------------------------------------------------

(defun taz_s_edit_beam_path_callback (reactor params / taz_s_cmd taz_s_line_now_p1 taz_s_line_now_p2 taz_s_line_count)

  ;; pobierz nazwę zakończonej komendy
  (setq taz_s_cmd (car params))

  ;; reaguj tylko jeśli tryb edycji ścieżki jest aktywny
  (if (not taz_s_edit_beam_path_mode)
    (princ)
    (progn

      ;; sprawdź czy linia sterująca nadal istnieje
      (if (not (and taz_s_attribs_line (entget taz_s_attribs_line)))
        (progn
          (princ "\nLinia sterująca została usunięta - edycja przerwana.")
          (setq taz_s_edit_beam_path_mode nil)
        )
        (progn

          ;; ---------------------------------------------------------
          ;; OBSŁUGA KOPIOWANIA LINII – usuń kopię, nie generuj bryły
          ;; Jeżeli użytkownik skopiował linię sterującą (COPY/COPYCLIP
          ;; itp.), w modelu pojawi się dodatkowa linia. Wykrywamy to
          ;; sprawdzając czy ostatni obiekt w rysunku to linia różna
          ;; od naszej linii sterującej i czy jest na warstwie edycji.
          ;; ---------------------------------------------------------

          (if (or (= taz_s_cmd "COPY")
                  (= taz_s_cmd "_COPY")
                  (= taz_s_cmd "COPYCLIP")
                  (= taz_s_cmd "_COPYCLIP")
                  (= taz_s_cmd "PASTECLIP")
                  (= taz_s_cmd "_PASTECLIP"))
            (progn
              (command "-VIEW" "_S" "taz_s_current_view")
              (setq taz_s_cleanup_layer (ssget "_X" (list (cons 8 (getvar "CLAYER")) '(-4 . "<NOT") (cons 5 (cdr (assoc 5 (entget taz_s_attribs_line)))) '(-4 . "NOT>"))))
              (command "_.ERASE" taz_s_cleanup_layer "")
            )

            ;; ---------------------------------------------------------
            ;; OBSŁUGA EDYCJI GEOMETRII LINII – reaguj na grip/move/rotate/stretch
            ;; ---------------------------------------------------------

            (if (or (= taz_s_cmd "GRIP_MOVE")
                    (= taz_s_cmd "_GRIP_MOVE")
                    (= taz_s_cmd "MOVE")
                    (= taz_s_cmd "_MOVE")
                    (= taz_s_cmd "ROTATE")
                    (= taz_s_cmd "_ROTATE")
                    (= taz_s_cmd "ROTATE3D")
                    (= taz_s_cmd "_ROTATE3D")
                    (= taz_s_cmd "STRETCH")
                    (= taz_s_cmd "_STRETCH")
                    (= taz_s_cmd "GRIP_STRETCH")
                    (= taz_s_cmd "_GRIP_STRETCH"))
              (progn
                
                (command "-VIEW" "_S" "taz_s_current_view")

                ;; pobierz nowe punkty linii sterującej
                (setq taz_s_line_now_p1 (cdr (assoc 10 (entget taz_s_attribs_line))))
                (setq taz_s_line_now_p2 (cdr (assoc 11 (entget taz_s_attribs_line))))

                ;; ustaw nowe punkty ścieżki dla create_beam
                (setq taz_s_edit_new_path_p1 taz_s_line_now_p1)
                (setq taz_s_edit_new_path_p2 taz_s_line_now_p2)

                ;; ---------------------------------------------------------
                ;; GENERUJ NOWĄ BRYŁĘ
                ;; (z zachowaniem starego kąta i pozycji przekroju)
                ;; ---------------------------------------------------------

                (c:taz_s_create_beam)

                ;; ---------------------------------------------------------
                ;; NOWA BRYŁA – pobierz jej handle
                ;; ---------------------------------------------------------

                (setq taz_s_attribs_object_new (entlast))
                (setq taz_s_attribs_object_name_new
                      (cdr (assoc 5 (entget taz_s_attribs_object_new))))

                ;; nadaj transparency 85 (tryb edycji)
                ;;(command "_CHPROP" taz_s_attribs_object_new "" "_P" "_TR" "85" "")

                ;; ---------------------------------------------------------
                ;; PRZENIEŚ ATRYBUTY ZE STAREJ BRYŁY NA NOWĄ
                ;; ---------------------------------------------------------

                (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr1"))  taz_s_attr1_old)
                (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr2"))  taz_s_attr2_old)
                (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr3"))  taz_s_attr3_old)
                (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr4"))  taz_s_attr4_old)
                (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr5"))  taz_s_attr5_old)
                (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr6"))  taz_s_attr6_old)
                (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr7"))  taz_s_attr7_old)
                (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr8"))  taz_s_attr8_old)
                (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr9"))  taz_s_attr9_old)
                (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr10")) taz_s_attr10_old)
                (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_section_angle"))    taz_s_section_angle_old)
                (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_section_position")) taz_s_section_position_old)

                ;; ---------------------------------------------------------
                ;; Usuń starą bryłę
                ;; ---------------------------------------------------------

                (command "_LAYER" "_U" "taz_s_beam" "")
                (if (and taz_s_attribs_object_old (entget taz_s_attribs_object_old))
                  (entdel taz_s_attribs_object_old)
                )
                (command "_LAYER" "_LO" "taz_s_beam" "")

                ;; ---------------------------------------------------------
                ;; Ustaw nową bryłę jako aktualną
                ;; ---------------------------------------------------------

                (setq taz_s_attribs_object      taz_s_attribs_object_new)
                (setq taz_s_attribs_object_name taz_s_attribs_object_name_new)
                (setq taz_s_attribs_object_old  taz_s_attribs_object_new)

                ;; ---------------------------------------------------------
                ;; ZAPIS DANYCH DO PLIKU
                ;; ---------------------------------------------------------

                (setq taz_s_f_beam_data (open taz_s_data_file "a"))

                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr1 \""           taz_s_attr1_old "\")") taz_s_f_beam_data)
                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr2 \""           taz_s_attr2_old "\")") taz_s_f_beam_data)
                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr3 \""           taz_s_attr3_old "\")") taz_s_f_beam_data)
                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr4 \""           taz_s_attr4_old "\")") taz_s_f_beam_data)
                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr5 \""           taz_s_attr5_old "\")") taz_s_f_beam_data)
                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr6 \""           taz_s_attr6_old "\")") taz_s_f_beam_data)
                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr7 \""           taz_s_attr7_old "\")") taz_s_f_beam_data)
                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr8 \""           taz_s_attr8_old "\")") taz_s_f_beam_data)
                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr9 \""           taz_s_attr9_old "\")") taz_s_f_beam_data)
                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr10 \""          taz_s_attr10_old "\")") taz_s_f_beam_data)
                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_section_angle "    (rtos taz_s_section_angle_old 2 6) ")") taz_s_f_beam_data)
                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_section_position " (itoa taz_s_section_position_old) ")") taz_s_f_beam_data)
                
                ;;(write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_sweep_p1 (list " (rtos taz_s_p1x 2 6) " " (rtos taz_s_p1y 2 6) " " (rtos taz_s_p1z 2 6) "))") taz_s_f_beam_data)
                ;;(write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_sweep_p2 (list " (rtos taz_s_p2x 2 6) " " (rtos taz_s_p2y 2 6) " " (rtos taz_s_p2z 2 6) "))") taz_s_f_beam_data)
                
                ;; -- punkt początkowy ścieżki sterującej --
                (setq taz_s_p1x (car taz_s_edit_new_path_p1))
                (setq taz_s_p1y (cadr taz_s_edit_new_path_p1))
                (setq taz_s_p1z (caddr taz_s_edit_new_path_p1))
                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_sweep_p1 (list " (rtos taz_s_p1x 2 6) " " (rtos taz_s_p1y 2 6) " " (rtos taz_s_p1z 2 6) "))") taz_s_f_beam_data)

                ;; -- punkt końcowy ścieżki sterującej --
                (setq taz_s_p2x (car taz_s_edit_new_path_p2))
                (setq taz_s_p2y (cadr taz_s_edit_new_path_p2))
                (setq taz_s_p2z (caddr taz_s_edit_new_path_p2))
                (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_sweep_p2 (list " (rtos taz_s_p2x 2 6) " " (rtos taz_s_p2y 2 6) " " (rtos taz_s_p2z 2 6) "))") taz_s_f_beam_data)

                (close taz_s_f_beam_data)
                
                ;;(setq taz_s_data_file taz_s_f_beam_data)
                ;;(taz_s_cleanup_data_file)

                (setq taz_s_edit_new_path_p1 nil)
                (setq taz_s_edit_new_path_p2 nil)
                (princ "\nNowa bryła wygenerowana – linia sterująca gotowa do kolejnej edycji.")
              )
              (princ)
            )
          )
        )
      )
    )
  )
  (princ)
)

;; =============================================================
;; GŁÓWNA KOMENDA STARTOWA
;; =============================================================

(defun c:taz_s_edit_beam_path_start ( / )
  
  (setq taz_s_attribs_line nil)

  (taz_s_current_settings_save)
  (command "_LAYER" "_U" "taz_s_editing_layer" "")
  (command "_LAYER" "_S" "taz_s_editing_layer" "")

  ;; ---------------------------------------------------------
  ;; SCIEZKA DO PLIKU DANYCH I WCZYTANIE
  ;; ---------------------------------------------------------

  (setq taz_s_data_file
    (strcat (taz_s_path) "taz_s_beam_data.txt"))

  (load taz_s_data_file)

  ;; TRYB EDYCJI

  (setq taz_s_edit_mode T)
  (setq taz_s_edit_beam_path_mode T)

  ;; Reset UCS do World
  (command "_.UCS" "_W")

  ;; pobierz aktualną selekcję
  (setq taz_s_attribs_selection (ssget "_I"))

  ;; jeśli zaznaczono więcej niż jeden obiekt → wymuś wybór jednego
  (if (and taz_s_attribs_selection
           (> (sslength taz_s_attribs_selection) 1))
    (progn
      (sssetfirst nil nil)
      (setq taz_s_attribs_selection (ssget "_+.:E:S"))
    )
  )

  ;; jeśli brak selekcji → poproś o wskazanie
  (if (null taz_s_attribs_selection)
    (setq taz_s_attribs_selection (ssget "_+.:E:S"))
  )

  ;; jeśli nadal brak selekcji → zakończ
  (if (null taz_s_attribs_selection)
    (progn
      (print "Nie wybrano obiektu.")
      (exit)
    )
  )

  ;; pobierz obiekt
  (setq taz_s_attribs_object (ssname taz_s_attribs_selection 0))

  ;; zapamiętaj starą bryłę (tylko raz)
  (if (not taz_s_attribs_object_old)
    (setq taz_s_attribs_object_old taz_s_attribs_object)
  )

  ;; sprawdź typ
  (if (/= (cdr (assoc 0 (entget taz_s_attribs_object))) "3DSOLID")
    (progn
      (print "Wybrany obiekt nie jest bryłą 3D.")
      (exit)
    )
  )

  ;; pobierz nazwę obiektu (handle)
  (setq taz_s_attribs_object_name
        (cdr (assoc 5 (entget taz_s_attribs_object))))

  ;; pobierz zapisane punkty ścieżki
  (setq taz_s_edit_p1
        (eval (read
               (strcat "taz_s_"
                       taz_s_attribs_object_name
                       "_sweep_p1"))))

  (setq taz_s_edit_p2
        (eval (read
               (strcat "taz_s_"
                       taz_s_attribs_object_name
                       "_sweep_p2"))))

  ;; narysuj linię sterującą
  (command "_ZOOM" "_SCALE" "10000X")
  (command "_LINE" taz_s_edit_p1 taz_s_edit_p2 "")
  (command "_ZOOM" "_SCALE" "0.0001X")
  (setq taz_s_attribs_line (entlast))

  ;; ustaw kolor czerwony
  (command "_CHPROP" taz_s_attribs_line "" "_P" "_C" "1" "")

  ;; ustaw transparency 85 na bryle
  ;;(command "_CHPROP" taz_s_attribs_object "" "_P" "_TR" "85" "")

  ;; ---------------------------------------------------------
  ;; POBIERZ ATRYBUTY STAREJ BRYŁY (PRZED create_beam!)
  ;; ---------------------------------------------------------

  (setq taz_s_attr1_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr1"))))
  (setq taz_s_attr2_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr2"))))
  (setq taz_s_attr3_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr3"))))
  (setq taz_s_attr4_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr4"))))
  (setq taz_s_attr5_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr5"))))
  (setq taz_s_attr6_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr6"))))
  (setq taz_s_attr7_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr7"))))
  (setq taz_s_attr8_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr8"))))
  (setq taz_s_attr9_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr9"))))
  (setq taz_s_attr10_old (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr10"))))
  (setq taz_s_section_position_old (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))))
  (setq taz_s_section_angle_old    (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle"))))

  ;;; FIX: Ustaw globalne zmienne taz_s_family / taz_s_type / taz_s_category
  (setq taz_s_family taz_s_attr6_old)
  (setq taz_s_type   taz_s_attr7_old)

  ;;; FIX: Odtwórz taz_s_category na podstawie rodziny profilu
  (cond
    ((or (= taz_s_family "HEA")
         (= taz_s_family "HEB")
         (= taz_s_family "IPE")
         (= taz_s_family "IPN"))
     (setq taz_s_category "Dwuteowniki"))
    ((or (= taz_s_family "UPE")
         (= taz_s_family "UPN"))
     (setq taz_s_category "Ceowniki"))
    ((or (= taz_s_family "LR")
         (= taz_s_family "LN"))
     (setq taz_s_category "Katowniki"))
    ((or (= taz_s_family "SHS")
         (= taz_s_family "RHS")
         (= taz_s_family "CHS"))
     (setq taz_s_category "Rury"))
  )

  ;; ---------------------------------------------------------
  ;; Włącz reaktor jeśli jeszcze nie istnieje
  ;; ---------------------------------------------------------

  (if (not taz_s_edit_beam_path_reactor)
    (setq taz_s_edit_beam_path_reactor
      (vlr-command-reactor
        "taz_s_edit_beam_path_reactor"
        '((:vlr-commandEnded . taz_s_edit_beam_path_callback))
      )
    )
  )

  (princ "\nEdycja ścieżki otwarta - przesuń, obróć lub rozciągnij linię sterującą.")
  (princ "\nAby zakończyć edycję wpisz: taz_s_edit_beam_path_stop")
  
  (princ)

)
