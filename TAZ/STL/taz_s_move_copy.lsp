(defun c:taz_s_move_copy ()

  ;; ---------------------------------------------------------
  ;; ZMIENNE GLOBALNE
  ;; ---------------------------------------------------------

  (setq taz_s_move_copy_selection nil)
  (setq taz_s_move_copy_ok T)
  (setq taz_s_move_copy_i 0)
  (setq taz_s_move_copy_ent nil)
  (setq taz_s_move_copy_type nil)

  (setq taz_s_move_copy_mode "1")
  (setq taz_s_move_copy_x "0")
  (setq taz_s_move_copy_y "0")
  (setq taz_s_move_copy_z "0")
  
  (setq taz_s_move_copy_counter 1)   ;; <-- DODANE

  (setq taz_s_move_copy_dialog_result 0)

  (setq taz_s_move_copy_list nil)
  (setq taz_s_move_copy_obj nil)

  ;; ---------------------------------------------------------
  ;; WYBÓR OBIEKTÓW
  ;; ---------------------------------------------------------

  (setq taz_s_move_copy_selection (ssget))

  (if (null taz_s_move_copy_selection)
    (progn
      (alert "Nie wybrano żadnych obiektów.")
      (setq taz_s_move_copy_ok nil)
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; SPRAWDZENIE CZY WSZYSTKIE OBIEKTY TO 3DSOLID
  ;; ---------------------------------------------------------

  (if taz_s_move_copy_ok
    (progn
      (setq taz_s_move_copy_i 0)
      (while (< taz_s_move_copy_i (sslength taz_s_move_copy_selection))

        (setq taz_s_move_copy_ent (ssname taz_s_move_copy_selection taz_s_move_copy_i))
        (setq taz_s_move_copy_type (cdr (assoc 0 (entget taz_s_move_copy_ent))))

        (if (/= taz_s_move_copy_type "3DSOLID")
          (setq taz_s_move_copy_ok nil)
          (princ)
        )

        (setq taz_s_move_copy_i (+ taz_s_move_copy_i 1))
      )
    )
    (princ)
  )

  (if (not taz_s_move_copy_ok)
    (progn
      (if taz_s_move_copy_selection
        (alert "Wszystkie zaznaczone obiekty muszą być bryłami 3D (3DSOLID).")
        (princ)
      )
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; OKNO DIALOGOWE
  ;; Wynik 0 = Anuluj, 1 = OK, 2 = kliknięto "Point"
  ;; Jeśli 2 -> pobieramy punkty, liczymy wektor i otwieramy
  ;; okno ponownie z uzupełnionymi polami.
  ;; ---------------------------------------------------------

  (setq taz_s_move_copy_dialog_result 2)

  (if taz_s_move_copy_ok
    (while (= taz_s_move_copy_dialog_result 2)

      (setq taz_s_move_copy_dcl_id (load_dialog "taz_s_move_copy.dcl"))
      (new_dialog "taz_s_move_copy_dialog" taz_s_move_copy_dcl_id)

      (if (= taz_s_move_copy_mode "1")
        (progn
          (set_tile "taz_s_mode_move" "1")
          (set_tile "taz_s_mode_copy" "0")
        )
        (progn
          (set_tile "taz_s_mode_move" "0")
          (set_tile "taz_s_mode_copy" "1")
        )
      )

      (set_tile "taz_s_x" taz_s_move_copy_x)
      (set_tile "taz_s_y" taz_s_move_copy_y)
      (set_tile "taz_s_z" taz_s_move_copy_z)

      (action_tile "taz_s_mode_move" "(setq taz_s_move_copy_mode \"1\")")
      (action_tile "taz_s_mode_copy" "(setq taz_s_move_copy_mode \"0\")")

      (action_tile "taz_s_point" "(taz_s_move_copy_read_values)(done_dialog 2)")
      (action_tile "accept" "(taz_s_move_copy_read_values)(done_dialog 1)")
      (action_tile "cancel" "(done_dialog 0)")

      (setq taz_s_move_copy_dialog_result (start_dialog))

      (unload_dialog taz_s_move_copy_dcl_id)

      (if (= taz_s_move_copy_dialog_result 2)
        (taz_s_move_copy_pick_vector)
        (princ)
      )

    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; PRZETWORZENIE ZAZNACZONYCH OBIEKTÓW
  ;; ---------------------------------------------------------

  (if (and taz_s_move_copy_ok (= taz_s_move_copy_dialog_result 1))
    (progn

      ;; wektor przesunięcia (ten sam dla wszystkich obiektów)
      (setq taz_s_move_copy_xval (atof taz_s_move_copy_x))
      (setq taz_s_move_copy_yval (atof taz_s_move_copy_y))
      (setq taz_s_move_copy_zval (atof taz_s_move_copy_z))

      (setq taz_s_move_copy_p1 (list 0 0 0))
      (setq taz_s_move_copy_p2 (list taz_s_move_copy_xval taz_s_move_copy_yval taz_s_move_copy_zval))

      ;; zapamiętaj listę encji PRZED jakąkolwiek edycją
      (setq taz_s_move_copy_list nil)
      (setq taz_s_move_copy_i 0)
      (while (< taz_s_move_copy_i (sslength taz_s_move_copy_selection))
        (setq taz_s_move_copy_ent (ssname taz_s_move_copy_selection taz_s_move_copy_i))
        (setq taz_s_move_copy_list (append taz_s_move_copy_list (list taz_s_move_copy_ent)))
        (setq taz_s_move_copy_i (+ taz_s_move_copy_i 1))
      )

      ;; ścieżka do pliku danych i wczytanie
      (setq taz_s_data_file (strcat (taz_s_path) "taz_s_beam_data.txt"))
      (load taz_s_data_file)

      ;; warstwa robocza
      (taz_s_current_settings_save)
      (command "_LAYER" "_U" "taz_s_editing_layer" "")
      (command "_LAYER" "_S" "taz_s_editing_layer" "")

      ;; pętla po wszystkich zaznaczonych obiektach
      (setq taz_s_move_copy_i 0)
      (while (< taz_s_move_copy_i (length taz_s_move_copy_list))

        (setq taz_s_move_copy_obj (nth taz_s_move_copy_i taz_s_move_copy_list))

        (taz_s_move_copy_process_object)

        (setq taz_s_move_copy_i (+ taz_s_move_copy_i 1))
      )

      ;; przywróć warstwę
      (command "_LAYER" "_LO" "taz_s_editing_layer" "")
      (taz_s_current_settings_restore)

    )
    (princ)
  )

  (princ)

)

;; ---------------------------------------------------------
;; ODCZYT WARTOŚCI Z OKNA DIALOGOWEGO
;; ---------------------------------------------------------

(defun taz_s_move_copy_read_values ()

  (setq taz_s_move_copy_x (get_tile "taz_s_x"))
  (setq taz_s_move_copy_y (get_tile "taz_s_y"))
  (setq taz_s_move_copy_z (get_tile "taz_s_z"))

  (if (= taz_s_move_copy_x "") (setq taz_s_move_copy_x "0") (princ))
  (if (= taz_s_move_copy_y "") (setq taz_s_move_copy_y "0") (princ))
  (if (= taz_s_move_copy_z "") (setq taz_s_move_copy_z "0") (princ))

  (princ)

)

;; ---------------------------------------------------------
;; POBRANIE WEKTORA PRZESUNIĘCIA Z DWÓCH PUNKTÓW
;; ---------------------------------------------------------

(defun taz_s_move_copy_pick_vector ()

  (setq taz_s_move_copy_pt1 nil)
  (setq taz_s_move_copy_pt2 nil)

  ;; -----------------------------------------------------------
  ;; Obejście problemu z "widmowym" punktem przy DRUGIM cyklu:
  ;; jeśli licznik ma wartość 2, "pochłaniamy" zalegające
  ;; zdarzenie pustym getpoint przed właściwym pobraniem punktów.
  ;; -----------------------------------------------------------

  (if (= taz_s_move_copy_counter 2)
    (getpoint)
    (princ)
  )

  (setq taz_s_move_copy_pt1 (getpoint "\nPodaj pierwszy punkt wektora przesunięcia: "))

  (if taz_s_move_copy_pt1
    (setq taz_s_move_copy_pt2 (getpoint taz_s_move_copy_pt1 "\nPodaj drugi punkt wektora przesunięcia: "))
    (princ)
  )

  (if (and taz_s_move_copy_pt1 taz_s_move_copy_pt2)
    (progn
      (setq taz_s_move_copy_vecx (- (car taz_s_move_copy_pt2) (car taz_s_move_copy_pt1)))
      (setq taz_s_move_copy_vecy (- (cadr taz_s_move_copy_pt2) (cadr taz_s_move_copy_pt1)))
      (setq taz_s_move_copy_vecz (- (caddr taz_s_move_copy_pt2) (caddr taz_s_move_copy_pt1)))

      (setq taz_s_move_copy_x (rtos taz_s_move_copy_vecx 2 6))
      (setq taz_s_move_copy_y (rtos taz_s_move_copy_vecy 2 6))
      (setq taz_s_move_copy_z (rtos taz_s_move_copy_vecz 2 6))
    )
    (princ)
  )

  (setq taz_s_move_copy_pt1 nil)
  (setq taz_s_move_copy_pt2 nil)

  ;; licznik cykli - inkrementacja na końcu każdego przebiegu
  (setq taz_s_move_copy_counter (+ taz_s_move_copy_counter 1))

  (princ)

)

;; ---------------------------------------------------------
;; PRZETWORZENIE JEDNEGO OBIEKTU (taz_s_move_copy_obj)
;; ---------------------------------------------------------

(defun taz_s_move_copy_process_object ()

  (setq taz_s_attribs_line nil)
  (setq taz_s_attribs_line_new nil)

  ;; TRYB EDYCJI - zachowaj stary kąt i pozycję przekroju
  (setq taz_s_edit_mode T)
  (setq taz_s_edit_beam_path_mode T)

  ;; Reset UCS do World
  (command "_.UCS" "_W")

  ;; sprawdź czy obiekt nadal istnieje
  (if (and taz_s_move_copy_obj (entget taz_s_move_copy_obj))
    (progn

      ;; pobierz obiekt
      (setq taz_s_attribs_object taz_s_move_copy_obj)
      (setq taz_s_attribs_object_old taz_s_attribs_object)

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
      (setq taz_s_section_angle_old (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle"))))

      ;;; Ustaw globalne zmienne taz_s_family / taz_s_type / taz_s_category,
      ;;; których używają funkcje rysujące przekrój wewnątrz taz_s_create_beam.
      (setq taz_s_family taz_s_attr6_old)
      (setq taz_s_type   taz_s_attr7_old)

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
      ;; PRZESUŃ LUB SKOPIUJ LINIĘ STERUJĄCĄ O PODANY WEKTOR
      ;; ---------------------------------------------------------

      (if (= taz_s_move_copy_mode "1")
        (progn
          (command "_MOVE" taz_s_attribs_line "" taz_s_move_copy_p1 taz_s_move_copy_p2)
          (setq taz_s_attribs_line_new taz_s_attribs_line)
        )
        (progn
          (command "_COPY" taz_s_attribs_line "" taz_s_move_copy_p1 taz_s_move_copy_p2)
          (setq taz_s_attribs_line_new (entlast))
        )
      )

      ;; ---------------------------------------------------------
      ;; Zapisz nowe punkty ścieżki
      ;; ---------------------------------------------------------

      (setq taz_s_edit_new_path_p1 (cdr (assoc 10 (entget taz_s_attribs_line_new))))
      (setq taz_s_edit_new_path_p2 (cdr (assoc 11 (entget taz_s_attribs_line_new))))

      ;; ---------------------------------------------------------
      ;; GENERUJ NOWĄ BRYŁĘ
      ;; ---------------------------------------------------------

      (c:taz_s_create_beam)

      ;; ---------------------------------------------------------
      ;; NOWA BRYŁA – pobierz jej handle
      ;; ---------------------------------------------------------

      (setq taz_s_attribs_object_new (entlast))
      (setq taz_s_attribs_object_name_new
            (cdr (assoc 5 (entget taz_s_attribs_object_new))))

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
      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_section_angle")) taz_s_section_angle_old)
      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_section_position")) taz_s_section_position_old)

      ;; ---------------------------------------------------------
      ;; MOVE → usuń starą bryłę. COPY → stara bryła zostaje.
      ;; ---------------------------------------------------------

      (if (= taz_s_move_copy_mode "1")
        (progn
          (command "_LAYER" "_U" "taz_s_beam" "")
          (if (and taz_s_attribs_object_old (entget taz_s_attribs_object_old))
            (entdel taz_s_attribs_object_old)
          )
          (command "_LAYER" "_LO" "taz_s_beam" "")
        )
        (princ)
      )

      ;; ---------------------------------------------------------
      ;; USUŃ LINIE POMOCNICZE
      ;; ---------------------------------------------------------

      (if (and taz_s_attribs_line (entget taz_s_attribs_line))
        (entdel taz_s_attribs_line)
      )
      (if (and taz_s_attribs_line_new
               (/= taz_s_attribs_line_new taz_s_attribs_line)
               (entget taz_s_attribs_line_new))
        (entdel taz_s_attribs_line_new)
      )

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
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_section_angle " (rtos taz_s_section_angle_old 2 6) ")") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_section_position "(itoa taz_s_section_position_old) ")") taz_s_f_beam_data)

      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_sweep_p1 (list " (rtos taz_s_p1x 2 6) " " (rtos taz_s_p1y 2 6) " " (rtos taz_s_p1z 2 6) "))") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_sweep_p2 (list " (rtos taz_s_p2x 2 6) " " (rtos taz_s_p2y 2 6) " " (rtos taz_s_p2z 2 6) "))") taz_s_f_beam_data)

      (close taz_s_f_beam_data)

      ;; wyczyść zmienne obiektu
      (setq taz_s_attribs_line nil)
      (setq taz_s_attribs_line_new nil)
      (setq taz_s_attribs_object_old nil)

    )
    (princ)
  )

  ;; wyłącz tryb edycji
  (setq taz_s_edit_mode nil)
  (setq taz_s_edit_beam_path_mode nil)

  (setq taz_s_move_copy_counter 1)
  
  (princ)

)
