(defun taz_s_edit_section_position_parametres()
  
  (setq taz_s_r1 1)
  (setq taz_s_r2 1)
  
  (if (= taz_s_family "HEA")
    (taz_s_section_ibeam_draw_parametres_hea)
    (princ)
  )
  (if (= taz_s_family "HEB")
    (taz_s_section_ibeam_draw_parametres_heb)
    (princ)
  )
  (if (= taz_s_family "IPE")
    (taz_s_section_ibeam_draw_parametres_ipe)
    (princ)
  )
  (if (= taz_s_family "IPN")
    (taz_s_section_ibeam_draw_parametres_ipn)
    (princ)
  )
  (if (= taz_s_family "UPE")
    (taz_s_section_cbeam_draw_parametres_upe)
    (princ)
  )
  (if (= taz_s_family "UPN")
    (taz_s_section_cbeam_draw_parametres_upn)
    (princ)
  )
  (if (= taz_s_family "LR")
    (taz_s_section_lbeam_draw_parametres_katownik_rownoramienny)
    (princ)
  )
  (if (= taz_s_family "LN")
    (taz_s_section_lbeam_draw_parametres_katownik_nierownoramienny)
    (princ)
  )
  (if (= taz_s_family "SHS")
    (taz_s_section_hsbeam_draw_parametres_rura_kwadratowa)
    (princ)
  )
  (if (= taz_s_family "RHS")
    (taz_s_section_hsbeam_draw_parametres_rura_prostokatna)
    (princ)
  )  
  (if (= taz_s_family "CHS")
    (progn
    (taz_s_section_hsbeam_draw_parametres_rura_okragla)
    (setq taz_s_b taz_s_d)
    (setq taz_s_h taz_s_d)
    )
    (princ)
  )
  
  (if (= taz_s_category "Dwuteowniki")
    (progn
    (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "0")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list 0 0 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )
    (princ)
    )
    )
    (princ)
  )
  
  (if (= taz_s_category "Rury")
    (progn
    (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "0")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list 0 0 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )
    (princ)
    )
    )
    (princ)
  )
  
  (if (= taz_s_family "UPE")
    (progn
    (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "0")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list (+ (/ (- taz_s_b) 2) taz_s_ey) 0 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )
    (princ)
    )
    )
    (princ)
  )
  
  (if (= taz_s_family "UPN")
    (progn
    (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "0")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list (+ (/ (- taz_s_b) 2) taz_s_ey) 0 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )
    (princ)
    )
    )
    (princ)
  )
  
  (if (= taz_s_family "LR")
    (progn
    (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "0")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list (- (/ taz_s_b 2) taz_s_ey) (+ (/ (- taz_s_h) 2) taz_s_ex) 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )
    (princ)
    )
    )
    (princ)
  )
  
  (if (= taz_s_family "LN")
    (progn
    (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "0")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list (- (/ taz_s_b 2) taz_s_ey) (+ (/ (- taz_s_h) 2) taz_s_ex) 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )
    (princ)
    )
    )
    (princ)
  )
  
  (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "1")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list (/ (- taz_s_b) 2) (/ taz_s_h 2) 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )  
    (princ)
  )
  
  (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "2")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list 0 (/ taz_s_h 2) 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )  
    (princ)
  )
  
  (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "3")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list (/ taz_s_b 2) (/ taz_s_h 2) 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )  
    (princ)
  )

  (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "4")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list (/ (- taz_s_b) 2) 0 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )  
    (princ)
  )
  
  (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "5")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list 0 0 0))
    (command "_ZOOM" "_SCALE" "1000X")  
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )  
    (princ)
  )
  
  (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "6")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list (/ taz_s_b 2) 0 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )  
    (princ)
  )
  
  (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "7")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list (/ (- taz_s_b) 2) (/ (- taz_s_h) 2) 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )  
    (princ)
  )
  
  (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "8")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list 0 (/ (- taz_s_h) 2) 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )  
    (princ)
  )
  
  (if (= (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2) "9")
    (progn
    (setq taz_s_edit_section_position_parametres_origin (list (/ taz_s_b 2) (/ (- taz_s_h) 2) 0))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "_.UCS" "_O" taz_s_edit_section_position_parametres_origin)
    (command "_ZOOM" "_SCALE" "0.001X")
    )  
    (princ)
  )
)

(defun c:taz_s_create_beam ( / taz_s_create_beam_p1 taz_s_create_beam_p2)
  
  (if taz_s_edit_mode
    (princ)
    (progn
      (taz_s_current_settings_save)
      (command "_LAYER" "_U" "taz_s_editing_layer" "")
      (command "_LAYER" "_S" "taz_s_editing_layer" "")
    )
  )

  ;; Reset UCS do World
  (command "_.UCS" "_W")

  ;; ---------------------------------------------------------
  ;; POBRANIE PUNKTÓW – TRYB TWORZENIA / EDYCJI
  ;; ---------------------------------------------------------

  (if (and taz_s_edit_new_path_p1 taz_s_edit_new_path_p2)
    (progn
      (setq taz_s_create_beam_p1 taz_s_edit_new_path_p1)
      (setq taz_s_create_beam_p2 taz_s_edit_new_path_p2)
    )
    (progn
      (setq taz_s_create_beam_p1
            (getpoint "\nPodaj pierwszy punkt linii: "))
      (setq taz_s_create_beam_p2
            (getpoint taz_s_create_beam_p1 "\nPodaj drugi punkt linii: "))
      (command "-VIEW" "_S" "taz_s_current_view")
    )
  )
  
  ;; ---------------------------------------------------------
  ;; WYBOR PRZEKROJU
  ;; ---------------------------------------------------------
  
  (if (not taz_s_edit_mode)
    (taz_s_select_section)
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; RYSOWANIE LINII ŚCIEŻKI
  ;; ---------------------------------------------------------

  (command "_.LINE" taz_s_create_beam_p1 taz_s_create_beam_p2 "")
  (setq taz_s_create_beam_path (cdr (assoc -1 (entget (entlast)))))
  
  (if (= taz_s_category "Rury")
    (progn
    (command "_.LINE" taz_s_create_beam_p1 taz_s_create_beam_p2 "")
    (setq taz_s_create_beam_profile_cut_path (cdr (assoc -1 (entget (entlast)))))
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; USTAWIENIE UCS DO OBIEKTU I OBROTY
  ;; ---------------------------------------------------------
  
  (command "_.UCS" "_ZA" taz_s_create_beam_p1 taz_s_create_beam_p2)
  
  (if taz_s_edit_section_angle_mode
    (progn
      ;;(print (strcat "Aktualnie profil znajduje się pod kątem: " (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle"))) 2 2)))
      ;;(set (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle")) (getreal "\nPodaj kąt obrotu przekroju: "))
      (print (strcat "Aktualnie profil znajduje się pod kątem: " (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle"))) 2 2)))
      (setq taz_s_temp_angle_val (getreal "\nPodaj kąt obrotu przekroju: "))
      (if taz_s_temp_angle_val
        (set (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle")) taz_s_temp_angle_val)
      )
      ;;(command "-VIEW" "_S" "taz_s_current_view")
      (command "_ZOOM" "_SCALE" "1000X")
      (command "_.UCS" "_Z" (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle"))))
      (taz_s_edit_section_position_parametres)
      (command "_ZOOM" "_SCALE" "0.001X")
      (setq taz_s_section_angle_old (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle"))))
    )
      (princ)
  )
  
  (if taz_s_edit_section_position_mode
    (progn
      ;;(print (strcat "Aktualnie profil znajduje się w pozycji: " (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2)))
      ;;(set (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position")) (getint "\nPodaj punkt położenia przekroju względem osi od 0 do 9: "))
      (print (strcat "Aktualnie profil znajduje się w pozycji: " (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))) 2 2)))
      (setq taz_s_temp_position_val (getint "\nPodaj punkt położenia przekroju względem osi od 0 do 9: "))
      (if taz_s_temp_position_val
        (set (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position")) taz_s_temp_position_val)
      )
      ;;(command "-VIEW" "_S" "taz_s_current_view")
      (command "_ZOOM" "_SCALE" "1000X")
      (command "_.UCS" "_Z" (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle"))))
      (taz_s_edit_section_position_parametres)
      (command "_ZOOM" "_SCALE" "0.001X")
      (setq taz_s_section_position_old (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))))
    )
      (princ)
  )
  
  (if taz_s_edit_beam_path_mode
    (progn
      ;;(print (strcat "Aktualnie profil znajduje się pod kątem: " (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle"))) 2 2)))
      ;;(set (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle")) (getreal "\nPodaj kąt obrotu przekroju: "))
      (command "_ZOOM" "_SCALE" "1000X")
      (command "_.UCS" "_Z" (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle"))))
      (taz_s_edit_section_position_parametres)
      (command "_ZOOM" "_SCALE" "0.001X")
      ;;(setq taz_s_section_angle_old (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle"))))
    )
      (princ)
  )

  ;; ---------------------------------------------------------
  ;; WYBÓR I RYSOWANIE PRZEKROJU BELKI
  ;; ---------------------------------------------------------
  
  (if (= taz_s_category "Dwuteowniki")
    (taz_s_section_ibeam_draw)
    (princ)
  )
  (if (= taz_s_category "Ceowniki")
    (taz_s_section_cbeam_draw)
    (princ)
  )
  (if (= taz_s_category "Katowniki")
    (taz_s_section_lbeam_draw)
    (princ)
  )
  (if (= taz_s_category "Rury")
    (taz_s_section_hsbeam_draw)
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; POBRANIE HANDLE ELEMENTU
  ;; Handle to unikalny identyfikator nadawany przez AutoCAD
  ;; ---------------------------------------------------------

  (setq taz_s_attribs_object_name
        (cdr (assoc 5 (entget (entlast)))))

  ;; ---------------------------------------------------------
  ;; RESET UCS DO WORLD
  ;; ---------------------------------------------------------

  (command "_.UCS" "_W")

  ;; ---------------------------------------------------------
  ;; ŚCIEŻKA DO PLIKU DANYCH
  ;; Plik .txt z danymi leży obok rysunku
  ;; ---------------------------------------------------------

  (setq taz_s_dwg_path (getvar "DWGPREFIX"))
  (setq taz_s_data_file (strcat taz_s_dwg_path (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4)) "/" "taz_s_beam_data.txt"))

  ;; ---------------------------------------------------------
  ;; ZAPIS DANYCH ELEMENTU DO PLIKU TEKSTOWEGO
  ;; "a" oznacza dopisywanie na koniec - poprzednie dane nie znikają
  ;; ---------------------------------------------------------

  (if taz_s_edit_mode
    
  (princ)
  (progn
  (setq taz_s_f_beam_data (open taz_s_data_file "a"))

  ;; -- atrybuty ogólne --
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr1 \"\")") taz_s_f_beam_data)
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr2 \"\")") taz_s_f_beam_data)
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr3 \"\")") taz_s_f_beam_data)
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr4 \"\")") taz_s_f_beam_data)
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr5 \"\")") taz_s_f_beam_data)
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr6 \"" taz_s_family "\")") taz_s_f_beam_data)
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr7 \"" taz_s_type "\")") taz_s_f_beam_data)
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr8 \"\")") taz_s_f_beam_data)
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr9 \"BELKA\")") taz_s_f_beam_data)
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr10 \"\")") taz_s_f_beam_data)

  ;; -- kąt obrotu przekroju (po inicjalizacji zawsze 0) --
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_section_angle 0)") taz_s_f_beam_data)

  ;; -- pozycja przekroju względem osi (po inicjalizacji zawsze 5) --
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_section_position 5)") taz_s_f_beam_data)

  ;; -- punkt początkowy ścieżki sterującej --
  (setq taz_s_p1x (car taz_s_create_beam_p1))
  (setq taz_s_p1y (cadr taz_s_create_beam_p1))
  (setq taz_s_p1z (caddr taz_s_create_beam_p1))
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_sweep_p1 (list " (rtos taz_s_p1x 2 6) " " (rtos taz_s_p1y 2 6) " " (rtos taz_s_p1z 2 6) "))") taz_s_f_beam_data)

  ;; -- punkt końcowy ścieżki sterującej --
  (setq taz_s_p2x (car taz_s_create_beam_p2))
  (setq taz_s_p2y (cadr taz_s_create_beam_p2))
  (setq taz_s_p2z (caddr taz_s_create_beam_p2))
  (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_sweep_p2 (list " (rtos taz_s_p2x 2 6) " " (rtos taz_s_p2y 2 6) " " (rtos taz_s_p2z 2 6) "))") taz_s_f_beam_data)

  (close taz_s_f_beam_data)
  )    
  )
  
  (if (and taz_s_edit_new_path_p1 taz_s_edit_new_path_p2)
    (progn
    (setq taz_s_p1x (car   taz_s_edit_new_path_p1))
    (setq taz_s_p1y (cadr  taz_s_edit_new_path_p1))
    (setq taz_s_p1z (caddr taz_s_edit_new_path_p1))
    (setq taz_s_p2x (car   taz_s_edit_new_path_p2))
    (setq taz_s_p2y (cadr  taz_s_edit_new_path_p2))
    (setq taz_s_p2z (caddr taz_s_edit_new_path_p2))
    )
    (princ)
  )
  
  ;;(setq taz_s_data_file taz_s_f_beam_data)
  ;;(taz_s_cleanup_data_file)
  
  ;; ---------------------------------------------------------
  ;; WCZYTANIE DANYCH Z PLIKU TXT DO ZMIENNYCH GLOBALNYCH
  ;; Plik zawiera gotowe (setq ...) wiec load wystarczy
  ;; ---------------------------------------------------------

  ;;(load taz_s_data_file)

  ;; ---------------------------------------------------------
  ;; WYCZYSZCZENIE ZMIENNYCH EDYCJI
  ;; ---------------------------------------------------------

  (setq taz_s_edit_new_path_p1 nil)
  (setq taz_s_edit_new_path_p2 nil)
  
  (setq taz_s_create_beam_p1 nil)
  (setq taz_s_create_beam_p2 nil)
  
  (setq taz_s_create_beam_profile nil)
  (setq taz_s_create_beam_profile_cut nil)
  (setq taz_s_create_beam_path nil)
  (setq taz_s_create_beam_profile_cut_path nil)
  
  (setq taz_s_p nil)
  
  (setq taz_s_r1 nil)
  (setq taz_s_r2 nil)
  (setq taz_s_r nil)
  
  (princ)
  
  (if taz_s_edit_mode
    (princ)
    (progn
      (command "_LAYER" "_LO" "taz_s_editing_layer" "")
      (taz_s_current_settings_restore)
    )
  )
  
  (princ)
  
)
