;; ---------------------------------------------------------
;; taz_s_program_settings_change
;; Zapisuje aktualne ustawienia programu i dostosowuje pod nakladkę
;; ---------------------------------------------------------
(defun taz_s_program_settings_change()
  (setq taz_s_current_locked_layer_fade (getvar "LAYLOCKFADECTL"))
  (setvar "LAYLOCKFADECTL" 0)
  (setq taz_s_current_grid_mode (getvar "GRIDMODE"))  
  (setvar "GRIDMODE" 0)
)


;; ---------------------------------------------------------
;; taz_s_program_settings_restore
;; Przywraca domyślne ustawienia programu
;; ---------------------------------------------------------
(defun taz_s_program_settings_restore()
  (setvar "LAYLOCKFADECTL" taz_s_current_locked_layer_fade)
  (setvar "GRIDMODE" taz_s_current_grid_mode)
)

;; ---------------------------------------------------------
;; taz_s_unlock_all_layers
;; Odblokowuje wszystkie warstwy
;; ---------------------------------------------------------
(defun taz_s_unlock_all_layers()
  (command "_LAYER" "_U" "taz_s_editing_layer" "")
  (command "_LAYER" "_U" "taz_s_beam" "")
  (command "_LAYER" "_U" "taz_s_plate" "")
  (command "_LAYER" "_U" "taz_s_axes" "")
  (command "_LAYER" "_U" "taz_s_visible" "")
  (command "_LAYER" "_U" "taz_s_hidden" "")
  (command "_LAYER" "_U" "taz_s_labels" "")
  (command "_LAYER" "_U" "taz_s_frame" "")
  
)

;; ---------------------------------------------------------
;; taz_s_lock_all_layers
;; Blokuje wszystkie warstwy
;; ---------------------------------------------------------
(defun taz_s_lock_all_layers()
  (command "_LAYER" "_LO" "taz_s_editing_layer" "")
  (command "_LAYER" "_LO" "taz_s_beam" "")
  (command "_LAYER" "_LO" "taz_s_plate" "")
  (command "_LAYER" "_LO" "taz_s_axes" "")
  (command "_LAYER" "_LO" "taz_s_visible" "")
  (command "_LAYER" "_LO" "taz_s_hidden" "")
  (command "_LAYER" "_LO" "taz_s_labels" "")
  (command "_LAYER" "_LO" "taz_s_frame" "")
)

;; ---------------------------------------------------------
;; taz_s_current_settings_save
;; Zapisuje aktualny stan wybranych ustawień.
;; ---------------------------------------------------------
(defun taz_s_current_settings_save()
  
  ;; WARSTWA
  (setq taz_s_current_layer (getvar "CLAYER"))
  
  ;; UCS
  (setq taz_s_current_ucs_exist (tblsearch "UCS" "taz_s_current_ucs"))
  (if taz_s_current_ucs_exist
    (progn
      (command "_.UCS" "_NA" "_S" "taz_s_current_ucs_placeholder")
      (command "_.UCS" "_NA" "_R" "taz_s_current_ucs_placeholder")
      (command "_.UCS" "_NA" "_D" "taz_s_current_ucs")
      (command "_.UCS" "_NA" "_S" "taz_s_current_ucs")
      (command "_.UCS" "_NA" "_R" "taz_s_current_ucs")
      (command "_.UCS" "_NA" "_D" "taz_s_current_ucs_placeholder")
    )
    (command "_.UCS" "_NA" "_S" "taz_s_current_ucs")
  )
  
  ;; USTAWIENIE KAMERY
  (command "-VIEW" "_S" "taz_s_current_view")
)

;; ---------------------------------------------------------
;; taz_s_current_settings_restore
;; Przywraca aktualny stan wybranych ustawień.
;; ---------------------------------------------------------
(defun taz_s_current_settings_restore()
  
  ;; WARSTWA
  (setvar "CLAYER" taz_s_current_layer)
  
  ;; UCS
  (if taz_s_current_ucs_exist
    (command "_.UCS" "_NA" "_R" "taz_s_current_ucs")
    (princ)
  )
  
  ;; USTAWIENIE KAMERY
  (command "-VIEW" "_R" "taz_s_current_view")

)

;; ---------------------------------------------------------
;; taz_s_path
;; Zwraca aktualną ścieżkę do katalogu danych projektu.
;; Zawsze liczy na bieżąco z DWGPREFIX i DWGNAME.
;; ---------------------------------------------------------
(defun taz_s_path ()
  (strcat
    (getvar "DWGPREFIX")
    (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4))
    "/"
  )
)

;; ---------------------------------------------------------
;; taz_s_cleanup_data_file
;; Czyści plik który został przypisany do zmiennej
;; taz_s_data_file wewnątrz katalogu danych projektu.
;; ---------------------------------------------------------
(defun taz_s_cleanup_data_file ()
  (setq taz_s_cl_file taz_s_data_file)

  ;; --- WCZYTAJ WSZYSTKIE LINIE ---
  (setq taz_s_cl_f (open taz_s_cl_file "r"))
  (setq taz_s_cl_all_lines (list))
  (setq taz_s_cl_line (read-line taz_s_cl_f))
  (while taz_s_cl_line
    (setq taz_s_cl_all_lines (append taz_s_cl_all_lines (list taz_s_cl_line)))
    (setq taz_s_cl_line (read-line taz_s_cl_f))
  )
  (close taz_s_cl_f)

  ;; --- ODWROC LISTE ---
  (setq taz_s_cl_reversed (reverse taz_s_cl_all_lines))

  ;; --- PRZEJDZ OD KONCA ---
  (setq taz_s_cl_seen (list))
  (setq taz_s_cl_result (list))
  (setq taz_s_cl_i 0)

  (while (< taz_s_cl_i (length taz_s_cl_reversed))

    (setq taz_s_cl_line (nth taz_s_cl_i taz_s_cl_reversed))

    (if (= (substr taz_s_cl_line 1 6) "(setq ")
      (progn
        ;; Wyciagnij nazwe zmiennej - szukaj spacji bez vl
        (setq taz_s_cl_rest (substr taz_s_cl_line 7))
        (setq taz_s_cl_sp nil)
        (setq taz_s_cl_j 1)
        (while (and (not taz_s_cl_sp) (<= taz_s_cl_j (strlen taz_s_cl_rest)))
          (if (= (substr taz_s_cl_rest taz_s_cl_j 1) " ")
            (setq taz_s_cl_sp taz_s_cl_j)
          )
          (setq taz_s_cl_j (1+ taz_s_cl_j))
        )
        (setq taz_s_cl_varname (substr taz_s_cl_rest 1 (1- taz_s_cl_sp)))

        ;; Jesli zmienna nie byla jeszcze widziana - dodaj do wyniku
        (if (not (member taz_s_cl_varname taz_s_cl_seen))
          (progn
            (setq taz_s_cl_seen (append taz_s_cl_seen (list taz_s_cl_varname)))
            (setq taz_s_cl_result (append taz_s_cl_result (list taz_s_cl_line)))
          )
        )
      )
    )

    (setq taz_s_cl_i (1+ taz_s_cl_i))
  )

  ;; --- ODWROC WYNIK ---
  (setq taz_s_cl_result (reverse taz_s_cl_result))

  ;; --- ZAPISZ DO PLIKU ---
  (setq taz_s_cl_f (open taz_s_cl_file "w"))
  (setq taz_s_cl_i 0)
  (while (< taz_s_cl_i (length taz_s_cl_result))
    (write-line (nth taz_s_cl_i taz_s_cl_result) taz_s_cl_f)
    (setq taz_s_cl_i (1+ taz_s_cl_i))
  )
  (close taz_s_cl_f)

)

;; ---------------------------------------------------------
;; taz_s_start
;; Tworzy niezbędne warstwy oraz zapisuje plik projektu
;; jeżeli ten nie został wcześniej zapisany.
;; ---------------------------------------------------------

(defun taz_s_start ()
  
  (taz_s_current_settings_save)
  (taz_s_program_settings_change)

  ;; warstwy
  (if (tblsearch "LAYER" "taz_s_editing_layer")
    (princ)
    (command "_LAYER" "_M" "taz_s_editing_layer" "_C" "1" "" "_LO" "taz_s_editing_layer" "")
  )
  (if (tblsearch "LAYER" "taz_s_beam")
    (princ)
    (command "_LAYER" "_M" "taz_s_beam" "_C" "145" "" "_LO" "taz_s_beam" "")
  )
  (if (tblsearch "LAYER" "taz_s_plate")
    (princ)
    (command "_LAYER" "_M" "taz_s_plate" "_C" "30" "" "_LO" "taz_s_plate" "")
  )
  (if (tblsearch "LAYER" "taz_s_axes")
    (princ)
    (command "_LAYER" "_M" "taz_s_axes" "_C" "109" "" "_LO" "taz_s_axes" "")
  )
  (if (tblsearch "LAYER" "taz_s_visible")
    (princ)
    (command "_LAYER" "_M" "taz_s_visible" "_C" "2" "" "_LO" "taz_s_visible" "")
  )
  (if (tblsearch "LAYER" "taz_s_hidden")
    (princ)
    (command "_LAYER" "_M" "taz_s_hidden" "_C" "4" "" "_LO" "taz_s_hidden" "")
  )
  (if (tblsearch "LAYER" "taz_s_labels")
    (princ)
    (command "_LAYER" "_M" "taz_s_labels" "_C" "7" "" "_LO" "taz_s_labels" "")
  )
  (if (tblsearch "LAYER" "taz_s_frame")
    (princ)
    (command "_LAYER" "_M" "taz_s_frame" "_C" "7" "" "_LO" "taz_s_frame" "")
  )

  ;; ---------------------------------------------------------
  ;; SPRAWDZENIE CZY PROJEKT JUZ ISTNIEJE
  ;; Projekt uznajemy za utworzony, jezeli istnieje jego katalog danych.
  ;; Nie sprawdzamy zawartosci Modelu, bo zapisany projekt moze byc pusty.
  ;; ---------------------------------------------------------

  (setq taz_s_existing_project_dir
    (strcat
      (getvar "DWGPREFIX")
      (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4))
    )
  )

  (if (vl-file-directory-p taz_s_existing_project_dir)
    (princ)

    ;; Katalog danych nie istnieje — nowy projekt, pytamy użytkownika o katalog
    (progn

      ;; Użytkownik wskazuje katalog gdzie ma być zapisany projekt
      (setq taz_s_chosen_dir (getfiled "Wskaż katalog projektu" "" "dwg" 9))

      ;; Jeśli anulował — przerywamy
      (if (not taz_s_chosen_dir)
        (princ)

        (progn

          ;; getfiled zwraca pełną ścieżkę wraz z nazwą wpisaną przez użytkownika.
          ;; Zachowujemy ją jako docelową nazwę pliku DWG.
          (setq taz_s_target_dwg taz_s_chosen_dir)

          ;; Szukamy ostatniego znaku \ lub / w pełnej ścieżce.
          ;; Robimy to zwykłymi funkcjami AutoLISP, bez vl-filename-*.
          (setq taz_s_i (strlen taz_s_target_dwg))
          (while
            (and
              (> taz_s_i 0)
              (/= (substr taz_s_target_dwg taz_s_i 1) "\\")
              (/= (substr taz_s_target_dwg taz_s_i 1) "/")
            )
            (setq taz_s_i (- taz_s_i 1))
          )

          ;; Katalog wybrany przez użytkownika.
          (setq taz_s_chosen_dir
            (substr taz_s_target_dwg 1 taz_s_i)
          )

          ;; Nazwa pliku bez rozszerzenia .dwg.
          (setq taz_s_dwgname
            (substr
              taz_s_target_dwg
              (+ taz_s_i 1)
              (- (strlen taz_s_target_dwg) taz_s_i 4)
            )
          )

          ;; Katalog danych projektu ma tę samą nazwę co zapisany plik DWG.
          (setq taz_s_target_dir
            (strcat taz_s_chosen_dir taz_s_dwgname)
          )

          ;; Jeśli istnieje stary plik dwg o tej nazwie — usuń go
          (if (findfile taz_s_target_dwg)
            (vl-file-delete taz_s_target_dwg)
          )

          ;; Jeśli istnieje stary katalog danych — wyczyść go z plików
          (if (vl-file-directory-p taz_s_target_dir)
            (foreach taz_s_f (vl-directory-files taz_s_target_dir nil 1)
              (vl-file-delete (strcat taz_s_target_dir "/" taz_s_f))
            )
            ;; Katalog nie istnieje — utwórz go
            (vl-mkdir taz_s_target_dir)
          )

          ;; Zapisz rysunek we wskazanym katalogu
          (command "_SAVEAS" "" taz_s_target_dwg)

        )
      )
    )
  )
  (taz_s_current_settings_restore)
  
  ;; WCZYTANIE DANYCH RYSUNKU - JEZELI ISTNIEJA
  (setq taz_s_dwg_path (getvar "DWGPREFIX"))
  (setq taz_s_data_file (strcat taz_s_dwg_path (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4)) "/" "taz_s_beam_data.txt"))
  (if (findfile taz_s_data_file)
    (load taz_s_data_file)
  )
  (setq taz_s_axes_data_file (strcat taz_s_dwg_path (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4)) "/" "taz_s_axes_data.txt"))
  (if (findfile taz_s_axes_data_file)
    (load taz_s_axes_data_file)
  )
  
  ;; PARAMETRY PROJEKTU
  (setq taz_s_project_parameters_data_file
    (strcat taz_s_dwg_path
            (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4))
            "/"
            "taz_s_project_parameters_data.txt"))

  ;; wartości domyślne zawsze najpierw trafiają do pamięci
  (setq taz_s_unit_weight_steel 7500.0)
  (setq taz_s_unit_weight_concrete 2500.0)
  (setq taz_s_additional_mass 10.0)

  (if (findfile taz_s_project_parameters_data_file)

    ;; jeżeli plik istnieje, jego wartości nadpiszą domyślne
    (load taz_s_project_parameters_data_file)

    ;; jeżeli nie istnieje, tworzymy go ze wszystkimi parametrami
    (progn
      (setq taz_s_f_project_parameters_data
        (open taz_s_project_parameters_data_file "w"))

      (write-line
        "(setq taz_s_unit_weight_steel 7500.0)"
        taz_s_f_project_parameters_data)

      (write-line
        "(setq taz_s_unit_weight_concrete 2500.0)"
        taz_s_f_project_parameters_data)

      (write-line
        "(setq taz_s_additional_mass 10.0)"
        taz_s_f_project_parameters_data)

      (close taz_s_f_project_parameters_data)
    )
  )
  
  (princ)
)

(taz_s_start)
