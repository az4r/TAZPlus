(defun c:taz_s_project_parameters ( / dcl_id taz_s_project_parameters_dialog_result steel_text concrete_text additional_mass_text steel_value concrete_value additional_mass_value taz_s_dwg_path taz_s_project_parameters_data_file taz_s_f_project_parameters_data)

  ;; ---------------------------------------------------------
  ;; ŚCIEŻKA DO PLIKU PARAMETRÓW PROJEKTU
  ;; Plik .txt z danymi leży obok pozostałych danych projektu
  ;; ---------------------------------------------------------

  (setq taz_s_dwg_path (getvar "DWGPREFIX"))
  (setq taz_s_project_parameters_data_file
    (strcat taz_s_dwg_path
            (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4))
            "/"
            "taz_s_project_parameters_data.txt"))

  ;; ---------------------------------------------------------
  ;; WCZYTANIE DANYCH Z PLIKU TXT
  ;; Jeżeli plik istnieje, przywracamy zapisane parametry
  ;; ---------------------------------------------------------

  (if (findfile taz_s_project_parameters_data_file)
    (load taz_s_project_parameters_data_file)
  )

  (if (not (boundp 'taz_s_additional_mass))
    (setq taz_s_additional_mass 10.0)
  )

  ;; wczytanie pliku DCL
  (setq dcl_id (load_dialog "taz_s_project_parameters.dcl"))

  (if (not (new_dialog "taz_s_project_parameters_dialog" dcl_id))
    (progn
      (alert "Nie mogę załadować okienka DCL.")
      (exit)
    )
  )

  ;; ustawienie proponowanych wartości
  (set_tile "taz_s_unit_weight_steel_edit" (rtos taz_s_unit_weight_steel 2 0))
  (set_tile "taz_s_unit_weight_concrete_edit" (rtos taz_s_unit_weight_concrete 2 0))
  (set_tile "taz_s_additional_mass_edit" (rtos taz_s_additional_mass 2 0))

  ;; obsługa OK
  (action_tile "accept"
    "(progn
        (setq steel_text (get_tile \"taz_s_unit_weight_steel_edit\"))
        (setq concrete_text (get_tile \"taz_s_unit_weight_concrete_edit\"))
        (setq additional_mass_text (get_tile \"taz_s_additional_mass_edit\"))

        (setq steel_value (distof steel_text 2))
        (setq concrete_value (distof concrete_text 2))
        (setq additional_mass_value (distof additional_mass_text 2))

        (if (and steel_value (> steel_value 0.0))
          (setq taz_s_unit_weight_steel steel_value)
        )

        (if (and concrete_value (> concrete_value 0.0))
          (setq taz_s_unit_weight_concrete concrete_value)
        )

        (if (and additional_mass_value (>= additional_mass_value 0.0))
          (setq taz_s_additional_mass additional_mass_value)
        )

        (done_dialog 1)
    )"
  )

  ;; obsługa ANULUJ
  (action_tile "cancel"
    "(progn
        (done_dialog 0)
    )"
  )

  ;; uruchom dialog
  (setq taz_s_project_parameters_dialog_result (start_dialog))

  ;; zwolnij DCL
  (unload_dialog dcl_id)

  ;; jeśli anulowano → przerwij skrypt
  (if (= taz_s_project_parameters_dialog_result 0)
    (progn
      (setq taz_s_old_error *error*)
      (setq *error* (lambda (msg) (princ "")))
      (exit)
    )
  )

  ;; ---------------------------------------------------------
  ;; ZAPIS PARAMETRÓW PROJEKTU DO PLIKU TXT
  ;; Plik jest nadpisywany po każdym zatwierdzeniu OK
  ;; ---------------------------------------------------------

  (setq taz_s_f_project_parameters_data
    (open taz_s_project_parameters_data_file "w"))

  (write-line
    (strcat "(setq taz_s_unit_weight_steel "
            (rtos taz_s_unit_weight_steel 2 6)
            ")")
    taz_s_f_project_parameters_data)

  (write-line
    (strcat "(setq taz_s_unit_weight_concrete "
            (rtos taz_s_unit_weight_concrete 2 6)
            ")")
    taz_s_f_project_parameters_data)

  (write-line
    (strcat "(setq taz_s_additional_mass "
            (rtos taz_s_additional_mass 2 6)
            ")")
    taz_s_f_project_parameters_data)

  (close taz_s_f_project_parameters_data)

  ;; debug
  (princ (strcat "\nCiężar objętościowy stali = " (rtos taz_s_unit_weight_steel 2 2) " kg/m3"))
  (princ (strcat "\nCiężar objętościowy betonu = " (rtos taz_s_unit_weight_concrete 2 2) " kg/m3"))
  (princ (strcat "\nNaddatek na połączenia = " (rtos taz_s_additional_mass 2 2) " %"))

  (princ)
)
