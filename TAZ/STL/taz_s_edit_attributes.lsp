(defun taz_s_get_common_value()
  
  (setq taz_s_attribs_count_index 0)
  (setq taz_s_attribs_object_first_value nil)
  (setq taz_s_attribs_object_mixed nil)
  
  (while (< taz_s_attribs_count_index taz_s_attribs_count)
    
    (setq taz_s_attribs_object (ssname taz_s_attribs_selection taz_s_attribs_count_index))
    (setq taz_s_attribs_object_name (cdr (assoc 5 (entget taz_s_attribs_object))))
    (setq taz_s_attribs_object_value (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_" taz_s_attr_name))))
    
    (if (not taz_s_attribs_object_first_value)
      (setq taz_s_attribs_object_first_value taz_s_attribs_object_value)
    )

    (if (/= taz_s_attribs_object_first_value taz_s_attribs_object_value)
      (setq taz_s_attribs_object_mixed T)
    )

    
    (setq taz_s_attribs_count_index (1+ taz_s_attribs_count_index))
    
  )
  
  (if taz_s_attribs_object_mixed
    "*ROZNE*"
    taz_s_attribs_object_first_value
  )
)

(defun c:taz_s_edit_attributes()
  
  ;; ---------------------------------------------------------
  ;; SCIEZKA DO PLIKU DANYCH
  ;; ---------------------------------------------------------

  (setq taz_s_dwg_path (getvar "DWGPREFIX"))
  (setq taz_s_data_file (strcat taz_s_dwg_path (substr (getvar "DWGNAME") 1 (- (strlen (getvar "DWGNAME")) 4)) "/" "taz_s_beam_data.txt"))

  ;; ---------------------------------------------------------
  ;; WCZYTANIE DANYCH Z PLIKU TXT DO ZMIENNYCH GLOBALNYCH
  ;; Plik zawiera gotowe (setq ...) wiec load wystarczy
  ;; ---------------------------------------------------------

  (load taz_s_data_file)

  ;; ---------------------------------------------------------
  ;; SELEKCJA OBIEKTOW
  ;; ---------------------------------------------------------

  (setq taz_s_attribs_selection (ssget "_I"))

  (if (not taz_s_attribs_selection)
    (setq taz_s_attribs_selection (ssget "_+.:E:S" '((0 . "*"))))
  )

  (if (not taz_s_attribs_selection)
    (progn
      (alert "Nie wybrano obiektu.")
      (exit)
    )
  )
  
  (setq taz_s_attribs_count (sslength taz_s_attribs_selection))
  (setq taz_s_dcl_id (load_dialog "taz_s_edit_attributes.dcl"))
  (new_dialog "taz_s_edit_attributes_dialog" taz_s_dcl_id)
  
  ;; ---------------------------------------------------------
  ;; Ustawienie wartosci w polach
  ;; ---------------------------------------------------------

  (setq taz_s_attr_name "attr1")
  (set_tile "taz_s_attr1" (taz_s_get_common_value))
  
  (setq taz_s_attr_name "attr2")
  (set_tile "taz_s_attr2" (taz_s_get_common_value))
  
  (setq taz_s_attr_name "attr3")
  (set_tile "taz_s_attr3" (taz_s_get_common_value))
  
  (setq taz_s_attr_name "attr4")
  (set_tile "taz_s_attr4" (taz_s_get_common_value))
  
  (setq taz_s_attr_name "attr5")
  (set_tile "taz_s_attr5" (taz_s_get_common_value))
  
  (setq taz_s_attr_name "attr6")
  (set_tile "taz_s_attr6" (taz_s_get_common_value))
  
  (setq taz_s_attr_name "attr7")
  (set_tile "taz_s_attr7" (taz_s_get_common_value))
  
  (setq taz_s_attr_name "attr8")
  (set_tile "taz_s_attr8" (taz_s_get_common_value))
  
  (setq taz_s_attr_name "attr9")
  (set_tile "taz_s_attr9" (taz_s_get_common_value))
  
  (setq taz_s_attr_name "attr10")
  (set_tile "taz_s_attr10" (taz_s_get_common_value))

  ;; ---------------------------------------------------------
  ;; Zapis wartosci po kliknieciu OK
  ;; ---------------------------------------------------------

  (action_tile
    "accept"
    "(progn
        (setq taz_s_attribs_count_index 0)

        (while (< taz_s_attribs_count_index taz_s_attribs_count)

          (setq taz_s_attribs_object
                (ssname taz_s_attribs_selection taz_s_attribs_count_index))

          (setq taz_s_attribs_object_name
                (cdr (assoc 5 (entget taz_s_attribs_object))))

          ;; Atrybut 1
          (setq taz_s_attribs_object_value (get_tile \"taz_s_attr1\"))
          (if (/= taz_s_attribs_object_value \"*ROZNE*\")
            (set (read (strcat \"taz_s_\" taz_s_attribs_object_name \"_attr1\"))
                 taz_s_attribs_object_value)
          )

          ;; Atrybut 2
          (setq taz_s_attribs_object_value (get_tile \"taz_s_attr2\"))
          (if (/= taz_s_attribs_object_value \"*ROZNE*\")
            (set (read (strcat \"taz_s_\" taz_s_attribs_object_name \"_attr2\"))
                 taz_s_attribs_object_value)
          )

          ;; Atrybut 3
          (setq taz_s_attribs_object_value (get_tile \"taz_s_attr3\"))
          (if (/= taz_s_attribs_object_value \"*ROZNE*\")
            (set (read (strcat \"taz_s_\" taz_s_attribs_object_name \"_attr3\"))
                 taz_s_attribs_object_value)
          )

          ;; Atrybut 4
          (setq taz_s_attribs_object_value (get_tile \"taz_s_attr4\"))
          (if (/= taz_s_attribs_object_value \"*ROZNE*\")
            (set (read (strcat \"taz_s_\" taz_s_attribs_object_name \"_attr4\"))
                 taz_s_attribs_object_value)
          )

          ;; Atrybut 5
          (setq taz_s_attribs_object_value (get_tile \"taz_s_attr5\"))
          (if (/= taz_s_attribs_object_value \"*ROZNE*\")
            (set (read (strcat \"taz_s_\" taz_s_attribs_object_name \"_attr5\"))
                 taz_s_attribs_object_value)
          )

          ;; Atrybut 6
          (setq taz_s_attribs_object_value (get_tile \"taz_s_attr6\"))
          (if (/= taz_s_attribs_object_value \"*ROZNE*\")
            (set (read (strcat \"taz_s_\" taz_s_attribs_object_name \"_attr6\"))
                 taz_s_attribs_object_value)
          )

          ;; Atrybut 7
          (setq taz_s_attribs_object_value (get_tile \"taz_s_attr7\"))
          (if (/= taz_s_attribs_object_value \"*ROZNE*\")
            (set (read (strcat \"taz_s_\" taz_s_attribs_object_name \"_attr7\"))
                 taz_s_attribs_object_value)
          )

          ;; Atrybut 8
          (setq taz_s_attribs_object_value (get_tile \"taz_s_attr8\"))
          (if (/= taz_s_attribs_object_value \"*ROZNE*\")
            (set (read (strcat \"taz_s_\" taz_s_attribs_object_name \"_attr8\"))
                 taz_s_attribs_object_value)
          )

          ;; Atrybut 9
          (setq taz_s_attribs_object_value (get_tile \"taz_s_attr9\"))
          (if (/= taz_s_attribs_object_value \"*ROZNE*\")
            (set (read (strcat \"taz_s_\" taz_s_attribs_object_name \"_attr9\"))
                 taz_s_attribs_object_value)
          )

          ;; Atrybut 10
          (setq taz_s_attribs_object_value (get_tile \"taz_s_attr10\"))
          (if (/= taz_s_attribs_object_value \"*ROZNE*\")
            (set (read (strcat \"taz_s_\" taz_s_attribs_object_name \"_attr10\"))
                 taz_s_attribs_object_value)
          )

          (setq taz_s_attribs_count_index
                (1+ taz_s_attribs_count_index))
        )

        (done_dialog 1)
     )"
  )
  
  ;; ---------------------------------------------------------
  ;; ANULUJ
  ;; ---------------------------------------------------------

  (action_tile "cancel" "(done_dialog 0)")
  
  ;; ---------------------------
  ;; Uruchomienie dialogu
  ;; ---------------------------
  (setq taz_s_edit_attributes_dialog_result (start_dialog))
  
  ;; ---------------------------
  ;; Zwolnij DCL
  ;; ---------------------------
  (unload_dialog taz_s_dcl_id)
  
  (if (= taz_s_edit_attributes_dialog_result 1)
    (princ)
    (progn
      (princ "\nAnulowano.")
      (setq taz_s_old_error *error*)
      (setq *error* (lambda (msg) (princ "")))
      ;;(taz_s_lock_all_layers)
      ;;(taz_s_current_settings_restore)
      (exit)
    )
  )

  ;; ---------------------------------------------------------
  ;; ZAPIS ZMIENNYCH GLOBALNYCH Z POWROTEM DO PLIKU TXT
  ;; Robimy to po zamknieciu dialogu - tylko jesli kliknieto OK
  ;; Wczytujemy wszystkie uchwyty z selekcji i zapisujemy ich atrybuty
  ;; ---------------------------------------------------------

  (setq taz_s_f_beam_data (open taz_s_data_file "a"))

  (setq taz_s_attribs_count_index 0)
  (while (< taz_s_attribs_count_index taz_s_attribs_count)

    (setq taz_s_attribs_object (ssname taz_s_attribs_selection taz_s_attribs_count_index))
    (setq taz_s_attribs_object_name (cdr (assoc 5 (entget taz_s_attribs_object))))

    (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr1 \"" (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr1"))) "\")") taz_s_f_beam_data)
    (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr2 \"" (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr2"))) "\")") taz_s_f_beam_data)
    (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr3 \"" (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr3"))) "\")") taz_s_f_beam_data)
    (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr4 \"" (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr4"))) "\")") taz_s_f_beam_data)
    (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr5 \"" (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr5"))) "\")") taz_s_f_beam_data)
    (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr6 \"" (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr6"))) "\")") taz_s_f_beam_data)
    (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr7 \"" (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr7"))) "\")") taz_s_f_beam_data)
    (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr8 \"" (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr8"))) "\")") taz_s_f_beam_data)
    (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr9 \"" (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr9"))) "\")") taz_s_f_beam_data)
    (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_attr10 \"" (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr10"))) "\")") taz_s_f_beam_data)
    
    ;;(write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_section_angle "    (rtos taz_s_section_angle_old 2 6) ")") taz_s_f_beam_data)
    ;;(write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_section_position " (itoa taz_s_section_position_old) ")") taz_s_f_beam_data)
    
    ;;(write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_sweep_p1 (list " (rtos taz_s_p1x 2 6) " " (rtos taz_s_p1y 2 6) " " (rtos taz_s_p1z 2 6) "))") taz_s_f_beam_data)
    ;;(write-line (strcat "(setq taz_s_" taz_s_attribs_object_name "_sweep_p2 (list " (rtos taz_s_p2x 2 6) " " (rtos taz_s_p2y 2 6) " " (rtos taz_s_p2z 2 6) "))") taz_s_f_beam_data)
    
    ;; Section angle
    (write-line 
      (strcat "(setq taz_s_" taz_s_attribs_object_name "_section_angle " 
              (rtos (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle"))) 2 6) 
              ")") 
      taz_s_f_beam_data)

    ;; Section position
    (write-line 
      (strcat "(setq taz_s_" taz_s_attribs_object_name "_section_position " 
              (itoa (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position")))) 
              ")") 
      taz_s_f_beam_data)

    ;; Sweep p1
    (setq taz_s_tmp_p1 (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_sweep_p1"))))
    (write-line 
      (strcat "(setq taz_s_" taz_s_attribs_object_name "_sweep_p1 (list " 
              (rtos (nth 0 taz_s_tmp_p1) 2 6) " " 
              (rtos (nth 1 taz_s_tmp_p1) 2 6) " " 
              (rtos (nth 2 taz_s_tmp_p1) 2 6) 
              "))") 
      taz_s_f_beam_data)

    ;; Sweep p2
    (setq taz_s_tmp_p2 (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_sweep_p2"))))
    (write-line 
      (strcat "(setq taz_s_" taz_s_attribs_object_name "_sweep_p2 (list " 
              (rtos (nth 0 taz_s_tmp_p2) 2 6) " " 
              (rtos (nth 1 taz_s_tmp_p2) 2 6) " " 
              (rtos (nth 2 taz_s_tmp_p2) 2 6) 
              "))") 
      taz_s_f_beam_data)


    (setq taz_s_attribs_count_index (1+ taz_s_attribs_count_index))
  )

  (close taz_s_f_beam_data)
  
  ;;(setq taz_s_data_file taz_s_f_beam_data)
  ;;(taz_s_cleanup_data_file)

  (princ)
)