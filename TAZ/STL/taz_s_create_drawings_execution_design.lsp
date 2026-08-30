(defun taz_s_merge_solprof_layers ()

  (setq taz_s_layer_rec (tblnext "LAYER" T))

  (while taz_s_layer_rec

    (setq taz_s_layer_name
          (cdr (assoc 2 taz_s_layer_rec))
    )

    ;; PH* -> taz_s_hidden
    (if (= "PH"
           (strcase
             (substr
               taz_s_layer_name
               1
               (min 2 (strlen taz_s_layer_name))
             )
           )
        )
      (progn

        ;; Rozbij bloki
        (setq taz_s_blkss
              (ssget "X"
                     (list
                       (cons 8 taz_s_layer_name)
                       (cons 0 "INSERT")
                     )
              )
        )

        (if taz_s_blkss
          (command "_.explode" taz_s_blkss)
        )

        ;; Pobierz wszystkie obiekty po rozbiciu
        (setq taz_s_ss
              (ssget "X"
                     (list (cons 8 taz_s_layer_name))
              )
        )

        (if taz_s_ss
          (progn

            (setq taz_s_idx (sslength taz_s_ss))

            (repeat taz_s_idx

              (setq taz_s_idx (1- taz_s_idx))

              (setq taz_s_ent
                    (ssname taz_s_ss taz_s_idx)
              )

              (setq taz_s_edata
                    (entget taz_s_ent)
              )

              (entmod
                (subst
                  (cons 8 "taz_s_hidden")
                  (assoc 8 taz_s_edata)
                  taz_s_edata
                )
              )

            )
          )
        )
      )
    )

    ;; PV* -> taz_s_visible
    (if (= "PV"
           (strcase
             (substr
               taz_s_layer_name
               1
               (min 2 (strlen taz_s_layer_name))
             )
           )
        )
      (progn

        ;; Rozbij bloki
        (setq taz_s_blkss
              (ssget "X"
                     (list
                       (cons 8 taz_s_layer_name)
                       (cons 0 "INSERT")
                     )
              )
        )

        (if taz_s_blkss
          (command "_.explode" taz_s_blkss)
        )

        ;; Pobierz wszystkie obiekty po rozbiciu
        (setq taz_s_ss
              (ssget "X"
                     (list (cons 8 taz_s_layer_name))
              )
        )

        (if taz_s_ss
          (progn

            (setq taz_s_idx (sslength taz_s_ss))

            (repeat taz_s_idx

              (setq taz_s_idx (1- taz_s_idx))

              (setq taz_s_ent
                    (ssname taz_s_ss taz_s_idx)
              )

              (setq taz_s_edata
                    (entget taz_s_ent)
              )

              (entmod
                (subst
                  (cons 8 "taz_s_visible")
                  (assoc 8 taz_s_edata)
                  taz_s_edata
                )
              )

            )
          )
        )
      )
    )

    (setq taz_s_layer_rec (tblnext "LAYER"))
  )
  
  ;; Purge bloków *u...
  (setq taz_s_block_rec (tblnext "BLOCK" T))

  (while taz_s_block_rec

    (setq taz_s_block_name
          (cdr (assoc 2 taz_s_block_rec))
    )

    (if (= "*U"
          (strcase
            (substr
              taz_s_block_name
              1
              (min 2 (strlen taz_s_block_name))
            )
          )
        )
      (command "_.-purge" "_B" taz_s_block_name "_N")
    )

    (setq taz_s_block_rec (tblnext "BLOCK"))
  )

  ;; Usuń puste warstwy PH* i PV*
  (setq taz_s_layer_rec (tblnext "LAYER" T))

  (while taz_s_layer_rec

    (setq taz_s_layer_name
          (cdr (assoc 2 taz_s_layer_rec))
    )

    (if (or
          (= "PH"
            (strcase
              (substr
                taz_s_layer_name
                1
                (min 2 (strlen taz_s_layer_name))
              )
            )
          )
          (= "PV"
            (strcase
              (substr
                taz_s_layer_name
                1
                (min 2 (strlen taz_s_layer_name))
              )
            )
          )
        )
      (command "_.-layer" "_delete" taz_s_layer_name "")
    )

    (setq taz_s_layer_rec (tblnext "LAYER"))
  )
  
  (command "_.REGEN")

  (princ "\nPrzeniesiono obiekty z warstw PH* i PV*.")
  (princ)
)

;; =================================================================
;; PRAWDZIWY OSTATNI ENAME W BAZIE RYSUNKU
;; =================================================================
;; ENTLAST zwraca ostatni obiekt glowny, ale stara POLYLINE lub INSERT
;; moze miec jeszcze podobiekty dostepne przez ENTNEXT. Ta funkcja dochodzi
;; do rzeczywistego konca bazy przed utworzeniem tabeli.
;; =================================================================

(defun taz_s_execution_design_get_last_entity ()

  (setq taz_s_execution_design_last_ent (entlast))

  (if taz_s_execution_design_last_ent
    (progn
      (setq taz_s_execution_design_last_next
        (entnext taz_s_execution_design_last_ent)
      )

      (while taz_s_execution_design_last_next
        (setq taz_s_execution_design_last_ent
          taz_s_execution_design_last_next
        )
        (setq taz_s_execution_design_last_next
          (entnext taz_s_execution_design_last_ent)
        )
      )
    )
  )

  taz_s_execution_design_last_ent
)


;; =================================================================
;; ZBIERANIE OBIEKTOW UTWORZONYCH PO WSKAZANYM ENAME
;; =================================================================
;; Uzywane do zapamietania kompletnej tabeli zestawienia stali.
;; Funkcja zwraca liste wszystkich nowych obiektow utworzonych po
;; taz_s_execution_design_before_arg.
;; =================================================================

(defun taz_s_execution_design_collect_new_entities
  (taz_s_execution_design_before_arg)

  (setq taz_s_execution_design_new_entities '())

  (if taz_s_execution_design_before_arg
    (setq taz_s_execution_design_new_ent
      (entnext taz_s_execution_design_before_arg)
    )
    (setq taz_s_execution_design_new_ent (entnext))
  )

  (while taz_s_execution_design_new_ent

    (setq taz_s_execution_design_new_data
      (entget taz_s_execution_design_new_ent)
    )

    (setq taz_s_execution_design_new_type
      (cdr (assoc 0 taz_s_execution_design_new_data))
    )

    ;; Nie zapisujemy podobiektow starej POLYLINE ani atrybutow INSERT.
    ;; Do selection setu potrzebne sa obiekty glowne.
    (if
      (not
        (member
          taz_s_execution_design_new_type
          '("VERTEX" "SEQEND" "ATTRIB")
        )
      )
      (setq taz_s_execution_design_new_entities
        (append
          taz_s_execution_design_new_entities
          (list taz_s_execution_design_new_ent)
        )
      )
    )

    (setq taz_s_execution_design_new_ent
      (entnext taz_s_execution_design_new_ent)
    )
  )

  taz_s_execution_design_new_entities
)


(defun c:taz_s_create_drawings_execution_design ()

  ;; ---------------------------------
  ;; ZAPIS ORYGINALNEGO PLIKU I NAZWA PLIKU DRAWINGS
  ;; ---------------------------------

  (command "_.QSAVE")

  (setq taz_s_original_dwg_name (getvar "DWGNAME"))
  (setq taz_s_original_dwg_path (getvar "DWGPREFIX"))

  (setq taz_s_original_dwg_name_no_ext
    (substr
      taz_s_original_dwg_name
      1
      (- (strlen taz_s_original_dwg_name) 4)
    )
  )

  (setq taz_s_drawings_file
    (strcat
      taz_s_original_dwg_path
      taz_s_original_dwg_name_no_ext
      "_DRAWINGS.dwg"
    )
  )

  ;; ---------------------------------
  ;; WCZYTANIE DANYCH
  ;; ---------------------------------

  ;;(setq taz_s_x_data taz_s_axis_data_x)
  ;;(setq taz_s_y_data taz_s_axis_data_y)
  ;;(setq taz_s_z_data taz_s_axis_data_z)
  
  ;;(load taz_s_data_file)
  
  (if (findfile taz_s_data_file)
    (load taz_s_data_file)
    (progn
      (setq taz_s_old_error *error*)
      (setq *error* (lambda (msg) (princ "")))
      (print "Brak konstrukcji - rysunki nie moga zostac wykonane!")
      (exit)
      (setq *error* taz_s_old_error)
    )
  )
  
  (if (findfile taz_s_axes_data_file)
    (progn
      (setq taz_s_x_data taz_s_axis_data_x)
      (setq taz_s_y_data taz_s_axis_data_y)
      (setq taz_s_z_data taz_s_axis_data_z)
    )
    (progn
      (setq taz_s_old_error *error*)
      (setq *error* (lambda (msg) (princ "")))
      (print "Brak osi konstrukcyjnych - rysunki nie moga zostac wykonane!")
      (exit)
      (setq *error* taz_s_old_error)
    )    
  )


  ;; =================================================================
  ;; PREFLIGHT DCL - WSZYSTKIE USTAWIENIA PRZED JAKAKOLWIEK GENERACJA
  ;; =================================================================
  ;; Na tym etapie sa tylko wczytane dane konstrukcji i osi.
  ;; Nie tworzymy jeszcze warstw roboczych, nie kasujemy obiektow,
  ;; nie kopiujemy modelu, nie tworzymy layoutow i nie uruchamiamy SOLPROF.
  ;;
  ;; Najpierw zbieramy ustawienia dla calej sekwencji:
  ;;   IZO, wszystkie X, wszystkie Y, wszystkie Z.
  ;; Dopiero po zaakceptowaniu ostatniego wymaganego okna skrypt przechodzi
  ;; do przygotowania rysunku i generowania przypadkow.
  ;; =================================================================

  (setq taz_s_execution_design_dcl_id nil)
  (setq taz_s_execution_design_dialog_result nil)
  (setq taz_s_execution_design_scale_selected_index 0)
  (setq taz_s_execution_design_frame_selected_index 5)
  (setq taz_s_execution_design_frame_format "A1")
  (setq taz_s_execution_design_apply_to_all nil)
  (setq taz_s_execution_design_apply_all_requested nil)
  (setq taz_s_execution_design_apply_all_checkbox_state T)

  (setq taz_s_execution_design_scale_values
    '(1 2 5 10 20 25 50 100 200)
  )

  (setq taz_s_execution_design_frame_formats_list
    '("A0"
      "A0+1" "A0+2" "A0+3" "A0+4"
      "A1" "A1+1" "A1+2" "A1+3" "A1+4"
      "A2" "A2+1" "A2+2" "A2+3" "A2+4"
      "A3" "A3+1" "A3+2" "A3+3" "A3+4"
      "A4")
  )

  ;; Kolejnosc generatora: IZO, wszystkie X, wszystkie Y, wszystkie Z.
  (setq taz_s_execution_design_case_scale_factors '())
  (setq taz_s_execution_design_case_frame_formats '())

  ;; Jesli skala byla juz ustawiona, zachowaj ja jako propozycje.
  (if (not taz_s_annotation_scale)
    (setq taz_s_annotation_scale 1)
  )

  (if (= taz_s_annotation_scale 1)   (setq taz_s_execution_design_scale_selected_index 0))
  (if (= taz_s_annotation_scale 2)   (setq taz_s_execution_design_scale_selected_index 1))
  (if (= taz_s_annotation_scale 5)   (setq taz_s_execution_design_scale_selected_index 2))
  (if (= taz_s_annotation_scale 10)  (setq taz_s_execution_design_scale_selected_index 3))
  (if (= taz_s_annotation_scale 20)  (setq taz_s_execution_design_scale_selected_index 4))
  (if (= taz_s_annotation_scale 25)  (setq taz_s_execution_design_scale_selected_index 5))
  (if (= taz_s_annotation_scale 50)  (setq taz_s_execution_design_scale_selected_index 6))
  (if (= taz_s_annotation_scale 100) (setq taz_s_execution_design_scale_selected_index 7))
  (if (= taz_s_annotation_scale 200) (setq taz_s_execution_design_scale_selected_index 8))

  ;; ---------------------------------
  ;; POMOCNICZA: NAZWA OSI Z WIERSZA
  ;; ---------------------------------

  (defun taz_s_execution_design_get_axis_name_from_row
    (taz_s_execution_design_row_arg
     /
     taz_s_execution_design_i
     taz_s_execution_design_len
     taz_s_execution_design_result)

    (setq taz_s_execution_design_i 2)
    (setq taz_s_execution_design_len (strlen taz_s_execution_design_row_arg))
    (setq taz_s_execution_design_result "")

    (while
      (and
        (<= taz_s_execution_design_i taz_s_execution_design_len)
        (/= (substr taz_s_execution_design_row_arg taz_s_execution_design_i 1) "]")
      )
      (setq taz_s_execution_design_result
        (strcat
          taz_s_execution_design_result
          (substr taz_s_execution_design_row_arg taz_s_execution_design_i 1)
        )
      )
      (setq taz_s_execution_design_i (+ taz_s_execution_design_i 1))
    )

    taz_s_execution_design_result
  )

  ;; ---------------------------------
  ;; POMOCNICZA: WARTOSC LICZBOWA Z WIERSZA OSI
  ;; ---------------------------------
  ;; Wiersz ma postac np. "[Z1]  3500.0".
  ;; Funkcja nie zmienia zadnego obiektu CAD - sluzy tylko do tytulu DCL.

  (defun taz_s_execution_design_get_value_from_row
    (taz_s_execution_design_row_arg
     /
     taz_s_execution_design_i
     taz_s_execution_design_len)

    (setq taz_s_execution_design_i 1)
    (setq taz_s_execution_design_len (strlen taz_s_execution_design_row_arg))

    (while
      (and
        (<= taz_s_execution_design_i taz_s_execution_design_len)
        (/= (substr taz_s_execution_design_row_arg taz_s_execution_design_i 1) "]")
      )
      (setq taz_s_execution_design_i (+ taz_s_execution_design_i 1))
    )

    ;; Przejdz za "]" i pomin dowolna liczbe spacji.
    (setq taz_s_execution_design_i (+ taz_s_execution_design_i 1))
    (while
      (and
        (<= taz_s_execution_design_i taz_s_execution_design_len)
        (= (substr taz_s_execution_design_row_arg taz_s_execution_design_i 1) " ")
      )
      (setq taz_s_execution_design_i (+ taz_s_execution_design_i 1))
    )

    (if (<= taz_s_execution_design_i taz_s_execution_design_len)
      (atof (substr taz_s_execution_design_row_arg taz_s_execution_design_i))
      0.0
    )
  )

  ;; ---------------------------------
  ;; POMOCNICZA: FORMAT POZIOMU DO TYTULU / DCL
  ;; ---------------------------------

  (defun taz_s_execution_design_get_level_title_value
    (taz_s_execution_design_level_mm_arg
     /
     taz_s_execution_design_level_old_dimzin
     taz_s_execution_design_level_value)

    (setq taz_s_execution_design_level_old_dimzin (getvar "DIMZIN"))
    (setvar "DIMZIN" 0)
    (setq taz_s_execution_design_level_value
      (rtos (/ taz_s_execution_design_level_mm_arg 1000.0) 2 3)
    )
    (setvar "DIMZIN" taz_s_execution_design_level_old_dimzin)

    taz_s_execution_design_level_value
  )

  ;; ---------------------------------
  ;; POMOCNICZA: DOKLADNY TYTUL PRZYPADKU
  ;; ---------------------------------

  (defun taz_s_execution_design_get_case_title
    (taz_s_execution_design_case_arg
     taz_s_execution_design_axis_name_arg
     taz_s_execution_design_level_mm_arg)

    (cond
      ((= taz_s_execution_design_case_arg "IZO")
        "WIDOK 3D"
      )

      ((or
         (= taz_s_execution_design_case_arg "X")
         (= taz_s_execution_design_case_arg "Y")
       )
        (strcat
          "PRZEKROJ "
          taz_s_execution_design_axis_name_arg
          " - "
          taz_s_execution_design_axis_name_arg
        )
      )

      ((= taz_s_execution_design_case_arg "Z")
        (strcat
          "RZUT POZIOMU "
          (taz_s_execution_design_get_level_title_value
            taz_s_execution_design_level_mm_arg
          )
          " m"
        )
      )

      (T "")
    )
  )

  ;; ---------------------------------
  ;; POMOCNICZA: ZAPIS USTAWIEN BIEZACEGO PRZYPADKU
  ;; ---------------------------------
  ;; Tutaj tylko zapisujemy dane. Nie uruchamiamy jeszcze funkcji skali,
  ;; nie rysujemy i nie zmieniamy geometrii.

  (defun taz_s_execution_design_store_current_case_settings ()

    (setq taz_s_execution_design_case_scale_factors
      (append
        taz_s_execution_design_case_scale_factors
        (list taz_s_annotation_scale)
      )
    )

    (setq taz_s_execution_design_case_frame_formats
      (append
        taz_s_execution_design_case_frame_formats
        (list taz_s_execution_design_frame_format)
      )
    )

    (princ)
  )

  ;; ---------------------------------
  ;; POMOCNICZA: DCL DLA JEDNEGO PRZYPADKU
  ;; ---------------------------------

  (defun taz_s_execution_design_show_case_dialog
    (taz_s_execution_design_case_title_arg)

    ;; Po "Zastosuj do wszystkich" nie wyswietlamy kolejnych okien,
    ;; ale nadal dopisujemy odziedziczone ustawienia do list.
    (if (= taz_s_execution_design_apply_to_all nil)
      (progn

        (setq taz_s_execution_design_dcl_id
          (load_dialog "taz_s_create_drawings_execution_design.dcl")
        )

        (if (< taz_s_execution_design_dcl_id 0)
          (progn
            (alert "Nie moge zaladowac pliku taz_s_create_drawings_execution_design.dcl.")
            (exit)
          )
        )

        (if
          (not
            (new_dialog
              "taz_s_create_drawings_execution_design_organize_dialog"
              taz_s_execution_design_dcl_id
            )
          )
          (progn
            (alert "Nie moge zaladowac okienka DCL.")
            (unload_dialog taz_s_execution_design_dcl_id)
            (exit)
          )
        )

        (set_tile
          "taz_s_execution_design_case_label"
          taz_s_execution_design_case_title_arg
        )

        (start_list "taz_s_annotation_scale_popup")
        (mapcar
          'add_list
          '("1:1" "1:2" "1:5" "1:10" "1:20" "1:25" "1:50" "1:100" "1:200")
        )
        (end_list)
        (set_tile
          "taz_s_annotation_scale_popup"
          (itoa taz_s_execution_design_scale_selected_index)
        )

        (start_list "taz_s_organize_frame_format_popup")
        (mapcar 'add_list taz_s_execution_design_frame_formats_list)
        (end_list)
        (set_tile
          "taz_s_organize_frame_format_popup"
          (itoa taz_s_execution_design_frame_selected_index)
        )

        (if taz_s_execution_design_apply_all_checkbox_state
          (set_tile "taz_s_execution_design_apply_all_toggle" "1")
          (set_tile "taz_s_execution_design_apply_all_toggle" "0")
        )
        (setq taz_s_execution_design_apply_all_requested nil)

        (action_tile "accept"
          "(progn
              (setq taz_s_execution_design_scale_selected_index
                (atoi (get_tile \"taz_s_annotation_scale_popup\"))
              )
              (setq taz_s_execution_design_frame_selected_index
                (atoi (get_tile \"taz_s_organize_frame_format_popup\"))
              )
              (setq taz_s_execution_design_apply_all_requested
                (= (get_tile \"taz_s_execution_design_apply_all_toggle\") \"1\")
              )
              (done_dialog 1)
          )"
        )

        (action_tile "cancel"
          "(done_dialog 0)"
        )

        (setq taz_s_execution_design_dialog_result (start_dialog))
        (unload_dialog taz_s_execution_design_dcl_id)

        ;; Na tym etapie nie zostal jeszcze utworzony zaden przypadek.
        (if (= taz_s_execution_design_dialog_result 0)
          (progn
            (setq taz_s_old_error *error*)
            (setq *error* (lambda (msg) (princ "")))
            (exit)
          )
        )

        (setq taz_s_annotation_scale
          (nth
            taz_s_execution_design_scale_selected_index
            taz_s_execution_design_scale_values
          )
        )

        (setq taz_s_execution_design_frame_format
          (nth
            taz_s_execution_design_frame_selected_index
            taz_s_execution_design_frame_formats_list
          )
        )

        (setq taz_s_execution_design_apply_all_checkbox_state
          taz_s_execution_design_apply_all_requested
        )

        (if taz_s_execution_design_apply_all_requested
          (setq taz_s_execution_design_apply_to_all T)
        )
      )
    )

    (taz_s_execution_design_store_current_case_settings)
    (princ)
  )

  ;; ---------------------------------
  ;; PREFLIGHT: PRZEJDZ PO WSZYSTKICH PRZYPADKACH
  ;; ---------------------------------

  (defun taz_s_execution_design_run_settings_preflight
    (/
     taz_s_execution_design_preflight_tmp
     taz_s_execution_design_preflight_row
     taz_s_execution_design_preflight_level)

    ;; IZO
    (taz_s_execution_design_show_case_dialog
      (taz_s_execution_design_get_case_title "IZO" nil nil)
    )

    ;; X
    (setq taz_s_execution_design_preflight_tmp taz_s_x_data)
    (while taz_s_execution_design_preflight_tmp
      (setq taz_s_execution_design_preflight_row
        (car taz_s_execution_design_preflight_tmp)
      )
      (taz_s_execution_design_show_case_dialog
        (taz_s_execution_design_get_case_title
          "X"
          (taz_s_execution_design_get_axis_name_from_row
            taz_s_execution_design_preflight_row
          )
          nil
        )
      )
      (setq taz_s_execution_design_preflight_tmp
        (cdr taz_s_execution_design_preflight_tmp)
      )
    )

    ;; Y
    (setq taz_s_execution_design_preflight_tmp taz_s_y_data)
    (while taz_s_execution_design_preflight_tmp
      (setq taz_s_execution_design_preflight_row
        (car taz_s_execution_design_preflight_tmp)
      )
      (taz_s_execution_design_show_case_dialog
        (taz_s_execution_design_get_case_title
          "Y"
          (taz_s_execution_design_get_axis_name_from_row
            taz_s_execution_design_preflight_row
          )
          nil
        )
      )
      (setq taz_s_execution_design_preflight_tmp
        (cdr taz_s_execution_design_preflight_tmp)
      )
    )

    ;; Z
    (setq taz_s_execution_design_preflight_tmp taz_s_z_data)
    (while taz_s_execution_design_preflight_tmp
      (setq taz_s_execution_design_preflight_row
        (car taz_s_execution_design_preflight_tmp)
      )
      (setq taz_s_execution_design_preflight_level
        (taz_s_execution_design_get_value_from_row
          taz_s_execution_design_preflight_row
        )
      )
      (taz_s_execution_design_show_case_dialog
        (taz_s_execution_design_get_case_title
          "Z"
          nil
          taz_s_execution_design_preflight_level
        )
      )
      (setq taz_s_execution_design_preflight_tmp
        (cdr taz_s_execution_design_preflight_tmp)
      )
    )

    (princ)
  )

  ;; ---------------------------------
  ;; POMOCNICZA: UZYJ USTAWIEN KOLEJNEGO PRZYPADKU
  ;; ---------------------------------
  ;; Wywolywana podczas generowania. Nie wyswietla DCL.

  (defun taz_s_execution_design_use_next_case_settings ()

    (if
      (and
        (<
          taz_s_execution_design_current_case_settings_index
          (length taz_s_execution_design_case_scale_factors)
        )
        (<
          taz_s_execution_design_current_case_settings_index
          (length taz_s_execution_design_case_frame_formats)
        )
      )
      (progn
        (setq taz_s_annotation_scale
          (nth
            taz_s_execution_design_current_case_settings_index
            taz_s_execution_design_case_scale_factors
          )
        )

        (setq taz_s_execution_design_frame_format
          (nth
            taz_s_execution_design_current_case_settings_index
            taz_s_execution_design_case_frame_formats
          )
        )

        (taz_s_annotation_scale_apply)

        (setq taz_s_execution_design_frame_scale_factor
          taz_s_annotation_scale
        )

        (setq taz_s_st_offset (* 100.0 taz_s_annotation_scale))

        (setq taz_s_execution_design_current_case_settings_index
          (+ taz_s_execution_design_current_case_settings_index 1)
        )
      )
      (progn
        (alert "Brak zapisanych ustawien dla kolejnego przypadku.")
        (exit)
      )
    )

    (princ)
  )

  ;; Wszystkie pytania sa wykonywane TERAZ.
  (taz_s_execution_design_run_settings_preflight)
  (setq taz_s_execution_design_current_case_settings_index 0)

  ;; ---------------------------------
  ;; DOPIERO TERAZ PRZYGOTOWANIE RYSUNKU
  ;; ---------------------------------

  (taz_s_current_settings_save)
  (taz_s_unlock_all_layers)

  ;; UCS WORLD
  (command "_UCS" "_W")

  ;; ---------------------------------
  ;; POBRANIE ODLEGLOSCI
  ;; ---------------------------------

  (defun taz_s_get_dist ()
    (setq taz_s_i 1)
    (setq taz_s_len (strlen taz_s_row))
    (while
      (and
        (<= taz_s_i taz_s_len)
        (/= (substr taz_s_row taz_s_i 1) "]")
      )
      (setq taz_s_i (+ taz_s_i 1))
    )
    (setq taz_s_i (+ taz_s_i 3))
    (setq taz_s_val
      (atof (substr taz_s_row taz_s_i))
    )
  )

  ;; ---------------------------------
  ;; MIN
  ;; ---------------------------------

  (defun taz_s_min ()
    (setq taz_s_m (car taz_s_list))
    (setq taz_s_list (cdr taz_s_list))
    (while taz_s_list
      (if (< (car taz_s_list) taz_s_m)
        (setq taz_s_m (car taz_s_list))
      )
      (setq taz_s_list (cdr taz_s_list))
    )
  )

  ;; ---------------------------------
  ;; MAX
  ;; ---------------------------------

  (defun taz_s_max ()
    (setq taz_s_m (car taz_s_list))
    (setq taz_s_list (cdr taz_s_list))
    (while taz_s_list
      (if (> (car taz_s_list) taz_s_m)
        (setq taz_s_m (car taz_s_list))
      )
      (setq taz_s_list (cdr taz_s_list))
    )
  )

  ;; ---------------------------------
  ;; X VALUES
  ;; ---------------------------------

  (setq taz_s_xvals '())
  (setq taz_s_tmp taz_s_x_data)
  (while taz_s_tmp
    (setq taz_s_row (car taz_s_tmp))
    (taz_s_get_dist)
    (setq taz_s_xvals (append taz_s_xvals (list taz_s_val)))
    (setq taz_s_tmp (cdr taz_s_tmp))
  )

  ;; ---------------------------------
  ;; Y VALUES
  ;; ---------------------------------

  (setq taz_s_yvals '())
  (setq taz_s_tmp taz_s_y_data)
  (while taz_s_tmp
    (setq taz_s_row (car taz_s_tmp))
    (taz_s_get_dist)
    (setq taz_s_yvals (append taz_s_yvals (list taz_s_val)))
    (setq taz_s_tmp (cdr taz_s_tmp))
  )

  ;; ---------------------------------
  ;; Z VALUES
  ;; ---------------------------------

  (setq taz_s_zvals '())
  (setq taz_s_tmp taz_s_z_data)
  (while taz_s_tmp
    (setq taz_s_row (car taz_s_tmp))
    (taz_s_get_dist)
    (setq taz_s_zvals (append taz_s_zvals (list taz_s_val)))
    (setq taz_s_tmp (cdr taz_s_tmp))
  )

  ;; ---------------------------------
  ;; GRANICE MODELU
  ;; ---------------------------------

  (setq taz_s_list taz_s_yvals) (taz_s_min) (setq taz_s_xmin taz_s_m)
  (setq taz_s_list taz_s_yvals) (taz_s_max) (setq taz_s_xmax taz_s_m)
  (setq taz_s_list taz_s_xvals) (taz_s_min) (setq taz_s_ymin taz_s_m)
  (setq taz_s_list taz_s_xvals) (taz_s_max) (setq taz_s_ymax taz_s_m)
  (setq taz_s_list taz_s_zvals) (taz_s_min) (setq taz_s_zmin taz_s_m)
  (setq taz_s_list taz_s_zvals) (taz_s_max) (setq taz_s_zmax taz_s_m)
  
  ;; ---------------------------------
  ;; GRANICE BEZ MARGINESU
  ;; ---------------------------------
  
  (setq taz_s_xmin_nomargin taz_s_xmin)
  (setq taz_s_xmax_nomargin taz_s_xmax)
  (setq taz_s_ymin_nomargin taz_s_ymin)
  (setq taz_s_ymax_nomargin taz_s_ymax)
  (setq taz_s_zmin_nomargin taz_s_zmin)
  (setq taz_s_zmax_nomargin taz_s_zmax)

  ;; ---------------------------------
  ;; MARGINES PROSTOKATOW
  ;; ---------------------------------

  (setq taz_s_xmin (- taz_s_xmin 1000))
  (setq taz_s_xmax (+ taz_s_xmax 1000))
  (setq taz_s_ymin (- taz_s_ymin 1000))
  (setq taz_s_ymax (+ taz_s_ymax 1000))
  (setq taz_s_zmin (- taz_s_zmin 1000))
  (setq taz_s_zmax (+ taz_s_zmax 1000))

  ;; ---------------------------------
  ;; WARSTWA execution_design
  ;; ---------------------------------

  (if
    (not (tblsearch "LAYER" "taz_s_execution_design"))
    (command "_LAYER" "_M" "taz_s_execution_design" "_C" "30" "" "")
  )

  ;; ---------------------------------
  ;; CZYSZCZENIE WARSTWY execution_design
  ;; ---------------------------------

  (setq taz_s_ss
    (ssget "X" '((8 . "taz_s_execution_design")))
  )
  (if taz_s_ss
    (command "ERASE" taz_s_ss "")
  )

  ;; ---------------------------------
  ;; CZYSZCZENIE WARSTWY editing_layer
  ;; (wyniki poprzednich intersectow jesli skrypt byl juz uruchamiany)
  ;; ---------------------------------

  (setq taz_s_ss
    (ssget "X" '((8 . "taz_s_editing_layer")))
  )
  (if taz_s_ss
    (command "ERASE" taz_s_ss "")
  )

  ;; ---------------------------------
  ;; SELEKCJA ORYGINALU - raz, przed wszystkimi petlami
  ;;
  ;; Zbieramy enames oryginalu teraz gdy w rysunku sa tylko:
  ;;   - oryginalny model
  ;;   - osie (taz_s_axes)
  ;; Wykluczone: osie, execution_design, editing_layer
  ;; ---------------------------------

  (setq taz_s_orig_ss
    (ssget "X"
      (list
        (cons -4 "<AND")
        (cons 67 0)                                          ; tylko model space
        (cons -4 "<NOT") (cons 8 "taz_s_axes")             (cons -4 "NOT>")
        (cons -4 "<NOT") (cons 8 "taz_s_execution_design") (cons -4 "NOT>")
        (cons -4 "<NOT") (cons 8 "taz_s_editing_layer")    (cons -4 "NOT>")
        (cons -4 "AND>")
      )
    )
  )

  ;; Zapamietaj tylko osie istniejace przed tworzeniem nowych widokow
  (setq taz_s_orig_axes_ss
    (ssget "X"
      (list
        (cons 67 0)
        (cons 8 "taz_s_axes")
      )
    )
  )

  ;; Zamien selection set na liste enames - bedziemy ja uzywac
  ;; do wykluczania oryginalu przy ssget w kazdym przypadku
  (setq taz_s_orig_enames '())
  (if taz_s_orig_ss
    (progn
      (setq taz_s_oi 0)
      (while (< taz_s_oi (sslength taz_s_orig_ss))
        (setq taz_s_orig_enames
          (append taz_s_orig_enames
            (list (ssname taz_s_orig_ss taz_s_oi))
          )
        )
        (setq taz_s_oi (+ taz_s_oi 1))
      )
    )
  )

  ;; ---------------------------------
  ;; POMOCNICZA: PRZENIES ATRYBUTY Z ORYGINALU NA KOPIE
  ;; Zaraz po COPY oryginal i kopia istnieja obok siebie. Nowe
  ;; kopie powstaja w tej samej kolejnosci co taz_s_orig_enames,
  ;; wiec parujemy je pozycyjnie. Dla kazdej pary tworzymy nowa
  ;; zmienna globalna pod handle KOPII, z wartoscia skopiowana
  ;; z odpowiedniego oryginalu - jesli oryginal w ogole ja mial.
  ;; ---------------------------------

  (defun taz_s_copy_attrs_to_copies (taz_s_last_before)
    (setq taz_s_new_ent (entnext taz_s_last_before))
    (setq taz_s_map_index 0)
    (while taz_s_new_ent
      (setq taz_s_orig_h
        (cdr (assoc 5 (entget (nth taz_s_map_index taz_s_orig_enames))))
      )
      (setq taz_s_new_h (cdr (assoc 5 (entget taz_s_new_ent))))

      (setq taz_s_orig_attr6_sym (read (strcat "taz_s_" taz_s_orig_h "_attr6")))
      (if (boundp taz_s_orig_attr6_sym)
        (set (read (strcat "taz_s_" taz_s_new_h "_attr6")) (eval taz_s_orig_attr6_sym))
      )

      (setq taz_s_orig_attr7_sym (read (strcat "taz_s_" taz_s_orig_h "_attr7")))
      (if (boundp taz_s_orig_attr7_sym)
        (set (read (strcat "taz_s_" taz_s_new_h "_attr7")) (eval taz_s_orig_attr7_sym))
      )
      
      (setq taz_s_orig_sweep_p1_sym (read (strcat "taz_s_" taz_s_orig_h "_sweep_p1")))
      (if (boundp taz_s_orig_sweep_p1_sym)
        (set (read (strcat "taz_s_" taz_s_new_h "_sweep_p1")) (eval taz_s_orig_sweep_p1_sym))
      )

      (setq taz_s_orig_sweep_p2_sym (read (strcat "taz_s_" taz_s_orig_h "_sweep_p2")))
      (if (boundp taz_s_orig_sweep_p2_sym)
        (set (read (strcat "taz_s_" taz_s_new_h "_sweep_p2")) (eval taz_s_orig_sweep_p2_sym))
      )

      (setq taz_s_map_index (+ taz_s_map_index 1))
      (setq taz_s_new_ent (entnext taz_s_new_ent))
    )
  )

  ;; ---------------------------------
  ;; POMOCNICZA: sprawdz czy ename jest na liscie oryginalu
  ;; ---------------------------------

  (defun taz_s_is_original (taz_s_ent)
    (setq taz_s_found nil)
    (setq taz_s_oi 0)
    (while (< taz_s_oi (length taz_s_orig_enames))
      (if (equal taz_s_ent (nth taz_s_oi taz_s_orig_enames))
        (setq taz_s_found T)
      )
      (setq taz_s_oi (+ taz_s_oi 1))
    )
    taz_s_found
  )
  
  ;; ---------------------------------
  ;; POMOCNICZA: SRODEK SCIEZKI SWEEP ORYGINALNEGO PROFILU
  ;; ---------------------------------

  (defun taz_s_get_center (taz_s_ent)
    (setq taz_s_annotation_h (cdr (assoc 5 (entget taz_s_ent))))
    (setq taz_s_annotation_p1
      (eval (read (strcat "taz_s_" taz_s_annotation_h "_sweep_p1")))
    )
    (setq taz_s_annotation_p2
      (eval (read (strcat "taz_s_" taz_s_annotation_h "_sweep_p2")))
    )
    (if (and taz_s_annotation_p1 taz_s_annotation_p2)
      (list
        (/ (+ (car taz_s_annotation_p1) (car taz_s_annotation_p2)) 2.0)
        (/ (+ (cadr taz_s_annotation_p1) (cadr taz_s_annotation_p2)) 2.0)
        (+ (/ (+ (caddr taz_s_annotation_p1) (caddr taz_s_annotation_p2)) 2.0) taz_s_zoffset)
      )
      (progn
        ;;(princ (strcat "\nUWAGA: brak danych linii sterujacej " taz_s_annotation_h ", uzywam (0,0,0)"))
        (list 0.0 0.0 taz_s_zoffset)
      )
    )
  )
  
  ;; ---------------------------------
  ;; POMOCNICZA: PRZECIECIE ODCINKA Z PLASZCZYZNA (X=const lub Y=const)
  ;; taz_s_annotation_coord_index: 0 = wspolrzedna X, 1 = wspolrzedna Y
  ;; Zwraca punkt przeciecia jesli odcinek p1-p2 rzeczywiscie
  ;; przecina te plaszczyzne (t w zakresie 0..1), w przeciwnym
  ;; razie nil (linia rownolegla do plaszczyzny lub przeciecie
  ;; poza odcinkiem).
  ;; ---------------------------------

  (defun taz_s_line_plane_intersect (taz_s_annotation_p1 taz_s_annotation_p2 taz_s_annotation_coord_index taz_s_annotation_target_val)
    (setq taz_s_annotation_v1 (nth taz_s_annotation_coord_index taz_s_annotation_p1))
    (setq taz_s_annotation_v2 (nth taz_s_annotation_coord_index taz_s_annotation_p2))
    (setq taz_s_annotation_denom (- taz_s_annotation_v2 taz_s_annotation_v1))
    (if (equal taz_s_annotation_denom 0.0 1e-8)
      nil
      (progn
        (setq taz_s_annotation_t (/ (- taz_s_annotation_target_val taz_s_annotation_v1) taz_s_annotation_denom))
        (if (and (>= taz_s_annotation_t 0.0) (<= taz_s_annotation_t 1.0))
          (list
            (+ (car   taz_s_annotation_p1) (* taz_s_annotation_t (- (car   taz_s_annotation_p2) (car   taz_s_annotation_p1))))
            (+ (cadr  taz_s_annotation_p1) (* taz_s_annotation_t (- (cadr  taz_s_annotation_p2) (cadr  taz_s_annotation_p1))))
            (+ (caddr taz_s_annotation_p1) (* taz_s_annotation_t (- (caddr taz_s_annotation_p2) (caddr taz_s_annotation_p1))))
          )
          nil
        )
      )
    )
  )

  ;; ---------------------------------
  ;; POMOCNICZA: PUNKT PRZECIECIA SCIEZKI SWEEP Z PLASZCZYZNA CIECIA
  ;; Zwraca punkt lub nil jesli sciezka nie przecina danej plaszczyzny
  ;; ---------------------------------

  (defun taz_s_get_sweep_plane_point (taz_s_annotation_ent taz_s_annotation_coord_index taz_s_annotation_target_val)
    (setq taz_s_annotation_sp_h (cdr (assoc 5 (entget taz_s_annotation_ent))))
    (setq taz_s_annotation_sp1 (eval (read (strcat "taz_s_" taz_s_annotation_sp_h "_sweep_p1"))))
    (setq taz_s_annotation_sp2 (eval (read (strcat "taz_s_" taz_s_annotation_sp_h "_sweep_p2"))))
    (if (and taz_s_annotation_sp1 taz_s_annotation_sp2)
      (taz_s_line_plane_intersect taz_s_annotation_sp1 taz_s_annotation_sp2 taz_s_annotation_coord_index taz_s_annotation_target_val)
      nil
    )
  )
  

  ;; ---------------------------------
  ;; POMOCNICZA: INTERSECT PARAMI
  ;;
  ;; Argumenty:
  ;;   taz_s_cut_ename  - ename bryly tnacej (wzorzec)
  ;;   taz_s_elems_list - lista ename elementow kopii do obrobki
  ;;
  ;; Przed kazdym intersectem ustawia warstwe na taz_s_editing_layer
  ;; dzieki czemu wyniki intersect trafiaja na te warstwe.
  ;; Dla wszystkich elementow oprocz ostatniego: kopiuje bryle tnaca
  ;; w to samo miejsce i uzywa duplikatu. Ostatni element: uzywa
  ;; oryginalnej bryly tnacej bezposrednio (oszczednosc jednego COPY).
  ;;
  ;; Dodatkowo: przed kazdym intersectem sprawdzamy przez -INTERFERE
  ;; (na warstwie "0") czy przeciecie danej pary w ogole wystepuje.
  ;; Jesli tak - entlast sie zmienia (powstaje bryla interferencji) -
  ;; wtedy sprzatamy wszystko co powstalo na warstwie "0". Jesli nie -
  ;; entlast sie nie zmienia - nic nie sprzatamy, bo nic nie powstalo.
  ;; Po samym -INTERFERE lecimy pusta komenda kilka razy, zeby
  ;; wyzerowac linie polecen niezaleznie od tego czy padlo pytanie
  ;; o utworzenie bryly wynikowej czy nie.
  ;; ---------------------------------
  (defun taz_s_intersect_pairs (taz_s_cut_ename taz_s_elems_list taz_s_case)
    (setq taz_s_visible_handles '())
    (setq taz_s_ei 0)
    (setq taz_s_total_elems (length taz_s_elems_list))
    (while (< taz_s_ei taz_s_total_elems)
      (setq taz_s_target_ent (nth taz_s_ei taz_s_elems_list))
      (setq taz_s_orig_ent (nth taz_s_ei taz_s_orig_enames))
      ;; --- SPRAWDZENIE CZY WYSTEPUJE PRZECIECIE (-INTERFERE) ---
      ;; Kopiujemy bryle tnaca na miejsce oryginalu (bez zoffset),
      ;; sprawdzamy przeciecie wzgledem ORYGINALU, potem kasujemy kopie.
      (setvar "CLAYER" "taz_s_editing_layer")
      (setq taz_s_cut_tmp_ss (ssadd))
      (ssadd taz_s_cut_ename taz_s_cut_tmp_ss)
      (command "COPY" taz_s_cut_tmp_ss "" "0,0,0" (list 0 0 (- taz_s_zoffset)))
      (setq taz_s_cut_tmp_ent (entlast))
      (setq taz_s_layer0_ss_before (ssget "X" (list (cons 8 "taz_s_editing_layer"))))
      (setq taz_s_layer0_count_before (if taz_s_layer0_ss_before (sslength taz_s_layer0_ss_before) 0))
      (setq taz_s_if_set1 (ssadd))
      (ssadd taz_s_cut_tmp_ent taz_s_if_set1)
      (setq taz_s_if_set2 (ssadd))
      (ssadd taz_s_orig_ent taz_s_if_set2)
      (command "-INTERFERE" taz_s_if_set1 "" taz_s_if_set2 "" "Y")
      (command)
      (command)
      (command)
      (setq taz_s_layer0_ss (ssget "X" (list (cons 8 "taz_s_editing_layer"))))
      (setq taz_s_layer0_count_after (if taz_s_layer0_ss (sslength taz_s_layer0_ss) 0))
      (if (> taz_s_layer0_count_after taz_s_layer0_count_before)
        (progn
          (if taz_s_layer0_ss
            (command "ERASE" taz_s_layer0_ss "")
          )
          (setq taz_s_visible_handles
            (append taz_s_visible_handles
              (list (cdr (assoc 5 (entget taz_s_orig_ent))))
            )
          )
          (setq taz_s_annotation_text
          (strcat
            (eval (read (strcat "taz_s_" (cdr (assoc 5 (entget taz_s_orig_ent))) "_attr6")))
            " "
            (eval (read (strcat "taz_s_" (cdr (assoc 5 (entget taz_s_orig_ent))) "_attr7")))
          )
          )
          
          (if (= (eval (read (strcat "taz_s_" (cdr (assoc 5 (entget taz_s_orig_ent))) "_attr6"))) "LR")
            (progn
              (setq taz_s_annotation_text
                (strcat
                  "L "
                  (eval (read (strcat "taz_s_" (cdr (assoc 5 (entget taz_s_orig_ent))) "_attr7")))
                )
              )
            )
          )
          
          (if (= (eval (read (strcat "taz_s_" (cdr (assoc 5 (entget taz_s_orig_ent))) "_attr6"))) "LN")
            (progn
              (setq taz_s_annotation_text
                (strcat
                  "L "
                  (eval (read (strcat "taz_s_" (cdr (assoc 5 (entget taz_s_orig_ent))) "_attr7")))
                )
              )
            )
          )
          
          ;; Punkt wstawienia - skorygowany wzgledem plaszczyzny ciecia
          (setq taz_s_annotation_ins_pt (taz_s_get_center taz_s_orig_ent))
          (cond
            ((= taz_s_case "X")
             (setq taz_s_annotation_plane_pt
               (taz_s_get_sweep_plane_point taz_s_orig_ent 1 taz_s_y)
             )
             (if taz_s_annotation_plane_pt
               (setq taz_s_annotation_ins_pt
                 (list
                   (car   taz_s_annotation_plane_pt)
                   taz_s_y
                   (+ (caddr taz_s_annotation_plane_pt) taz_s_zoffset)
                 )
               )
               (setq taz_s_annotation_ins_pt
                 (list
                   (car   taz_s_annotation_ins_pt)
                   taz_s_y
                   (caddr taz_s_annotation_ins_pt)
                 )
               )
             )
            )
            ((= taz_s_case "Y")
             (setq taz_s_annotation_plane_pt
               (taz_s_get_sweep_plane_point taz_s_orig_ent 0 taz_s_x)
             )
             (if taz_s_annotation_plane_pt
               (setq taz_s_annotation_ins_pt
                 (list
                   taz_s_x
                   (cadr  taz_s_annotation_plane_pt)
                   (+ (caddr taz_s_annotation_plane_pt) taz_s_zoffset)
                 )
               )
               (setq taz_s_annotation_ins_pt
                 (list
                   taz_s_x
                   (cadr  taz_s_annotation_ins_pt)
                   (caddr taz_s_annotation_ins_pt)
                 )
               )
             )
            )
            ((= taz_s_case "Z")
             (setq taz_s_annotation_plane_pt
               (taz_s_get_sweep_plane_point taz_s_orig_ent 2 taz_s_z)
             )
             (if taz_s_annotation_plane_pt
               (setq taz_s_annotation_ins_pt
                 (list
                   (car  taz_s_annotation_plane_pt)
                   (cadr taz_s_annotation_plane_pt)
                   (+ taz_s_z taz_s_zoffset)
                 )
               )
               (setq taz_s_annotation_ins_pt
                 (list
                   (car  taz_s_annotation_ins_pt)
                   (cadr taz_s_annotation_ins_pt)
                   (+ taz_s_z taz_s_zoffset)
                 )
               )
             )
            )
          )
          (entmake
            (list
              (cons 0 "MTEXT")
              (cons 10 taz_s_annotation_ins_pt)
              (cons 1 taz_s_annotation_text)
              (cons 7 "Standard")
              (cons 8 "taz_s_labels")   ; <- warstwa od razu przy tworzeniu
              (cons 40 taz_s_annotation_scale_label) ; wysokość tekstu
              (cons 71 5)   ; wyrównanie: 5 = środek centrum
              (cons 90 16)
            )
          )
          ;; Obrot etykiety do plaszczyzny ciecia (analogicznie do opisow osi)
          (cond
            ((= taz_s_case "X")
             (command "_.ROTATE3D" (entlast) "" "X" taz_s_annotation_ins_pt "90")
            )
            ((= taz_s_case "Y")
             (command "_.ROTATE3D" (entlast) "" "Y" taz_s_annotation_ins_pt "90")
             (command "_.ROTATE3D" (entlast) "" "X" taz_s_annotation_ins_pt "90")
            )
          )
        )
      )
      ;; usun tymczasowa kopie bryly tnacej - nie jest juz potrzebna
      (entdel taz_s_cut_tmp_ent)
      ;; --- KONIEC SPRAWDZENIA ---
      ;; Zawsze kopiuj bryle tnaca - oryginał zostaje nienaruszony
      (setq taz_s_cut_ss1 (ssadd))
      (ssadd taz_s_cut_ename taz_s_cut_ss1)
      (command "COPY" taz_s_cut_ss1 "" "0,0,0" "0,0,0")
      (setq taz_s_cut_work_ent (entlast))
      (setvar "CLAYER" "taz_s_editing_layer")
      (setq taz_s_int_ss (ssadd))
      (ssadd taz_s_cut_work_ent taz_s_int_ss)      
      (ssadd taz_s_target_ent   taz_s_int_ss)
      (command "INTERSECT" taz_s_int_ss "")
      (setq taz_s_ei (+ taz_s_ei 1))
    )
    ;; Oryginal bryly tnacej nigdy nie byl uzyty w INTERSECT
    ;; wiec na pewno nadal istnieje - kasujemy go tutaj
    (entdel taz_s_cut_ename)
  )

  ;; ---------------------------------
  ;; POMOCNICZA: ZBIERZ ENAMES KOPII BIEZACEGO PRZYPADKU
  ;;
  ;; Pobiera wszystko oprocz:
  ;;   - warstwy osi (taz_s_axes)
  ;;   - warstwy bryly tnacej (taz_s_execution_design)
  ;;   - warstwy wynikow intersect (taz_s_editing_layer)
  ;; ...a nastepnie wyklucza oryginalne enames modelu.
  ;; To co zostaje to wylacznie elementy skopiowane dla biezacego przypadku.
  ;; ---------------------------------

  (defun taz_s_collect_copy_enames ()

    (setq taz_s_copy_enames '())

    (setq taz_s_all_candidate
      (ssget "X"
        (list
          (cons -4 "<AND")
          (cons 67 0)
          (cons -4 "<NOT") (cons 8 "taz_s_axes")             (cons -4 "NOT>")
          (cons -4 "<NOT") (cons 8 "taz_s_execution_design") (cons -4 "NOT>")
          (cons -4 "<NOT") (cons 8 "taz_s_editing_layer")    (cons -4 "NOT>")
          (cons -4 "AND>")
        )
      )
    )

    (if taz_s_all_candidate
      (progn
        (setq taz_s_ci 0)
        (while (< taz_s_ci (sslength taz_s_all_candidate))
          (setq taz_s_cand_ent (ssname taz_s_all_candidate taz_s_ci))

          ;; FILTR: tylko 3DSOLID
          (setq taz_s_ed (entget taz_s_cand_ent))
          (setq taz_s_type (cdr (assoc 0 taz_s_ed)))

          (if (and
                (equal taz_s_type "3DSOLID")
                (not (taz_s_is_original taz_s_cand_ent))
              )
            (setq taz_s_copy_enames
              (append taz_s_copy_enames (list taz_s_cand_ent))
            )
          )

          (setq taz_s_ci (+ taz_s_ci 1))
        )
      )
    )

    taz_s_copy_enames
  )


  ;; ---------------------------------
  ;; POMOCNICZA: POBIERZ NAZWE OSI Z WIERSZA
  ;; Format wiersza: "[X1]  5000.0"
  ;; Zwraca np. "X1"
  ;; ---------------------------------

  (defun taz_s_get_axis_name (taz_s_row_arg)
    (setq taz_s_ni 2)
    (setq taz_s_nres "")
    (while (/= (substr taz_s_row_arg taz_s_ni 1) "]")
      (setq taz_s_nres (strcat taz_s_nres (substr taz_s_row_arg taz_s_ni 1)))
      (setq taz_s_ni (+ taz_s_ni 1))
    )
    taz_s_nres
  )

  ;; ---------------------------------
  ;; POMOCNICZA: TEKST SKALI POD TYTULEM
  ;;
  ;; taz_s_annotation_scale jest mnoznikiem skali:
  ;; 1.0 = 1:1, 50.0 = 1:50 itd.
  ;; Zwracamy tekst np. "Skala 1:50".
  ;; Druga linia tytulu ma w MTEXT wysokosc 0.5x wysokosci tytulu,
  ;; czyli 2.5 w skali 1:1 i proporcjonalnie dla pozostalych skal.
  ;; ---------------------------------

  (defun taz_s_get_scale_title_text ()

    (setq taz_s_scale_title_value
      (if
        (equal
          taz_s_annotation_scale
          (float (fix taz_s_annotation_scale))
          1e-8
        )
        (itoa (fix taz_s_annotation_scale))
        (rtos taz_s_annotation_scale 2 3)
      )
    )

    (strcat "Skala 1:" taz_s_scale_title_value)
  )

  ;; ---------------------------------
  ;; POMOCNICZA: TYTUL WIDOKU DLA PRZYPADKOW X / Y
  ;;
  ;; Tresc:  PRZEKROJ [nazwa osi] - [nazwa osi] + druga linia "Skala 1:X"
  ;; Wysokosc: tytul 5.0, skala 2.5 w skali 1:1; obie skalowane
  ;;            przez taz_s_annotation_scale
  ;; Polozenie: centralnie, 500 jednostek nad gorna krawedzia przypadku
  ;; Warstwa: taz_s_labels
  ;; ---------------------------------

  (defun taz_s_create_view_title (taz_s_case taz_s_axis_name_arg)

    (setq taz_s_view_title_pt nil)

    (setq taz_s_view_title_text
      (strcat
        (taz_s_execution_design_get_case_title
          taz_s_case
          taz_s_axis_name_arg
          nil
        )
        "\\P\\H0.5x;"
        (taz_s_get_scale_title_text)
      )
    )

    (setq taz_s_view_title_height
      (* 5.0 taz_s_annotation_scale)
    )

    (cond
      ((= taz_s_case "X")
        (setq taz_s_view_title_pt
          (list
            (/ (+ taz_s_xmin taz_s_xmax) 2.0)
            taz_s_y
            (+ taz_s_zmax taz_s_zoffset 500.0)
          )
        )
      )

      ((= taz_s_case "Y")
        (setq taz_s_view_title_pt
          (list
            taz_s_x
            (/ (+ taz_s_ymin taz_s_ymax) 2.0)
            (+ taz_s_zmax taz_s_zoffset 500.0)
          )
        )
      )
    )

    (if taz_s_view_title_pt
      (progn
        (entmake
          (list
            (cons 0 "MTEXT")
            (cons 10 taz_s_view_title_pt)
            (cons 1 taz_s_view_title_text)
            (cons 7 "Standard")
            (cons 8 "taz_s_labels")
            (cons 40 taz_s_view_title_height)
            (cons 71 5)
          )
        )

        ;; Ustaw tekst w tej samej plaszczyznie co dany widok.
        (cond
          ((= taz_s_case "X")
            (command
              "_.ROTATE3D"
              (entlast)
              ""
              "X"
              taz_s_view_title_pt
              "90"
            )
          )

          ((= taz_s_case "Y")
            (command
              "_.ROTATE3D"
              (entlast)
              ""
              "Y"
              taz_s_view_title_pt
              "90"
            )
            (command
              "_.ROTATE3D"
              (entlast)
              ""
              "X"
              taz_s_view_title_pt
              "90"
            )
          )
        )
      )
    )

    (princ)
  )

  ;; ---------------------------------
  ;; POMOCNICZA: TYTUL RZUTU DLA PRZYPADKOW Z
  ;;
  ;; Poziom jest pobierany bezposrednio z taz_s_z, czyli z tej samej
  ;; wartosci, ktora steruje plaszczyzna ciecia danego przypadku Z.
  ;; Dane osi sa w mm, dlatego do tytulu poziom jest dzielony przez 1000.0.
  ;; Tresc:  RZUT POZIOMU [poziom w m] + druga linia "Skala 1:X"
  ;; Wysokosc: tytul 5.0, skala 2.5 w skali 1:1; obie skalowane
  ;;            przez taz_s_annotation_scale
  ;; Polozenie: centralnie, 500 jednostek nad gorna krawedzia przypadku
  ;; Warstwa: taz_s_labels
  ;; ---------------------------------

  (defun taz_s_create_level_title (taz_s_level_mm)

    ;; Ta sama funkcja buduje pierwsza linie tytulu w DCL i na rysunku.
    ;; Dzieki temu nazwa przypadku w oknie jest identyczna z tytulem widoku.
    (setq taz_s_level_title_value
      (taz_s_execution_design_get_level_title_value taz_s_level_mm)
    )

    (setq taz_s_level_title_text
      (strcat
        (taz_s_execution_design_get_case_title "Z" nil taz_s_level_mm)
        "\\P\\H0.5x;"
        (taz_s_get_scale_title_text)
      )
    )

    (setq taz_s_level_title_height
      (* 5.0 taz_s_annotation_scale)
    )

    (setq taz_s_level_title_pt
      (list
        (/ (+ taz_s_xmin taz_s_xmax) 2.0)
        (+ taz_s_ymax 500.0)
        (+ taz_s_level_mm taz_s_zoffset)
      )
    )

    (entmake
      (list
        (cons 0 "MTEXT")
        (cons 10 taz_s_level_title_pt)
        (cons 1 taz_s_level_title_text)
        (cons 7 "Standard")
        (cons 8 "taz_s_labels")
        (cons 40 taz_s_level_title_height)
        (cons 71 5)
      )
    )

    (princ)
  )

  ;; =================================================================
  ;; GLOWNA PETLA - jeden przypadek na raz:
  ;;   1. Narysuj bryle tnaca w strefie Z tego przypadku
  ;;   2. Skopiuj oryginalny model do tej samej strefy Z
  ;;   3. Zbierz enames kopii (bez oryginalu, bez pomocniczych warstw)
  ;;   4. Intersect parami (wyniki na taz_s_editing_layer)
  ;; =================================================================

  ;; ---------------------------------
  ;; ZAPAMIETANIE TABEL DLA ORGANIZERA
  ;; ---------------------------------
  ;; Dla kazdego przypadku przechowujemy:
  ;; - liste wszystkich obiektow utworzonych przez taz_s_create_steel_table,
  ;; - punkt kotwiczenia przekazany do tej funkcji.
  ;;
  ;; Listy maja dokladnie taka sama kolejnosc jak przypadki X, Y, Z.

  (setq taz_s_execution_design_table_groups '())
  (setq taz_s_execution_design_table_anchor_points '())

  (setq taz_s_copy_nr 1)
  
  (defun taz_s_get_number (taz_s_txt / taz_s_i taz_s_len taz_s_pos)
    (setq taz_s_i 1
          taz_s_len (strlen taz_s_txt)
          taz_s_pos 0)

    ;; szukamy pierwszej spacji
    (while (and (<= taz_s_i taz_s_len) (= taz_s_pos 0))
      (if (= (substr taz_s_txt taz_s_i 1) " ")
        (setq taz_s_pos taz_s_i)
      )
      (setq taz_s_i (1+ taz_s_i))
    )

    ;; pobieramy wszystko po spacji
    (if taz_s_pos
      (atof (substr taz_s_txt (1+ taz_s_pos)))
      0.0
    )
  )

  ;; -------------------------------------------------------
  ;; PRZYPADEK IZO
  ;; Oryginalny model bez ciecia, pokazany w izometrii.
  ;; Wszystkie elementy konstrukcji sa traktowane jako widoczne:
  ;; - kazdy dostaje etykiete,
  ;; - wszystkie trafiaja do tabeli zestawienia stali.
  ;; Przypadek jest umieszczony nad wszystkimi przypadkami X/Y/Z.
  ;; Nie zmieniamy taz_s_copy_nr, dzieki czemu dotychczasowe
  ;; polozenia i numeracja przypadkow X/Y/Z pozostaja bez zmian.
  ;; -------------------------------------------------------

  (setq taz_s_izo_zoffset
    (*
      (+
        (length taz_s_x_data)
        (length taz_s_y_data)
        (length taz_s_z_data)
        1
      )
      100000
    )
  )

  (setq taz_s_zoffset taz_s_izo_zoffset)
  (setq taz_s_view_axis_name "IZO")
  (setq taz_s_view_name (strcat "taz_s_view_" taz_s_view_axis_name))

  ;; Ustawienia IZO zostaly zebrane w preflight DCL.
  (taz_s_execution_design_use_next_case_settings)

  ;; -------------------------------------------------------
  ;; IZO - PLASZCZYZNA RZUTU SOLPROF
  ;; Ten sam punkt poczatkowy UCS jest pozniej uzywany przy SOLPROF.
  ;; Etykiety rzutujemy na plaszczyzne przechodzaca przez ten punkt
  ;; i prostopadla do kierunku izometrycznego 1,-1,1.
  ;; -------------------------------------------------------

  (setq taz_s_izo_ucs_origin
    (list
      (/ (+ taz_s_xmin_nomargin taz_s_xmax_nomargin) 2.0)
      (/ (+ taz_s_ymin_nomargin taz_s_ymax_nomargin) 2.0)
      (+ (/ (+ taz_s_zmin_nomargin taz_s_zmax_nomargin) 2.0)
         taz_s_izo_zoffset)
    )
  )

  ;; -------------------------------------------------------
  ;; IZO - WSZYSTKIE ELEMENTY = WIDOCZNE
  ;; Zbieramy handle wszystkich oryginalnych bryl 3DSOLID.
  ;; Ta lista zasila pozniej tabele zestawienia stali.
  ;;
  ;; Etykiet NIE tworzymy jeszcze tutaj.
  ;; Powstana dopiero po ustawieniu dokladnie tego samego UCS,
  ;; z ktorego korzysta SOLPROF IZO. Dzieki temu ich polozenie
  ;; i obrot beda dokladnie zgodne z plaszczyzna wyniku SOLPROF.
  ;; -------------------------------------------------------

  (setq taz_s_visible_handles '())
  (setq taz_s_izo_orig_tmp taz_s_orig_enames)

  (while taz_s_izo_orig_tmp

    (setq taz_s_izo_orig_ent (car taz_s_izo_orig_tmp))
    (setq taz_s_izo_orig_data (entget taz_s_izo_orig_ent))
    (setq taz_s_izo_orig_type (cdr (assoc 0 taz_s_izo_orig_data)))

    (if (= taz_s_izo_orig_type "3DSOLID")
      (progn
        (setq taz_s_izo_orig_h (cdr (assoc 5 taz_s_izo_orig_data)))

        (setq taz_s_visible_handles
          (append taz_s_visible_handles (list taz_s_izo_orig_h))
        )
      )
    )

    (setq taz_s_izo_orig_tmp (cdr taz_s_izo_orig_tmp))
  )

  ;; -------------------------------------------------------
  ;; IZO - TABELA ZESTAWIENIA STALI
  ;; Pelna lista taz_s_visible_handles = cala konstrukcja.
  ;;
  ;; Tabele tworzymy nadal wariantem "Z", czyli poziomo.
  ;; Tym razem jednak:
  ;; - najpierw wyznaczamy DOKLADNA plaszczyzne IZO,
  ;; - punkt kotwiczenia od razu ustawiamy na tej plaszczyznie,
  ;; - tabela jest obracana wokol tego SAMEGO punktu.
  ;;
  ;; Nie ma zadnego dodatkowego przesuniecia tabeli.
  ;; ALIGN zmienia tylko jej orientacje.
  ;; -------------------------------------------------------

  ;; Punkt referencyjny zachowuje dotychczasowe polozenie tabeli
  ;; wzgledem modelu.
  (setq taz_s_izo_table_anchor_reference
    (list
      (+ taz_s_xmax 5000.0)
      taz_s_ymax
      (+ taz_s_zmax taz_s_izo_zoffset)
    )
  )

  ;; Chwilowo ustawiamy dokladnie ten sam UCS IZO co dla
  ;; etykiet i SOLPROF, tylko po to aby pobrac jego geometrie.
  (command "_UCS" "_W")
  (command "_UCS" "_O" taz_s_izo_ucs_origin)
  (command "_UCS" "_X" 45)
  (command "_UCS" "_Y" 35.264389683)

  ;; Normalna identyczna jak dla etykiet IZO.
  (setq taz_s_izo_table_normal
    (trans (list 0.0 0.0 1.0) 1 0 T)
  )

  ;; Os X identyczna z Rotation = 0 etykiet IZO.
  (setq taz_s_izo_table_xdir
    (trans
      (list 1.0 0.0 0.0)
      taz_s_izo_table_normal
      0
      T
    )
  )

  ;; Os Y tego samego OCS.
  (setq taz_s_izo_table_ydir
    (trans
      (list 0.0 1.0 0.0)
      taz_s_izo_table_normal
      0
      T
    )
  )

  ;; Punkt referencyjny przeliczamy do UCS IZO i ustawiamy Z=0.
  ;; To daje punkt DOKLADNIE na tej samej plaszczyznie co
  ;; etykiety oraz wynik SOLPROF.
  (setq taz_s_izo_table_anchor_ucs
    (trans taz_s_izo_table_anchor_reference 0 1)
  )

  (setq taz_s_table_anchor_point
    (trans
      (list
        (car taz_s_izo_table_anchor_ucs)
        (cadr taz_s_izo_table_anchor_ucs)
        0.0
      )
      1
      0
    )
  )

  ;; Wracamy do WORLD. Tabela powstaje poziomo w WCS,
  ;; ale od razu w swoim OSTATECZNYM punkcie kotwiczenia.
  (command "_UCS" "_W")

  (setq taz_s_table_before
    (taz_s_execution_design_get_last_entity)
  )

  (taz_s_create_steel_table
    taz_s_visible_handles
    taz_s_table_anchor_point
    "IZO"
  )

  (setq taz_s_table_group
    (taz_s_execution_design_collect_new_entities taz_s_table_before)
  )

  ;; Obracamy cala gotowa tabele wokol nieruchomego kotwiczenia.
  ;; Wszystkie punkty ALIGN sa podane w WCS.
  (if taz_s_table_group
    (progn

      (setq taz_s_izo_table_ss (ssadd))
      (setq taz_s_izo_table_tmp taz_s_table_group)

      (while taz_s_izo_table_tmp
        (if (entget (car taz_s_izo_table_tmp))
          (ssadd (car taz_s_izo_table_tmp) taz_s_izo_table_ss)
        )
        (setq taz_s_izo_table_tmp (cdr taz_s_izo_table_tmp))
      )

      ;; Baza zrodlowa: pozioma XY WCS.
      (setq taz_s_izo_table_src1 taz_s_table_anchor_point)

      (setq taz_s_izo_table_src2
        (list
          (+ (car taz_s_table_anchor_point) 1000.0)
          (cadr taz_s_table_anchor_point)
          (caddr taz_s_table_anchor_point)
        )
      )

      (setq taz_s_izo_table_src3
        (list
          (car taz_s_table_anchor_point)
          (+ (cadr taz_s_table_anchor_point) 1000.0)
          (caddr taz_s_table_anchor_point)
        )
      )

      ;; Baza docelowa: DOKLADNIE OCS etykiet IZO.
      ;; Punkt 1 jest TEN SAM, wiec tabela nie moze odjechac.
      (setq taz_s_izo_table_dst1 taz_s_table_anchor_point)

      (setq taz_s_izo_table_dst2
        (list
          (+ (car   taz_s_table_anchor_point)
             (* 1000.0 (car   taz_s_izo_table_xdir)))
          (+ (cadr  taz_s_table_anchor_point)
             (* 1000.0 (cadr  taz_s_izo_table_xdir)))
          (+ (caddr taz_s_table_anchor_point)
             (* 1000.0 (caddr taz_s_izo_table_xdir)))
        )
      )

      (setq taz_s_izo_table_dst3
        (list
          (+ (car   taz_s_table_anchor_point)
             (* 1000.0 (car   taz_s_izo_table_ydir)))
          (+ (cadr  taz_s_table_anchor_point)
             (* 1000.0 (cadr  taz_s_izo_table_ydir)))
          (+ (caddr taz_s_table_anchor_point)
             (* 1000.0 (caddr taz_s_izo_table_ydir)))
        )
      )

      (if (> (sslength taz_s_izo_table_ss) 0)
        (command
          "_.ALIGN"
          taz_s_izo_table_ss
          ""
          taz_s_izo_table_src1
          taz_s_izo_table_dst1
          taz_s_izo_table_src2
          taz_s_izo_table_dst2
          taz_s_izo_table_src3
          taz_s_izo_table_dst3
          "_N"
        )
      )
    )
  )

  ;; IZO jest pierwszym przypadkiem, wiec jego tabela jest pierwsza
  ;; na listach przekazywanych pozniej do organizera.
  (setq taz_s_execution_design_table_groups
    (append
      taz_s_execution_design_table_groups
      (list taz_s_table_group)
    )
  )

  (if taz_s_table_group
    (setq taz_s_execution_design_table_anchor_points
      (append
        taz_s_execution_design_table_anchor_points
        (list taz_s_table_anchor_point)
      )
    )
    (setq taz_s_execution_design_table_anchor_points
      (append
        taz_s_execution_design_table_anchor_points
        (list nil)
      )
    )
  )

  ;; -------------------------------------------------------
  ;; IZO - KOPIA MODELU DO SOLPROF
  ;; Bez bryly tnacej i bez INTERSECT.
  ;; -------------------------------------------------------

  (if taz_s_orig_ss
    (command "COPY" taz_s_orig_ss "" "0,0,0" (list 0 0 taz_s_izo_zoffset))
  )

  ;; Zbierz tylko skopiowane bryly 3DSOLID
  (setq taz_s_izo_enames (taz_s_collect_copy_enames))
  (setq taz_s_izo_ss (ssadd))
  (setq taz_s_izo_tmp taz_s_izo_enames)

  (while taz_s_izo_tmp
    (ssadd (car taz_s_izo_tmp) taz_s_izo_ss)
    (setq taz_s_izo_tmp (cdr taz_s_izo_tmp))
  )

  ;; SOLPROF w dalszej czesci skryptu pracuje na tej warstwie,
  ;; dlatego tylko kopie IZO przenosimy tymczasowo na execution_design.
  (if (> (sslength taz_s_izo_ss) 0)
    (command "_.CHPROP" taz_s_izo_ss "" "LA" "taz_s_execution_design" "")
  )

  ;; -------------------------------------------------------
  ;; IZO - LAYOUT I PRAWIDLOWA PLASZCZYZNA SOLPROF
  ;;
  ;; Nie uzywamy UCS 3-punktowego - GstarCAD w tym miejscu
  ;; potrafil potraktowac punkty jako zbieżne.
  ;;
  ;; Korzystamy tylko z operacji UCS, ktore sa juz uzywane
  ;; w dzialajacych przypadkach X/Y/Z:
  ;;   1. UCS WORLD
  ;;   2. UCS ORIGIN w srodku kopii IZO
  ;;   3. obrot UCS wokol X o 45 stopni
  ;;   4. obrot UCS wokol Y o 35.264389683 stopnia
  ;;
  ;; Po tych obrotach os Z UCS ma kierunek 1,-1,1,
  ;; czyli plaszczyzna XY UCS jest plaszczyzna izometryczna.
  ;; PLAN Current ustawia widok prostopadle do tej plaszczyzny.
  ;; -------------------------------------------------------

  ;; taz_s_izo_ucs_origin zostal wyliczony wyzej przed tworzeniem
  ;; etykiet, aby etykiety i SOLPROF korzystaly z tej samej plaszczyzny.

  (command "_layout" "_N" "3D")
  (command "_layout" "_S" "3D")
  (command "_mspace")

  (command "_UCS" "_W")
  (command "_UCS" "_O" taz_s_izo_ucs_origin)
  (command "_UCS" "_X" 45)
  (command "_UCS" "_Y" 35.264389683)
  (command "_PLAN" "_C")


  ;; -------------------------------------------------------
  ;; IZO - ETYKIETY DOKLADNIE NA PLASZCZYZNIE SOLPROF
  ;;
  ;; W tym miejscu aktywny jest juz DOKLADNIE ten sam UCS,
  ;; z ktorego za chwile korzysta SOLPROF.
  ;;
  ;; Dla kazdego elementu:
  ;; 1. pobieramy jego srodek w WCS,
  ;; 2. przeliczamy punkt do aktualnego UCS,
  ;; 3. ustawiamy Z=0 w tym UCS,
  ;; 4. przeliczamy punkt z powrotem do WCS.
  ;;
  ;; To daje punkt dokladnie na plaszczyznie XY aktualnego UCS,
  ;; czyli na tej samej plaszczyznie, na ktorej powstaje SOLPROF.
  ;;
  ;; Kierunek osi X MTEXT i normalna tekstu sa pobierane rowniez
  ;; bezposrednio z aktualnego UCS. Dlatego tekst jest poziomy
  ;; w widoku IZO i nie wymaga zadnego ROTATE3D.
  ;; -------------------------------------------------------

  ;; Normalna plaszczyzny etykiet = os Z aktualnego UCS IZO.
  (setq taz_s_izo_label_normal
    (trans (list 0.0 0.0 1.0) 1 0 T)
  )

  ;; Rotation = 0 dla MTEXT oznacza os X jego wlasnego OCS.
  ;; TRANS przyjmuje wektor normalny jako definicje OCS, dlatego
  ;; pobieramy os X tego OCS i zapisujemy ja jako wektor 11 w WCS.
  ;; Nie zgadujemy zadnego kata - kierunek wylicza sam CAD.
  (setq taz_s_izo_label_xdir
    (trans
      (list 1.0 0.0 0.0)
      taz_s_izo_label_normal
      0
      T
    )
  )

  ;; Os Y tego samego OCS.
  ;; Jest to dokladnie ten sam kierunek, ktorego organizer uzywa
  ;; pozniej jako pionowego kierunku zrodlowego przy ALIGN przypadku IZO.
  (setq taz_s_izo_label_ydir
    (trans
      (list 0.0 1.0 0.0)
      taz_s_izo_label_normal
      0
      T
    )
  )

  ;; -------------------------------------------------------
  ;; IZO - TYTUL WIDOKU 3D
  ;;
  ;; Tresc: WIDOK 3D + druga linia "Skala 1:X"
  ;; Wysokosc: tytul 5.0, skala 2.5 w skali 1:1; obie skalowane
  ;;            przez taz_s_annotation_scale
  ;; Polozenie: centralnie, 500 jednostek nad gorna krawedzia rzutu.
  ;; Warstwa: taz_s_labels
  ;;
  ;; WAZNE:
  ;; Punkt tytulu liczymy w bazie OCS etykiet IZO, czyli DOKLADNIE
  ;; w tej samej bazie, ktorej organizer uzywa pozniej przy ALIGN.
  ;; Poczatkiem tej bazy jest taz_s_izo_ucs_origin - ten sam punkt,
  ;; ktory po uporzadkowaniu trafia w srodek przypadku / ramki.
  ;;
  ;; Dlatego wspolrzedna pozioma tytulu wzgledem tego punktu wynosi 0.0.
  ;; Nie powstaje juz pozioma skladowa przesuniecia po ALIGN.
  ;; -------------------------------------------------------

  (setq taz_s_izo_title_corners
    (list
      (list taz_s_xmin taz_s_ymin (+ taz_s_zmin taz_s_izo_zoffset))
      (list taz_s_xmin taz_s_ymin (+ taz_s_zmax taz_s_izo_zoffset))
      (list taz_s_xmin taz_s_ymax (+ taz_s_zmin taz_s_izo_zoffset))
      (list taz_s_xmin taz_s_ymax (+ taz_s_zmax taz_s_izo_zoffset))
      (list taz_s_xmax taz_s_ymin (+ taz_s_zmin taz_s_izo_zoffset))
      (list taz_s_xmax taz_s_ymin (+ taz_s_zmax taz_s_izo_zoffset))
      (list taz_s_xmax taz_s_ymax (+ taz_s_zmin taz_s_izo_zoffset))
      (list taz_s_xmax taz_s_ymax (+ taz_s_zmax taz_s_izo_zoffset))
    )
  )

  ;; Maksymalna wspolrzedna pionowa obwiedni w bazie OCS IZO.
  ;; Liczymy ja wzgledem taz_s_izo_ucs_origin, aby punkt tytulu mial
  ;; dokladnie X=0.0 w tej samej bazie, ktora organizer prostuje ALIGN-em.
  (setq taz_s_izo_title_ymax nil)
  (setq taz_s_izo_title_tmp taz_s_izo_title_corners)

  (while taz_s_izo_title_tmp

    (setq taz_s_izo_title_corner_wcs
      (car taz_s_izo_title_tmp)
    )

    (setq taz_s_izo_title_corner_vec
      (list
        (- (car taz_s_izo_title_corner_wcs)
           (car taz_s_izo_ucs_origin))
        (- (cadr taz_s_izo_title_corner_wcs)
           (cadr taz_s_izo_ucs_origin))
        (- (caddr taz_s_izo_title_corner_wcs)
           (caddr taz_s_izo_ucs_origin))
      )
    )

    ;; Iloczyn skalarny z osia Y OCS IZO = pion po pozniejszym ALIGN.
    (setq taz_s_izo_title_corner_y
      (+
        (* (car taz_s_izo_title_corner_vec)
           (car taz_s_izo_label_ydir))
        (* (cadr taz_s_izo_title_corner_vec)
           (cadr taz_s_izo_label_ydir))
        (* (caddr taz_s_izo_title_corner_vec)
           (caddr taz_s_izo_label_ydir))
      )
    )

    (if
      (or
        (null taz_s_izo_title_ymax)
        (> taz_s_izo_title_corner_y taz_s_izo_title_ymax)
      )
      (setq taz_s_izo_title_ymax taz_s_izo_title_corner_y)
    )

    (setq taz_s_izo_title_tmp (cdr taz_s_izo_title_tmp))
  )

  (if taz_s_izo_title_ymax
    (progn

      ;; Punkt tytulu = srodek przypadku + tylko skladowa pionowa.
      ;; Brak skladowej taz_s_izo_label_xdir oznacza, ze po ALIGN
      ;; tytul trafi dokladnie nad X srodka przypadku / ramki.
      (setq taz_s_izo_title_offset_y
        (+ taz_s_izo_title_ymax 500.0)
      )

      (setq taz_s_izo_title_pt
        (list
          (+
            (car taz_s_izo_ucs_origin)
            (* taz_s_izo_title_offset_y
               (car taz_s_izo_label_ydir))
          )
          (+
            (cadr taz_s_izo_ucs_origin)
            (* taz_s_izo_title_offset_y
               (cadr taz_s_izo_label_ydir))
          )
          (+
            (caddr taz_s_izo_ucs_origin)
            (* taz_s_izo_title_offset_y
               (caddr taz_s_izo_label_ydir))
          )
        )
      )

      (entmake
        (list
          (cons 0 "MTEXT")
          (cons 10 taz_s_izo_title_pt)
          (cons
            1
            (strcat
              (taz_s_execution_design_get_case_title "IZO" nil nil)
              "\\P\\H0.5x;"
              (taz_s_get_scale_title_text)
            )
          )
          (cons 7 "Standard")
          (cons 8 "taz_s_labels")
          (cons 40 (* 5.0 taz_s_annotation_scale))
          (cons 71 5)
          (cons 11 taz_s_izo_label_xdir)
          (cons 210 taz_s_izo_label_normal)
        )
      )
    )
  )

  (setq taz_s_izo_orig_tmp taz_s_orig_enames)

  (while taz_s_izo_orig_tmp

    (setq taz_s_izo_orig_ent (car taz_s_izo_orig_tmp))
    (setq taz_s_izo_orig_data (entget taz_s_izo_orig_ent))
    (setq taz_s_izo_orig_type (cdr (assoc 0 taz_s_izo_orig_data)))

    (if (= taz_s_izo_orig_type "3DSOLID")
      (progn

        (setq taz_s_izo_orig_h (cdr (assoc 5 taz_s_izo_orig_data)))

        (setq taz_s_izo_attr6_sym
          (read (strcat "taz_s_" taz_s_izo_orig_h "_attr6"))
        )

        (setq taz_s_izo_attr7_sym
          (read (strcat "taz_s_" taz_s_izo_orig_h "_attr7"))
        )

        (if
          (and
            (boundp taz_s_izo_attr6_sym)
            (boundp taz_s_izo_attr7_sym)
          )
          (progn

            (setq taz_s_annotation_text
              (strcat
                (eval taz_s_izo_attr6_sym)
                " "
                (eval taz_s_izo_attr7_sym)
              )
            )

            (if (= (eval taz_s_izo_attr6_sym) "LR")
              (setq taz_s_annotation_text
                (strcat "L " (eval taz_s_izo_attr7_sym))
              )
            )

            (if (= (eval taz_s_izo_attr6_sym) "LN")
              (setq taz_s_annotation_text
                (strcat "L " (eval taz_s_izo_attr7_sym))
              )
            )

            ;; Srodek elementu w strefie IZO - WCS
            (setq taz_s_izo_label_center_wcs
              (taz_s_get_center taz_s_izo_orig_ent)
            )

            ;; Ten sam punkt w aktualnym UCS SOLPROF
            (setq taz_s_izo_label_center_ucs
              (trans taz_s_izo_label_center_wcs 0 1)
            )

            ;; Dokladnie plaszczyzna XY aktualnego UCS: Z = 0
            (setq taz_s_annotation_ins_pt
              (trans
                (list
                  (car taz_s_izo_label_center_ucs)
                  (cadr taz_s_izo_label_center_ucs)
                  0.0
                )
                1
                0
              )
            )

            (entmake
              (list
                (cons 0 "MTEXT")
                (cons 10 taz_s_annotation_ins_pt)
                (cons 1 taz_s_annotation_text)
                (cons 7 "Standard")
                (cons 8 "taz_s_labels")
                (cons 40 taz_s_annotation_scale_label)
                (cons 71 5)
                (cons 90 16)
                (cons 11 taz_s_izo_label_xdir)
                (cons 210 taz_s_izo_label_normal)
              )
            )
          )
        )
      )
    )

    (setq taz_s_izo_orig_tmp (cdr taz_s_izo_orig_tmp))
  )

  (if (> (sslength taz_s_izo_ss) 0)
    (progn
      (command "_ZOOM" "_OBJECT" taz_s_izo_ss "")
      (command "-VIEW" "_S" taz_s_view_name)
      (command "_.SOLPROF")
      (command taz_s_izo_ss)
      (command "" "_Y" "_Y" "_Y")
      (command "_.ERASE" taz_s_izo_ss "")
    )
  )

  (command "_pspace")
  (command "_layout" "_S" "Model")
  (command "_UCS" "_W")

  ;; -------------------------------------------------------
  ;; PRZYPADKI X
  ;; Plaszczyzna prostopadla do osi Y
  ;; -------------------------------------------------------

  (setq taz_s_tmp taz_s_x_data)
  
  (setq taz_s_initial_solprof 1)

  (while taz_s_tmp

    (setq taz_s_row (car taz_s_tmp))
    (taz_s_get_dist)
    (setq taz_s_y taz_s_val)
    (setq taz_s_zoffset (* taz_s_copy_nr 100000))

    ;; Ustawienia tego przekroju X zostaly zebrane w preflight DCL.
    (taz_s_execution_design_use_next_case_settings)

    ;; KROK 1: narysuj bryle tnaca i osie
    (setvar "CLAYER" "taz_s_execution_design")
    
    (foreach taz_s_axis taz_s_axis_data_y

      (setq taz_s_x (taz_s_get_number taz_s_axis))
      (setq taz_s_axis_name (taz_s_get_axis_name taz_s_axis))

      ;; linia osi (góra / dół)
      (setq taz_s_p1_axis (list taz_s_x taz_s_y (+ taz_s_zmin taz_s_zoffset)))
      (setq taz_s_p2_axis (list taz_s_x taz_s_y (+ taz_s_zmax taz_s_zoffset)))

      (command "3DPOLY" taz_s_p1_axis taz_s_p2_axis "")
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      (setq taz_s_circle_radius (* taz_s_annotation_scale_axis (/ 250.0 150.0)))
      (command "_.CIRCLE" (list taz_s_x taz_s_y (- (+ taz_s_zmin taz_s_zoffset) taz_s_circle_radius)) taz_s_circle_radius)
      (setq taz_s_circle_center (list taz_s_x taz_s_y (- (+ taz_s_zmin taz_s_zoffset) taz_s_circle_radius)))
      (command "_.ROTATE3D" (entlast) "" "X" taz_s_circle_center "90")
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      (command "_.TEXT" "_J" "_MC" taz_s_circle_center taz_s_annotation_scale_axis 0 taz_s_axis_name)
      (command "_.ROTATE3D" (entlast) "" "X" taz_s_circle_center "90")
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
    )

    
    (setq taz_s_p1_nomargin (list taz_s_xmin_nomargin taz_s_y (+ taz_s_zmin taz_s_zoffset)))
    (setq taz_s_p2_nomargin (list taz_s_xmax_nomargin taz_s_y (+ taz_s_zmin taz_s_zoffset)))
    (setq taz_s_p3_nomargin (list taz_s_xmax_nomargin taz_s_y (+ taz_s_zmax taz_s_zoffset)))
    (setq taz_s_p4_nomargin (list taz_s_xmin_nomargin taz_s_y (+ taz_s_zmax taz_s_zoffset)))

    (setq taz_s_p1 (list taz_s_xmin taz_s_y (+ taz_s_zmin taz_s_zoffset)))
    (setq taz_s_p2 (list taz_s_xmax taz_s_y (+ taz_s_zmin taz_s_zoffset)))
    (setq taz_s_p3 (list taz_s_xmax taz_s_y (+ taz_s_zmax taz_s_zoffset)))
    (setq taz_s_p4 (list taz_s_xmin taz_s_y (+ taz_s_zmax taz_s_zoffset)))

    (command "3DPOLY" taz_s_p1 taz_s_p2 taz_s_p3 taz_s_p4 taz_s_p1 "")
    (command "EXTRUDE" (entlast) "" "1000" "0")
    (command "_ZOOM" "_OBJECT" (entlast) "")
    (command "_ZOOM" "_SCALE" "1000X")
    (command "REGEN")
    (command "MOVE" (entlast) "" "0,0,0" "0,-500,0")
    (setq taz_s_cutting_ename (entlast))
    (command "-VIEW" "_S" "taz_s_view_cutting_view")

    ;; WIDOK: zoom na bryle tnaca zgodnie z kierunkiem extrude (os Y) i zapis widoku
    (setq taz_s_view_axis_name (taz_s_get_axis_name taz_s_row))
    (setq taz_s_view_name (strcat "taz_s_view_" taz_s_view_axis_name))
    (command "_VPOINT" "0,-1,0")
    (command "_ZOOM" "_OBJECT" taz_s_cutting_ename "")
    (command "-VIEW" "_S" taz_s_view_name)
    (command "-VIEW" "_R" "taz_s_view_cutting_view")

    ;; KROK 2: skopiuj oryginalny model
    (setq taz_s_last_before_copy (entlast))
    (if taz_s_orig_ss
      (command "COPY" taz_s_orig_ss "" "0,0,0" (list 0 0 taz_s_zoffset))
    )
    (taz_s_copy_attrs_to_copies taz_s_last_before_copy)

    ;; KROK 3: zbierz enames kopii biezacego przypadku
    (setq taz_s_copy_enames (taz_s_collect_copy_enames))
    
    ;; KROK 4: intersect parami (zbiera tez taz_s_visible_handles)
    (setq taz_s_visible_handles '())
    (if (> (length taz_s_copy_enames) 0)
      (taz_s_intersect_pairs taz_s_cutting_ename taz_s_copy_enames "X")
      (progn
        (princ (strcat "\nPrzypadek X nr " (itoa taz_s_copy_nr) ": brak elementow kopii - pomijam."))
        (entdel taz_s_cutting_ename)
      )
    )

    ;; KROK 4.25: tytul widoku
    (taz_s_create_view_title
      "X"
      (taz_s_get_axis_name taz_s_row)
    )

    ;; KROK 4.5: tabela zestawienia stali - korzysta z juz policzonej widocznosci
    ;;(taz_s_create_steel_table
      ;;taz_s_visible_handles
      ;;(list (+ taz_s_xmax taz_s_st_offset) taz_s_y taz_s_zoffset)
      ;;"X"
    ;;)
    
    (setq taz_s_table_anchor_point
      (list (+ taz_s_xmax 5000.0) taz_s_y taz_s_zoffset)
    )

    (setq taz_s_table_before (taz_s_execution_design_get_last_entity))

    (taz_s_create_steel_table
      taz_s_visible_handles
      taz_s_table_anchor_point
      "X"
    )

    (setq taz_s_table_group
      (taz_s_execution_design_collect_new_entities taz_s_table_before)
    )

    (setq taz_s_execution_design_table_groups
      (append
        taz_s_execution_design_table_groups
        (list taz_s_table_group)
      )
    )

    (if taz_s_table_group
      (setq taz_s_execution_design_table_anchor_points
        (append
          taz_s_execution_design_table_anchor_points
          (list taz_s_table_anchor_point)
        )
      )
      (setq taz_s_execution_design_table_anchor_points
        (append
          taz_s_execution_design_table_anchor_points
          (list nil)
        )
      )
    )

    (setq taz_s_copy_nr (+ taz_s_copy_nr 1))
    (setq taz_s_tmp (cdr taz_s_tmp))
    
    (command "_layout" "_N" (strcat taz_s_view_axis_name "-" taz_s_view_axis_name))
    (command "_layout" "_S" (strcat taz_s_view_axis_name "-" taz_s_view_axis_name))
    (command "_mspace")
    ;;(command "-VIEW" "_R" taz_s_view_name)
    
    (command "_UCS" "_W")
    (if (= taz_s_initial_solprof 1)
      (command "_PLAN" "_W")
    )
    
    (setq taz_s_initial_solprof 0)
    
    (command "_UCS" "_O" (list (/ (+ taz_s_xmin taz_s_xmax) 2.0) taz_s_y taz_s_zoffset))
    (command "_UCS" "_X" 90)
    (command "_PLAN" "_C")
    ;;(command "_REGEN")
    
    (setq taz_s_solprof_ss (ssget "_X" (list (cons 8 "taz_s_execution_design"))))    
    (command "_.SOLPROF")
    (command taz_s_solprof_ss)
    (command "" "_Y" "_Y" "_Y")
    (command "_.ERASE" (ssget "_X" (list (cons 8 "taz_s_execution_design"))) "")
    (command "_pspace")
    (command "_layout" "_S" "Model")
    
  )

  ;; -------------------------------------------------------
  ;; PRZYPADKI Y
  ;; Plaszczyzna prostopadla do osi X
  ;; -------------------------------------------------------

  (setq taz_s_tmp taz_s_y_data)

  (while taz_s_tmp

    (setq taz_s_row (car taz_s_tmp))
    (taz_s_get_dist)
    (setq taz_s_x taz_s_val)
    (setq taz_s_zoffset (* taz_s_copy_nr 100000))

    ;; Ustawienia tego przekroju Y zostaly zebrane w preflight DCL.
    (taz_s_execution_design_use_next_case_settings)

    ;; KROK 1: narysuj bryle tnaca i osie
    (setvar "CLAYER" "taz_s_execution_design")
    
    (foreach taz_s_axis taz_s_axis_data_x

      ;; Y z tekstu osi
      (setq taz_s_y (taz_s_get_number taz_s_axis))
      (setq taz_s_axis_name (taz_s_get_axis_name taz_s_axis))

      ;; punkty osi
      (setq taz_s_p1_axis (list taz_s_x taz_s_y (+ taz_s_zmin taz_s_zoffset)))
      (setq taz_s_p2_axis (list taz_s_x taz_s_y (+ taz_s_zmax taz_s_zoffset)))

      (command "3DPOLY" taz_s_p1_axis taz_s_p2_axis "")
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      (setq taz_s_circle_radius (* taz_s_annotation_scale_axis (/ 250.0 150.0)))
      (command "_.CIRCLE" (list taz_s_x taz_s_y (- (+ taz_s_zmin taz_s_zoffset) taz_s_circle_radius)) taz_s_circle_radius)
      (setq taz_s_circle_center (list taz_s_x taz_s_y (- (+ taz_s_zmin taz_s_zoffset) taz_s_circle_radius)))
      (command "_.ROTATE3D" (entlast) "" "Y" taz_s_circle_center "90")
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      (command "_.TEXT" "_J" "_MC" taz_s_circle_center taz_s_annotation_scale_axis 90 taz_s_axis_name)
      (command "_.ROTATE3D" (entlast) "" "Y" taz_s_circle_center "90")
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
    )
    
    (setq taz_s_p1_nomargin (list taz_s_x taz_s_ymin_nomargin (+ taz_s_zmin taz_s_zoffset)))
    (setq taz_s_p2_nomargin (list taz_s_x taz_s_ymax_nomargin (+ taz_s_zmin taz_s_zoffset)))
    (setq taz_s_p3_nomargin (list taz_s_x taz_s_ymax_nomargin (+ taz_s_zmax taz_s_zoffset)))
    (setq taz_s_p4_nomargin (list taz_s_x taz_s_ymin_nomargin (+ taz_s_zmax taz_s_zoffset)))

    (setq taz_s_p1 (list taz_s_x taz_s_ymin (+ taz_s_zmin taz_s_zoffset)))
    (setq taz_s_p2 (list taz_s_x taz_s_ymax (+ taz_s_zmin taz_s_zoffset)))
    (setq taz_s_p3 (list taz_s_x taz_s_ymax (+ taz_s_zmax taz_s_zoffset)))
    (setq taz_s_p4 (list taz_s_x taz_s_ymin (+ taz_s_zmax taz_s_zoffset)))

    (command "3DPOLY" taz_s_p1 taz_s_p2 taz_s_p3 taz_s_p4 taz_s_p1 "")
    (command "EXTRUDE" (entlast) "" "1000" "0")
    (command "_ZOOM" "_OBJECT" (entlast) "")
    (command "_ZOOM" "_SCALE" "1000X")
    (command "REGEN")
    (command "MOVE" (entlast) "" "0,0,0" "-500,0,0")
    (setq taz_s_cutting_ename (entlast))
    (command "-VIEW" "_S" "taz_s_view_cutting_view")

    ;; WIDOK: zoom na bryle tnaca zgodnie z kierunkiem extrude (os X) i zapis widoku
    (setq taz_s_view_axis_name (taz_s_get_axis_name taz_s_row))
    (setq taz_s_view_name (strcat "taz_s_view_" taz_s_view_axis_name))
    (command "_VPOINT" "-1,0,0")
    (command "_ZOOM" "_OBJECT" taz_s_cutting_ename "")
    (command "-VIEW" "_S" taz_s_view_name)
    (command "-VIEW" "_R" "taz_s_view_cutting_view")

    ;; KROK 2: skopiuj oryginalny model
    (setq taz_s_last_before_copy (entlast))
    (if taz_s_orig_ss
      (command "COPY" taz_s_orig_ss "" "0,0,0" (list 0 0 taz_s_zoffset))
    )
    (taz_s_copy_attrs_to_copies taz_s_last_before_copy)

    ;; KROK 3: zbierz enames kopii biezacego przypadku
    (setq taz_s_copy_enames (taz_s_collect_copy_enames))
    
    ;; KROK 4: intersect parami (zbiera tez taz_s_visible_handles)
    (setq taz_s_visible_handles '())
    (if (> (length taz_s_copy_enames) 0)
      (taz_s_intersect_pairs taz_s_cutting_ename taz_s_copy_enames "Y")
      (progn
        (princ (strcat "\nPrzypadek Y nr " (itoa taz_s_copy_nr) ": brak elementow kopii - pomijam."))
        (entdel taz_s_cutting_ename)
      )
    )

    ;; KROK 4.25: tytul widoku
    (taz_s_create_view_title
      "Y"
      (taz_s_get_axis_name taz_s_row)
    )

    ;; KROK 4.5: tabela zestawienia stali
    ;;(taz_s_create_steel_table
      ;;taz_s_visible_handles
      ;;(list taz_s_x (+ taz_s_ymax taz_s_st_offset) taz_s_zoffset)
      ;;"Y"
    ;;)
    
    (setq taz_s_table_anchor_point
      (list taz_s_x (+ taz_s_ymax 5000.0) taz_s_zoffset)
    )

    (setq taz_s_table_before (taz_s_execution_design_get_last_entity))

    (taz_s_create_steel_table
      taz_s_visible_handles
      taz_s_table_anchor_point
      "Y"
    )

    (setq taz_s_table_group
      (taz_s_execution_design_collect_new_entities taz_s_table_before)
    )

    (setq taz_s_execution_design_table_groups
      (append
        taz_s_execution_design_table_groups
        (list taz_s_table_group)
      )
    )

    (if taz_s_table_group
      (setq taz_s_execution_design_table_anchor_points
        (append
          taz_s_execution_design_table_anchor_points
          (list taz_s_table_anchor_point)
        )
      )
      (setq taz_s_execution_design_table_anchor_points
        (append
          taz_s_execution_design_table_anchor_points
          (list nil)
        )
      )
    )

    (setq taz_s_copy_nr (+ taz_s_copy_nr 1))
    (setq taz_s_tmp (cdr taz_s_tmp))
    
    (command "_layout" "_N" (strcat taz_s_view_axis_name "-" taz_s_view_axis_name))
    (command "_layout" "_S" (strcat taz_s_view_axis_name "-" taz_s_view_axis_name))
    (command "_mspace")
    ;;(command "-VIEW" "_R" taz_s_view_name)
    
    (command "_UCS" "_W")
    ;;(command "_PLAN" "_W")
    (command "_UCS" "_O" (list taz_s_x (/ (+ taz_s_ymin taz_s_ymax) 2.0) taz_s_zoffset))
    (command "_UCS" "_X" 90)
    (command "_UCS" "_Y" 90)
    (command "_PLAN" "_C")
    ;;(command "_REGEN")
    
    (setq taz_s_solprof_ss (ssget "_X" (list (cons 8 "taz_s_execution_design"))))    
    (command "_.SOLPROF")
    (command taz_s_solprof_ss)
    (command "" "_Y" "_Y" "_Y")
    (command "_.ERASE" (ssget "_X" (list (cons 8 "taz_s_execution_design"))) "")
    (command "_pspace")
    (command "_layout" "_S" "Model")
    
  )

  ;; -------------------------------------------------------
  ;; PRZYPADKI Z
  ;; Plaszczyzna pozioma
  ;; -------------------------------------------------------

  (setq taz_s_tmp taz_s_z_data)

  (while taz_s_tmp

    (setq taz_s_row (car taz_s_tmp))
    (taz_s_get_dist)
    (setq taz_s_z taz_s_val)
    (setq taz_s_zoffset (* taz_s_copy_nr 100000))

    ;; Ustawienia tego rzutu Z zostaly zebrane w preflight DCL.
    (taz_s_execution_design_use_next_case_settings)

    ;; KROK 1: narysuj bryle tnaca i osie
    (setvar "CLAYER" "taz_s_execution_design")
    
    (setq taz_s_circle_radius (* taz_s_annotation_scale_axis (/ 250.0 150.0)))
    ;; ----------------------------------------
    ;; Osie X (linie równoległe do osi Y)
    ;; ----------------------------------------
    (foreach taz_s_axis taz_s_axis_data_y
      (setq taz_s_x (taz_s_get_number taz_s_axis))
      (setq taz_s_axis_name (taz_s_get_axis_name taz_s_axis))
      (setq taz_s_p1_axis
            (list taz_s_x
                  taz_s_ymin
                  (+ taz_s_z taz_s_zoffset)))
      (setq taz_s_p2_axis
            (list taz_s_x
                  taz_s_ymax
                  (+ taz_s_z taz_s_zoffset)))
      (command "3DPOLY" taz_s_p1_axis taz_s_p2_axis "")
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      ;; dolne kółko
      (setq taz_s_circle_center
            (list taz_s_x
                  (- taz_s_ymin taz_s_circle_radius)
                  (+ taz_s_z taz_s_zoffset)))
      (command "_.CIRCLE" taz_s_circle_center taz_s_circle_radius)
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      (command "_.TEXT" "_J" "_MC" taz_s_circle_center taz_s_annotation_scale_axis 0 taz_s_axis_name)
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      ;; górne kółko
      (setq taz_s_circle_center
            (list taz_s_x
                  (+ taz_s_ymax taz_s_circle_radius)
                  (+ taz_s_z taz_s_zoffset)))
      (command "_.CIRCLE" taz_s_circle_center taz_s_circle_radius)
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      (command "_.TEXT" "_J" "_MC" taz_s_circle_center taz_s_annotation_scale_axis 0 taz_s_axis_name)
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
    )
    ;; ----------------------------------------
    ;; Osie Y (linie równoległe do osi X)
    ;; ----------------------------------------
    (foreach taz_s_axis taz_s_axis_data_x
      (setq taz_s_y (taz_s_get_number taz_s_axis))
      (setq taz_s_axis_name (taz_s_get_axis_name taz_s_axis))
      (setq taz_s_p1_axis
            (list taz_s_xmin
                  taz_s_y
                  (+ taz_s_z taz_s_zoffset)))
      (setq taz_s_p2_axis
            (list taz_s_xmax
                  taz_s_y
                  (+ taz_s_z taz_s_zoffset)))
      (command "3DPOLY" taz_s_p1_axis taz_s_p2_axis "")
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      ;; lewe kółko
      (setq taz_s_circle_center
            (list (- taz_s_xmin taz_s_circle_radius)
                  taz_s_y
                  (+ taz_s_z taz_s_zoffset)))
      (command "_.CIRCLE" taz_s_circle_center taz_s_circle_radius)
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      (command "_.TEXT" "_J" "_MC" taz_s_circle_center taz_s_annotation_scale_axis 0 taz_s_axis_name)
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      ;; prawe kółko
      (setq taz_s_circle_center
            (list (+ taz_s_xmax taz_s_circle_radius)
                  taz_s_y
                  (+ taz_s_z taz_s_zoffset)))
      (command "_.CIRCLE" taz_s_circle_center taz_s_circle_radius)
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      (command "_.TEXT" "_J" "_MC" taz_s_circle_center taz_s_annotation_scale_axis 0 taz_s_axis_name)
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
    )
    
    (setq taz_s_p1_nomargin (list taz_s_xmin taz_s_ymin_nomargin (+ taz_s_z taz_s_zoffset)))
    (setq taz_s_p2_nomargin (list taz_s_xmin_nomargin taz_s_ymin (+ taz_s_z taz_s_zoffset)))
    (setq taz_s_p3_nomargin (list taz_s_xmax_nomargin taz_s_ymin (+ taz_s_z taz_s_zoffset)))
    (setq taz_s_p4_nomargin (list taz_s_xmax taz_s_ymin_nomargin (+ taz_s_z taz_s_zoffset)))
    (setq taz_s_p5_nomargin (list taz_s_xmax taz_s_ymax_nomargin (+ taz_s_z taz_s_zoffset)))
    (setq taz_s_p6_nomargin (list taz_s_xmax_nomargin taz_s_ymax (+ taz_s_z taz_s_zoffset)))
    (setq taz_s_p7_nomargin (list taz_s_xmin_nomargin taz_s_ymax (+ taz_s_z taz_s_zoffset)))
    (setq taz_s_p8_nomargin (list taz_s_xmin taz_s_ymax_nomargin (+ taz_s_z taz_s_zoffset)))

    (setq taz_s_p1 (list taz_s_xmin taz_s_ymin (+ taz_s_z taz_s_zoffset)))
    (setq taz_s_p2 (list taz_s_xmax taz_s_ymin (+ taz_s_z taz_s_zoffset)))
    (setq taz_s_p3 (list taz_s_xmax taz_s_ymax (+ taz_s_z taz_s_zoffset)))
    (setq taz_s_p4 (list taz_s_xmin taz_s_ymax (+ taz_s_z taz_s_zoffset)))

    (command "3DPOLY" taz_s_p1 taz_s_p2 taz_s_p3 taz_s_p4 taz_s_p1 "")
    (command "EXTRUDE" (entlast) "" "1000" "0")
    (command "_ZOOM" "_OBJECT" (entlast) "")
    (command "_ZOOM" "_SCALE" "1000X")
    (command "REGEN")
    (command "MOVE" (entlast) "" "0,0,0" "0,0,-500")
    (setq taz_s_cutting_ename (entlast))
    (command "-VIEW" "_S" "taz_s_view_cutting_view")
    
    ;; WIDOK: zoom na bryle tnaca zgodnie z kierunkiem extrude (os Y) i zapis widoku
    (setq taz_s_view_axis_name (taz_s_get_axis_name taz_s_row))
    (setq taz_s_view_name (strcat "taz_s_view_" taz_s_view_axis_name))
    (command "_VPOINT" "0,0,-1")
    (command "_ZOOM" "_OBJECT" taz_s_cutting_ename "")
    (command "-VIEW" "_S" taz_s_view_name)
    (command "-VIEW" "_R" "taz_s_view_cutting_view")

    ;; KROK 2: skopiuj oryginalny model
    (setq taz_s_last_before_copy (entlast))
    (if taz_s_orig_ss
      (command "COPY" taz_s_orig_ss "" "0,0,0" (list 0 0 taz_s_zoffset))
    )
    (taz_s_copy_attrs_to_copies taz_s_last_before_copy)

    ;; KROK 3: zbierz enames kopii biezacego przypadku
    (setq taz_s_copy_enames (taz_s_collect_copy_enames))
    
    ;; KROK 4: intersect parami (zbiera tez taz_s_visible_handles)
    (setq taz_s_visible_handles '())
    (if (> (length taz_s_copy_enames) 0)
      (taz_s_intersect_pairs taz_s_cutting_ename taz_s_copy_enames "Z")
      (progn
        (princ (strcat "\nPrzypadek Z nr " (itoa taz_s_copy_nr) ": brak elementow kopii - pomijam."))
        (entdel taz_s_cutting_ename)
      )
    )

    ;; KROK 4.25: tytul rzutu poziomu
    (taz_s_create_level_title taz_s_z)

    ;; KROK 4.5: tabela zestawienia stali
    ;;(taz_s_create_steel_table
      ;;taz_s_visible_handles
      ;;(list (+ taz_s_xmax taz_s_st_offset) taz_s_ymax (+ taz_s_z taz_s_zoffset))
      ;;"Z"
    ;;)
    
    (setq taz_s_table_anchor_point
      (list (+ taz_s_xmax 5000.0) taz_s_ymax (+ taz_s_z taz_s_zoffset))
    )

    (setq taz_s_table_before (taz_s_execution_design_get_last_entity))

    (taz_s_create_steel_table
      taz_s_visible_handles
      taz_s_table_anchor_point
      "Z"
    )

    (setq taz_s_table_group
      (taz_s_execution_design_collect_new_entities taz_s_table_before)
    )

    (setq taz_s_execution_design_table_groups
      (append
        taz_s_execution_design_table_groups
        (list taz_s_table_group)
      )
    )

    (if taz_s_table_group
      (setq taz_s_execution_design_table_anchor_points
        (append
          taz_s_execution_design_table_anchor_points
          (list taz_s_table_anchor_point)
        )
      )
      (setq taz_s_execution_design_table_anchor_points
        (append
          taz_s_execution_design_table_anchor_points
          (list nil)
        )
      )
    )

    (setq taz_s_copy_nr (+ taz_s_copy_nr 1))
    (setq taz_s_tmp (cdr taz_s_tmp))
    
    (command "_layout" "_N" taz_s_level_title_value)
    (command "_layout" "_S" taz_s_level_title_value)
    (command "_mspace")
    ;;(command "-VIEW" "_R" taz_s_view_name)
    
    (command "_UCS" "_W")
    (command "_UCS" "_O" (list 0 0 taz_s_zoffset))
    (command "_PLAN" "_C")
    ;;(command "_REGEN")
    
    (setq taz_s_solprof_ss (ssget "_X" (list (cons 8 "taz_s_execution_design"))))    
    (command "_.SOLPROF")
    (command taz_s_solprof_ss)
    (command "" "_Y" "_Y" "_Y")
    (command "_.ERASE" (ssget "_X" (list (cons 8 "taz_s_execution_design"))) "")
    (command "_pspace")
    (command "_layout" "_S" "Model")
    
  )

  (command "-LAYDEL" "N" "taz_s_execution_design" "" "_Y")
  (taz_s_merge_solprof_layers)

  ;; ---------------------------------
  ;; USUN ORYGINALNY MODEL I ORYGINALNE OSIE
  ;; ---------------------------------

  (command "_layout" "_S" "Model")

  (if taz_s_orig_ss
    (command "_.ERASE" taz_s_orig_ss "")
  )

  (if taz_s_orig_axes_ss
    (command "_.ERASE" taz_s_orig_axes_ss "")
  )

  (taz_s_lock_all_layers)
  (taz_s_current_settings_restore)

  ;; ---------------------------------
  ;; ZAPIS PLIKU DRAWINGS
  ;; ---------------------------------
  
  (if (findfile taz_s_drawings_file)
    (command "_.SAVEAS" "" taz_s_drawings_file "_Y")
    (command "_.SAVEAS" "" taz_s_drawings_file)
  )

  ;; ---------------------------------
  ;; AUTOMATYCZNE URUCHOMIENIE ORGANIZERA
  ;; ---------------------------------

  (taz_s_create_drawings_execution_design_organize)

  ;; Zapisz wynik porzadkowania w pliku _DRAWINGS.dwg.
  (command "_.QSAVE")

  (princ)
  
)

;; ============================================================================
;; ORGANIZER - SCALONY Z TYM PLIKIEM
;; ============================================================================

;; ============================================================================
;; taz_s_create_drawings_execution_design_organize.lsp
;;
;; Wersja 9 porzadkowania widokow wykonanych przez:
;; taz_s_create_drawings_execution_design
;;
;; ZALOZENIA:
;; - przypadki sa utworzone kolejno: X, potem Y, potem Z,
;; - kazdy kolejny przypadek byl tworzony wyzej o 100000,
;; - dla przypadkow X/Y obiekty sa rozpoznawane po rzeczywistym zakresie Z
;;   ich geometrii, bez sprawdzania nazw warstw,
;; - po uporzadkowaniu wszystkie przypadki maja lezec na globalnej XY,
;; - pierwszy przypadek trafia do 0,0,0,
;; - kazdy kolejny trafia o 100000 w prawo po osi X,
;; - kazdy punkt przekazywany do ALIGN jest poprzedzony _NON, aby aktywny
;;   OSNAP nie zmienial wspolrzednych wyliczonych przez skrypt.
;;
;; Skrypt celowo jest napisany prosto:
;; - setq,
;; - if,
;; - while,
;; - zmienne globalne,
;; - bez dodatkowych bibliotek.
;; ============================================================================


;; ----------------------------------------------------------------------------
;; POBRANIE LICZBY Z WIERSZA TYPU:
;; [X1]  5000.0
;; ----------------------------------------------------------------------------

(defun taz_s_organize_get_dist (taz_s_organize_row_arg)

  (setq taz_s_organize_i 1)
  (setq taz_s_organize_len (strlen taz_s_organize_row_arg))
  (setq taz_s_organize_val 0.0)

  (while
    (and
      (<= taz_s_organize_i taz_s_organize_len)
      (/= (substr taz_s_organize_row_arg taz_s_organize_i 1) "]")
    )
    (setq taz_s_organize_i (+ taz_s_organize_i 1))
  )

  (if (<= taz_s_organize_i taz_s_organize_len)
    (setq taz_s_organize_val
      (atof
        (substr
          taz_s_organize_row_arg
          (+ taz_s_organize_i 1)
        )
      )
    )
  )

  taz_s_organize_val
)


;; ----------------------------------------------------------------------------
;; ZAMIANA LISTY WIERSZY OSI NA LISTE LICZB
;; ----------------------------------------------------------------------------

(defun taz_s_organize_make_values (taz_s_organize_source_list)

  (setq taz_s_organize_values '())
  (setq taz_s_organize_tmp_values taz_s_organize_source_list)

  (while taz_s_organize_tmp_values

    (setq taz_s_organize_row_values (car taz_s_organize_tmp_values))
    (setq taz_s_organize_value_values
      (taz_s_organize_get_dist taz_s_organize_row_values)
    )

    (setq taz_s_organize_values
      (append
        taz_s_organize_values
        (list taz_s_organize_value_values)
      )
    )

    (setq taz_s_organize_tmp_values (cdr taz_s_organize_tmp_values))
  )

  taz_s_organize_values
)


;; ----------------------------------------------------------------------------
;; MINIMUM Z LISTY
;; ----------------------------------------------------------------------------

(defun taz_s_organize_min (taz_s_organize_min_list)

  (setq taz_s_organize_min_value (car taz_s_organize_min_list))
  (setq taz_s_organize_min_tmp (cdr taz_s_organize_min_list))

  (while taz_s_organize_min_tmp

    (if (< (car taz_s_organize_min_tmp) taz_s_organize_min_value)
      (setq taz_s_organize_min_value (car taz_s_organize_min_tmp))
    )

    (setq taz_s_organize_min_tmp (cdr taz_s_organize_min_tmp))
  )

  taz_s_organize_min_value
)


;; ----------------------------------------------------------------------------
;; MAKSIMUM Z LISTY
;; ----------------------------------------------------------------------------

(defun taz_s_organize_max (taz_s_organize_max_list)

  (setq taz_s_organize_max_value (car taz_s_organize_max_list))
  (setq taz_s_organize_max_tmp (cdr taz_s_organize_max_list))

  (while taz_s_organize_max_tmp

    (if (> (car taz_s_organize_max_tmp) taz_s_organize_max_value)
      (setq taz_s_organize_max_value (car taz_s_organize_max_tmp))
    )

    (setq taz_s_organize_max_tmp (cdr taz_s_organize_max_tmp))
  )

  taz_s_organize_max_value
)


;; ----------------------------------------------------------------------------
;; POBRANIE PRZYBLIZONEGO Z OBIEKTU
;;
;; Ta starsza funkcja zostaje tylko dla przypadkow Z,
;; bo przypadki Z dzialaja poprawnie i celowo ich nie zmieniamy.
;;
;; Dla przypadkow X/Y uzywana jest nizej nowa funkcja,
;; ktora wyznacza Zmin i Zmax calej geometrii obiektu.
;; ----------------------------------------------------------------------------

(defun taz_s_organize_get_entity_z (taz_s_organize_entity_arg)

  (setq taz_s_organize_entity_z nil)
  (setq taz_s_organize_entity_point nil)
  (setq taz_s_organize_entity_point_wcs nil)
  (setq taz_s_organize_entity_data (entget taz_s_organize_entity_arg))
  (setq taz_s_organize_entity_type
    (cdr (assoc 0 taz_s_organize_entity_data))
  )

  ;; ----------------------------------------------------------
  ;; STARA POLYLINE
  ;; Bierzemy pierwszy VERTEX.
  ;; ----------------------------------------------------------

  (if (= taz_s_organize_entity_type "POLYLINE")
    (progn

      (setq taz_s_organize_poly_next (entnext taz_s_organize_entity_arg))
      (setq taz_s_organize_poly_done nil)

      (while
        (and
          taz_s_organize_poly_next
          (= taz_s_organize_poly_done nil)
        )

        (setq taz_s_organize_poly_data (entget taz_s_organize_poly_next))
        (setq taz_s_organize_poly_type
          (cdr (assoc 0 taz_s_organize_poly_data))
        )

        (if (= taz_s_organize_poly_type "VERTEX")
          (progn

            (setq taz_s_organize_entity_point
              (cdr (assoc 10 taz_s_organize_poly_data))
            )

            (if taz_s_organize_entity_point
              (progn

                (if (= (caddr taz_s_organize_entity_point) nil)
                  (setq taz_s_organize_entity_point
                    (list
                      (car taz_s_organize_entity_point)
                      (cadr taz_s_organize_entity_point)
                      0.0
                    )
                  )
                )

                ;; 3DPOLY przechowuje swoje wierzcholki juz w WCS.
                ;; Nie robimy tutaj TRANS, bo mogloby to zmienic poprawny punkt.
                (setq taz_s_organize_entity_point_wcs
                  taz_s_organize_entity_point
                )

                (setq taz_s_organize_entity_z
                  (caddr taz_s_organize_entity_point_wcs)
                )

                (setq taz_s_organize_poly_done T)
              )
            )
          )
        )

        (if (= taz_s_organize_poly_type "SEQEND")
          (setq taz_s_organize_poly_done T)
        )

        (if (= taz_s_organize_poly_done nil)
          (setq taz_s_organize_poly_next
            (entnext taz_s_organize_poly_next)
          )
        )
      )
    )
  )

  ;; ----------------------------------------------------------
  ;; LWPOLYLINE
  ;; Punkt 10 ma X/Y, a wysokosc moze siedziec w kodzie 38.
  ;; ----------------------------------------------------------

  (if
    (and
      (= taz_s_organize_entity_z nil)
      (= taz_s_organize_entity_type "LWPOLYLINE")
    )
    (progn

      (setq taz_s_organize_entity_point
        (cdr (assoc 10 taz_s_organize_entity_data))
      )

      (setq taz_s_organize_entity_elevation 0.0)

      (if (assoc 38 taz_s_organize_entity_data)
        (setq taz_s_organize_entity_elevation
          (cdr (assoc 38 taz_s_organize_entity_data))
        )
      )

      (if taz_s_organize_entity_point
        (progn

          (setq taz_s_organize_entity_point
            (list
              (car taz_s_organize_entity_point)
              (cadr taz_s_organize_entity_point)
              taz_s_organize_entity_elevation
            )
          )

          (setq taz_s_organize_entity_point_wcs
            (trans
              taz_s_organize_entity_point
              taz_s_organize_entity_arg
              0
            )
          )

          (if taz_s_organize_entity_point_wcs
            (setq taz_s_organize_entity_z
              (caddr taz_s_organize_entity_point_wcs)
            )
          )
        )
      )
    )
  )

  ;; ----------------------------------------------------------
  ;; TEXT
  ;; Dla tekstu wyrownanego (np. MC) wazny jest punkt 11.
  ;; ----------------------------------------------------------

  (if
    (and
      (= taz_s_organize_entity_z nil)
      (= taz_s_organize_entity_type "TEXT")
    )
    (progn

      (setq taz_s_organize_text_h 0)
      (setq taz_s_organize_text_v 0)

      (if (assoc 72 taz_s_organize_entity_data)
        (setq taz_s_organize_text_h
          (cdr (assoc 72 taz_s_organize_entity_data))
        )
      )

      (if (assoc 73 taz_s_organize_entity_data)
        (setq taz_s_organize_text_v
          (cdr (assoc 73 taz_s_organize_entity_data))
        )
      )

      (if
        (and
          (or
            (/= taz_s_organize_text_h 0)
            (/= taz_s_organize_text_v 0)
          )
          (assoc 11 taz_s_organize_entity_data)
        )
        (setq taz_s_organize_entity_point
          (cdr (assoc 11 taz_s_organize_entity_data))
        )
        (setq taz_s_organize_entity_point
          (cdr (assoc 10 taz_s_organize_entity_data))
        )
      )
    )
  )

  ;; ----------------------------------------------------------
  ;; ELLIPSE
  ;; Punkt 10 elipsy jest zapisany bezposrednio w WCS.
  ;; ----------------------------------------------------------

  (if
    (and
      (= taz_s_organize_entity_z nil)
      (= taz_s_organize_entity_type "ELLIPSE")
    )
    (progn

      (setq taz_s_organize_entity_point
        (cdr (assoc 10 taz_s_organize_entity_data))
      )

      (if taz_s_organize_entity_point
        (progn

          (if (= (caddr taz_s_organize_entity_point) nil)
            (setq taz_s_organize_entity_point
              (list
                (car taz_s_organize_entity_point)
                (cadr taz_s_organize_entity_point)
                0.0
              )
            )
          )

          (setq taz_s_organize_entity_point_wcs
            taz_s_organize_entity_point
          )

          (setq taz_s_organize_entity_z
            (caddr taz_s_organize_entity_point_wcs)
          )
        )
      )
    )
  )

  ;; ----------------------------------------------------------
  ;; POZOSTALE OBIEKTY
  ;; ----------------------------------------------------------

  (if
    (and
      (= taz_s_organize_entity_z nil)
      (= taz_s_organize_entity_point nil)
    )
    (setq taz_s_organize_entity_point
      (cdr (assoc 10 taz_s_organize_entity_data))
    )
  )

  ;; ----------------------------------------------------------
  ;; Punkt obiektu moze byc zapisany w OCS.
  ;; Przeliczamy go do globalnego WCS.
  ;; ----------------------------------------------------------

  (if
    (and
      (= taz_s_organize_entity_z nil)
      taz_s_organize_entity_point
    )
    (progn

      (if (= (caddr taz_s_organize_entity_point) nil)
        (setq taz_s_organize_entity_point
          (list
            (car taz_s_organize_entity_point)
            (cadr taz_s_organize_entity_point)
            0.0
          )
        )
      )

      (setq taz_s_organize_entity_point_wcs
        (trans
          taz_s_organize_entity_point
          taz_s_organize_entity_arg
          0
        )
      )

      (if taz_s_organize_entity_point_wcs
        (setq taz_s_organize_entity_z
          (caddr taz_s_organize_entity_point_wcs)
        )
      )
    )
  )

  taz_s_organize_entity_z
)


;; ----------------------------------------------------------------------------
;; DODANIE JEDNEJ WARTOSCI Z DO ZAKRESU OBIEKTU
;; ----------------------------------------------------------------------------

(defun taz_s_organize_update_entity_z_value (taz_s_organize_z_value_arg)

  (if taz_s_organize_z_value_arg
    (progn

      (if (= taz_s_organize_entity_z_min nil)
        (setq taz_s_organize_entity_z_min taz_s_organize_z_value_arg)
        (if (< taz_s_organize_z_value_arg taz_s_organize_entity_z_min)
          (setq taz_s_organize_entity_z_min taz_s_organize_z_value_arg)
        )
      )

      (if (= taz_s_organize_entity_z_max nil)
        (setq taz_s_organize_entity_z_max taz_s_organize_z_value_arg)
        (if (> taz_s_organize_z_value_arg taz_s_organize_entity_z_max)
          (setq taz_s_organize_entity_z_max taz_s_organize_z_value_arg)
        )
      )
    )
  )
)


;; ----------------------------------------------------------------------------
;; DODANIE PUNKTU DO ZAKRESU Z OBIEKTU
;;
;; taz_s_organize_point_is_wcs_arg:
;; T   - punkt jest juz w globalnym WCS
;; nil - punkt jest w OCS obiektu i trzeba uzyc TRANS
;; ----------------------------------------------------------------------------

(defun taz_s_organize_update_entity_z_from_point
  (
    taz_s_organize_point_arg
    taz_s_organize_point_entity_arg
    taz_s_organize_point_is_wcs_arg
  )

  (if taz_s_organize_point_arg
    (progn

      (setq taz_s_organize_range_point taz_s_organize_point_arg)

      (if (= (caddr taz_s_organize_range_point) nil)
        (setq taz_s_organize_range_point
          (list
            (car taz_s_organize_range_point)
            (cadr taz_s_organize_range_point)
            0.0
          )
        )
      )

      (if taz_s_organize_point_is_wcs_arg
        (setq taz_s_organize_range_point_wcs
          taz_s_organize_range_point
        )
        (setq taz_s_organize_range_point_wcs
          (trans
            taz_s_organize_range_point
            taz_s_organize_point_entity_arg
            0
          )
        )
      )

      (if taz_s_organize_range_point_wcs
        (taz_s_organize_update_entity_z_value
          (caddr taz_s_organize_range_point_wcs)
        )
      )
    )
  )
)


;; ----------------------------------------------------------------------------
;; RZECZYWISTY ZAKRES Z OBIEKTU
;;
;; Zamiast pytac tylko o jeden punkt obiektu, probujemy ustalic:
;; - najnizszy Z obiektu,
;; - najwyzszy Z obiektu.
;;
;; Nie ma tutaj zadnego sprawdzania nazw warstw.
;; Rozrozniane sa tylko typy obiektow, bo rozne typy przechowuja
;; geometrie w inny sposob.
;;
;; Najwazniejsze przypadki:
;; - LINE       -> poczatek i koniec,
;; - 3DPOLY     -> wszystkie VERTEX,
;; - LWPOLYLINE -> wszystkie wierzcholki,
;; - TEXT       -> punkty 10 i 11,
;; - MTEXT      -> punkt wstawienia,
;; - CIRCLE/ARC -> srodek i promien,
;; - ELLIPSE    -> srodek i polowa osi glownej.
;; ----------------------------------------------------------------------------

(defun taz_s_organize_get_entity_z_range (taz_s_organize_range_entity_arg)

  (setq taz_s_organize_entity_z_min nil)
  (setq taz_s_organize_entity_z_max nil)

  (setq taz_s_organize_range_data
    (entget taz_s_organize_range_entity_arg)
  )

  (setq taz_s_organize_range_type
    (cdr (assoc 0 taz_s_organize_range_data))
  )

  ;; --------------------------------------------------------------------------
  ;; POLYLINE
  ;;
  ;; Dla 3DPOLY / 3D MESH wierzcholki sa juz w WCS.
  ;; Dla starej polilinii 2D punkt przechodzi przez OCS -> WCS.
  ;; Sprawdzamy WSZYSTKIE wierzcholki.
  ;; --------------------------------------------------------------------------

  (if (= taz_s_organize_range_type "POLYLINE")
    (progn

      (setq taz_s_organize_range_poly_flags 0)

      (if (assoc 70 taz_s_organize_range_data)
        (setq taz_s_organize_range_poly_flags
          (cdr (assoc 70 taz_s_organize_range_data))
        )
      )

      (setq taz_s_organize_range_poly_wcs nil)

      (if (/= (logand taz_s_organize_range_poly_flags 8) 0)
        (setq taz_s_organize_range_poly_wcs T)
      )

      (if (/= (logand taz_s_organize_range_poly_flags 16) 0)
        (setq taz_s_organize_range_poly_wcs T)
      )

      (if (/= (logand taz_s_organize_range_poly_flags 64) 0)
        (setq taz_s_organize_range_poly_wcs T)
      )

      (setq taz_s_organize_range_poly_next
        (entnext taz_s_organize_range_entity_arg)
      )

      (setq taz_s_organize_range_poly_done nil)

      (while
        (and
          taz_s_organize_range_poly_next
          (= taz_s_organize_range_poly_done nil)
        )

        (setq taz_s_organize_range_poly_vertex_data
          (entget taz_s_organize_range_poly_next)
        )

        (setq taz_s_organize_range_poly_vertex_type
          (cdr
            (assoc 0 taz_s_organize_range_poly_vertex_data)
          )
        )

        (if (= taz_s_organize_range_poly_vertex_type "VERTEX")
          (progn

            (setq taz_s_organize_range_poly_vertex_point
              (cdr
                (assoc 10 taz_s_organize_range_poly_vertex_data)
              )
            )

            (taz_s_organize_update_entity_z_from_point
              taz_s_organize_range_poly_vertex_point
              taz_s_organize_range_entity_arg
              taz_s_organize_range_poly_wcs
            )
          )
        )

        (if (= taz_s_organize_range_poly_vertex_type "SEQEND")
          (setq taz_s_organize_range_poly_done T)
        )

        (if (= taz_s_organize_range_poly_done nil)
          (setq taz_s_organize_range_poly_next
            (entnext taz_s_organize_range_poly_next)
          )
        )
      )
    )
  )

  ;; --------------------------------------------------------------------------
  ;; LWPOLYLINE
  ;;
  ;; Kod 10 moze wystapic wiele razy.
  ;; Wysokosc polilinii siedzi w kodzie 38.
  ;; --------------------------------------------------------------------------

  (if (= taz_s_organize_range_type "LWPOLYLINE")
    (progn

      (setq taz_s_organize_range_lw_elevation 0.0)

      (if (assoc 38 taz_s_organize_range_data)
        (setq taz_s_organize_range_lw_elevation
          (cdr (assoc 38 taz_s_organize_range_data))
        )
      )

      (setq taz_s_organize_range_lw_tmp
        taz_s_organize_range_data
      )

      (while taz_s_organize_range_lw_tmp

        (setq taz_s_organize_range_lw_item
          (car taz_s_organize_range_lw_tmp)
        )

        (if (= (car taz_s_organize_range_lw_item) 10)
          (progn

            (setq taz_s_organize_range_lw_point
              (cdr taz_s_organize_range_lw_item)
            )

            (setq taz_s_organize_range_lw_point
              (list
                (car taz_s_organize_range_lw_point)
                (cadr taz_s_organize_range_lw_point)
                taz_s_organize_range_lw_elevation
              )
            )

            (taz_s_organize_update_entity_z_from_point
              taz_s_organize_range_lw_point
              taz_s_organize_range_entity_arg
              nil
            )
          )
        )

        (setq taz_s_organize_range_lw_tmp
          (cdr taz_s_organize_range_lw_tmp)
        )
      )
    )
  )

  ;; --------------------------------------------------------------------------
  ;; LINE
  ;; Punkty linii sa juz w WCS.
  ;; --------------------------------------------------------------------------

  (if (= taz_s_organize_range_type "LINE")
    (progn

      (taz_s_organize_update_entity_z_from_point
        (cdr (assoc 10 taz_s_organize_range_data))
        taz_s_organize_range_entity_arg
        T
      )

      (taz_s_organize_update_entity_z_from_point
        (cdr (assoc 11 taz_s_organize_range_data))
        taz_s_organize_range_entity_arg
        T
      )
    )
  )

  ;; --------------------------------------------------------------------------
  ;; 3DFACE
  ;; Wszystkie punkty sa juz w WCS.
  ;; --------------------------------------------------------------------------

  (if (= taz_s_organize_range_type "3DFACE")
    (progn

      (setq taz_s_organize_range_face_code 10)

      (while (<= taz_s_organize_range_face_code 13)

        (if (assoc taz_s_organize_range_face_code taz_s_organize_range_data)
          (taz_s_organize_update_entity_z_from_point
            (cdr
              (assoc
                taz_s_organize_range_face_code
                taz_s_organize_range_data
              )
            )
            taz_s_organize_range_entity_arg
            T
          )
        )

        (setq taz_s_organize_range_face_code
          (+ taz_s_organize_range_face_code 1)
        )
      )
    )
  )

  ;; --------------------------------------------------------------------------
  ;; POINT
  ;; Punkt jest juz w WCS.
  ;; --------------------------------------------------------------------------

  (if (= taz_s_organize_range_type "POINT")
    (taz_s_organize_update_entity_z_from_point
      (cdr (assoc 10 taz_s_organize_range_data))
      taz_s_organize_range_entity_arg
      T
    )
  )

  ;; --------------------------------------------------------------------------
  ;; DIMENSION
  ;; Punkty wymiaru sa w WCS.
  ;; --------------------------------------------------------------------------

  (if (= taz_s_organize_range_type "DIMENSION")
    (progn

      (setq taz_s_organize_range_dim_code 10)

      (while (<= taz_s_organize_range_dim_code 16)

        (if (assoc taz_s_organize_range_dim_code taz_s_organize_range_data)
          (taz_s_organize_update_entity_z_from_point
            (cdr
              (assoc
                taz_s_organize_range_dim_code
                taz_s_organize_range_data
              )
            )
            taz_s_organize_range_entity_arg
            T
          )
        )

        (setq taz_s_organize_range_dim_code
          (+ taz_s_organize_range_dim_code 1)
        )
      )
    )
  )

  ;; --------------------------------------------------------------------------
  ;; TEXT
  ;;
  ;; Bierzemy oba punkty 10 i 11.
  ;; Dla tekstu MC punkt 11 jest szczegolnie wazny.
  ;; --------------------------------------------------------------------------

  (if (= taz_s_organize_range_type "TEXT")
    (progn

      (if (assoc 10 taz_s_organize_range_data)
        (taz_s_organize_update_entity_z_from_point
          (cdr (assoc 10 taz_s_organize_range_data))
          taz_s_organize_range_entity_arg
          nil
        )
      )

      (if (assoc 11 taz_s_organize_range_data)
        (taz_s_organize_update_entity_z_from_point
          (cdr (assoc 11 taz_s_organize_range_data))
          taz_s_organize_range_entity_arg
          nil
        )
      )
    )
  )

  ;; --------------------------------------------------------------------------
  ;; MTEXT
  ;;
  ;; W GstarCAD po ROTATE3D punkt 10 zachowuje rzeczywiste polozenie
  ;; potrzebne do naszego grupowania. Czytamy go bez dodatkowego TRANS.
  ;; To rozwiazuje przypadek etykiet umieszczonych centralnie w widoku.
  ;; --------------------------------------------------------------------------

  (if (= taz_s_organize_range_type "MTEXT")
    (taz_s_organize_update_entity_z_from_point
      (cdr (assoc 10 taz_s_organize_range_data))
      taz_s_organize_range_entity_arg
      T
    )
  )

  ;; --------------------------------------------------------------------------
  ;; CIRCLE ORAZ ARC
  ;;
  ;; Bierzemy srodek po OCS -> WCS.
  ;; Do zakresu dodajemy tez promien z obu stron.
  ;; Jest to celowo bezpieczne przy okregu / luku obroconym w 3D.
  ;; --------------------------------------------------------------------------

  (if
    (or
      (= taz_s_organize_range_type "CIRCLE")
      (= taz_s_organize_range_type "ARC")
    )
    (progn

      (setq taz_s_organize_range_point_wcs nil)

      (taz_s_organize_update_entity_z_from_point
        (cdr (assoc 10 taz_s_organize_range_data))
        taz_s_organize_range_entity_arg
        nil
      )

      (setq taz_s_organize_range_radius 0.0)

      (if (assoc 40 taz_s_organize_range_data)
        (setq taz_s_organize_range_radius
          (cdr (assoc 40 taz_s_organize_range_data))
        )
      )

      (if taz_s_organize_range_point_wcs
        (progn

          (setq taz_s_organize_range_center_z
            (caddr taz_s_organize_range_point_wcs)
          )

          (taz_s_organize_update_entity_z_value
            (- taz_s_organize_range_center_z taz_s_organize_range_radius)
          )

          (taz_s_organize_update_entity_z_value
            (+ taz_s_organize_range_center_z taz_s_organize_range_radius)
          )
        )
      )
    )
  )

  ;; --------------------------------------------------------------------------
  ;; ELLIPSE
  ;;
  ;; Punkt 10 oraz wektor osi glownej 11 sa zapisane w WCS.
  ;; Do zakresu Z bierzemy srodek oraz bezpieczny zakres +/- polowa osi
  ;; glownej. Dzieki temu elipsa trafia do wyboru tak samo jak CIRCLE/ARC.
  ;; --------------------------------------------------------------------------

  (if (= taz_s_organize_range_type "ELLIPSE")
    (progn

      (setq taz_s_organize_range_point_wcs nil)

      (taz_s_organize_update_entity_z_from_point
        (cdr (assoc 10 taz_s_organize_range_data))
        taz_s_organize_range_entity_arg
        T
      )

      (setq taz_s_organize_range_ellipse_radius 0.0)

      (if (assoc 11 taz_s_organize_range_data)
        (setq taz_s_organize_range_ellipse_radius
          (distance
            '(0.0 0.0 0.0)
            (cdr (assoc 11 taz_s_organize_range_data))
          )
        )
      )

      (if taz_s_organize_range_point_wcs
        (progn

          (setq taz_s_organize_range_center_z
            (caddr taz_s_organize_range_point_wcs)
          )

          (taz_s_organize_update_entity_z_value
            (-
              taz_s_organize_range_center_z
              taz_s_organize_range_ellipse_radius
            )
          )

          (taz_s_organize_update_entity_z_value
            (+
              taz_s_organize_range_center_z
              taz_s_organize_range_ellipse_radius
            )
          )
        )
      )
    )
  )

  ;; --------------------------------------------------------------------------
  ;; POZOSTALE OBIEKTY
  ;;
  ;; Jesli powyzej nie udalo sie znalezc zadnego Z, probujemy punktu 10.
  ;; Jest to zachowanie awaryjne dla pozostalych typow.
  ;; --------------------------------------------------------------------------

  (if
    (and
      (= taz_s_organize_entity_z_min nil)
      (assoc 10 taz_s_organize_range_data)
    )
    (taz_s_organize_update_entity_z_from_point
      (cdr (assoc 10 taz_s_organize_range_data))
      taz_s_organize_range_entity_arg
      nil
    )
  )

  (if
    (and
      taz_s_organize_entity_z_min
      taz_s_organize_entity_z_max
    )
    (list
      taz_s_organize_entity_z_min
      taz_s_organize_entity_z_max
    )
    nil
  )
)


;; ----------------------------------------------------------------------------
;; ZBIERANIE OBIEKTOW JEDNEGO PRZYPADKU Z
;;
;; Ta starsza selekcja zostaje tylko dla przypadkow Z,
;; poniewaz przypadki Z dzialaja poprawnie.
;; Kazdy przypadek byl tworzony co 100000 w osi Z.
;; Przyjmujemy pas +/- 49000 wokol danego poziomu.
;; ----------------------------------------------------------------------------

(defun taz_s_organize_collect_case (taz_s_organize_case_z_arg)

  (setq taz_s_organize_case_ss (ssadd))

  (setq taz_s_organize_all_ss
    (ssget
      "_X"
      (list
        (cons 67 0)
      )
    )
  )

  (if taz_s_organize_all_ss
    (progn

      (setq taz_s_organize_all_i 0)

      (while (< taz_s_organize_all_i (sslength taz_s_organize_all_ss))

        (setq taz_s_organize_all_ent
          (ssname taz_s_organize_all_ss taz_s_organize_all_i)
        )

        (setq taz_s_organize_all_z
          (taz_s_organize_get_entity_z taz_s_organize_all_ent)
        )

        (if taz_s_organize_all_z
          (progn
            (if
              (and
                (>=
                  taz_s_organize_all_z
                  (- taz_s_organize_case_z_arg taz_s_organize_case_half_range)
                )
                (<=
                  taz_s_organize_all_z
                  (+ taz_s_organize_case_z_arg taz_s_organize_case_half_range)
                )
              )
              (ssadd taz_s_organize_all_ent taz_s_organize_case_ss)
            )
          )
        )

        (setq taz_s_organize_all_i (+ taz_s_organize_all_i 1))
      )
    )
  )

  taz_s_organize_case_ss
)


;; ----------------------------------------------------------------------------
;; ZBIERANIE OBIEKTOW Z CALEGO ZAKRESU Z
;;
;; Ta funkcja jest uzywana tylko dla przypadkow X oraz Y.
;;
;; Najpierw kazdy obiekt dostaje swoj rzeczywisty zakres:
;;   Zmin obiektu ... Zmax obiektu
;;
;; Potem sprawdzamy, czy ten zakres przecina zakres danego przypadku.
;; Nie ma zadnego filtrowania po nazwach warstw.
;; ----------------------------------------------------------------------------

(defun taz_s_organize_collect_case_z_range
  (taz_s_organize_range_min_arg taz_s_organize_range_max_arg)

  (setq taz_s_organize_case_ss (ssadd))

  (setq taz_s_organize_all_ss
    (ssget
      "_X"
      (list
        (cons 67 0)
      )
    )
  )

  (if taz_s_organize_all_ss
    (progn

      (setq taz_s_organize_all_i 0)

      (while (< taz_s_organize_all_i (sslength taz_s_organize_all_ss))

        (setq taz_s_organize_all_ent
          (ssname taz_s_organize_all_ss taz_s_organize_all_i)
        )

        (setq taz_s_organize_all_z_range
          (taz_s_organize_get_entity_z_range
            taz_s_organize_all_ent
          )
        )

        (if taz_s_organize_all_z_range
          (progn

            (setq taz_s_organize_all_z_min
              (car taz_s_organize_all_z_range)
            )

            (setq taz_s_organize_all_z_max
              (cadr taz_s_organize_all_z_range)
            )

            ;; --------------------------------------------------------------
            ;; Zakres obiektu przecina zakres przypadku, jezeli:
            ;;
            ;; Zmin obiektu <= Zmax przypadku
            ;; ORAZ
            ;; Zmax obiektu >= Zmin przypadku
            ;;
            ;; Dzieki temu dluga os zostanie wybrana nawet wtedy,
            ;; gdy jej poczatek i koniec leza poza naszym zakresem,
            ;; ale sama os przechodzi przez ten zakres.
            ;; --------------------------------------------------------------

            (if
              (and
                (<=
                  taz_s_organize_all_z_min
                  taz_s_organize_range_max_arg
                )
                (>=
                  taz_s_organize_all_z_max
                  taz_s_organize_range_min_arg
                )
              )
              (ssadd
                taz_s_organize_all_ent
                taz_s_organize_case_ss
              )
            )
          )
        )

        (setq taz_s_organize_all_i (+ taz_s_organize_all_i 1))
      )
    )
  )

  taz_s_organize_case_ss
)


;; ----------------------------------------------------------------------------
;; ZAPAMIETANIE WARSTW, KTORE BYLY ZABLOKOWANE
;; I ODBLOKOWANIE WARSTW NA CZAS PRACY
;; ----------------------------------------------------------------------------

(defun taz_s_organize_unlock_layers ()

  (setq taz_s_organize_locked_layers '())
  (setq taz_s_organize_layer_rec (tblnext "LAYER" T))

  (while taz_s_organize_layer_rec

    (setq taz_s_organize_layer_name
      (cdr (assoc 2 taz_s_organize_layer_rec))
    )

    (setq taz_s_organize_layer_flags
      (cdr (assoc 70 taz_s_organize_layer_rec))
    )

    (if (= (logand taz_s_organize_layer_flags 4) 4)
      (setq taz_s_organize_locked_layers
        (append
          taz_s_organize_locked_layers
          (list taz_s_organize_layer_name)
        )
      )
    )

    (setq taz_s_organize_layer_rec (tblnext "LAYER"))
  )

  (command "_.-LAYER" "_UNLOCK" "*" "")
)


;; ----------------------------------------------------------------------------
;; PONOWNE ZABLOKOWANIE TYLKO TYCH WARSTW,
;; KTORE BYLY ZABLOKOWANE PRZED STARTEM SKRYPTU
;; ----------------------------------------------------------------------------

(defun taz_s_organize_restore_locked_layers ()

  (setq taz_s_organize_locked_tmp taz_s_organize_locked_layers)

  (while taz_s_organize_locked_tmp

    (setq taz_s_organize_locked_name
      (car taz_s_organize_locked_tmp)
    )

    (command "_.-LAYER" "_LOCK" taz_s_organize_locked_name "")

    (setq taz_s_organize_locked_tmp (cdr taz_s_organize_locked_tmp))
  )
)


;; ----------------------------------------------------------------------------
;; PRZYGOTOWANIE DANYCH OSI
;;
;; Najpierw probujemy wykorzystac dane, ktore zostaly w pamieci po skrypcie
;; tworzacym rysunki. Jesli ich nie ma, probujemy zaladowac taz_s_data_file.
;; ----------------------------------------------------------------------------

(defun taz_s_organize_prepare_data ()

  (setq taz_s_organize_can_run T)

  (if
    (or
      (not (boundp 'taz_s_axis_data_x))
      (not (boundp 'taz_s_axis_data_y))
      (not (boundp 'taz_s_axis_data_z))
    )
    (progn
      (if (boundp 'taz_s_data_file)
        (progn
          (if (findfile taz_s_data_file)
            (load taz_s_data_file)
          )
        )
      )
    )
  )

  (if
    (and
      (boundp 'taz_s_axis_data_x)
      (boundp 'taz_s_axis_data_y)
      (boundp 'taz_s_axis_data_z)
    )
    (progn
      (setq taz_s_organize_x_data taz_s_axis_data_x)
      (setq taz_s_organize_y_data taz_s_axis_data_y)
      (setq taz_s_organize_z_data taz_s_axis_data_z)
    )
    (progn
      (setq taz_s_organize_can_run nil)
      (princ "\nBrak danych osi taz_s_axis_data_x / y / z - porzadkowanie przerwane.")
    )
  )
)


;; ============================================================================
;; MAPOWANIE NUMERU PRZYPADKU ORGANIZERA NA INDEKS TABELI
;; ============================================================================
;; Generator zapisuje tabele w kolejnosci:
;;   IZO, X, Y, Z
;;
;; Organizer pozostaje w kolejnosci:
;;   X, Y, Z, IZO
;;
;; Dla X/Y/Z:
;;   numer organizera 1..N -> indeks listy 1..N
;;
;; Dla IZO:
;;   numer organizera N+1 -> indeks listy 0
;; ============================================================================

(defun taz_s_organize_get_table_list_index
  (taz_s_organize_table_case_nr_arg)

  (setq taz_s_organize_table_section_count
    (+
      (length taz_s_organize_x_data)
      (length taz_s_organize_y_data)
      (length taz_s_organize_z_data)
    )
  )

  (if
    (=
      taz_s_organize_table_case_nr_arg
      (+ taz_s_organize_table_section_count 1)
    )
    0
    taz_s_organize_table_case_nr_arg
  )
)


;; ============================================================================
;; TABELA ZESTAWIENIA STALI - PRZYGOTOWANIE PRZYPADKU
;; ============================================================================
;; Jesli taz_s_create_drawings_execution_design zapamietal obiekty tabeli
;; oraz jej punkt kotwiczenia, dodajemy te obiekty jawnie do selection setu
;; przypadku. Dodatkowo tworzymy tymczasowy POINT dokladnie w punkcie
;; kotwiczenia tabeli. POINT przechodzi przez ten sam ALIGN co caly przypadek,
;; dzieki czemu po uporzadkowaniu znamy dokladne nowe polozenie kotwy tabeli.
;; ============================================================================

(defun taz_s_organize_prepare_table_case
  (taz_s_organize_table_case_nr_arg taz_s_organize_table_case_ss_arg)

  (setq taz_s_organize_current_table_group nil)
  (setq taz_s_organize_current_table_anchor nil)
  (setq taz_s_organize_current_table_marker nil)

  (if
    (and
      (boundp 'taz_s_execution_design_table_groups)
      taz_s_execution_design_table_groups
    )
    (setq taz_s_organize_current_table_group
      (nth
        (taz_s_organize_get_table_list_index
          taz_s_organize_table_case_nr_arg
        )
        taz_s_execution_design_table_groups
      )
    )
  )

  (if
    (and
      (boundp 'taz_s_execution_design_table_anchor_points)
      taz_s_execution_design_table_anchor_points
    )
    (setq taz_s_organize_current_table_anchor
      (nth
        (taz_s_organize_get_table_list_index
          taz_s_organize_table_case_nr_arg
        )
        taz_s_execution_design_table_anchor_points
      )
    )
  )

  ;; Jawnie dodajemy wszystkie obiekty tabeli do przypadku.
  ;; Jesli juz byly w selection secie, SSADD ich nie dubluje.
  (setq taz_s_organize_table_group_tmp
    taz_s_organize_current_table_group
  )

  (while taz_s_organize_table_group_tmp

    (setq taz_s_organize_table_group_ent
      (car taz_s_organize_table_group_tmp)
    )

    (if
      (and
        taz_s_organize_table_group_ent
        (entget taz_s_organize_table_group_ent)
      )
      (ssadd
        taz_s_organize_table_group_ent
        taz_s_organize_table_case_ss_arg
      )
    )

    (setq taz_s_organize_table_group_tmp
      (cdr taz_s_organize_table_group_tmp)
    )
  )

  ;; Tymczasowy marker punktu kotwiczenia tabeli.
  (if
    (and
      taz_s_organize_current_table_group
      taz_s_organize_current_table_anchor
    )
    (progn

      (setq taz_s_organize_current_table_marker
        (entmakex
          (list
            '(0 . "POINT")
            '(8 . "0")
            (cons 10 taz_s_organize_current_table_anchor)
          )
        )
      )

      (if taz_s_organize_current_table_marker
        (ssadd
          taz_s_organize_current_table_marker
          taz_s_organize_table_case_ss_arg
        )
      )
    )
  )

  taz_s_organize_table_case_ss_arg
)


;; ============================================================================
;; TABELA ZESTAWIENIA STALI - ZAKONCZENIE PRZYPADKU
;; ============================================================================
;; Po ALIGN odczytujemy nowe polozenie tymczasowego POINT, usuwamy go z
;; selection setu oraz z rysunku i zapamietujemy nowe polozenie kotwy tabeli.
;; Lista ma kolejnosc organizera: X, Y, Z, IZO.
;; ============================================================================

(defun taz_s_organize_finish_table_case (taz_s_organize_table_case_ss_arg)

  (setq taz_s_organize_current_table_anchor_after nil)

  (if taz_s_organize_current_table_marker
    (progn

      (setq taz_s_organize_table_marker_data
        (entget taz_s_organize_current_table_marker)
      )

      (if
        (and
          taz_s_organize_table_marker_data
          (assoc 10 taz_s_organize_table_marker_data)
        )
        (setq taz_s_organize_current_table_anchor_after
          (cdr (assoc 10 taz_s_organize_table_marker_data))
        )
      )

      (ssdel
        taz_s_organize_current_table_marker
        taz_s_organize_table_case_ss_arg
      )

      (entdel taz_s_organize_current_table_marker)
    )
  )

  (setq taz_s_organize_table_anchor_points_after
    (append
      taz_s_organize_table_anchor_points_after
      (list taz_s_organize_current_table_anchor_after)
    )
  )
)


;; ============================================================================
;; LEWY DOLNY NAROZNIK GRUPY TABELI - BEZ VL / COM
;; ============================================================================
;; Po ALIGN przechodzimy po geometrii tabeli i szukamy najmniejszego X oraz Y.
;; Logika: SETQ + IF + WHILE + ENTGET / ENTNEXT.
;; ============================================================================

(defun taz_s_organize_table_check_point (taz_s_organize_table_point_arg)

  (if taz_s_organize_table_point_arg
    (progn

      (setq taz_s_organize_table_point_x
        (car taz_s_organize_table_point_arg)
      )

      (setq taz_s_organize_table_point_y
        (cadr taz_s_organize_table_point_arg)
      )

      (if (= taz_s_organize_table_xmin nil)
        (setq taz_s_organize_table_xmin taz_s_organize_table_point_x)
        (if (< taz_s_organize_table_point_x taz_s_organize_table_xmin)
          (setq taz_s_organize_table_xmin taz_s_organize_table_point_x)
        )
      )

      (if (= taz_s_organize_table_ymin nil)
        (setq taz_s_organize_table_ymin taz_s_organize_table_point_y)
        (if (< taz_s_organize_table_point_y taz_s_organize_table_ymin)
          (setq taz_s_organize_table_ymin taz_s_organize_table_point_y)
        )
      )
    )
  )
)


(defun taz_s_organize_get_table_group_lower_left
  (taz_s_organize_table_group_arg)

  (setq taz_s_organize_table_xmin nil)
  (setq taz_s_organize_table_ymin nil)
  (setq taz_s_organize_table_tmp taz_s_organize_table_group_arg)

  (while taz_s_organize_table_tmp

    (setq taz_s_organize_table_ent
      (car taz_s_organize_table_tmp)
    )

    (if
      (and
        taz_s_organize_table_ent
        (entget taz_s_organize_table_ent)
      )
      (progn

        (setq taz_s_organize_table_data
          (entget taz_s_organize_table_ent)
        )

        (setq taz_s_organize_table_type
          (cdr (assoc 0 taz_s_organize_table_data))
        )

        (if (= taz_s_organize_table_type "LINE")
          (progn
            (taz_s_organize_table_check_point
              (cdr (assoc 10 taz_s_organize_table_data))
            )
            (taz_s_organize_table_check_point
              (cdr (assoc 11 taz_s_organize_table_data))
            )
          )
        )

        (if (= taz_s_organize_table_type "LWPOLYLINE")
          (progn

            (setq taz_s_organize_table_dxf_tmp
              taz_s_organize_table_data
            )

            (while taz_s_organize_table_dxf_tmp

              (setq taz_s_organize_table_dxf_item
                (car taz_s_organize_table_dxf_tmp)
              )

              (if (= (car taz_s_organize_table_dxf_item) 10)
                (taz_s_organize_table_check_point
                  (cdr taz_s_organize_table_dxf_item)
                )
              )

              (setq taz_s_organize_table_dxf_tmp
                (cdr taz_s_organize_table_dxf_tmp)
              )
            )
          )
        )

        (if (= taz_s_organize_table_type "POLYLINE")
          (progn

            (setq taz_s_organize_table_poly_next
              (entnext taz_s_organize_table_ent)
            )

            (setq taz_s_organize_table_poly_done nil)

            (while
              (and
                taz_s_organize_table_poly_next
                (= taz_s_organize_table_poly_done nil)
              )

              (setq taz_s_organize_table_poly_data
                (entget taz_s_organize_table_poly_next)
              )

              (setq taz_s_organize_table_poly_type
                (cdr (assoc 0 taz_s_organize_table_poly_data))
              )

              (if (= taz_s_organize_table_poly_type "VERTEX")
                (taz_s_organize_table_check_point
                  (cdr (assoc 10 taz_s_organize_table_poly_data))
                )
              )

              (if (= taz_s_organize_table_poly_type "SEQEND")
                (setq taz_s_organize_table_poly_done T)
              )

              (if (= taz_s_organize_table_poly_done nil)
                (setq taz_s_organize_table_poly_next
                  (entnext taz_s_organize_table_poly_next)
                )
              )
            )
          )
        )

        (if
          (or
            (= taz_s_organize_table_type "SOLID")
            (= taz_s_organize_table_type "TRACE")
            (= taz_s_organize_table_type "3DFACE")
          )
          (progn
            (taz_s_organize_table_check_point
              (cdr (assoc 10 taz_s_organize_table_data))
            )
            (taz_s_organize_table_check_point
              (cdr (assoc 11 taz_s_organize_table_data))
            )
            (taz_s_organize_table_check_point
              (cdr (assoc 12 taz_s_organize_table_data))
            )
            (taz_s_organize_table_check_point
              (cdr (assoc 13 taz_s_organize_table_data))
            )
          )
        )
      )
    )

    (setq taz_s_organize_table_tmp
      (cdr taz_s_organize_table_tmp)
    )
  )

  (if
    (and
      taz_s_organize_table_xmin
      taz_s_organize_table_ymin
    )
    (list
      taz_s_organize_table_xmin
      taz_s_organize_table_ymin
      0.0
    )
    nil
  )
)


;; ============================================================================
;; KONCOWE PRZESUNIECIE TABELI DO NAROZNIKA RAMKI
;; ============================================================================
;; Przesuwamy komplet obiektow tabeli z jej rzeczywistego lewego dolnego
;; naroznika po ALIGN do lewego dolnego naroznika wewnetrznej ramki.
;; Warstwy tabeli sa odblokowywane tylko na czas MOVE i ponownie blokowane
;; wyłącznie wtedy, gdy byly zablokowane przed przesunieciem.
;; ============================================================================

(defun taz_s_organize_move_table_group
  (
    taz_s_organize_move_table_group_arg
    taz_s_organize_move_table_source_arg
    taz_s_organize_move_table_target_arg
  )

  (setq taz_s_organize_move_table_ss (ssadd))
  (setq taz_s_organize_move_table_layers '())
  (setq taz_s_organize_move_table_relock_layers '())

  (setq taz_s_organize_move_table_tmp
    taz_s_organize_move_table_group_arg
  )

  (while taz_s_organize_move_table_tmp

    (setq taz_s_organize_move_table_ent
      (car taz_s_organize_move_table_tmp)
    )

    (if
      (and
        taz_s_organize_move_table_ent
        (entget taz_s_organize_move_table_ent)
      )
      (progn

        (ssadd
          taz_s_organize_move_table_ent
          taz_s_organize_move_table_ss
        )

        (setq taz_s_organize_move_table_data
          (entget taz_s_organize_move_table_ent)
        )

        (setq taz_s_organize_move_table_layer
          (cdr (assoc 8 taz_s_organize_move_table_data))
        )

        (if
          (and
            taz_s_organize_move_table_layer
            (not
              (member
                taz_s_organize_move_table_layer
                taz_s_organize_move_table_layers
              )
            )
          )
          (setq taz_s_organize_move_table_layers
            (append
              taz_s_organize_move_table_layers
              (list taz_s_organize_move_table_layer)
            )
          )
        )
      )
    )

    (setq taz_s_organize_move_table_tmp
      (cdr taz_s_organize_move_table_tmp)
    )
  )

  ;; Zapamietaj stan blokady warstw i odblokuj je na czas MOVE.
  (setq taz_s_organize_move_table_layer_tmp
    taz_s_organize_move_table_layers
  )

  (while taz_s_organize_move_table_layer_tmp

    (setq taz_s_organize_move_table_layer_name
      (car taz_s_organize_move_table_layer_tmp)
    )

    (setq taz_s_organize_move_table_layer_rec
      (tblsearch "LAYER" taz_s_organize_move_table_layer_name)
    )

    (if
      (and
        taz_s_organize_move_table_layer_rec
        (= 
          (logand
            (cdr (assoc 70 taz_s_organize_move_table_layer_rec))
            4
          )
          4
        )
      )
      (setq taz_s_organize_move_table_relock_layers
        (append
          taz_s_organize_move_table_relock_layers
          (list taz_s_organize_move_table_layer_name)
        )
      )
    )

    (command
      "_.-LAYER"
      "_UNLOCK"
      taz_s_organize_move_table_layer_name
      ""
    )

    (setq taz_s_organize_move_table_layer_tmp
      (cdr taz_s_organize_move_table_layer_tmp)
    )
  )

  (if
    (and
      (> (sslength taz_s_organize_move_table_ss) 0)
      taz_s_organize_move_table_source_arg
      taz_s_organize_move_table_target_arg
    )
    (command
      "_.MOVE"
      taz_s_organize_move_table_ss
      ""
      "_NON"
      taz_s_organize_move_table_source_arg
      "_NON"
      taz_s_organize_move_table_target_arg
    )
  )

  ;; Przywroc blokade tylko tam, gdzie byla przed przesunieciem.
  (setq taz_s_organize_move_table_layer_tmp
    taz_s_organize_move_table_relock_layers
  )

  (while taz_s_organize_move_table_layer_tmp

    (command
      "_.-LAYER"
      "_LOCK"
      (car taz_s_organize_move_table_layer_tmp)
      ""
    )

    (setq taz_s_organize_move_table_layer_tmp
      (cdr taz_s_organize_move_table_layer_tmp)
    )
  )
)


;; ============================================================================
;; CICHE PRZERWANIE ORGANIZERA
;; ============================================================================
;; Uzywane m.in. po wybraniu ANULUJ w nowym oknie formatu arkusza.
;; Podmieniamy na chwile *error*, wywolujemy exit i nie pokazujemy
;; komunikatu "quit / exit abort".
;; ============================================================================

(defun taz_s_organize_exit ()

  (setq taz_s_organize_old_error *error*)

  (setq *error*
    (lambda (taz_s_organize_error_message)
      (setq *error* taz_s_organize_old_error)
      (princ "")
    )
  )

  (exit)
)



     

;; ============================================================================
;; LAYOUTY - FUNKCJE POMOCNICZE
;; ============================================================================
;; Ten fragment jest wykonywany dopiero na samym koncu organizera.
;; Styl: SETQ + IF + WHILE + ENTGET / ENTMOD + COMMAND.
;;
;; Bez VL / VLA / VLAX / COM.
;;
;; Plotter:
;; - najpierw probujemy bezposrednio "DWG To PDF.pc3",
;; - jezeli CAD nie przyjmie tej nazwy -> ustawiamy "None".
;; ============================================================================


(defun taz_s_organize_layout_get_last_entity ()

  (setq taz_s_organize_layout_last_ent (entlast))

  (if taz_s_organize_layout_last_ent
    (progn
      (setq taz_s_organize_layout_last_next
        (entnext taz_s_organize_layout_last_ent)
      )

      (while taz_s_organize_layout_last_next
        (setq taz_s_organize_layout_last_ent
          taz_s_organize_layout_last_next
        )
        (setq taz_s_organize_layout_last_next
          (entnext taz_s_organize_layout_last_ent)
        )
      )
    )
  )

  taz_s_organize_layout_last_ent
)


(defun taz_s_organize_layout_collect_new_entities
  (taz_s_organize_layout_before_arg)

  (setq taz_s_organize_layout_new_entities '())

  (if taz_s_organize_layout_before_arg
    (setq taz_s_organize_layout_new_ent
      (entnext taz_s_organize_layout_before_arg)
    )
    (setq taz_s_organize_layout_new_ent (entnext))
  )

  (while taz_s_organize_layout_new_ent

    (setq taz_s_organize_layout_new_data
      (entget taz_s_organize_layout_new_ent)
    )

    (setq taz_s_organize_layout_new_type
      (cdr (assoc 0 taz_s_organize_layout_new_data))
    )

    (if
      (not
        (member
          taz_s_organize_layout_new_type
          '("VERTEX" "SEQEND" "ATTRIB")
        )
      )
      (setq taz_s_organize_layout_new_entities
        (append
          taz_s_organize_layout_new_entities
          (list taz_s_organize_layout_new_ent)
        )
      )
    )

    (setq taz_s_organize_layout_new_ent
      (entnext taz_s_organize_layout_new_ent)
    )
  )

  taz_s_organize_layout_new_entities
)


;; ----------------------------------------------------------------------------
;; GRANICE GEOMETRII RAMKI - BEZ VL
;; ----------------------------------------------------------------------------

(defun taz_s_organize_layout_check_bound_point
  (taz_s_organize_layout_bound_point_arg)

  (if taz_s_organize_layout_bound_point_arg
    (progn

      (setq taz_s_organize_layout_bound_point_x
        (car taz_s_organize_layout_bound_point_arg)
      )
      (setq taz_s_organize_layout_bound_point_y
        (cadr taz_s_organize_layout_bound_point_arg)
      )

      (if (= taz_s_organize_layout_bound_xmin nil)
        (setq taz_s_organize_layout_bound_xmin
          taz_s_organize_layout_bound_point_x
        )
        (if
          (<
            taz_s_organize_layout_bound_point_x
            taz_s_organize_layout_bound_xmin
          )
          (setq taz_s_organize_layout_bound_xmin
            taz_s_organize_layout_bound_point_x
          )
        )
      )

      (if (= taz_s_organize_layout_bound_ymin nil)
        (setq taz_s_organize_layout_bound_ymin
          taz_s_organize_layout_bound_point_y
        )
        (if
          (<
            taz_s_organize_layout_bound_point_y
            taz_s_organize_layout_bound_ymin
          )
          (setq taz_s_organize_layout_bound_ymin
            taz_s_organize_layout_bound_point_y
          )
        )
      )

      (if (= taz_s_organize_layout_bound_xmax nil)
        (setq taz_s_organize_layout_bound_xmax
          taz_s_organize_layout_bound_point_x
        )
        (if
          (>
            taz_s_organize_layout_bound_point_x
            taz_s_organize_layout_bound_xmax
          )
          (setq taz_s_organize_layout_bound_xmax
            taz_s_organize_layout_bound_point_x
          )
        )
      )

      (if (= taz_s_organize_layout_bound_ymax nil)
        (setq taz_s_organize_layout_bound_ymax
          taz_s_organize_layout_bound_point_y
        )
        (if
          (>
            taz_s_organize_layout_bound_point_y
            taz_s_organize_layout_bound_ymax
          )
          (setq taz_s_organize_layout_bound_ymax
            taz_s_organize_layout_bound_point_y
          )
        )
      )
    )
  )
)


(defun taz_s_organize_layout_get_group_bounds
  (taz_s_organize_layout_group_arg)

  (setq taz_s_organize_layout_bound_xmin nil)
  (setq taz_s_organize_layout_bound_ymin nil)
  (setq taz_s_organize_layout_bound_xmax nil)
  (setq taz_s_organize_layout_bound_ymax nil)

  (setq taz_s_organize_layout_bound_tmp
    taz_s_organize_layout_group_arg
  )

  (while taz_s_organize_layout_bound_tmp

    (setq taz_s_organize_layout_bound_ent
      (car taz_s_organize_layout_bound_tmp)
    )

    (if
      (and
        taz_s_organize_layout_bound_ent
        (entget taz_s_organize_layout_bound_ent)
      )
      (progn

        (setq taz_s_organize_layout_bound_data
          (entget taz_s_organize_layout_bound_ent)
        )
        (setq taz_s_organize_layout_bound_type
          (cdr (assoc 0 taz_s_organize_layout_bound_data))
        )

        (if (= taz_s_organize_layout_bound_type "LINE")
          (progn
            (taz_s_organize_layout_check_bound_point
              (cdr (assoc 10 taz_s_organize_layout_bound_data))
            )
            (taz_s_organize_layout_check_bound_point
              (cdr (assoc 11 taz_s_organize_layout_bound_data))
            )
          )
        )

        (if (= taz_s_organize_layout_bound_type "LWPOLYLINE")
          (progn

            (setq taz_s_organize_layout_bound_dxf_tmp
              taz_s_organize_layout_bound_data
            )

            (while taz_s_organize_layout_bound_dxf_tmp

              (setq taz_s_organize_layout_bound_dxf_item
                (car taz_s_organize_layout_bound_dxf_tmp)
              )

              (if (= (car taz_s_organize_layout_bound_dxf_item) 10)
                (taz_s_organize_layout_check_bound_point
                  (cdr taz_s_organize_layout_bound_dxf_item)
                )
              )

              (setq taz_s_organize_layout_bound_dxf_tmp
                (cdr taz_s_organize_layout_bound_dxf_tmp)
              )
            )
          )
        )

        (if (= taz_s_organize_layout_bound_type "POLYLINE")
          (progn

            (setq taz_s_organize_layout_bound_poly_next
              (entnext taz_s_organize_layout_bound_ent)
            )
            (setq taz_s_organize_layout_bound_poly_done nil)

            (while
              (and
                taz_s_organize_layout_bound_poly_next
                (= taz_s_organize_layout_bound_poly_done nil)
              )

              (setq taz_s_organize_layout_bound_poly_data
                (entget taz_s_organize_layout_bound_poly_next)
              )
              (setq taz_s_organize_layout_bound_poly_type
                (cdr
                  (assoc
                    0
                    taz_s_organize_layout_bound_poly_data
                  )
                )
              )

              (if (= taz_s_organize_layout_bound_poly_type "VERTEX")
                (taz_s_organize_layout_check_bound_point
                  (cdr
                    (assoc
                      10
                      taz_s_organize_layout_bound_poly_data
                    )
                  )
                )
              )

              (if (= taz_s_organize_layout_bound_poly_type "SEQEND")
                (setq taz_s_organize_layout_bound_poly_done T)
              )

              (if (= taz_s_organize_layout_bound_poly_done nil)
                (setq taz_s_organize_layout_bound_poly_next
                  (entnext taz_s_organize_layout_bound_poly_next)
                )
              )
            )
          )
        )

        (if
          (or
            (= taz_s_organize_layout_bound_type "SOLID")
            (= taz_s_organize_layout_bound_type "TRACE")
            (= taz_s_organize_layout_bound_type "3DFACE")
          )
          (progn
            (taz_s_organize_layout_check_bound_point
              (cdr (assoc 10 taz_s_organize_layout_bound_data))
            )
            (taz_s_organize_layout_check_bound_point
              (cdr (assoc 11 taz_s_organize_layout_bound_data))
            )
            (taz_s_organize_layout_check_bound_point
              (cdr (assoc 12 taz_s_organize_layout_bound_data))
            )
            (taz_s_organize_layout_check_bound_point
              (cdr (assoc 13 taz_s_organize_layout_bound_data))
            )
          )
        )
      )
    )

    (setq taz_s_organize_layout_bound_tmp
      (cdr taz_s_organize_layout_bound_tmp)
    )
  )

  (if
    (and
      taz_s_organize_layout_bound_xmin
      taz_s_organize_layout_bound_ymin
      taz_s_organize_layout_bound_xmax
      taz_s_organize_layout_bound_ymax
    )
    (list
      taz_s_organize_layout_bound_xmin
      taz_s_organize_layout_bound_ymin
      taz_s_organize_layout_bound_xmax
      taz_s_organize_layout_bound_ymax
    )
    nil
  )
)


;; ----------------------------------------------------------------------------
;; NAZWY LAYOUTOW
;; ----------------------------------------------------------------------------

(defun taz_s_organize_layout_get_axis_name
  (taz_s_organize_layout_row_arg)

  (setq taz_s_organize_layout_name_i 2)
  (setq taz_s_organize_layout_axis_name "")

  (while
    (and
      (<=
        taz_s_organize_layout_name_i
        (strlen taz_s_organize_layout_row_arg)
      )
      (/=
        (substr
          taz_s_organize_layout_row_arg
          taz_s_organize_layout_name_i
          1
        )
        "]"
      )
    )

    (setq taz_s_organize_layout_axis_name
      (strcat
        taz_s_organize_layout_axis_name
        (substr
          taz_s_organize_layout_row_arg
          taz_s_organize_layout_name_i
          1
        )
      )
    )

    (setq taz_s_organize_layout_name_i
      (+ taz_s_organize_layout_name_i 1)
    )
  )

  taz_s_organize_layout_axis_name
)


(defun taz_s_organize_layout_make_names ()

  (setq taz_s_organize_layout_names '())

  (setq taz_s_organize_layout_names_tmp
    taz_s_organize_x_data
  )

  (while taz_s_organize_layout_names_tmp

    (setq taz_s_organize_layout_axis_name
      (taz_s_organize_layout_get_axis_name
        (car taz_s_organize_layout_names_tmp)
      )
    )

    (setq taz_s_organize_layout_names
      (append
        taz_s_organize_layout_names
        (list
          (strcat
            taz_s_organize_layout_axis_name
            "-"
            taz_s_organize_layout_axis_name
          )
        )
      )
    )

    (setq taz_s_organize_layout_names_tmp
      (cdr taz_s_organize_layout_names_tmp)
    )
  )

  (setq taz_s_organize_layout_names_tmp
    taz_s_organize_y_data
  )

  (while taz_s_organize_layout_names_tmp

    (setq taz_s_organize_layout_axis_name
      (taz_s_organize_layout_get_axis_name
        (car taz_s_organize_layout_names_tmp)
      )
    )

    (setq taz_s_organize_layout_names
      (append
        taz_s_organize_layout_names
        (list
          (strcat
            taz_s_organize_layout_axis_name
            "-"
            taz_s_organize_layout_axis_name
          )
        )
      )
    )

    (setq taz_s_organize_layout_names_tmp
      (cdr taz_s_organize_layout_names_tmp)
    )
  )

  (setq taz_s_organize_layout_old_dimzin (getvar "DIMZIN"))
  (setvar "DIMZIN" 0)

  (setq taz_s_organize_layout_names_tmp
    taz_s_organize_z_data
  )

  (while taz_s_organize_layout_names_tmp

    (setq taz_s_organize_layout_level_mm
      (taz_s_organize_get_dist
        (car taz_s_organize_layout_names_tmp)
      )
    )

    (setq taz_s_organize_layout_names
      (append
        taz_s_organize_layout_names
        (list
          (rtos
            (/ taz_s_organize_layout_level_mm 1000.0)
            2
            3
          )
        )
      )
    )

    (setq taz_s_organize_layout_names_tmp
      (cdr taz_s_organize_layout_names_tmp)
    )
  )

  (setvar "DIMZIN" taz_s_organize_layout_old_dimzin)

  (setq taz_s_organize_layout_names
    (append
      taz_s_organize_layout_names
      (list "3D")
    )
  )

  taz_s_organize_layout_names
)


;; ----------------------------------------------------------------------------
;; LAYOUT Z ACAD_LAYOUT
;; ----------------------------------------------------------------------------

(defun taz_s_organize_layout_get_layout_entity
  (taz_s_organize_layout_name_arg)

  (setq taz_s_organize_layout_entity nil)

  (setq taz_s_organize_layout_dictionary_data
    (dictsearch
      (namedobjdict)
      "ACAD_LAYOUT"
    )
  )

  (if taz_s_organize_layout_dictionary_data
    (progn

      (setq taz_s_organize_layout_dictionary_ent
        (cdr
          (assoc
            -1
            taz_s_organize_layout_dictionary_data
          )
        )
      )

      (if taz_s_organize_layout_dictionary_ent
        (progn

          (setq taz_s_organize_layout_record_data
            (dictsearch
              taz_s_organize_layout_dictionary_ent
              taz_s_organize_layout_name_arg
            )
          )

          (if taz_s_organize_layout_record_data
            (setq taz_s_organize_layout_entity
              (cdr
                (assoc
                  -1
                  taz_s_organize_layout_record_data
                )
              )
            )
          )
        )
      )
    )
  )

  taz_s_organize_layout_entity
)


(defun taz_s_organize_layout_set_dxf
  (
    taz_s_organize_layout_code_arg
    taz_s_organize_layout_value_arg
    taz_s_organize_layout_data_arg
  )

  (if
    (assoc
      taz_s_organize_layout_code_arg
      taz_s_organize_layout_data_arg
    )
    (subst
      (cons
        taz_s_organize_layout_code_arg
        taz_s_organize_layout_value_arg
      )
      (assoc
        taz_s_organize_layout_code_arg
        taz_s_organize_layout_data_arg
      )
      taz_s_organize_layout_data_arg
    )
    taz_s_organize_layout_data_arg
  )
)


;; ----------------------------------------------------------------------------
;; STANDARDOWE A0-A4 W MM, ORIENTACJA POZIOMA
;; ----------------------------------------------------------------------------

(defun taz_s_organize_layout_get_standard_paper_size
  (taz_s_organize_layout_format_arg)

  (setq taz_s_organize_layout_standard_paper nil)

  (if (= taz_s_organize_layout_format_arg "A0")
    (setq taz_s_organize_layout_standard_paper
      (list 1189.0 841.0)
    )
  )
  (if (= taz_s_organize_layout_format_arg "A1")
    (setq taz_s_organize_layout_standard_paper
      (list 841.0 594.0)
    )
  )
  (if (= taz_s_organize_layout_format_arg "A2")
    (setq taz_s_organize_layout_standard_paper
      (list 594.0 420.0)
    )
  )
  (if (= taz_s_organize_layout_format_arg "A3")
    (setq taz_s_organize_layout_standard_paper
      (list 420.0 297.0)
    )
  )
  (if (= taz_s_organize_layout_format_arg "A4")
    (setq taz_s_organize_layout_standard_paper
      (list 297.0 210.0)
    )
  )

  taz_s_organize_layout_standard_paper
)


;; ----------------------------------------------------------------------------
;; STANDARDOWE A0-A4 - NAZWA PAPIERU ISO FULL BLEED
;; ----------------------------------------------------------------------------

(defun taz_s_organize_layout_get_full_bleed_paper_name
  (taz_s_organize_layout_format_arg)

  (setq taz_s_organize_layout_full_bleed_paper_name nil)

  (if (= taz_s_organize_layout_format_arg "A0")
    (setq taz_s_organize_layout_full_bleed_paper_name
      "ISO_full_bleed_A0_(1189.00_x_841.00_MM)"
    )
  )
  (if (= taz_s_organize_layout_format_arg "A1")
    (setq taz_s_organize_layout_full_bleed_paper_name
      "ISO_full_bleed_A1_(841.00_x_594.00_MM)"
    )
  )
  (if (= taz_s_organize_layout_format_arg "A2")
    (setq taz_s_organize_layout_full_bleed_paper_name
      "ISO_full_bleed_A2_(594.00_x_420.00_MM)"
    )
  )
  (if (= taz_s_organize_layout_format_arg "A3")
    (setq taz_s_organize_layout_full_bleed_paper_name
      "ISO_full_bleed_A3_(420.00_x_297.00_MM)"
    )
  )
  (if (= taz_s_organize_layout_format_arg "A4")
    (setq taz_s_organize_layout_full_bleed_paper_name
      "ISO_full_bleed_A4_(297.00_x_210.00_MM)"
    )
  )

  taz_s_organize_layout_full_bleed_paper_name
)


;; ----------------------------------------------------------------------------
;; PLOTER + ROZMIAR PAPIERU
;; ----------------------------------------------------------------------------

(defun taz_s_organize_layout_set_paper
  (
    taz_s_organize_layout_name_arg
    taz_s_organize_layout_format_arg
    taz_s_organize_layout_frame_paper_w_arg
    taz_s_organize_layout_frame_paper_h_arg
  )

  ;; Na Linuxie FINDFILE moze nie znalezc PC3 mimo ze GstarCAD
  ;; pokazuje go na liscie ploterow. Dlatego najpierw probujemy
  ;; wpisac nazwe wbudowanego plotera bezposrednio do layoutu.
  (setq taz_s_organize_layout_plotter_name
    "DWG To PDF.pc3"
  )

  ;; Zwykle A0-A4 zawsze z dokladnych wymiarow ISO.
  ;; Format wydluzony bierze wymiar rzeczywistej ramki.
  (setq taz_s_organize_layout_standard_paper
    (taz_s_organize_layout_get_standard_paper_size
      taz_s_organize_layout_format_arg
    )
  )

  (setq taz_s_organize_layout_full_bleed_paper_name
    (taz_s_organize_layout_get_full_bleed_paper_name
      taz_s_organize_layout_format_arg
    )
  )

  (if taz_s_organize_layout_standard_paper
    (progn
      (setq taz_s_organize_layout_paper_w
        (car taz_s_organize_layout_standard_paper)
      )
      (setq taz_s_organize_layout_paper_h
        (cadr taz_s_organize_layout_standard_paper)
      )
    )
    (progn
      (setq taz_s_organize_layout_paper_w
        taz_s_organize_layout_frame_paper_w_arg
      )
      (setq taz_s_organize_layout_paper_h
        taz_s_organize_layout_frame_paper_h_arg
      )
    )
  )

  (setq taz_s_organize_layout_layout_ent
    (taz_s_organize_layout_get_layout_entity
      taz_s_organize_layout_name_arg
    )
  )

  (if taz_s_organize_layout_layout_ent
    (progn
      (setq taz_s_organize_layout_layout_data
        (entget taz_s_organize_layout_layout_ent)
      )

      (setq taz_s_organize_layout_layout_data
        (taz_s_organize_layout_set_dxf
          2
          taz_s_organize_layout_plotter_name
          taz_s_organize_layout_layout_data
        )
      )

      ;; Dla standardowych A0-A4 wybieramy wariant ISO FULL BLEED.
      (if taz_s_organize_layout_full_bleed_paper_name
        (setq taz_s_organize_layout_layout_data
          (taz_s_organize_layout_set_dxf
            4
            taz_s_organize_layout_full_bleed_paper_name
            taz_s_organize_layout_layout_data
          )
        )
      )

      ;; Marginesy papieru = 0.
      (setq taz_s_organize_layout_layout_data
        (taz_s_organize_layout_set_dxf
          40
          0.0
          taz_s_organize_layout_layout_data
        )
      )
      (setq taz_s_organize_layout_layout_data
        (taz_s_organize_layout_set_dxf
          41
          0.0
          taz_s_organize_layout_layout_data
        )
      )
      (setq taz_s_organize_layout_layout_data
        (taz_s_organize_layout_set_dxf
          42
          0.0
          taz_s_organize_layout_layout_data
        )
      )
      (setq taz_s_organize_layout_layout_data
        (taz_s_organize_layout_set_dxf
          43
          0.0
          taz_s_organize_layout_layout_data
        )
      )

      (setq taz_s_organize_layout_layout_data
        (taz_s_organize_layout_set_dxf
          44
          taz_s_organize_layout_paper_w
          taz_s_organize_layout_layout_data
        )
      )
      (setq taz_s_organize_layout_layout_data
        (taz_s_organize_layout_set_dxf
          45
          taz_s_organize_layout_paper_h
          taz_s_organize_layout_layout_data
        )
      )
      (setq taz_s_organize_layout_layout_data
        (taz_s_organize_layout_set_dxf
          46
          0.0
          taz_s_organize_layout_layout_data
        )
      )
      (setq taz_s_organize_layout_layout_data
        (taz_s_organize_layout_set_dxf
          47
          0.0
          taz_s_organize_layout_layout_data
        )
      )
      (setq taz_s_organize_layout_layout_data
        (taz_s_organize_layout_set_dxf
          72
          1
          taz_s_organize_layout_layout_data
        )
      )
      (setq taz_s_organize_layout_layout_data
        (taz_s_organize_layout_set_dxf
          73
          0
          taz_s_organize_layout_layout_data
        )
      )

      (entmod taz_s_organize_layout_layout_data)

      ;; Kontrola tego co faktycznie zostalo zapisane przez CAD.
      (setq taz_s_organize_layout_layout_data
        (entget taz_s_organize_layout_layout_ent)
      )
      (setq taz_s_organize_layout_plotter_check
        (cdr (assoc 2 taz_s_organize_layout_layout_data))
      )

      (if
        (or
          (= taz_s_organize_layout_plotter_check nil)
          (/=
            (strcase taz_s_organize_layout_plotter_check)
            (strcase "DWG To PDF.pc3")
          )
        )
        (progn
          (setq taz_s_organize_layout_plotter_name "None")
          (setq taz_s_organize_layout_layout_data
            (taz_s_organize_layout_set_dxf
              2
              "None"
              taz_s_organize_layout_layout_data
            )
          )
          (entmod taz_s_organize_layout_layout_data)
        )
      )
    )
  )

  (list
    taz_s_organize_layout_paper_w
    taz_s_organize_layout_paper_h
    taz_s_organize_layout_plotter_name
  )
)


;; ----------------------------------------------------------------------------
;; PIERWSZA RZUTNIA UZYTKOWA NA DANYM LAYOUTCIE
;; ----------------------------------------------------------------------------

(defun taz_s_organize_layout_get_viewport
  (taz_s_organize_layout_name_arg)

  (setq taz_s_organize_layout_viewport_ent nil)

  (setq taz_s_organize_layout_viewport_ss
    (ssget
      "X"
      (list
        (cons 0 "VIEWPORT")
        (cons 410 taz_s_organize_layout_name_arg)
      )
    )
  )

  (if taz_s_organize_layout_viewport_ss
    (progn

      (setq taz_s_organize_layout_viewport_i 0)

      (while
        (and
          (<
            taz_s_organize_layout_viewport_i
            (sslength taz_s_organize_layout_viewport_ss)
          )
          (= taz_s_organize_layout_viewport_ent nil)
        )

        (setq taz_s_organize_layout_viewport_test_ent
          (ssname
            taz_s_organize_layout_viewport_ss
            taz_s_organize_layout_viewport_i
          )
        )
        (setq taz_s_organize_layout_viewport_test_data
          (entget taz_s_organize_layout_viewport_test_ent)
        )
        (setq taz_s_organize_layout_viewport_number
          (cdr
            (assoc
              69
              taz_s_organize_layout_viewport_test_data
            )
          )
        )

        (if
          (and
            taz_s_organize_layout_viewport_number
            (> taz_s_organize_layout_viewport_number 1)
          )
          (setq taz_s_organize_layout_viewport_ent
            taz_s_organize_layout_viewport_test_ent
          )
        )

        (setq taz_s_organize_layout_viewport_i
          (+ taz_s_organize_layout_viewport_i 1)
        )
      )
    )
  )

  taz_s_organize_layout_viewport_ent
)


;; ----------------------------------------------------------------------------
;; DOSTOSOWANIE JEDNEGO LAYOUTU
;; ----------------------------------------------------------------------------

(defun taz_s_organize_layout_configure_one
  (
    taz_s_organize_layout_name_arg
    taz_s_organize_layout_bounds_arg
    taz_s_organize_layout_insert_point_arg
    taz_s_organize_layout_scale_arg
    taz_s_organize_layout_format_arg
  )

  (setq taz_s_organize_layout_configured nil)

  (if
    (and
      taz_s_organize_layout_name_arg
      taz_s_organize_layout_bounds_arg
      taz_s_organize_layout_insert_point_arg
      taz_s_organize_layout_scale_arg
      (> taz_s_organize_layout_scale_arg 0.0)
      (member taz_s_organize_layout_name_arg (layoutlist))
    )
    (progn

      ;; Bounding box ramki jest uzywany tylko do formatu wydluzonego.
      (setq taz_s_organize_layout_xmin
        (nth 0 taz_s_organize_layout_bounds_arg)
      )
      (setq taz_s_organize_layout_ymin
        (nth 1 taz_s_organize_layout_bounds_arg)
      )
      (setq taz_s_organize_layout_xmax
        (nth 2 taz_s_organize_layout_bounds_arg)
      )
      (setq taz_s_organize_layout_ymax
        (nth 3 taz_s_organize_layout_bounds_arg)
      )

      (setq taz_s_organize_layout_model_w
        (- taz_s_organize_layout_xmax taz_s_organize_layout_xmin)
      )
      (setq taz_s_organize_layout_model_h
        (- taz_s_organize_layout_ymax taz_s_organize_layout_ymin)
      )

      (setq taz_s_organize_layout_frame_paper_w
        (/ taz_s_organize_layout_model_w taz_s_organize_layout_scale_arg)
      )
      (setq taz_s_organize_layout_frame_paper_h
        (/ taz_s_organize_layout_model_h taz_s_organize_layout_scale_arg)
      )

      (setq taz_s_organize_layout_paper_result
        (taz_s_organize_layout_set_paper
          taz_s_organize_layout_name_arg
          taz_s_organize_layout_format_arg
          taz_s_organize_layout_frame_paper_w
          taz_s_organize_layout_frame_paper_h
        )
      )

      (setq taz_s_organize_layout_paper_w
        (car taz_s_organize_layout_paper_result)
      )
      (setq taz_s_organize_layout_paper_h
        (cadr taz_s_organize_layout_paper_result)
      )
      (setq taz_s_organize_layout_plotter_name
        (caddr taz_s_organize_layout_paper_result)
      )

      (setvar "CTAB" taz_s_organize_layout_name_arg)
      (command "_.PSPACE")

      ;; Na tym layoucie docelowo ma zostac tylko jedna nowa rzutnia.
      ;; Warstwy mogly zostac ponownie zablokowane wczesniej przez organizer,
      ;; dlatego na czas czyszczenia odblokowujemy wszystkie warstwy.
      (command "_.-LAYER" "_UNLOCK" "*" "")

      ;; Usuwamy wszystko z paperspace przed utworzeniem docelowej rzutni.
      (command "_.ERASE" "_ALL" "")

      ;; Rzutnia ma byc zawsze na warstwie 0.
      (if (tblsearch "LAYER" "0")
        (setvar "CLAYER" "0")
      )

      ;; Przywracamy blokady warstw zapamietane przy starcie organizera.
      (taz_s_organize_restore_locked_layers)

      ;; Pobieramy rzeczywiste marginesy niedrukowalne aktualnego layoutu.
      ;; W paperspace punkt 0,0 odpowiada poczatkowi obszaru drukowalnego,
      ;; dlatego fizyczna kartka zaczyna sie o lewy/dolny margines wczesniej.
      (setq taz_s_organize_layout_layout_ent
        (taz_s_organize_layout_get_layout_entity
          taz_s_organize_layout_name_arg
        )
      )

      (setq taz_s_organize_layout_margin_left 0.0)
      (setq taz_s_organize_layout_margin_bottom 0.0)

      (if taz_s_organize_layout_layout_ent
        (progn
          (setq taz_s_organize_layout_layout_data
            (entget taz_s_organize_layout_layout_ent)
          )

          (if (assoc 40 taz_s_organize_layout_layout_data)
            (setq taz_s_organize_layout_margin_left
              (cdr (assoc 40 taz_s_organize_layout_layout_data))
            )
          )

          (if (assoc 41 taz_s_organize_layout_layout_data)
            (setq taz_s_organize_layout_margin_bottom
              (cdr (assoc 41 taz_s_organize_layout_layout_data))
            )
          )
        )
      )

      (setq taz_s_organize_layout_viewport_p1
        (list
          (- 0.0 taz_s_organize_layout_margin_left)
          (- 0.0 taz_s_organize_layout_margin_bottom)
        )
      )

      (setq taz_s_organize_layout_viewport_p2
        (list
          (-
            taz_s_organize_layout_paper_w
            taz_s_organize_layout_margin_left
          )
          (-
            taz_s_organize_layout_paper_h
            taz_s_organize_layout_margin_bottom
          )
        )
      )

      ;; Nowa rzutnia pokrywa dokladnie fizyczna kartke.
      (command
        "_.MVIEW"
        "_NON"
        taz_s_organize_layout_viewport_p1
        "_NON"
        taz_s_organize_layout_viewport_p2
      )

      (setq taz_s_organize_layout_viewport_ent
        (taz_s_organize_layout_get_viewport
          taz_s_organize_layout_name_arg
        )
      )

      (if taz_s_organize_layout_viewport_ent
        (progn
          (command
            "_.CHPROP"
            taz_s_organize_layout_viewport_ent
            ""
            "_LA"
            "0"
            ""
          )

          ;; Ustawienie widoku zwyklymi komendami GstarCAD.
          (command "_.MSPACE")
          (command "_.UCS" "_W")
          (command "_.PLAN" "_W")

          ;; Punkt wstawienia ramki jest srodkiem danego przypadku.
          ;; Nie uzywamy juz srodka bounding boxa calej geometrii ramki.
          (setq taz_s_organize_layout_model_center
            (list
              (car taz_s_organize_layout_insert_point_arg)
              (cadr taz_s_organize_layout_insert_point_arg)
              0.0
            )
          )

          ;; Dokladna skala 1:S:
          ;; wysokosc widoku modelu = wysokosc papieru * S.
          (setq taz_s_organize_layout_view_height
            (*
              taz_s_organize_layout_paper_h
              taz_s_organize_layout_scale_arg
            )
          )

          (command
            "_.ZOOM"
            "_C"
            "_NON"
            taz_s_organize_layout_model_center
            taz_s_organize_layout_view_height
          )

          (command "_.PSPACE")

          ;; Blokada gotowej rzutni = bit 16384.
          (setq taz_s_organize_layout_viewport_data
            (entget taz_s_organize_layout_viewport_ent)
          )
          (setq taz_s_organize_layout_viewport_flags
            (cdr (assoc 90 taz_s_organize_layout_viewport_data))
          )
          (if (= taz_s_organize_layout_viewport_flags nil)
            (setq taz_s_organize_layout_viewport_flags 0)
          )
          (setq taz_s_organize_layout_viewport_data
            (taz_s_organize_layout_set_dxf
              90
              (logior taz_s_organize_layout_viewport_flags 16384)
              taz_s_organize_layout_viewport_data
            )
          )
          (entmod taz_s_organize_layout_viewport_data)
          (entupd taz_s_organize_layout_viewport_ent)

          (setq taz_s_organize_layout_configured T)

          (princ
            (strcat
              "\nLayout "
              taz_s_organize_layout_name_arg
              " - plotter: "
              taz_s_organize_layout_plotter_name
              ", skala 1:"
              (rtos taz_s_organize_layout_scale_arg 2 3)
              "."
            )
          )
        )
      )
    )
  )

  taz_s_organize_layout_configured
)


(defun taz_s_organize_finalize_layouts ()

  (setq taz_s_organize_layout_old_ctab
    (getvar "CTAB")
  )

  (setq taz_s_organize_layout_old_clayer
    (getvar "CLAYER")
  )

  (setq taz_s_organize_layout_names
    (taz_s_organize_layout_make_names)
  )

  (setq taz_s_organize_layout_index 0)

  (while
    (and
      (<
        taz_s_organize_layout_index
        (length taz_s_organize_layout_names)
      )
      (<
        taz_s_organize_layout_index
        (length taz_s_organize_frame_bounds)
      )
      (<
        taz_s_organize_layout_index
        (length taz_s_organize_frame_insert_points)
      )
      (<
        taz_s_organize_layout_index
        (length taz_s_organize_frame_scale_factors)
      )
      (<
        taz_s_organize_layout_index
        (length taz_s_organize_frame_formats)
      )
    )

    (taz_s_organize_layout_configure_one
      (nth
        taz_s_organize_layout_index
        taz_s_organize_layout_names
      )
      (nth
        taz_s_organize_layout_index
        taz_s_organize_frame_bounds
      )
      (nth
        taz_s_organize_layout_index
        taz_s_organize_frame_insert_points
      )
      (nth
        taz_s_organize_layout_index
        taz_s_organize_frame_scale_factors
      )
      (nth
        taz_s_organize_layout_index
        taz_s_organize_frame_formats
      )
    )

    (setq taz_s_organize_layout_index
      (+ taz_s_organize_layout_index 1)
    )
  )

  (if
    (member
      taz_s_organize_layout_old_ctab
      (append
        (layoutlist)
        (list "Model")
      )
    )
    (setvar
      "CTAB"
      taz_s_organize_layout_old_ctab
    )
  )

  (if
    (and
      taz_s_organize_layout_old_clayer
      (tblsearch "LAYER" taz_s_organize_layout_old_clayer)
    )
    (setvar
      "CLAYER"
      taz_s_organize_layout_old_clayer
    )
  )

  (command "_.UCS" "_W")
  (command "_.REGENALL")
)


;; ============================================================================
;; GLOWNA KOMENDA
;; ============================================================================

(defun taz_s_create_drawings_execution_design_organize ()

  (setq taz_s_organize_old_cmdecho (getvar "CMDECHO"))
  (setq taz_s_organize_old_clayer (getvar "CLAYER"))

  (setvar "CMDECHO" 0)

  ;; Na sztywno zgodnie z pierwszym zalozeniem
  (setq taz_s_organize_spacing 100000.0)
  (setq taz_s_organize_case_half_range 49000.0)
  (setq taz_s_organize_z_range_margin 5000.0)
  (setq taz_s_organize_align_size 1000.0)

  (taz_s_organize_prepare_data)

  (if taz_s_organize_can_run
    (progn

      (command "_.UCS" "_W")

      (taz_s_organize_unlock_layers)

      ;; ----------------------------------------------------------------------
      ;; WARTOSCI LICZBOWE OSI
      ;; ----------------------------------------------------------------------

      (setq taz_s_organize_x_values
        (taz_s_organize_make_values taz_s_organize_x_data)
      )

      (setq taz_s_organize_y_values
        (taz_s_organize_make_values taz_s_organize_y_data)
      )

      (setq taz_s_organize_z_values
        (taz_s_organize_make_values taz_s_organize_z_data)
      )

      ;; ----------------------------------------------------------------------
      ;; ZAKRES Z DLA PRZYPADKOW X ORAZ Y
      ;;
      ;; Bierzemy najnizsza i najwyzsza os Z.
      ;; Zakres zawsze obejmuje tez wzgledne Z = 0, bo na tym poziomie
      ;; jest kotwiczona tabela.
      ;; Na koncu dodajemy margines 5000 z obu stron.
      ;; ----------------------------------------------------------------------

      (setq taz_s_organize_z_relative_min 0.0)
      (setq taz_s_organize_z_relative_max 0.0)

      (if taz_s_organize_z_values
        (progn

          (setq taz_s_organize_z_relative_min
            (taz_s_organize_min taz_s_organize_z_values)
          )

          (setq taz_s_organize_z_relative_max
            (taz_s_organize_max taz_s_organize_z_values)
          )

          (if (> taz_s_organize_z_relative_min 0.0)
            (setq taz_s_organize_z_relative_min 0.0)
          )

          (if (< taz_s_organize_z_relative_max 0.0)
            (setq taz_s_organize_z_relative_max 0.0)
          )
        )
      )

      (setq taz_s_organize_z_relative_min
        (- taz_s_organize_z_relative_min taz_s_organize_z_range_margin)
      )

      (setq taz_s_organize_z_relative_max
        (+ taz_s_organize_z_relative_max taz_s_organize_z_range_margin)
      )

      ;; ----------------------------------------------------------------------
      ;; SRODEK KONSTRUKCJI W X I Y
      ;;
      ;; Tak samo jak w skrypcie tworzacym widoki:
      ;; - taz_s_axis_data_y daje polozenia w globalnym X,
      ;; - taz_s_axis_data_x daje polozenia w globalnym Y.
      ;; ----------------------------------------------------------------------

      (setq taz_s_organize_x_center 0.0)
      (setq taz_s_organize_y_center 0.0)

      (if taz_s_organize_y_values
        (progn
          (setq taz_s_organize_x_min
            (taz_s_organize_min taz_s_organize_y_values)
          )
          (setq taz_s_organize_x_max
            (taz_s_organize_max taz_s_organize_y_values)
          )
          (setq taz_s_organize_x_center
            (/ (+ taz_s_organize_x_min taz_s_organize_x_max) 2.0)
          )
        )
      )

      (if taz_s_organize_x_values
        (progn
          (setq taz_s_organize_y_min
            (taz_s_organize_min taz_s_organize_x_values)
          )
          (setq taz_s_organize_y_max
            (taz_s_organize_max taz_s_organize_x_values)
          )
          (setq taz_s_organize_y_center
            (/ (+ taz_s_organize_y_min taz_s_organize_y_max) 2.0)
          )
        )
      )

      ;; ----------------------------------------------------------------------
      ;; SRODEK KONSTRUKCJI W Z
      ;; Uzywany tylko przez dodatkowy przypadek IZO.
      ;; ----------------------------------------------------------------------

      (setq taz_s_organize_z_center 0.0)

      (if taz_s_organize_z_values
        (progn
          (setq taz_s_organize_z_min
            (taz_s_organize_min taz_s_organize_z_values)
          )
          (setq taz_s_organize_z_max
            (taz_s_organize_max taz_s_organize_z_values)
          )
          (setq taz_s_organize_z_center
            (/ (+ taz_s_organize_z_min taz_s_organize_z_max) 2.0)
          )
        )
      )

      ;; ----------------------------------------------------------------------
      ;; NUMER PRZYPADKU
      ;; Odpowiada taz_s_copy_nr ze skryptu tworzacego widoki.
      ;; ----------------------------------------------------------------------

      (setq taz_s_organize_case_nr 1)
      (setq taz_s_organize_total_moved 0)

      ;; Nowe polozenia punktow kotwiczenia tabel po ALIGN.
      ;; Po jednym wpisie dla kazdego przypadku.
      (setq taz_s_organize_table_anchor_points_after '())

      ;; ======================================================================
      ;; PRZYPADKI X
      ;;
      ;; Oryginalna plaszczyzna: X-Z, Y = wartosc osi.
      ;; Po ALIGN:
      ;; - globalny X zostaje poziomo,
      ;; - globalny Z idzie do gory po nowym Y.
      ;; ======================================================================

      (setq taz_s_organize_tmp taz_s_organize_x_data)

      (while taz_s_organize_tmp

        (setq taz_s_organize_row (car taz_s_organize_tmp))
        (setq taz_s_organize_section_value
          (taz_s_organize_get_dist taz_s_organize_row)
        )

        (setq taz_s_organize_case_z
          (* taz_s_organize_case_nr taz_s_organize_spacing)
        )

        (setq taz_s_organize_destination_x
          (* (- taz_s_organize_case_nr 1) taz_s_organize_spacing)
        )

        (setq taz_s_organize_case_range_min
          (+ taz_s_organize_case_z taz_s_organize_z_relative_min)
        )

        (setq taz_s_organize_case_range_max
          (+ taz_s_organize_case_z taz_s_organize_z_relative_max)
        )

        (setq taz_s_organize_case_ss
          (taz_s_organize_collect_case_z_range
            taz_s_organize_case_range_min
            taz_s_organize_case_range_max
          )
        )

        (setq taz_s_organize_case_ss
          (taz_s_organize_prepare_table_case
            taz_s_organize_case_nr
            taz_s_organize_case_ss
          )
        )

        (if (> (sslength taz_s_organize_case_ss) 0)
          (progn

            ;; Punkt bazowy przypadku
            (setq taz_s_organize_source_1
              (list
                taz_s_organize_x_center
                taz_s_organize_section_value
                taz_s_organize_case_z
              )
            )

            ;; Kierunek poziomy po uporzadkowaniu = globalny X
            (setq taz_s_organize_source_2
              (list
                (+ taz_s_organize_x_center taz_s_organize_align_size)
                taz_s_organize_section_value
                taz_s_organize_case_z
              )
            )

            ;; Kierunek pionowy po uporzadkowaniu = globalny Z
            (setq taz_s_organize_source_3
              (list
                taz_s_organize_x_center
                taz_s_organize_section_value
                (+ taz_s_organize_case_z taz_s_organize_align_size)
              )
            )

            (setq taz_s_organize_destination_1
              (list taz_s_organize_destination_x 0.0 0.0)
            )

            (setq taz_s_organize_destination_2
              (list
                (+ taz_s_organize_destination_x taz_s_organize_align_size)
                0.0
                0.0
              )
            )

            (setq taz_s_organize_destination_3
              (list
                taz_s_organize_destination_x
                taz_s_organize_align_size
                0.0
              )
            )

            ;; --------------------------------------------------------------
            ;; _NON przed kazdym punktem wylacza OSNAP tylko dla tego punktu.
            ;; Jest to szczegolnie wazne dla pierwszego przypadku, ktory trafia
            ;; w okolice 0,0,0 i moze znajdowac sie blisko oryginalnego modelu.
            ;; --------------------------------------------------------------

            (command
              "_.ALIGN"
              taz_s_organize_case_ss
              ""
              "_NON"
              taz_s_organize_source_1
              "_NON"
              taz_s_organize_destination_1
              "_NON"
              taz_s_organize_source_2
              "_NON"
              taz_s_organize_destination_2
              "_NON"
              taz_s_organize_source_3
              "_NON"
              taz_s_organize_destination_3
            )

            (setq taz_s_organize_case_object_count
              (sslength taz_s_organize_case_ss)
            )

            ;; Tymczasowego POINT tabeli nie liczymy jako przeniesionego obiektu.
            (if taz_s_organize_current_table_marker
              (setq taz_s_organize_case_object_count
                (- taz_s_organize_case_object_count 1)
              )
            )

            (setq taz_s_organize_total_moved
              (+ taz_s_organize_total_moved taz_s_organize_case_object_count)
            )

            (princ
              (strcat
                "\nX - przypadek "
                (itoa taz_s_organize_case_nr)
                " - przeniesiono obiektow: "
                (itoa taz_s_organize_case_object_count)
              )
            )
          )
          (princ
            (strcat
              "\nX - przypadek "
              (itoa taz_s_organize_case_nr)
              " - nie znaleziono obiektow."
            )
          )
        )

        (taz_s_organize_finish_table_case taz_s_organize_case_ss)

        (setq taz_s_organize_case_nr (+ taz_s_organize_case_nr 1))
        (setq taz_s_organize_tmp (cdr taz_s_organize_tmp))
      )

      ;; ======================================================================
      ;; PRZYPADKI Y
      ;;
      ;; Oryginalna plaszczyzna: Y-Z, X = wartosc osi.
      ;; Po ALIGN:
      ;; - globalny Y idzie w prawo po nowym X,
      ;; - globalny Z idzie do gory po nowym Y.
      ;; ======================================================================

      (setq taz_s_organize_tmp taz_s_organize_y_data)

      (while taz_s_organize_tmp

        (setq taz_s_organize_row (car taz_s_organize_tmp))
        (setq taz_s_organize_section_value
          (taz_s_organize_get_dist taz_s_organize_row)
        )

        (setq taz_s_organize_case_z
          (* taz_s_organize_case_nr taz_s_organize_spacing)
        )

        (setq taz_s_organize_destination_x
          (* (- taz_s_organize_case_nr 1) taz_s_organize_spacing)
        )

        (setq taz_s_organize_case_range_min
          (+ taz_s_organize_case_z taz_s_organize_z_relative_min)
        )

        (setq taz_s_organize_case_range_max
          (+ taz_s_organize_case_z taz_s_organize_z_relative_max)
        )

        (setq taz_s_organize_case_ss
          (taz_s_organize_collect_case_z_range
            taz_s_organize_case_range_min
            taz_s_organize_case_range_max
          )
        )

        (setq taz_s_organize_case_ss
          (taz_s_organize_prepare_table_case
            taz_s_organize_case_nr
            taz_s_organize_case_ss
          )
        )

        (if (> (sslength taz_s_organize_case_ss) 0)
          (progn

            ;; Punkt bazowy przypadku
            (setq taz_s_organize_source_1
              (list
                taz_s_organize_section_value
                taz_s_organize_y_center
                taz_s_organize_case_z
              )
            )

            ;; Kierunek poziomy po uporzadkowaniu = globalny Y
            (setq taz_s_organize_source_2
              (list
                taz_s_organize_section_value
                (+ taz_s_organize_y_center taz_s_organize_align_size)
                taz_s_organize_case_z
              )
            )

            ;; Kierunek pionowy po uporzadkowaniu = globalny Z
            (setq taz_s_organize_source_3
              (list
                taz_s_organize_section_value
                taz_s_organize_y_center
                (+ taz_s_organize_case_z taz_s_organize_align_size)
              )
            )

            (setq taz_s_organize_destination_1
              (list taz_s_organize_destination_x 0.0 0.0)
            )

            (setq taz_s_organize_destination_2
              (list
                (+ taz_s_organize_destination_x taz_s_organize_align_size)
                0.0
                0.0
              )
            )

            (setq taz_s_organize_destination_3
              (list
                taz_s_organize_destination_x
                taz_s_organize_align_size
                0.0
              )
            )

            ;; --------------------------------------------------------------
            ;; _NON przed kazdym punktem wylacza OSNAP tylko dla tego punktu.
            ;; Jest to szczegolnie wazne dla pierwszego przypadku, ktory trafia
            ;; w okolice 0,0,0 i moze znajdowac sie blisko oryginalnego modelu.
            ;; --------------------------------------------------------------

            (command
              "_.ALIGN"
              taz_s_organize_case_ss
              ""
              "_NON"
              taz_s_organize_source_1
              "_NON"
              taz_s_organize_destination_1
              "_NON"
              taz_s_organize_source_2
              "_NON"
              taz_s_organize_destination_2
              "_NON"
              taz_s_organize_source_3
              "_NON"
              taz_s_organize_destination_3
            )

            (setq taz_s_organize_case_object_count
              (sslength taz_s_organize_case_ss)
            )

            ;; Tymczasowego POINT tabeli nie liczymy jako przeniesionego obiektu.
            (if taz_s_organize_current_table_marker
              (setq taz_s_organize_case_object_count
                (- taz_s_organize_case_object_count 1)
              )
            )

            (setq taz_s_organize_total_moved
              (+ taz_s_organize_total_moved taz_s_organize_case_object_count)
            )

            (princ
              (strcat
                "\nY - przypadek "
                (itoa taz_s_organize_case_nr)
                " - przeniesiono obiektow: "
                (itoa taz_s_organize_case_object_count)
              )
            )
          )
          (princ
            (strcat
              "\nY - przypadek "
              (itoa taz_s_organize_case_nr)
              " - nie znaleziono obiektow."
            )
          )
        )

        (taz_s_organize_finish_table_case taz_s_organize_case_ss)

        (setq taz_s_organize_case_nr (+ taz_s_organize_case_nr 1))
        (setq taz_s_organize_tmp (cdr taz_s_organize_tmp))
      )

      ;; ======================================================================
      ;; PRZYPADKI Z
      ;;
      ;; Oryginalna plaszczyzna jest juz rownolegla do globalnej XY.
      ;; Po ALIGN:
      ;; - globalny X zostaje poziomo,
      ;; - globalny Y zostaje pionowo.
      ;; ======================================================================

      (setq taz_s_organize_tmp taz_s_organize_z_data)

      (while taz_s_organize_tmp

        (setq taz_s_organize_row (car taz_s_organize_tmp))
        (setq taz_s_organize_section_value
          (taz_s_organize_get_dist taz_s_organize_row)
        )

        (setq taz_s_organize_case_z
          (* taz_s_organize_case_nr taz_s_organize_spacing)
        )

        (setq taz_s_organize_destination_x
          (* (- taz_s_organize_case_nr 1) taz_s_organize_spacing)
        )

        (setq taz_s_organize_case_ss
          (taz_s_organize_collect_case taz_s_organize_case_z)
        )

        (setq taz_s_organize_case_ss
          (taz_s_organize_prepare_table_case
            taz_s_organize_case_nr
            taz_s_organize_case_ss
          )
        )

        (if (> (sslength taz_s_organize_case_ss) 0)
          (progn

            ;; Punkt bazowy przypadku lezy na rzeczywistej plaszczyznie Z
            (setq taz_s_organize_source_1
              (list
                taz_s_organize_x_center
                taz_s_organize_y_center
                (+ taz_s_organize_case_z taz_s_organize_section_value)
              )
            )

            ;; Globalny X
            (setq taz_s_organize_source_2
              (list
                (+ taz_s_organize_x_center taz_s_organize_align_size)
                taz_s_organize_y_center
                (+ taz_s_organize_case_z taz_s_organize_section_value)
              )
            )

            ;; Globalny Y
            (setq taz_s_organize_source_3
              (list
                taz_s_organize_x_center
                (+ taz_s_organize_y_center taz_s_organize_align_size)
                (+ taz_s_organize_case_z taz_s_organize_section_value)
              )
            )

            (setq taz_s_organize_destination_1
              (list taz_s_organize_destination_x 0.0 0.0)
            )

            (setq taz_s_organize_destination_2
              (list
                (+ taz_s_organize_destination_x taz_s_organize_align_size)
                0.0
                0.0
              )
            )

            (setq taz_s_organize_destination_3
              (list
                taz_s_organize_destination_x
                taz_s_organize_align_size
                0.0
              )
            )

            ;; --------------------------------------------------------------
            ;; _NON przed kazdym punktem wylacza OSNAP tylko dla tego punktu.
            ;; Jest to szczegolnie wazne dla pierwszego przypadku, ktory trafia
            ;; w okolice 0,0,0 i moze znajdowac sie blisko oryginalnego modelu.
            ;; --------------------------------------------------------------

            (command
              "_.ALIGN"
              taz_s_organize_case_ss
              ""
              "_NON"
              taz_s_organize_source_1
              "_NON"
              taz_s_organize_destination_1
              "_NON"
              taz_s_organize_source_2
              "_NON"
              taz_s_organize_destination_2
              "_NON"
              taz_s_organize_source_3
              "_NON"
              taz_s_organize_destination_3
            )

            (setq taz_s_organize_case_object_count
              (sslength taz_s_organize_case_ss)
            )

            ;; Tymczasowego POINT tabeli nie liczymy jako przeniesionego obiektu.
            (if taz_s_organize_current_table_marker
              (setq taz_s_organize_case_object_count
                (- taz_s_organize_case_object_count 1)
              )
            )

            (setq taz_s_organize_total_moved
              (+ taz_s_organize_total_moved taz_s_organize_case_object_count)
            )

            (princ
              (strcat
                "\nZ - przypadek "
                (itoa taz_s_organize_case_nr)
                " - przeniesiono obiektow: "
                (itoa taz_s_organize_case_object_count)
              )
            )
          )
          (princ
            (strcat
              "\nZ - przypadek "
              (itoa taz_s_organize_case_nr)
              " - nie znaleziono obiektow."
            )
          )
        )

        (taz_s_organize_finish_table_case taz_s_organize_case_ss)

        (setq taz_s_organize_case_nr (+ taz_s_organize_case_nr 1))
        (setq taz_s_organize_tmp (cdr taz_s_organize_tmp))
      )

      ;; ----------------------------------------------------------------------
      ;; PRZYPADEK IZO - DODANY NA SAMYM KONCU
      ;;
      ;; Dotychczasowe przypadki X/Y/Z powyzej pozostaja bez zmian.
      ;;
      ;; W generatorze IZO znajduje sie na:
      ;;   (liczba X + liczba Y + liczba Z + 1) * 100000
      ;;
      ;; W tym miejscu taz_s_organize_case_nr ma juz dokladnie te wartosc
      ;; numeru przypadku.
      ;;
      ;; Po uporzadkowaniu IZO trafia za ostatni przypadek Z.
      ;; ======================================================================

      (setq taz_s_organize_izo_case_z
        (* taz_s_organize_case_nr taz_s_organize_spacing)
      )

      (setq taz_s_organize_destination_x
        (* (- taz_s_organize_case_nr 1) taz_s_organize_spacing)
      )

      ;; Dokladny poczatek UCS IZO — taki sam jak w generatorze.
      (setq taz_s_organize_izo_ucs_origin
        (list
          taz_s_organize_x_center
          taz_s_organize_y_center
          (+ taz_s_organize_z_center taz_s_organize_izo_case_z)
        )
      )

      ;; Odtworzenie dokładnie tej samej płaszczyzny IZO.
      (command "_.UCS" "_W")
      (command "_.UCS" "_O" taz_s_organize_izo_ucs_origin)
      (command "_.UCS" "_X" 45)
      (command "_.UCS" "_Y" 35.264389683)

      (setq taz_s_organize_izo_normal
        (trans (list 0.0 0.0 1.0) 1 0 T)
      )

      ;; Os X OCS — ta sama, która daje Rotation = 0 etykiet IZO.
      (setq taz_s_organize_izo_xdir
        (trans
          (list 1.0 0.0 0.0)
          taz_s_organize_izo_normal
          0
          T
        )
      )

      ;; Os Y tego samego OCS.
      (setq taz_s_organize_izo_ydir
        (trans
          (list 0.0 1.0 0.0)
          taz_s_organize_izo_normal
          0
          T
        )
      )

      (command "_.UCS" "_W")

      ;; Zbieramy geometrię najwyższego przypadku IZO.
      (setq taz_s_organize_case_ss
        (taz_s_organize_collect_case_z_range
          (- taz_s_organize_izo_case_z taz_s_organize_case_half_range)
          (+ taz_s_organize_izo_case_z taz_s_organize_case_half_range)
        )
      )

      ;; Jawnie dopinamy właściwą tabelę IZO i jej marker.
      ;; Funkcja mapująca pobiera dla tego ostatniego przypadku indeks 0.
      (setq taz_s_organize_case_ss
        (taz_s_organize_prepare_table_case
          taz_s_organize_case_nr
          taz_s_organize_case_ss
        )
      )

      (if (> (sslength taz_s_organize_case_ss) 0)
        (progn

          ;; Punkt bazowy leży dokładnie na płaszczyźnie IZO.
          (setq taz_s_organize_source_1
            taz_s_organize_izo_ucs_origin
          )

          ;; Poziom źródłowy = oś X OCS IZO.
          (setq taz_s_organize_source_2
            (list
              (+ (car taz_s_organize_izo_ucs_origin)
                 (* taz_s_organize_align_size
                    (car taz_s_organize_izo_xdir)))
              (+ (cadr taz_s_organize_izo_ucs_origin)
                 (* taz_s_organize_align_size
                    (cadr taz_s_organize_izo_xdir)))
              (+ (caddr taz_s_organize_izo_ucs_origin)
                 (* taz_s_organize_align_size
                    (caddr taz_s_organize_izo_xdir)))
            )
          )

          ;; Pion źródłowy = oś Y OCS IZO.
          (setq taz_s_organize_source_3
            (list
              (+ (car taz_s_organize_izo_ucs_origin)
                 (* taz_s_organize_align_size
                    (car taz_s_organize_izo_ydir)))
              (+ (cadr taz_s_organize_izo_ucs_origin)
                 (* taz_s_organize_align_size
                    (cadr taz_s_organize_izo_ydir)))
              (+ (caddr taz_s_organize_izo_ucs_origin)
                 (* taz_s_organize_align_size
                    (caddr taz_s_organize_izo_ydir)))
            )
          )

          ;; Docelowo IZO trafia za ostatni przypadek Z.
          (setq taz_s_organize_destination_1
            (list taz_s_organize_destination_x 0.0 0.0)
          )

          (setq taz_s_organize_destination_2
            (list
              (+ taz_s_organize_destination_x taz_s_organize_align_size)
              0.0
              0.0
            )
          )

          (setq taz_s_organize_destination_3
            (list
              taz_s_organize_destination_x
              taz_s_organize_align_size
              0.0
            )
          )

          (command
            "_.ALIGN"
            taz_s_organize_case_ss
            ""
            "_NON"
            taz_s_organize_source_1
            "_NON"
            taz_s_organize_destination_1
            "_NON"
            taz_s_organize_source_2
            "_NON"
            taz_s_organize_destination_2
            "_NON"
            taz_s_organize_source_3
            "_NON"
            taz_s_organize_destination_3
          )

          (setq taz_s_organize_case_object_count
            (sslength taz_s_organize_case_ss)
          )

          ;; Tymczasowego POINT tabeli nie liczymy jako przeniesionego obiektu.
          (if taz_s_organize_current_table_marker
            (setq taz_s_organize_case_object_count
              (- taz_s_organize_case_object_count 1)
            )
          )

          (setq taz_s_organize_total_moved
            (+ taz_s_organize_total_moved taz_s_organize_case_object_count)
          )

          (princ
            (strcat
              "\nIZO - przypadek "
              (itoa taz_s_organize_case_nr)
              " - przeniesiono obiektow: "
              (itoa taz_s_organize_case_object_count)
            )
          )
        )
        (princ
          (strcat
            "\nIZO - przypadek "
            (itoa taz_s_organize_case_nr)
            " - nie znaleziono obiektow."
          )
        )
      )

      ;; Oryginalna funkcja zapisuje kotwę po ALIGN w kolejności organizera.
      (taz_s_organize_finish_table_case taz_s_organize_case_ss)

      ;; Dzięki temu istniejąca pętla ramek utworzy jeszcze jedną ramkę dla IZO.
      (setq taz_s_organize_case_nr (+ taz_s_organize_case_nr 1))

      ;; ----------------------------------------------------------------------
      ;; KONIEC
      ;; ----------------------------------------------------------------------

      (command "_.UCS" "_W")
      (command "_.REGEN")
      (command "_.ZOOM" "_E")

      (if (tblsearch "LAYER" taz_s_organize_old_clayer)
        (setvar "CLAYER" taz_s_organize_old_clayer)
      )

      (taz_s_organize_restore_locked_layers)

      (princ
        (strcat
          "\nGotowe. Lacznie przeniesiono obiektow: "
          (itoa taz_s_organize_total_moved)
        )
      )
    )
  )

  (setvar "CMDECHO" taz_s_organize_old_cmdecho)

  ;; ----------------------------------------------------------------------
  ;; RAMKI DLA KOLEJNYCH PRZYPADKOW
  ;;
  ;; Po uporzadkowaniu srodek pierwszego przypadku jest w 0,0,0,
  ;; a kazdy kolejny lezy o taz_s_organize_spacing dalej po osi X.
  ;;
  ;; Format arkusza i skala zostaly wybrane razem w glownym oknie
  ;; taz_s_create_drawings_execution_design.
  ;; Format, skala i srodek sa przekazywane do taz_s_frame przez
  ;; zmienne globalne, dlatego taz_s_frame nie wyswietla swojego DCL.
  ;; ----------------------------------------------------------------------

  (if taz_s_organize_can_run
    (progn

      ;; --------------------------------------------------------------------
      ;; USTAWIENIA RAMEK ZAPISANE OSOBNO DLA KAZDEGO PRZYPADKU
      ;; --------------------------------------------------------------------
      ;; Generator zapisuje ustawienia w kolejnosci IZO, X, Y, Z.
      ;; Organizer pracuje w kolejnosci X, Y, Z, IZO, dlatego w petli ramek
      ;; korzystamy z tego samego mapowania co dla tabel zestawienia stali.
      ;;
      ;; Stare zmienne globalne pozostaja jako awaryjny fallback.

      (setq taz_s_organize_frame_scale_factor nil)

      (if
        (and
          (boundp 'taz_s_execution_design_frame_scale_factor)
          taz_s_execution_design_frame_scale_factor
        )
        (setq taz_s_organize_frame_scale_factor
          taz_s_execution_design_frame_scale_factor
        )
        (if
          (and
            (boundp 'taz_s_annotation_scale)
            taz_s_annotation_scale
          )
          (setq taz_s_organize_frame_scale_factor
            taz_s_annotation_scale
          )
        )
      )

      (setq taz_s_organize_frame_format "A1")

      (if
        (and
          (boundp 'taz_s_execution_design_frame_format)
          taz_s_execution_design_frame_format
        )
        (setq taz_s_organize_frame_format
          taz_s_execution_design_frame_format
        )
      )

      ;; --------------------------------------------------------------------
      ;; KOLEJNE RAMKI - USTAWIENIA WLASCIWE DLA DANEGO PRZYPADKU
      ;; --------------------------------------------------------------------

      (setq taz_s_organize_frame_case_nr 1)
      (setq taz_s_organize_frame_lower_left_points '())

      ;; Dane potrzebne pozniej do koncowego dostosowania layoutow.
      ;; Kazda lista ma ten sam indeks co przypadek organizera: X, Y, Z, IZO.
      (setq taz_s_organize_frame_bounds '())
      (setq taz_s_organize_frame_insert_points '())
      (setq taz_s_organize_frame_scale_factors '())
      (setq taz_s_organize_frame_formats '())

      (while (< taz_s_organize_frame_case_nr taz_s_organize_case_nr)

        (setq taz_s_frame_known_insert_point
          (list
            (*
              (- taz_s_organize_frame_case_nr 1)
              taz_s_organize_spacing
            )
            0.0
            0.0
          )
        )

        ;; Indeks ustawien w listach generatora (IZO, X, Y, Z).
        (setq taz_s_organize_frame_settings_index
          (taz_s_organize_get_table_list_index
            taz_s_organize_frame_case_nr
          )
        )

        ;; Domyslnie fallback do ostatnich/globalnych ustawien.
        (setq taz_s_organize_frame_current_scale_factor
          taz_s_organize_frame_scale_factor
        )
        (setq taz_s_organize_frame_current_format
          taz_s_organize_frame_format
        )

        ;; Jesli generator zapisal ustawienia per przypadek, pobierz wlasciwe.
        (if
          (and
            (boundp 'taz_s_execution_design_case_scale_factors)
            taz_s_execution_design_case_scale_factors
            (<
              taz_s_organize_frame_settings_index
              (length taz_s_execution_design_case_scale_factors)
            )
          )
          (setq taz_s_organize_frame_current_scale_factor
            (nth
              taz_s_organize_frame_settings_index
              taz_s_execution_design_case_scale_factors
            )
          )
        )

        (if
          (and
            (boundp 'taz_s_execution_design_case_frame_formats)
            taz_s_execution_design_case_frame_formats
            (<
              taz_s_organize_frame_settings_index
              (length taz_s_execution_design_case_frame_formats)
            )
          )
          (setq taz_s_organize_frame_current_format
            (nth
              taz_s_organize_frame_settings_index
              taz_s_execution_design_case_frame_formats
            )
          )
        )

        (setq taz_s_frame_known_format
          taz_s_organize_frame_current_format
        )

        (if taz_s_organize_frame_current_scale_factor
          (setq taz_s_frame_known_scale_factor
            taz_s_organize_frame_current_scale_factor
          )
        )

        ;; Zapamietujemy dane przekazane do taz_s_frame oraz stan bazy
        ;; tuz przed ramka. Dzieki temu nie zalezymy od tego, czy taz_s_frame
        ;; pozostawi zmienne wejściowe po swoim zakonczeniu.
        (setq taz_s_organize_frame_current_insert_point
          taz_s_frame_known_insert_point
        )

        (setq taz_s_organize_frame_before_entity
          (taz_s_organize_layout_get_last_entity)
        )

        (c:taz_s_frame)

        (setq taz_s_organize_frame_new_entities
          (taz_s_organize_layout_collect_new_entities
            taz_s_organize_frame_before_entity
          )
        )

        (setq taz_s_organize_frame_current_bounds
          (taz_s_organize_layout_get_group_bounds
            taz_s_organize_frame_new_entities
          )
        )

        (setq taz_s_organize_frame_bounds
          (append
            taz_s_organize_frame_bounds
            (list taz_s_organize_frame_current_bounds)
          )
        )

        (setq taz_s_organize_frame_insert_points
          (append
            taz_s_organize_frame_insert_points
            (list taz_s_organize_frame_current_insert_point)
          )
        )

        (setq taz_s_organize_frame_scale_factors
          (append
            taz_s_organize_frame_scale_factors
            (list taz_s_organize_frame_current_scale_factor)
          )
        )

        (setq taz_s_organize_frame_formats
          (append
            taz_s_organize_frame_formats
            (list taz_s_organize_frame_current_format)
          )
        )

        ;; Zapamietaj lewy dolny naroznik wewnetrznej ramki.
        (setq taz_s_organize_frame_lower_left_points
          (append
            taz_s_organize_frame_lower_left_points
            (list taz_s_frame_inner_p1)
          )
        )

        (setq taz_s_organize_frame_case_nr
          (+ taz_s_organize_frame_case_nr 1)
        )
      )

      (setq taz_s_frame_known_insert_point nil)
      (setq taz_s_frame_known_format nil)
      (setq taz_s_frame_known_scale_factor nil)

      ;; --------------------------------------------------------------------
      ;; NA SAM KONIEC: LEWY DOLNY TABELI -> LEWY DOLNY RAMKI WEWNETRZNEJ
      ;; --------------------------------------------------------------------
      ;; Lewy dolny tabeli liczymy z punktow geometrii po ALIGN.
      ;; Przesuwany jest caly komplet obiektow utworzonych przez
      ;; taz_s_create_steel_table dla danego przypadku.

      (command "_.UCS" "_W")

      (setq taz_s_organize_table_move_index 0)

      (while
        (<
          taz_s_organize_table_move_index
          (length taz_s_organize_frame_lower_left_points)
        )

        (setq taz_s_organize_table_move_group nil)
        (setq taz_s_organize_table_move_source nil)
        (setq taz_s_organize_table_move_target nil)

        (if
          (and
            (boundp 'taz_s_execution_design_table_groups)
            taz_s_execution_design_table_groups
          )
          (setq taz_s_organize_table_move_group
            (nth
              (taz_s_organize_get_table_list_index
                (+ taz_s_organize_table_move_index 1)
              )
              taz_s_execution_design_table_groups
            )
          )
        )

        ;; Rzeczywisty lewy dolny naroznik tabeli po ALIGN.
        (if taz_s_organize_table_move_group
          (setq taz_s_organize_table_move_source
            (taz_s_organize_get_table_group_lower_left
              taz_s_organize_table_move_group
            )
          )
        )

        ;; Stara kotwa zostaje tylko jako awaryjny punkt zrodlowy.
        (if
          (and
            (= taz_s_organize_table_move_source nil)
            taz_s_organize_table_anchor_points_after
          )
          (setq taz_s_organize_table_move_source
            (nth
              taz_s_organize_table_move_index
              taz_s_organize_table_anchor_points_after
            )
          )
        )

        (setq taz_s_organize_table_move_target
          (nth
            taz_s_organize_table_move_index
            taz_s_organize_frame_lower_left_points
          )
        )

        (if
          (and
            taz_s_organize_table_move_group
            taz_s_organize_table_move_source
            taz_s_organize_table_move_target
          )
          (taz_s_organize_move_table_group
            taz_s_organize_table_move_group
            taz_s_organize_table_move_source
            taz_s_organize_table_move_target
          )
        )

        (setq taz_s_organize_table_move_index
          (+ taz_s_organize_table_move_index 1)
        )
      )

      ;; --------------------------------------------------------------------
      ;; OSTATNI KROK: DOSTOSOWANIE LAYOUTOW DO GOTOWYCH PRZYPADKOW
      ;; --------------------------------------------------------------------
      ;; Na tym etapie:
      ;; - wszystkie przypadki sa juz ulozone,
      ;; - ramki istnieja,
      ;; - tabele sa juz dosuniete do ramek.
      ;; Dopiero teraz ustawiamy papier i rzutnie layoutow.

      (taz_s_organize_finalize_layouts)

      (command "_.REGEN")
    )
  )

  (command "_.PLAN" "_World")
  (command "_.ZOOM" "_Extents")

  (princ)
)

;; =====================================================================================
;; ZESTAWIENIE STALI
;; Tworzy tabele z lista profili WIDOCZNYCH w danym przypadku (X / Y / Z)
;; obok geometrii tego przypadku.
;;
;; WYDAJNOSC: ta wersja NIE robi wlasnego testu -INTERFERE. Widocznosc
;; elementow jest ustalana JEDEN RAZ, w taz_s_intersect_pairs (w skrypcie
;; taz_s_create_drawings_execution_design.lsp), ktora zbiera uchwyty
;; widocznych elementow do globalnej listy taz_s_visible_handles. Tabela
;; dostaje juz gotowa liste - dzieki temu -INTERFERE liczy sie tylko raz
;; na element/przypadek, a nie dwa razy.
;;
;; Dlatego KOLEJNOSC WYWOLANIA w petli glownej musi byc:
;;   1. taz_s_intersect_pairs (zbiera taz_s_visible_handles)
;;   2. taz_s_create_steel_table (korzysta z taz_s_visible_handles)
;;
;; Dane profilu/dlugosci/materialu pobierane sa z globalnych zmiennych
;; wczytanych z taz_s_beam_data.txt (musza byc juz zaladowane - load w skrypcie
;; glownym, tak jak dotychczas).
;;
;; Pole powierzchni przekroju (kolumna "Powierzchnia") pobierane jest przez
;; wywolanie wlasciwej funkcji taz_s_section_..._draw_parametres_..., ktora
;; jako efekt uboczny ustawia globalna zmienna taz_s_section_area. Wywolanie
;; to nadpisuje przy okazji taz_s_h/taz_s_b/taz_s_tw/... - jest to bezpieczne
;; TYLKO dlatego, ze taz_s_create_steel_table wywolywane jest w petli glownej
;; PRZED jakimkolwiek dalszym rysowaniem przekroju w tej samej iteracji.
;; taz_s_family/taz_s_type/taz_s_category sa zapisywane i przywracane po
;; odczycie, dla bezpieczenstwa.
;;
;; Kolumna "Objetosc": dlugosc jest w mm, powierzchnia w cm2. Zeby dostac m3:
;;   dlugosc_m  = dlugosc_mm / 1000
;;   pole_m2    = powierzchnia_cm2 / 10000
;;   objetosc_m3 = dlugosc_m * pole_m2 = dlugosc_mm * powierzchnia_cm2 / 10000000
;;
;; Kolumna "Waga": objetosc_m3 * taz_s_unit_weight_steel (ciezar objetosciowy
;; stali w kg/m3, zmienna juz istniejaca w projekcie) = waga w kg.
;;
;; Tabela jest rysowana plasko (w plaszczyznie X-Y przy zoffset), a nastepnie
;; obracana ROTATE3D dokladnie tak samo jak etykiety w taz_s_intersect_pairs
;; (przypadek X: obrot wokol osi X o 90; przypadek Y: obrot wokol osi Y o 90,
;; potem wokol osi X o 90; przypadek Z: bez obrotu). Dzieki temu tabela ladu
;; sie w tej samej plaszczyznie co etykiety w danym przypadku.
;; =====================================================================================

;; ---------------------------------------------------------------------
;; KONFIGURACJA - do latwej zmiany
;; ---------------------------------------------------------------------

;; UWAGA: zakladam ze material jest zapisany pod attr8. Jesli w Twoim
;; pliku txt material jest pod innym numerem atrybutu - zmien ponizej.
(setq taz_s_st_material_attr_no "8")

;; wymiary bazowe tabeli dla skali 1:1
;; rzeczywiste wymiary sa wyliczane w taz_s_create_steel_table
;; przez pomnozenie ponizszych wartosci przez taz_s_annotation_scale

;; wysokosci tekstu 1:1
(setq taz_s_st_base_h_head 5.0)
(setq taz_s_st_base_h_txt  2.5)

;; szerokosci kolumn 1:1
(setq taz_s_st_base_col_profil       26.0)
(setq taz_s_st_base_col_dlugosc      24.0)
(setq taz_s_st_base_col_material     20.0)
(setq taz_s_st_base_col_powierzchnia 32.0)
(setq taz_s_st_base_col_objetosc     26.0)
(setq taz_s_st_base_col_waga         20.0)
(setq taz_s_st_base_col_ilosc        18.0)
(setq taz_s_st_base_col_waga_calkowita 32.0)

;; wysokosci wierszy 1:1
(setq taz_s_st_base_row_h  8.0)
(setq taz_s_st_base_head_h 14.0)

;; warstwa na ktorej rysowana jest tabela (ta sama co etykiety)
(setq taz_s_st_layer "taz_s_labels")

;; tolerancja porownania dlugosci przy laczeniu wierszy (te same jednostki co rysunek)
(setq taz_s_st_len_tol 0.1)


;; ---------------------------------------------------------------------
;; POMOCNICZA: linia (uzywana do siatki tabeli)
;; Kazda utworzona encja jest dopisywana do taz_s_st_created_ss,
;; zeby na koncu mozna bylo obrocic cala tabele jednym ROTATE3D.
;; ---------------------------------------------------------------------

(defun taz_s_st_line (taz_s_st_p1 taz_s_st_p2)
  (entmake
    (list
      (cons 0 "LINE")
      (cons 8 taz_s_st_layer)
      (cons 10 taz_s_st_p1)
      (cons 11 taz_s_st_p2)
    )
  )
  (if taz_s_st_created_ss
    (ssadd (entlast) taz_s_st_created_ss)
  )
)

;; ---------------------------------------------------------------------
;; POMOCNICZA: wpis tekstowy do komorki (wysrodkowany)
;; ---------------------------------------------------------------------

(defun taz_s_st_write_cell (taz_s_st_txt taz_s_st_x taz_s_st_y taz_s_st_z taz_s_st_h)
  (entmake
    (list
      (cons 0 "TEXT")
      (cons 8 taz_s_st_layer)
      (cons 7 "Standard")
      (cons 10 (list taz_s_st_x taz_s_st_y taz_s_st_z))
      (cons 40 taz_s_st_h)
      (cons 1 taz_s_st_txt)
      (cons 72 1)   ;; center
      (cons 73 2)   ;; middle
      (cons 11 (list taz_s_st_x taz_s_st_y taz_s_st_z))
    )
  )
  (if taz_s_st_created_ss
    (ssadd (entlast) taz_s_st_created_ss)
  )
)

;; ---------------------------------------------------------------------
;; POMOCNICZE: odczyt danych elementu po handlu (z taz_s_beam_data.txt)
;; ---------------------------------------------------------------------

(defun taz_s_st_get_profile_text (taz_s_st_h)
  (setq taz_s_st_family (eval (read (strcat "taz_s_" taz_s_st_h "_attr6"))))
  (setq taz_s_st_type   (eval (read (strcat "taz_s_" taz_s_st_h "_attr7"))))
  (setq taz_s_st_txt (strcat taz_s_st_family " " taz_s_st_type))
  (if (or (= taz_s_st_family "LR") (= taz_s_st_family "LN"))
    (setq taz_s_st_txt (strcat "L " taz_s_st_type))
  )
  taz_s_st_txt
)

(defun taz_s_st_get_length (taz_s_st_h)
  (setq taz_s_st_p1 (eval (read (strcat "taz_s_" taz_s_st_h "_sweep_p1"))))
  (setq taz_s_st_p2 (eval (read (strcat "taz_s_" taz_s_st_h "_sweep_p2"))))
  (distance taz_s_st_p1 taz_s_st_p2)
)

(defun taz_s_st_get_material (taz_s_st_h)
  (setq taz_s_st_sym
    (read (strcat "taz_s_" taz_s_st_h "_attr" taz_s_st_material_attr_no))
  )
  (if (boundp taz_s_st_sym)
    (eval taz_s_st_sym)
    ""
  )
)

;; ---------------------------------------------------------------------
;; POMOCNICZA: odczyt pola powierzchni przekroju (taz_s_section_area)
;;
;; Odtwarza taz_s_family / taz_s_type / taz_s_category z atrybutow
;; elementu, wywoluje wlasciwa funkcje taz_s_section_..._draw_parametres_...,
;; ktora ustawia taz_s_section_area (czysta arytmetyka, bez zadnych
;; komend CAD - szybkie), a nastepnie PRZYWRACA poprzednie wartosci
;; taz_s_family/taz_s_type/taz_s_category.
;; ---------------------------------------------------------------------

(defun taz_s_st_get_area (taz_s_st_h)

  (setq taz_s_st_area_family (eval (read (strcat "taz_s_" taz_s_st_h "_attr6"))))
  (setq taz_s_st_area_type   (eval (read (strcat "taz_s_" taz_s_st_h "_attr7"))))

  ;; zapamietaj biezacy stan (jesli w ogole byl ustawiony)
  (setq taz_s_st_saved_family   (if (boundp 'taz_s_family)   taz_s_family   nil))
  (setq taz_s_st_saved_type     (if (boundp 'taz_s_type)     taz_s_type     nil))
  (setq taz_s_st_saved_category (if (boundp 'taz_s_category) taz_s_category nil))

  (setq taz_s_family taz_s_st_area_family)
  (setq taz_s_type   taz_s_st_area_type)

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

  (setq taz_s_section_area nil)

  (cond
    ((= taz_s_family "HEA") (taz_s_section_ibeam_draw_parametres_hea))
    ((= taz_s_family "HEB") (taz_s_section_ibeam_draw_parametres_heb))
    ((= taz_s_family "IPE") (taz_s_section_ibeam_draw_parametres_ipe))
    ((= taz_s_family "IPN") (taz_s_section_ibeam_draw_parametres_ipn))
    ((= taz_s_family "UPE") (taz_s_section_cbeam_draw_parametres_upe))
    ((= taz_s_family "UPN") (taz_s_section_cbeam_draw_parametres_upn))
    ((= taz_s_family "LR")  (taz_s_section_lbeam_draw_parametres_katownik_rownoramienny))
    ((= taz_s_family "LN")  (taz_s_section_lbeam_draw_parametres_katownik_nierownoramienny))
    ((= taz_s_family "SHS") (taz_s_section_hsbeam_draw_parametres_rura_kwadratowa))
    ((= taz_s_family "RHS") (taz_s_section_hsbeam_draw_parametres_rura_prostokatna))
    ((= taz_s_family "CHS") (taz_s_section_hsbeam_draw_parametres_rura_okragla))
  )

  (setq taz_s_st_area_result taz_s_section_area)

  ;; przywroc poprzedni stan
  (setq taz_s_family   taz_s_st_saved_family)
  (setq taz_s_type     taz_s_st_saved_type)
  (setq taz_s_category taz_s_st_saved_category)

  (if taz_s_st_area_result
    taz_s_st_area_result
    0.0
  )
)

;; ---------------------------------------------------------------------
;; POMOCNICZA: objetosc w m3
;; taz_s_st_length_mm - dlugosc w mm, taz_s_st_area_cm2 - powierzchnia w cm2
;; ---------------------------------------------------------------------

(defun taz_s_st_get_volume (taz_s_st_length_mm taz_s_st_area_cm2)
  (/ (* taz_s_st_length_mm taz_s_st_area_cm2) 10000000.0)
)

;; ---------------------------------------------------------------------
;; POMOCNICZA: waga w kg
;; taz_s_st_volume_m3 - objetosc w m3, korzysta z taz_s_unit_weight_steel
;; (ciezar objetosciowy stali w kg/m3, zmienna juz istniejaca w projekcie)
;; ---------------------------------------------------------------------

(defun taz_s_st_get_weight (taz_s_st_volume_m3)
  (* taz_s_st_volume_m3 taz_s_unit_weight_steel)
)

;; ---------------------------------------------------------------------
;; RYSOWANIE SIATKI TABELI
;; ---------------------------------------------------------------------

(defun taz_s_st_draw_grid (taz_s_st_top taz_s_st_w taz_s_st_h
                            taz_s_st_head_h taz_s_st_row_h taz_s_st_nrows
                            taz_s_st_colwidths taz_s_st_merge_bottom_rows)

  (setq taz_s_st_x0 (car   taz_s_st_top))
  (setq taz_s_st_y0 (cadr  taz_s_st_top))
  (setq taz_s_st_z0 (caddr taz_s_st_top))

  ;; ramka zewnetrzna
  (taz_s_st_line (list taz_s_st_x0 taz_s_st_y0 taz_s_st_z0)
                  (list (+ taz_s_st_x0 taz_s_st_w) taz_s_st_y0 taz_s_st_z0))
  (taz_s_st_line (list (+ taz_s_st_x0 taz_s_st_w) taz_s_st_y0 taz_s_st_z0)
                  (list (+ taz_s_st_x0 taz_s_st_w) (- taz_s_st_y0 taz_s_st_h) taz_s_st_z0))
  (taz_s_st_line (list (+ taz_s_st_x0 taz_s_st_w) (- taz_s_st_y0 taz_s_st_h) taz_s_st_z0)
                  (list taz_s_st_x0 (- taz_s_st_y0 taz_s_st_h) taz_s_st_z0))
  (taz_s_st_line (list taz_s_st_x0 (- taz_s_st_y0 taz_s_st_h) taz_s_st_z0)
                  (list taz_s_st_x0 taz_s_st_y0 taz_s_st_z0))

  ;; linia pod naglowkiem "ZESTAWIENIE STALI"
  (taz_s_st_line (list taz_s_st_x0 (- taz_s_st_y0 taz_s_st_head_h) taz_s_st_z0)
                  (list (+ taz_s_st_x0 taz_s_st_w) (- taz_s_st_y0 taz_s_st_head_h) taz_s_st_z0))

  ;; linie poziome (naglowki kolumn + kazdy wiersz danych)
  (setq taz_s_st_y (- taz_s_st_y0 taz_s_st_head_h))
  (repeat (1+ taz_s_st_nrows)
    (setq taz_s_st_y (- taz_s_st_y taz_s_st_row_h))
    (taz_s_st_line (list taz_s_st_x0 taz_s_st_y taz_s_st_z0)
                    (list (+ taz_s_st_x0 taz_s_st_w) taz_s_st_y taz_s_st_z0))
  )

  ;; linie pionowe kolumn (od naglowkow kolumn do dolu tabeli)
  ;; dla IZO w koncowych wierszach komorki od Profil do Waga sa scalone,
  ;; wiec wewnetrzne podzialy tych kolumn koncza sie nad tymi wierszami
  (setq taz_s_st_x taz_s_st_x0)
  (setq taz_s_st_col_index 0)
  (setq taz_s_st_col_count (length taz_s_st_colwidths))
  (foreach taz_s_st_cw taz_s_st_colwidths
    (setq taz_s_st_col_index (1+ taz_s_st_col_index))
    (setq taz_s_st_x (+ taz_s_st_x taz_s_st_cw))
    (taz_s_st_line
      (list taz_s_st_x (- taz_s_st_y0 taz_s_st_head_h) taz_s_st_z0)
      (list taz_s_st_x
        (if (and (> taz_s_st_merge_bottom_rows 0)
                 (< taz_s_st_col_index (- taz_s_st_col_count 1)))
          (+ (- taz_s_st_y0 taz_s_st_h)
             (* taz_s_st_merge_bottom_rows taz_s_st_row_h))
          (- taz_s_st_y0 taz_s_st_h)
        )
        taz_s_st_z0
      )
    )
  )

  (princ)
)

;; ---------------------------------------------------------------------
;; POMOCNICZA: rozpoznanie przypadku IZO
;; ---------------------------------------------------------------------

(defun taz_s_st_is_izo_case (taz_s_st_case / taz_s_st_case_upper)
  (if (= (type taz_s_st_case) 'STR)
    (progn
      (setq taz_s_st_case_upper (strcase taz_s_st_case))
      (= taz_s_st_case_upper "IZO")
    )
    (= taz_s_st_case 'IZO)
  )
)

;; ---------------------------------------------------------------------
;; RYSOWANIE TABELI Z DANYCH (naglowek + naglowki kolumn + wiersze)
;; ---------------------------------------------------------------------

(defun taz_s_st_draw_table (taz_s_st_rows taz_s_st_ins_pt taz_s_st_case)

  (setq taz_s_st_x0 (car   taz_s_st_ins_pt))
  (setq taz_s_st_y0 (cadr  taz_s_st_ins_pt))
  (setq taz_s_st_z0 (caddr taz_s_st_ins_pt))

  (setq taz_s_st_table_w
    (+ taz_s_st_col_profil taz_s_st_col_dlugosc taz_s_st_col_material
       taz_s_st_col_powierzchnia taz_s_st_col_objetosc taz_s_st_col_waga taz_s_st_col_ilosc
       (if (taz_s_st_is_izo_case taz_s_st_case) taz_s_st_col_waga_calkowita 0.0))
  )

  (setq taz_s_st_nrows (length taz_s_st_rows))
  ;; tylko IZO dostaje trzy dodatkowe, koncowe wiersze podsumowania
  (setq taz_s_st_grid_nrows
    (+ taz_s_st_nrows (if (taz_s_st_is_izo_case taz_s_st_case) 3 0))
  )
  (setq taz_s_st_table_h (+ taz_s_st_head_h taz_s_st_row_h (* taz_s_st_grid_nrows taz_s_st_row_h)))
  
  ;; punkt wstawienia = lewy-dolny rog tabeli (odkomentować jeżeli chce my lewy dolny zamiast prawego górnego)
  ;; dalsze rysowanie korzysta z lewego-gornego rogu
  ;;(setq taz_s_st_y0 (+ taz_s_st_y0 taz_s_st_table_h))

  ;; ---- naglowek "ZESTAWIENIE STALI" ----
  (taz_s_st_write_cell "ZESTAWIENIE STALI"
    (+ taz_s_st_x0 (/ taz_s_st_table_w 2.0))
    (- taz_s_st_y0 (/ taz_s_st_head_h 2.0))
    taz_s_st_z0
    taz_s_st_h_head)

  ;; ---- naglowki kolumn ----
  (setq taz_s_st_row_y (- taz_s_st_y0 taz_s_st_head_h (/ taz_s_st_row_h 2.0)))
  (setq taz_s_st_col_x taz_s_st_x0)

  (taz_s_st_write_cell "Profil"       (+ taz_s_st_col_x (/ taz_s_st_col_profil 2.0))       taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
  (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_profil))
  (taz_s_st_write_cell "Ilosc [szt.]"        (+ taz_s_st_col_x (/ taz_s_st_col_ilosc 2.0))        taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
  (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_ilosc))
  (taz_s_st_write_cell "Material"     (+ taz_s_st_col_x (/ taz_s_st_col_material 2.0))     taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
  (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_material))
  (taz_s_st_write_cell "Dlugosc [mm]"      (+ taz_s_st_col_x (/ taz_s_st_col_dlugosc 2.0))      taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
  (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_dlugosc))
  (taz_s_st_write_cell "Powierzchnia [cm2]" (+ taz_s_st_col_x (/ taz_s_st_col_powierzchnia 2.0)) taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
  (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_powierzchnia))
  (taz_s_st_write_cell "Objetosc [m3]"     (+ taz_s_st_col_x (/ taz_s_st_col_objetosc 2.0))     taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
  (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_objetosc))
  (taz_s_st_write_cell "Waga [kg]"         (+ taz_s_st_col_x (/ taz_s_st_col_waga 2.0))         taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)

  ;; dodatkowa kolumna tylko dla przypadku IZO
  (if (taz_s_st_is_izo_case taz_s_st_case)
    (progn
      (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_waga))
      (taz_s_st_write_cell "Waga calkowita [kg]"
        (+ taz_s_st_col_x (/ taz_s_st_col_waga_calkowita 2.0))
        taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
    )
  )

  ;; ---- wiersze danych ----
  ;; taz_s_st_row = (profil dlugosc material powierzchnia objetosc waga ilosc)
  (setq taz_s_st_row_y (- taz_s_st_y0 taz_s_st_head_h taz_s_st_row_h (/ taz_s_st_row_h 2.0)))
  (setq taz_s_st_total_weight 0.0)

  (foreach taz_s_st_row taz_s_st_rows
    (setq taz_s_st_col_x taz_s_st_x0)

    (taz_s_st_write_cell (nth 0 taz_s_st_row)
      (+ taz_s_st_col_x (/ taz_s_st_col_profil 2.0)) taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
    (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_profil))

    (taz_s_st_write_cell (itoa (nth 6 taz_s_st_row))
      (+ taz_s_st_col_x (/ taz_s_st_col_ilosc 2.0)) taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
    (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_ilosc))

    (taz_s_st_write_cell (nth 2 taz_s_st_row)
      (+ taz_s_st_col_x (/ taz_s_st_col_material 2.0)) taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
    (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_material))

    (taz_s_st_write_cell (rtos (nth 1 taz_s_st_row) 2 0)
      (+ taz_s_st_col_x (/ taz_s_st_col_dlugosc 2.0)) taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
    (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_dlugosc))

    (taz_s_st_write_cell (rtos (nth 3 taz_s_st_row) 2 2)
      (+ taz_s_st_col_x (/ taz_s_st_col_powierzchnia 2.0)) taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
    (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_powierzchnia))

    (taz_s_st_write_cell (rtos (nth 4 taz_s_st_row) 2 6)
      (+ taz_s_st_col_x (/ taz_s_st_col_objetosc 2.0)) taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
    (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_objetosc))

    (taz_s_st_write_cell (rtos (nth 5 taz_s_st_row) 2 2)
      (+ taz_s_st_col_x (/ taz_s_st_col_waga 2.0)) taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)

    ;; Waga calkowita = Ilosc * Waga, tylko dla IZO
    (if (taz_s_st_is_izo_case taz_s_st_case)
      (progn
        (setq taz_s_st_row_total_weight (* (nth 6 taz_s_st_row) (nth 5 taz_s_st_row)))
        (setq taz_s_st_total_weight (+ taz_s_st_total_weight taz_s_st_row_total_weight))
        (setq taz_s_st_col_x (+ taz_s_st_col_x taz_s_st_col_waga))
        (taz_s_st_write_cell (rtos taz_s_st_row_total_weight 2 2)
          (+ taz_s_st_col_x (/ taz_s_st_col_waga_calkowita 2.0)) taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
      )
    )

    (setq taz_s_st_row_y (- taz_s_st_row_y taz_s_st_row_h))
  )

  ;; ---- ostatni wiersz podsumowania, tylko dla IZO ----
  (if (taz_s_st_is_izo_case taz_s_st_case)
    (progn
      (setq taz_s_st_summary_left_w (- taz_s_st_table_w taz_s_st_col_waga_calkowita))

      ;; scalona komorka od Profil do Waga
      (taz_s_st_write_cell "Waga konstrukcji [kg]"
        (+ taz_s_st_x0 (/ taz_s_st_summary_left_w 2.0))
        taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)

      ;; suma kolumny Waga calkowita
      (taz_s_st_write_cell (rtos taz_s_st_total_weight 2 2)
        (+ taz_s_st_x0 taz_s_st_summary_left_w (/ taz_s_st_col_waga_calkowita 2.0))
        taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)

      ;; przejscie do kolejnego wiersza podsumowania
      (setq taz_s_st_row_y (- taz_s_st_row_y taz_s_st_row_h))

      ;; scalona komorka od Profil do Waga
      (taz_s_st_write_cell "Naddatek na polaczenia [%]"
        (+ taz_s_st_x0 (/ taz_s_st_summary_left_w 2.0))
        taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)

      ;; wartosc parametru taz_s_additional_mass
      (taz_s_st_write_cell (rtos taz_s_additional_mass 2 2)
        (+ taz_s_st_x0 taz_s_st_summary_left_w (/ taz_s_st_col_waga_calkowita 2.0))
        taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)

      ;; przejscie do kolejnego wiersza podsumowania
      (setq taz_s_st_row_y (- taz_s_st_row_y taz_s_st_row_h))

      ;; waga naddatku = waga konstrukcji * naddatek [%] / 100
      (setq taz_s_st_additional_weight
        (* taz_s_st_total_weight (/ taz_s_additional_mass 100.0))
      )

      ;; waga calkowita konstrukcji = waga konstrukcji + waga naddatku
      (setq taz_s_st_total_structure_weight
        (+ taz_s_st_total_weight taz_s_st_additional_weight)
      )

      ;; scalona komorka od Profil do Waga
      (taz_s_st_write_cell "Waga calkowita konstrukcji [kg]"
        (+ taz_s_st_x0 (/ taz_s_st_summary_left_w 2.0))
        taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)

      ;; obliczona waga calkowita konstrukcji
      (taz_s_st_write_cell (rtos taz_s_st_total_structure_weight 2 2)
        (+ taz_s_st_x0 taz_s_st_summary_left_w (/ taz_s_st_col_waga_calkowita 2.0))
        taz_s_st_row_y taz_s_st_z0 taz_s_st_h_txt)
    )
  )

  ;; ---- siatka tabeli ----
  (taz_s_st_draw_grid
    (list taz_s_st_x0 taz_s_st_y0 taz_s_st_z0)
    taz_s_st_table_w
    taz_s_st_table_h
    taz_s_st_head_h
    taz_s_st_row_h
    taz_s_st_grid_nrows
    (if (taz_s_st_is_izo_case taz_s_st_case)
      (list taz_s_st_col_profil taz_s_st_col_ilosc taz_s_st_col_material taz_s_st_col_dlugosc
            taz_s_st_col_powierzchnia taz_s_st_col_objetosc taz_s_st_col_waga taz_s_st_col_waga_calkowita)
      (list taz_s_st_col_profil taz_s_st_col_ilosc taz_s_st_col_material taz_s_st_col_dlugosc
            taz_s_st_col_powierzchnia taz_s_st_col_objetosc taz_s_st_col_waga)
    )
    (if (taz_s_st_is_izo_case taz_s_st_case) 3 0)
  )

  (princ)
)

;; =======================================================================================
;; GLOWNA FUNKCJA: taz_s_create_steel_table
;;
;; Parametry:
;;   taz_s_st_visible_handles - lista uchwytow (string) elementow widocznych
;;                          w tym przypadku - pochodzi z taz_s_visible_handles,
;;                          ustawionej przez taz_s_intersect_pairs. WYMAGANE
;;                          zeby taz_s_intersect_pairs bylo wywolane wczesniej
;;                          w tej samej iteracji petli.
;;   taz_s_st_ins_pt      - punkt wstawienia (lewy-gorny rog naglowka tabeli),
;;                          np. (list (+ taz_s_xmax 5000) taz_s_y taz_s_zoffset)
;;   taz_s_st_case        - "X" / "Y" / "Z" / "IZO" - decyduje o obrocie tabeli do
;;                          plaszczyzny etykiet danego przypadku (tak jak
;;                          w taz_s_intersect_pairs; "IZO" zachowuje geometrie jak "Z"
;;                          i dodaje kolumne "Waga calkowita [kg]"
;; =======================================================================================

(defun taz_s_create_steel_table (taz_s_st_visible_handles taz_s_st_ins_pt taz_s_st_case)

  ;; skalowanie tabeli zgodnie ze skala wybrana w taz_s_annotation_scale
  ;; (wartosci bazowe powyzej odpowiadaja skali 1:1)
  (if (boundp 'taz_s_annotation_scale)
    (setq taz_s_st_scale taz_s_annotation_scale)
    (setq taz_s_st_scale 1.0)
  )

  (setq taz_s_st_h_head          (* taz_s_st_base_h_head          taz_s_st_scale))
  (setq taz_s_st_h_txt           (* taz_s_st_base_h_txt           taz_s_st_scale))
  (setq taz_s_st_col_profil      (* taz_s_st_base_col_profil      taz_s_st_scale))
  (setq taz_s_st_col_dlugosc     (* taz_s_st_base_col_dlugosc     taz_s_st_scale))
  (setq taz_s_st_col_material    (* taz_s_st_base_col_material    taz_s_st_scale))
  (setq taz_s_st_col_powierzchnia (* taz_s_st_base_col_powierzchnia taz_s_st_scale))
  (setq taz_s_st_col_objetosc    (* taz_s_st_base_col_objetosc    taz_s_st_scale))
  (setq taz_s_st_col_waga        (* taz_s_st_base_col_waga        taz_s_st_scale))
  (setq taz_s_st_col_ilosc       (* taz_s_st_base_col_ilosc       taz_s_st_scale))
  (setq taz_s_st_col_waga_calkowita (* taz_s_st_base_col_waga_calkowita taz_s_st_scale))
  (setq taz_s_st_row_h           (* taz_s_st_base_row_h           taz_s_st_scale))
  (setq taz_s_st_head_h          (* taz_s_st_base_head_h          taz_s_st_scale))

  (setq taz_s_st_rows '())  ;; lista: (profil dlugosc material powierzchnia objetosc waga ilosc)

  (foreach taz_s_st_h taz_s_st_visible_handles
    (setq taz_s_st_profile  (taz_s_st_get_profile_text taz_s_st_h))
    (setq taz_s_st_length   (taz_s_st_get_length taz_s_st_h))
    (setq taz_s_st_material (taz_s_st_get_material taz_s_st_h))
    (setq taz_s_st_area     (taz_s_st_get_area taz_s_st_h))
    (setq taz_s_st_volume   (taz_s_st_get_volume taz_s_st_length taz_s_st_area))
    (setq taz_s_st_weight   (taz_s_st_get_weight taz_s_st_volume))

    ;; szukaj czy juz mamy wiersz o tym samym profilu / dlugosci / materiale
    (setq taz_s_st_found nil)
    (setq taz_s_st_newrows '())

    (foreach taz_s_st_row taz_s_st_rows
      (if (and (not taz_s_st_found)
               (= (nth 0 taz_s_st_row) taz_s_st_profile)
               (equal (nth 1 taz_s_st_row) taz_s_st_length taz_s_st_len_tol)
               (= (nth 2 taz_s_st_row) taz_s_st_material)
          )
        (progn
          (setq taz_s_st_row
            (list
              (nth 0 taz_s_st_row)
              (nth 1 taz_s_st_row)
              (nth 2 taz_s_st_row)
              (nth 3 taz_s_st_row)
              (nth 4 taz_s_st_row)
              (nth 5 taz_s_st_row)
              (1+ (nth 6 taz_s_st_row))
            )
          )
          (setq taz_s_st_found T)
        )
      )
      (setq taz_s_st_newrows (append taz_s_st_newrows (list taz_s_st_row)))
    )
    (setq taz_s_st_rows taz_s_st_newrows)

    (if (not taz_s_st_found)
      (setq taz_s_st_rows
        (append taz_s_st_rows
          (list (list taz_s_st_profile taz_s_st_length taz_s_st_material taz_s_st_area taz_s_st_volume taz_s_st_weight 1))
        )
      )
    )
  )

  (if taz_s_st_rows
    (progn
      ;; nowy, pusty zbior - do niego trafia kazda encja tabeli (linie + teksty)
      (setq taz_s_st_created_ss (ssadd))

      (taz_s_st_draw_table taz_s_st_rows taz_s_st_ins_pt taz_s_st_case)

      ;; ---- obrot calej tabeli do plaszczyzny etykiet danego przypadku ----
      ;; identyczna logika jak przy obrocie etykiet w taz_s_intersect_pairs
      (cond
        ((= taz_s_st_case "X")
         (command "_.ROTATE3D" taz_s_st_created_ss "" "X" taz_s_st_ins_pt "90")
        )
        ((= taz_s_st_case "Y")
         (command "_.ROTATE3D" taz_s_st_created_ss "" "Y" taz_s_st_ins_pt "90")
         (command "_.ROTATE3D" taz_s_st_created_ss "" "X" taz_s_st_ins_pt "90")
        )
        ;; przypadek "Z" - bez obrotu, plaszczyzna pozioma juz jest wlasciwa
      )

      (setq taz_s_st_created_ss nil)
    )
  )

  (princ)
)
