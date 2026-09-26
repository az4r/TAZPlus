(defun taz_s_merge_solprof_layers ()

  (setq taz_s_layer_rec (tblnext "LAYER" T))

  (while taz_s_layer_rec

    (setq taz_s_layer_name
          (cdr (assoc 2 taz_s_layer_rec))
    )

    ;; PH* -> taz_s_hidden / taz_s_xref_hidden
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
                  (cons 8 (if taz_s_solprof_xref_mode "taz_s_xref_hidden" "taz_s_hidden"))
                  (assoc 8 taz_s_edata)
                  taz_s_edata
                )
              )

            )
          )
        )
      )
    )

    ;; PV* -> taz_s_visible / taz_s_xref_visible
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
                  (cons 8 (if taz_s_solprof_xref_mode "taz_s_xref_visible" "taz_s_visible"))
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

  ;; Warstwy taz_s_xref_editing_layer / taz_s_xref_visible /
  ;; taz_s_xref_hidden sa tworzone przez skrypt startowy.
  (setq taz_s_solprof_xref_mode nil)

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
  ;; CZYSZCZENIE WARSTWY xref_editing_layer
  ;; ---------------------------------

  (setq taz_s_ss
    (ssget "X" '((8 . "taz_s_xref_editing_layer")))
  )
  (if taz_s_ss
    (command "ERASE" taz_s_ss "")
  )

  ;; ---------------------------------
  ;; KLASY ELEMENTOW
  ;;
  ;; 1) taz_s_beam / taz_s_plate = geometria z danymi
  ;; 2) taz_s_unassigned          = geometria modelu BEZ danych (bez atrybutow,
  ;;                                 bez etykiet - dane po prostu nie istnieja)
  ;; 3) taz_s_axes                = osie, pomijane
  ;; 4) kazdy inny 3DSOLID        = geometria referencyjna / podklad
  ;;
  ;; Podklad NIE jest zwiazany z konkretna nazwa warstwy. Moze lezec
  ;; np. na 0, taz_s_xref albo dowolnej innej warstwie zrodlowej.
  ;; Warstwy robocze generatora nie sa traktowane jako podklad.
  ;; ---------------------------------

  (defun taz_s_is_data_layer (taz_s_layer_name_arg)
    (if taz_s_layer_name_arg
      (or
        (= (strcase taz_s_layer_name_arg) "TAZ_S_BEAM")
        (= (strcase taz_s_layer_name_arg) "TAZ_S_PLATE")
      )
      nil
    )
  )

  (defun taz_s_is_xref_layer (taz_s_layer_name_arg)
    (if taz_s_layer_name_arg
      (and
        (not (taz_s_is_data_layer taz_s_layer_name_arg))
        (/= (strcase taz_s_layer_name_arg) "TAZ_S_AXES")
        (/= (strcase taz_s_layer_name_arg) "TAZ_S_EXECUTION_DESIGN")
        (/= (strcase taz_s_layer_name_arg) "TAZ_S_EDITING_LAYER")
        (/= (strcase taz_s_layer_name_arg) "TAZ_S_XREF_EDITING_LAYER")
        ;; taz_s_unassigned to geometria modelu bez danych, a NIE podklad
        (/= (strcase taz_s_layer_name_arg) "TAZ_S_UNASSIGNED")
      )
      nil
    )
  )


  ;; ---------------------------------
  ;; PODKLAD Z KRZYWYCH -> TYMCZASOWE 3DSOLID
  ;;
  ;; Obslugiwane sa tylko typowe obiekty geometryczne, ktore moga byc
  ;; sciezka SWEEP:
  ;;   LINE, ARC, CIRCLE, ELLIPSE, LWPOLYLINE, POLYLINE, SPLINE
  ;;
  ;; TEXT / MTEXT / INSERT / DIMENSION / HATCH i podobne obiekty sa
  ;; celowo pomijane i nie biora udzialu w tym mechanizmie.
  ;;
  ;; Zasada jest celowo prosta:
  ;;   1. odczytujemy dowolny latwo dostepny punkt krzywej,
  ;;   2. w tym punkcie tworzymy zwykle CIRCLE o srednicy 0.001,
  ;;      bez ustawiania go prostopadle do sciezki,
  ;;   3. SWEEP ma Alignment=Yes, wiec GstarCAD sam dopasowuje profil
  ;;      do kierunku sciezki,
  ;;   4. zamkniety profil CIRCLE + tryb Solid daje prawdziwy 3DSOLID,
  ;;   5. dalej generator traktuje ten obiekt jak kazdy inny solid podkladu.
  ;;
  ;; Bez VL / VLA / VLAX / COM.
  ;; ---------------------------------

  (setq taz_s_xref_sweep_diameter 0.01)
  (setq taz_s_xref_sweep_radius (/ taz_s_xref_sweep_diameter 2.0))

  (defun taz_s_is_sweepable_xref_curve (taz_s_curve_ent / taz_s_curve_ed taz_s_curve_type taz_s_curve_layer)
    (if (and taz_s_curve_ent (entget taz_s_curve_ent))
      (progn
        (setq taz_s_curve_ed (entget taz_s_curve_ent))
        (setq taz_s_curve_type (cdr (assoc 0 taz_s_curve_ed)))
        (setq taz_s_curve_layer (cdr (assoc 8 taz_s_curve_ed)))
        (and
          (member taz_s_curve_type
            '("LINE" "ARC" "CIRCLE" "ELLIPSE" "LWPOLYLINE" "POLYLINE" "SPLINE")
          )
          (taz_s_is_xref_layer taz_s_curve_layer)
        )
      )
      nil
    )
  )

  ;; Iloczyn wektorowy - potrzebny tylko do wyznaczenia dowolnego
  ;; rzeczywistego punktu ELLIPSE. Nie sluzy do ustawiania profilu.
  (defun taz_s_cross3 (taz_s_a taz_s_b)
    (list
      (- (* (cadr taz_s_a) (caddr taz_s_b)) (* (caddr taz_s_a) (cadr taz_s_b)))
      (- (* (caddr taz_s_a) (car taz_s_b)) (* (car taz_s_a) (caddr taz_s_b)))
      (- (* (car taz_s_a) (cadr taz_s_b)) (* (cadr taz_s_a) (car taz_s_b)))
    )
  )

  ;; Zwraca dowolny praktyczny punkt nalezacy do / opisujacy krzywa.
  ;; Dla SPLINE: najpierw bierzemy pierwszy FIT POINT (DXF 11), a gdy
  ;; go nie ma - pierwszy CONTROL POINT (DXF 10). Kazdy poprawny SPLINE
  ;; ma co najmniej punkty kontrolne, wiec nie wymagamy zadnego VL.
  (defun taz_s_get_xref_curve_point
    (taz_s_curve_ent
      / taz_s_ed taz_s_type taz_s_p taz_s_c taz_s_r taz_s_a
        taz_s_elev taz_s_vtx taz_s_major taz_s_minor taz_s_normal
        taz_s_ratio taz_s_param)

    (setq taz_s_p nil)
    (if (and taz_s_curve_ent (entget taz_s_curve_ent))
      (progn
        (setq taz_s_ed (entget taz_s_curve_ent))
        (setq taz_s_type (cdr (assoc 0 taz_s_ed)))

        (cond
          ;; Punkt poczatkowy linii jest zapisany bezposrednio w DXF 10.
          ((= taz_s_type "LINE")
            (setq taz_s_p (cdr (assoc 10 taz_s_ed)))
          )

          ;; Dla luku bierzemy jego rzeczywisty punkt startowy.
          ((= taz_s_type "ARC")
            (setq taz_s_c (cdr (assoc 10 taz_s_ed)))
            (setq taz_s_r (cdr (assoc 40 taz_s_ed)))
            (setq taz_s_a (cdr (assoc 50 taz_s_ed)))
            (if (and taz_s_c taz_s_r taz_s_a)
              (setq taz_s_p
                (trans
                  (list
                    (+ (car taz_s_c) (* taz_s_r (cos taz_s_a)))
                    (+ (cadr taz_s_c) (* taz_s_r (sin taz_s_a)))
                    (caddr taz_s_c)
                  )
                  taz_s_curve_ent
                  0
                )
              )
            )
          )

          ;; Dla okregu wybieramy punkt na promieniu w lokalnym kierunku X.
          ((= taz_s_type "CIRCLE")
            (setq taz_s_c (cdr (assoc 10 taz_s_ed)))
            (setq taz_s_r (cdr (assoc 40 taz_s_ed)))
            (if (and taz_s_c taz_s_r)
              (setq taz_s_p
                (trans
                  (list (+ (car taz_s_c) taz_s_r) (cadr taz_s_c) (caddr taz_s_c))
                  taz_s_curve_ent
                  0
                )
              )
            )
          )

          ;; Punkt ELLIPSE dla jej parametru poczatkowego.
          ;; DXF 10 = srodek, 11 = wektor osi glownej, 40 = stosunek osi,
          ;; 41 = parametr poczatkowy, 210 = normalna.
          ((= taz_s_type "ELLIPSE")
            (setq taz_s_c (cdr (assoc 10 taz_s_ed)))
            (setq taz_s_major (cdr (assoc 11 taz_s_ed)))
            (setq taz_s_ratio (cdr (assoc 40 taz_s_ed)))
            (setq taz_s_param (cdr (assoc 41 taz_s_ed)))
            (setq taz_s_normal (cdr (assoc 210 taz_s_ed)))
            (if (not taz_s_normal) (setq taz_s_normal '(0.0 0.0 1.0)))
            (if (not taz_s_param) (setq taz_s_param 0.0))
            (if (and taz_s_c taz_s_major taz_s_ratio)
              (progn
                (setq taz_s_minor
                  (mapcar
                    '(lambda (taz_s_q) (* taz_s_q taz_s_ratio))
                    (taz_s_cross3 taz_s_normal taz_s_major)
                  )
                )
                (setq taz_s_p
                  (list
                    (+ (car taz_s_c)
                       (* (car taz_s_major) (cos taz_s_param))
                       (* (car taz_s_minor) (sin taz_s_param)))
                    (+ (cadr taz_s_c)
                       (* (cadr taz_s_major) (cos taz_s_param))
                       (* (cadr taz_s_minor) (sin taz_s_param)))
                    (+ (caddr taz_s_c)
                       (* (caddr taz_s_major) (cos taz_s_param))
                       (* (caddr taz_s_minor) (sin taz_s_param)))
                  )
                )
              )
            )
          )

          ;; Pierwszy wierzcholek LWPOLYLINE. DXF 10 jest w OCS,
          ;; dlatego dodajemy elewacje i transformujemy do WCS.
          ((= taz_s_type "LWPOLYLINE")
            (setq taz_s_p (cdr (assoc 10 taz_s_ed)))
            (setq taz_s_elev (cdr (assoc 38 taz_s_ed)))
            (if (not taz_s_elev) (setq taz_s_elev 0.0))
            (if taz_s_p
              (setq taz_s_p
                (trans
                  (list (car taz_s_p) (cadr taz_s_p) taz_s_elev)
                  taz_s_curve_ent
                  0
                )
              )
            )
          )

          ;; Klasyczna POLYLINE przechowuje punkty w rekordach VERTEX.
          ((= taz_s_type "POLYLINE")
            (setq taz_s_vtx (entnext taz_s_curve_ent))
            (while
              (and
                taz_s_vtx
                (/= (cdr (assoc 0 (entget taz_s_vtx))) "VERTEX")
                (/= (cdr (assoc 0 (entget taz_s_vtx))) "SEQEND")
              )
              (setq taz_s_vtx (entnext taz_s_vtx))
            )
            (if
              (and
                taz_s_vtx
                (= (cdr (assoc 0 (entget taz_s_vtx))) "VERTEX")
              )
              (progn
                (setq taz_s_p (cdr (assoc 10 (entget taz_s_vtx))))
                (if taz_s_p
                  (setq taz_s_p (trans taz_s_p taz_s_curve_ent 0))
                )
              )
            )
          )

          ;; Wszystkie SPLINE: fit point jesli istnieje, inaczej control point.
          ;; Nie wyznaczamy stycznej ani parametru krzywej.
          ((= taz_s_type "SPLINE")
            (setq taz_s_p (cdr (assoc 11 taz_s_ed)))
            (if (not taz_s_p)
              (setq taz_s_p (cdr (assoc 10 taz_s_ed)))
            )
          )
        )
      )
    )
    taz_s_p
  )

  (defun taz_s_create_xref_sweep_solid
    (taz_s_path_ent
      / taz_s_path_ed taz_s_path_layer taz_s_circle_center
        taz_s_circle_ent taz_s_before_sweep taz_s_after_sweep
        taz_s_after_ed taz_s_result_ent)

    (setq taz_s_result_ent nil)

    (if (taz_s_is_sweepable_xref_curve taz_s_path_ent)
      (progn
        (setq taz_s_path_ed (entget taz_s_path_ent))
        (setq taz_s_path_layer (cdr (assoc 8 taz_s_path_ed)))
        (setq taz_s_circle_center (taz_s_get_xref_curve_point taz_s_path_ent))

        (if taz_s_circle_center
          (progn
            ;; Profil jest zwyklym kolem w plaszczyznie WCS.
            ;; Celowo NIE ustawiamy jego normalnej do stycznej sciezki.
            (setq taz_s_circle_ent
              (entmakex
                (list
                  (cons 0 "CIRCLE")
                  (cons 8 taz_s_path_layer)
                  (cons 10 taz_s_circle_center)
                  (cons 40 taz_s_xref_sweep_radius)
                  (cons 210 '(0.0 0.0 1.0))
                )
              )
            )

            (if taz_s_circle_ent
              (progn
                ;; Przy profilu D=0.001 wymuszamy mocne zblizenie i REGEN.
                (command "_ZOOM" "_OBJECT" taz_s_circle_ent "")
                (command "_ZOOM" "_SCALE" "1000X")
                (command "REGEN")

                (setq taz_s_before_sweep (entlast))

                ;; Zamkniety CIRCLE + tryb Solid = 3DSOLID.
                ;; Alignment=Yes zostawiamy, aby GstarCAD sam dopasowal
                ;; profil do kierunku sciezki.
                (command "_.SWEEP" "_MO" "_SO" taz_s_circle_ent "" "_A" "_Y" taz_s_path_ent)

                ;; Jesli konkretna krzywa pozostawila aktywny prompt,
                ;; anulujemy tylko ten SWEEP i kontynuujemy kolejne obiekty.
                (if (> (getvar "CMDACTIVE") 0)
                  (command)
                )

                (setq taz_s_after_sweep (entlast))

                (if
                  (and
                    taz_s_after_sweep
                    (not (equal taz_s_after_sweep taz_s_before_sweep))
                    (entget taz_s_after_sweep)
                  )
                  (progn
                    (setq taz_s_after_ed (entget taz_s_after_sweep))
                    (if (= (cdr (assoc 0 taz_s_after_ed)) "3DSOLID")
                      (progn
                        ;; Zachowujemy warstwe zrodlowej krzywej.
                        (if (assoc 8 taz_s_after_ed)
                          (entmod
                            (subst
                              (cons 8 taz_s_path_layer)
                              (assoc 8 taz_s_after_ed)
                              taz_s_after_ed
                            )
                          )
                        )
                        (setq taz_s_result_ent taz_s_after_sweep)
                      )
                    )
                  )
                )

                ;; Profil pomocniczy usuwamy niezaleznie od wyniku SWEEP.
                (if (entget taz_s_circle_ent)
                  (entdel taz_s_circle_ent)
                )
              )
            )
          )
        )
      )
    )

    taz_s_result_ent
  )

  (defun taz_s_prepare_xref_curve_solids
    (/ taz_s_curve_ss taz_s_curve_i taz_s_curve_ent
       taz_s_old_delobj taz_s_created_count taz_s_failed_count)

    (setq taz_s_created_count 0)
    (setq taz_s_failed_count 0)

    ;; Snapshot tylko typow, ktore swiadomie obslugujemy.
    (setq taz_s_curve_ss
      (ssget "_X"
        (list
          (cons -4 "<AND")
          (cons 67 0)
          (cons 0 "LINE,ARC,CIRCLE,ELLIPSE,LWPOLYLINE,POLYLINE,SPLINE")
          (cons -4 "AND>")
        )
      )
    )

    ;; Oryginalne krzywe musza zostac do koncowego sprzatania.
    (setq taz_s_old_delobj (getvar "DELOBJ"))
    (setvar "DELOBJ" 0)

    (if taz_s_curve_ss
      (progn
        (setq taz_s_curve_i 0)
        (while (< taz_s_curve_i (sslength taz_s_curve_ss))
          (setq taz_s_curve_ent (ssname taz_s_curve_ss taz_s_curve_i))

          (if (taz_s_is_sweepable_xref_curve taz_s_curve_ent)
            (if (taz_s_create_xref_sweep_solid taz_s_curve_ent)
              (setq taz_s_created_count (1+ taz_s_created_count))
              (setq taz_s_failed_count (1+ taz_s_failed_count))
            )
          )

          (setq taz_s_curve_i (1+ taz_s_curve_i))
        )
      )
    )

    (setvar "DELOBJ" taz_s_old_delobj)

    (if (> taz_s_created_count 0)
      (princ
        (strcat
          "\nPodklad SWEEP: utworzono "
          (itoa taz_s_created_count)
          " tymczasowych 3DSOLID (profil D=0.001)."
        )
      )
    )

    (if (> taz_s_failed_count 0)
      (princ
        (strcat
          "\nPodklad SWEEP: pominieto "
          (itoa taz_s_failed_count)
          " krzywych, dla ktorych SWEEP nie utworzyl 3DSOLID."
        )
      )
    )

    (princ)
  )

  ;; Konwersja odbywa sie PRZED taz_s_orig_cleanup_ss i taz_s_orig_ss.
  ;; Tymczasowe solidy automatycznie trafia wiec do istniejacego toru:
  ;; COPY -> INTERSECT -> SOLPROF -> xref visible / hidden,
  ;; a na koniec zostana usuniete razem z oryginalnym modelem.

  ;; ---------------------------------
  ;; PRAWDZIWY DWG XREF -> BIND -> EXPLODE
  ;;
  ;; Celowo prosta logika, bez NCOPY / VL / VLA / VLAX:
  ;;   1. znajdujemy bezposrednie INSERT-y bedace DWG XREF,
  ;;   2. zapamietujemy ich nazwy i encje,
  ;;   3. na czas BIND ustawiamy BINDTYPE=0,
  ;;   4. -XREF BIND zamienia definicje XREF na zwykle bloki,
  ;;   5. odblokowujemy warstwy i EXPLODE rozbija zapamietane INSERT-y,
  ;;   6. dalej istniejacy kod obsluguje powstale 3DSOLID-y i krzywe.
  ;;
  ;; BINDTYPE=0 zachowuje unikalne nazwy warstw zaleznch od XREF,
  ;; np. nazwa$0$taz_s_beam, zamiast laczyc je z lokalnym taz_s_beam.
  ;; ---------------------------------

  (defun taz_s_get_dwg_xref_name
    (taz_s_xref_ent / taz_s_xref_ed taz_s_xref_name taz_s_xref_block taz_s_xref_flags)

    (setq taz_s_xref_name nil)
    (setq taz_s_xref_flags 0)

    (if (and taz_s_xref_ent (entget taz_s_xref_ent))
      (progn
        (setq taz_s_xref_ed (entget taz_s_xref_ent))

        (if (= (cdr (assoc 0 taz_s_xref_ed)) "INSERT")
          (progn
            (setq taz_s_xref_name (cdr (assoc 2 taz_s_xref_ed)))

            (if taz_s_xref_name
              (setq taz_s_xref_block (tblsearch "BLOCK" taz_s_xref_name))
            )

            (if taz_s_xref_block
              (progn
                (if (assoc 70 taz_s_xref_block)
                  (setq taz_s_xref_flags (cdr (assoc 70 taz_s_xref_block)))
                )

                (if
                  (or
                    (/= (logand taz_s_xref_flags 4) 0)
                    (/= (logand taz_s_xref_flags 8) 0)
                  )
                  taz_s_xref_name
                  nil
                )
              )
              nil
            )
          )
          nil
        )
      )
      nil
    )
  )

  (defun taz_s_bind_explode_dwg_xrefs
    (/ taz_s_xref_ss taz_s_xref_i taz_s_xref_ent taz_s_xref_name
       taz_s_xref_items taz_s_xref_names taz_s_xref_pair
       taz_s_xref_bound_count taz_s_xref_exploded_count taz_s_xref_failed_count
       taz_s_old_bindtype)

    (setq taz_s_xref_items nil)
    (setq taz_s_xref_names nil)
    (setq taz_s_xref_bound_count 0)
    (setq taz_s_xref_exploded_count 0)
    (setq taz_s_xref_failed_count 0)

    ;; Snapshot wszystkich bezposrednich DWG XREF w Model Space.
    ;; Najpierw zapamietujemy encje, zanim pierwszy BIND zmieni definicje bloku.
    (setq taz_s_xref_ss
      (ssget "_X"
        (list
          (cons -4 "<AND")
          (cons 67 0)
          (cons 0 "INSERT")
          (cons -4 "AND>")
        )
      )
    )

    (if taz_s_xref_ss
      (progn
        (setq taz_s_xref_i 0)

        (while (< taz_s_xref_i (sslength taz_s_xref_ss))
          (setq taz_s_xref_ent (ssname taz_s_xref_ss taz_s_xref_i))
          (setq taz_s_xref_name (taz_s_get_dwg_xref_name taz_s_xref_ent))

          (if taz_s_xref_name
            (progn
              (setq taz_s_xref_items
                (cons (cons taz_s_xref_ent taz_s_xref_name) taz_s_xref_items)
              )

              (if (not (member taz_s_xref_name taz_s_xref_names))
                (setq taz_s_xref_names (cons taz_s_xref_name taz_s_xref_names))
              )
            )
          )

          (setq taz_s_xref_i (1+ taz_s_xref_i))
        )
      )
    )

    ;; Najpierw BIND kazdej definicji XREF tylko jeden raz.
    ;; BINDTYPE=0 tylko na czas tego etapu, aby warstwy XREF pozostaly unikalne.
    (setq taz_s_old_bindtype (getvar "BINDTYPE"))
    (setvar "BINDTYPE" 0)

    (while taz_s_xref_names
      (setq taz_s_xref_name (car taz_s_xref_names))

      (command "_.-XREF" "_BIND" taz_s_xref_name "")
      (setq taz_s_xref_bound_count (1+ taz_s_xref_bound_count))

      (setq taz_s_xref_names (cdr taz_s_xref_names))
    )

    ;; Po BIND warstwy zalezne sa juz lokalne. Odblokowujemy je przed EXPLODE,
    ;; aby powstale obiekty mogly normalnie przejsc przez dalsza maszynke.
    (command "_.-LAYER" "_UNLOCK" "*" "")

    ;; Po BIND zapamietane INSERT-y sa juz zwyklymi blokami i mozna je rozbic.
    (while taz_s_xref_items
      (setq taz_s_xref_pair (car taz_s_xref_items))
      (setq taz_s_xref_ent (car taz_s_xref_pair))

      (if (and taz_s_xref_ent (entget taz_s_xref_ent))
        (progn
          (command "_.EXPLODE" taz_s_xref_ent "")

          ;; Po udanym EXPLODE INSERT przestaje istniec.
          (if (entget taz_s_xref_ent)
            (setq taz_s_xref_failed_count (1+ taz_s_xref_failed_count))
            (setq taz_s_xref_exploded_count (1+ taz_s_xref_exploded_count))
          )
        )
        (setq taz_s_xref_failed_count (1+ taz_s_xref_failed_count))
      )

      (setq taz_s_xref_items (cdr taz_s_xref_items))
    )

    ;; Przywracamy ustawienie uzytkownika po zakonczeniu BIND + EXPLODE.
    (setvar "BINDTYPE" taz_s_old_bindtype)

    (if (> taz_s_xref_bound_count 0)
      (princ
        (strcat
          "\nXREF BIND: zwiazano "
          (itoa taz_s_xref_bound_count)
          " definicji DWG."
        )
      )
    )

    (if (> taz_s_xref_exploded_count 0)
      (princ
        (strcat
          "\nXREF EXPLODE: rozbito "
          (itoa taz_s_xref_exploded_count)
          " instancji DWG."
        )
      )
    )

    (if (> taz_s_xref_failed_count 0)
      (princ
        (strcat
          "\nXREF EXPLODE: nie udalo sie rozbic "
          (itoa taz_s_xref_failed_count)
          " instancji DWG."
        )
      )
    )

    (princ)
  )

  ;; Najpierw BIND + EXPLODE prawdziwych DWG XREF, aby ich geometria stala sie
  ;; zwyklymi obiektami modelu. Dopiero potem istniejaca funkcja zamienia
  ;; obslugiwane krzywe na tymczasowe 3DSOLID-y przez SWEEP.
  (taz_s_bind_explode_dwg_xrefs)

  (taz_s_prepare_xref_curve_solids)

  ;; ---------------------------------
  ;; ORYGINAL DO KONCOWEGO SPRZATANIA
  ;;
  ;; Zachowujemy dotychczasowe zachowanie pliku _DRAWINGS:
  ;; wszystko z oryginalnego modelu (poza osiami i warstwami roboczymi)
  ;; zostanie na koniec usuniete, nawet jezeli nie bierze udzialu
  ;; w generowaniu widokow.
  ;; ---------------------------------

  (setq taz_s_orig_cleanup_ss
    (ssget "X"
      (list
        (cons -4 "<AND")
        (cons 67 0)
        (cons -4 "<NOT") (cons 8 "taz_s_axes")               (cons -4 "NOT>")
        (cons -4 "<NOT") (cons 8 "taz_s_execution_design")   (cons -4 "NOT>")
        (cons -4 "<NOT") (cons 8 "taz_s_editing_layer")      (cons -4 "NOT>")
        (cons -4 "<NOT") (cons 8 "taz_s_xref_editing_layer") (cons -4 "NOT>")
        (cons -4 "AND>")
      )
    )
  )

  ;; ---------------------------------
  ;; SELEKCJA ELEMENTOW DO GENEROWANIA
  ;;
  ;; Wszystkie 3DSOLID z modelu dzielimy na trzy grupy:
  ;;   taz_s_beam / taz_s_plate -> elementy z danymi
  ;;   taz_s_axes                -> pomijane
  ;;   wszystkie inne warstwy   -> geometria referencyjna / podklad
  ;;
  ;; Wykluczamy dodatkowo warstwy robocze generatora.
  ;; ---------------------------------

  (setq taz_s_orig_ss
    (ssget "X"
      (list
        (cons -4 "<AND")
        (cons 67 0)
        (cons 0 "3DSOLID")
        (cons -4 "<NOT") (cons 8 "taz_s_axes")               (cons -4 "NOT>")
        (cons -4 "<NOT") (cons 8 "taz_s_execution_design")   (cons -4 "NOT>")
        (cons -4 "<NOT") (cons 8 "taz_s_editing_layer")      (cons -4 "NOT>")
        (cons -4 "<NOT") (cons 8 "taz_s_xref_editing_layer") (cons -4 "NOT>")
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

    (while
      (and
        taz_s_new_ent
        (< taz_s_map_index (length taz_s_orig_enames))
      )

      (setq taz_s_orig_ent_for_copy
        (nth taz_s_map_index taz_s_orig_enames)
      )
      (setq taz_s_orig_data_for_copy
        (entget taz_s_orig_ent_for_copy)
      )
      (setq taz_s_orig_layer_for_copy
        (cdr (assoc 8 taz_s_orig_data_for_copy))
      )

      ;; Tylko beam / plate maja dane elementu.
      ;; Podklad jest kopiowany geometrycznie, ale nie oczekujemy po nim
      ;; zadnych attr6 / attr7 / sweep_p1 / sweep_p2.
      (if (taz_s_is_data_layer taz_s_orig_layer_for_copy)
        (progn

          (setq taz_s_orig_h
            (cdr (assoc 5 taz_s_orig_data_for_copy))
          )
          (setq taz_s_new_h
            (cdr (assoc 5 (entget taz_s_new_ent)))
          )

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
        )
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
      (setq taz_s_orig_layer
        (cdr (assoc 8 (entget taz_s_orig_ent)))
      )

      ;; Beam / plate maja dane, dlatego tylko dla nich sprawdzamy
      ;; widocznosc przez -INTERFERE i tworzymy etykiety / tabele.
      ;; Podklad pomija caly ten blok i idzie od razu do INTERSECT.
      (if (taz_s_is_data_layer taz_s_orig_layer)
        (progn

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

        )
      )

      ;; Zawsze kopiuj bryle tnaca - oryginał zostaje nienaruszony
      (setq taz_s_cut_ss1 (ssadd))
      (ssadd taz_s_cut_ename taz_s_cut_ss1)
      (command "COPY" taz_s_cut_ss1 "" "0,0,0" "0,0,0")
      (setq taz_s_cut_work_ent (entlast))
      (setvar "CLAYER" "taz_s_editing_layer")
      (setq taz_s_int_ss (ssadd))
      (ssadd taz_s_cut_work_ent taz_s_int_ss)      
      (ssadd taz_s_target_ent   taz_s_int_ss)

      ;; Beam / plate -> zwykla geometria wykonawcza.
      ;; Podklad -> osobna geometria robocza do osobnego SOLPROF.
      (if (taz_s_is_xref_layer taz_s_orig_layer)
        (setq taz_s_intersect_layer "taz_s_xref_editing_layer")
        (setq taz_s_intersect_layer "taz_s_execution_design")
      )

      (command "_.CHPROP" taz_s_int_ss "" "LA" taz_s_intersect_layer "")
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
          (cons 0 "3DSOLID")
          (cons -4 "<NOT") (cons 8 "taz_s_axes")               (cons -4 "NOT>")
          (cons -4 "<NOT") (cons 8 "taz_s_execution_design")   (cons -4 "NOT>")
          (cons -4 "<NOT") (cons 8 "taz_s_editing_layer")      (cons -4 "NOT>")
          (cons -4 "<NOT") (cons 8 "taz_s_xref_editing_layer") (cons -4 "NOT>")
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

  ;; Lista encji utworzonych przez 1. przebieg SOLPROF (sam podklad),
  ;; zbierana ze wszystkich przypadkow IZO, X, Y, Z. Po zakonczeniu
  ;; wszystkich przebiegow te encje zostana usuniete - sluza tylko
  ;; jako material porownawczy dla taz_s_vide_correct_layers.
  (setq taz_s_vide_xref_pass_entities_to_delete '())

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
    (setq taz_s_izo_orig_layer (cdr (assoc 8 taz_s_izo_orig_data)))

    (if
      (and
        (= taz_s_izo_orig_type "3DSOLID")
        (taz_s_is_data_layer taz_s_izo_orig_layer)
      )
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

  ;; Zbierz tylko skopiowane bryly 3DSOLID i rozdziel je
  ;; na elementy z danymi oraz geometrie referencyjna / podklad.
  (setq taz_s_izo_enames (taz_s_collect_copy_enames))
  (setq taz_s_izo_ss (ssadd))
  (setq taz_s_izo_normal_ss (ssadd))
  (setq taz_s_izo_xref_ss (ssadd))
  (setq taz_s_izo_tmp taz_s_izo_enames)

  (while taz_s_izo_tmp

    (setq taz_s_izo_copy_ent (car taz_s_izo_tmp))
    (setq taz_s_izo_copy_layer
      (cdr (assoc 8 (entget taz_s_izo_copy_ent)))
    )

    (ssadd taz_s_izo_copy_ent taz_s_izo_ss)

    (if (taz_s_is_data_layer taz_s_izo_copy_layer)
      (ssadd taz_s_izo_copy_ent taz_s_izo_normal_ss)
    )

    ;; taz_s_unassigned to geometria modelu bez danych, a nie podklad,
    ;; wiec dla porzadku trafia do tego samego zestawu co beam / plate.
    (if (= (strcase taz_s_izo_copy_layer) "TAZ_S_UNASSIGNED")
      (ssadd taz_s_izo_copy_ent taz_s_izo_normal_ss)
    )

    (if (taz_s_is_xref_layer taz_s_izo_copy_layer)
      (ssadd taz_s_izo_copy_ent taz_s_izo_xref_ss)
    )

    (setq taz_s_izo_tmp (cdr taz_s_izo_tmp))
  )

  ;; Beam / plate ida do zwyklego SOLPROF.
  (if (> (sslength taz_s_izo_normal_ss) 0)
    (command "_.CHPROP" taz_s_izo_normal_ss "" "LA" "taz_s_execution_design" "")
  )

  ;; Podklad idzie do osobnego SOLPROF.
  (if (> (sslength taz_s_izo_xref_ss) 0)
    (command "_.CHPROP" taz_s_izo_xref_ss "" "LA" "taz_s_xref_editing_layer" "")
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
    (setq taz_s_izo_orig_layer (cdr (assoc 8 taz_s_izo_orig_data)))

    (if
      (and
        (= taz_s_izo_orig_type "3DSOLID")
        (taz_s_is_data_layer taz_s_izo_orig_layer)
      )
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
    )
  )

  ;; Najpierw podklad -> osobne warstwy xref_visible / xref_hidden.
  ;; Uwaga: bez ERASE tutaj - obiekty podkladu sa jeszcze potrzebne
  ;; w kolejnym, wspolnym przebiegu SOLPROF ponizej.
  (setq taz_s_vide_before_xref_pass nil)
  (if (> (sslength taz_s_izo_xref_ss) 0)
    (progn
      (setq taz_s_vide_before_xref_pass (taz_s_execution_design_get_last_entity))
      (setq taz_s_solprof_xref_mode T)
      (command "_.SOLPROF")
      (command taz_s_izo_xref_ss)
      (command "" "_Y" "_Y" "_Y")
      (taz_s_merge_solprof_layers)
      (setq taz_s_solprof_xref_mode nil)
      (setq taz_s_vide_xref_entities
        (taz_s_execution_design_collect_new_entities taz_s_vide_before_xref_pass)
      )
      (setq taz_s_vide_xref_pass_entities_to_delete
        (append
          taz_s_vide_xref_pass_entities_to_delete
          taz_s_vide_xref_entities
        )
      )
    )
  )

  ;; Potem podklad + beam / plate razem -> zwykle warstwy visible / hidden.
  (if (> (sslength taz_s_izo_ss) 0)
    (progn
      (setq taz_s_vide_before_combined_pass (taz_s_execution_design_get_last_entity))
      (setq taz_s_solprof_xref_mode nil)
      (command "_.SOLPROF")
      (command taz_s_izo_ss)
      (command "" "_Y" "_Y" "_Y")
      (command "_.ERASE" taz_s_izo_ss "")
      (taz_s_merge_solprof_layers)
      (taz_s_vide_correct_layers)
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
      (command "_.CHPROP" (entlast) "" "_LA" "taz_s_axes" "_LT" "Continuous" "")
      ;;(command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
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
    
    ;; Podklad -> osobny SOLPROF.
    ;; Uwaga: bez ERASE tutaj - obiekty podkladu sa jeszcze potrzebne
    ;; w kolejnym, wspolnym przebiegu SOLPROF ponizej.
    (setq taz_s_solprof_xref_ss
      (ssget "_X" (list (cons 8 "taz_s_xref_editing_layer")))
    )

    (setq taz_s_vide_before_xref_pass nil)
    (if taz_s_solprof_xref_ss
      (progn
        (setq taz_s_vide_before_xref_pass (taz_s_execution_design_get_last_entity))
        (setq taz_s_solprof_xref_mode T)
        (command "_.SOLPROF")
        (command taz_s_solprof_xref_ss)
        (command "" "_Y" "_Y" "_Y")
        (taz_s_merge_solprof_layers)
        (setq taz_s_solprof_xref_mode nil)
        (setq taz_s_vide_xref_entities
          (taz_s_execution_design_collect_new_entities taz_s_vide_before_xref_pass)
        )
        (setq taz_s_vide_xref_pass_entities_to_delete
          (append
            taz_s_vide_xref_pass_entities_to_delete
            taz_s_vide_xref_entities
          )
        )
      )
    )

    ;; Podklad + beam / plate razem -> wspolny SOLPROF.
    (setq taz_s_solprof_ss
      (ssget "_X"
        (list
          (cons -4 "<OR")
          (cons 8 "taz_s_execution_design")
          (cons 8 "taz_s_xref_editing_layer")
          (cons -4 "OR>")
        )
      )
    )

    (if taz_s_solprof_ss
      (progn
        (setq taz_s_vide_before_combined_pass (taz_s_execution_design_get_last_entity))
        (setq taz_s_solprof_xref_mode nil)
        (command "_.SOLPROF")
        (command taz_s_solprof_ss)
        (command "" "_Y" "_Y" "_Y")
        (command "_.ERASE" taz_s_solprof_ss "")
        (taz_s_merge_solprof_layers)
        (taz_s_vide_correct_layers)
      )
    )

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
      (command "_.CHPROP" (entlast) "" "_LA" "taz_s_axes" "_LT" "Continuous" "")
      ;;(command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
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
    
    ;; Podklad -> osobny SOLPROF.
    ;; Uwaga: bez ERASE tutaj - obiekty podkladu sa jeszcze potrzebne
    ;; w kolejnym, wspolnym przebiegu SOLPROF ponizej.
    (setq taz_s_solprof_xref_ss
      (ssget "_X" (list (cons 8 "taz_s_xref_editing_layer")))
    )

    (setq taz_s_vide_before_xref_pass nil)
    (if taz_s_solprof_xref_ss
      (progn
        (setq taz_s_vide_before_xref_pass (taz_s_execution_design_get_last_entity))
        (setq taz_s_solprof_xref_mode T)
        (command "_.SOLPROF")
        (command taz_s_solprof_xref_ss)
        (command "" "_Y" "_Y" "_Y")
        (taz_s_merge_solprof_layers)
        (setq taz_s_solprof_xref_mode nil)
        (setq taz_s_vide_xref_entities
          (taz_s_execution_design_collect_new_entities taz_s_vide_before_xref_pass)
        )
        (setq taz_s_vide_xref_pass_entities_to_delete
          (append
            taz_s_vide_xref_pass_entities_to_delete
            taz_s_vide_xref_entities
          )
        )
      )
    )

    ;; Podklad + beam / plate razem -> wspolny SOLPROF.
    (setq taz_s_solprof_ss
      (ssget "_X"
        (list
          (cons -4 "<OR")
          (cons 8 "taz_s_execution_design")
          (cons 8 "taz_s_xref_editing_layer")
          (cons -4 "OR>")
        )
      )
    )

    (if taz_s_solprof_ss
      (progn
        (setq taz_s_vide_before_combined_pass (taz_s_execution_design_get_last_entity))
        (setq taz_s_solprof_xref_mode nil)
        (command "_.SOLPROF")
        (command taz_s_solprof_ss)
        (command "" "_Y" "_Y" "_Y")
        (command "_.ERASE" taz_s_solprof_ss "")
        (taz_s_merge_solprof_layers)
        (taz_s_vide_correct_layers)
      )
    )

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
      (command "_.CHPROP" (entlast) "" "_LA" "taz_s_axes" "_LT" "Continuous" "")
      ;;(command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      (command "_.TEXT" "_J" "_MC" taz_s_circle_center taz_s_annotation_scale_axis 0 taz_s_axis_name)
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      ;; górne kółko
      (setq taz_s_circle_center
            (list taz_s_x
                  (+ taz_s_ymax taz_s_circle_radius)
                  (+ taz_s_z taz_s_zoffset)))
      (command "_.CIRCLE" taz_s_circle_center taz_s_circle_radius)
      (command "_.CHPROP" (entlast) "" "_LA" "taz_s_axes" "_LT" "Continuous" "")
      ;;(command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
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
      (command "_.CHPROP" (entlast) "" "_LA" "taz_s_axes" "_LT" "Continuous" "")
      ;;(command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      (command "_.TEXT" "_J" "_MC" taz_s_circle_center taz_s_annotation_scale_axis 0 taz_s_axis_name)
      (command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
      ;; prawe kółko
      (setq taz_s_circle_center
            (list (+ taz_s_xmax taz_s_circle_radius)
                  taz_s_y
                  (+ taz_s_z taz_s_zoffset)))
      (command "_.CIRCLE" taz_s_circle_center taz_s_circle_radius)
      (command "_.CHPROP" (entlast) "" "_LA" "taz_s_axes" "_LT" "Continuous" "")
      ;;(command "_.CHPROP" (entlast) "" "LA" "taz_s_axes" "")
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
    
    ;; Podklad -> osobny SOLPROF.
    ;; Uwaga: bez ERASE tutaj - obiekty podkladu sa jeszcze potrzebne
    ;; w kolejnym, wspolnym przebiegu SOLPROF ponizej.
    (setq taz_s_solprof_xref_ss
      (ssget "_X" (list (cons 8 "taz_s_xref_editing_layer")))
    )

    (setq taz_s_vide_before_xref_pass nil)
    (if taz_s_solprof_xref_ss
      (progn
        (setq taz_s_vide_before_xref_pass (taz_s_execution_design_get_last_entity))
        (setq taz_s_solprof_xref_mode T)
        (command "_.SOLPROF")
        (command taz_s_solprof_xref_ss)
        (command "" "_Y" "_Y" "_Y")
        (taz_s_merge_solprof_layers)
        (setq taz_s_solprof_xref_mode nil)
        (setq taz_s_vide_xref_entities
          (taz_s_execution_design_collect_new_entities taz_s_vide_before_xref_pass)
        )
        (setq taz_s_vide_xref_pass_entities_to_delete
          (append
            taz_s_vide_xref_pass_entities_to_delete
            taz_s_vide_xref_entities
          )
        )
      )
    )

    ;; Podklad + beam / plate razem -> wspolny SOLPROF.
    (setq taz_s_solprof_ss
      (ssget "_X"
        (list
          (cons -4 "<OR")
          (cons 8 "taz_s_execution_design")
          (cons 8 "taz_s_xref_editing_layer")
          (cons -4 "OR>")
        )
      )
    )

    (if taz_s_solprof_ss
      (progn
        (setq taz_s_vide_before_combined_pass (taz_s_execution_design_get_last_entity))
        (setq taz_s_solprof_xref_mode nil)
        (command "_.SOLPROF")
        (command taz_s_solprof_ss)
        (command "" "_Y" "_Y" "_Y")
        (command "_.ERASE" taz_s_solprof_ss "")
        (taz_s_merge_solprof_layers)
        (taz_s_vide_correct_layers)
      )
    )

    (command "_pspace")
    (command "_layout" "_S" "Model")
    
  )

  (command "-LAYDEL" "N" "taz_s_execution_design" "" "_Y")
  (taz_s_merge_solprof_layers)

  ;; ---------------------------------
  ;; USUN ENCJE Z 1. PRZEBIEGU SOLPROF DLA PODKLADU
  ;; ---------------------------------
  ;; Te encje sluzyly tylko jako material porownawczy dla
  ;; taz_s_vide_correct_layers (patrz przypadki IZO, X, Y, Z powyzej).
  ;; Porownanie linii zostalo juz wykonane dla wszystkich przypadkow,
  ;; wiec teraz mozna je bezpiecznie usunac.

  (setq taz_s_vide_xref_pass_delete_walk_list taz_s_vide_xref_pass_entities_to_delete)

  (while taz_s_vide_xref_pass_delete_walk_list

    (setq taz_s_vide_xref_pass_current_entity (car taz_s_vide_xref_pass_delete_walk_list))

    (if (and taz_s_vide_xref_pass_current_entity (entget taz_s_vide_xref_pass_current_entity))
      (entdel taz_s_vide_xref_pass_current_entity)
    )

    (setq taz_s_vide_xref_pass_delete_walk_list (cdr taz_s_vide_xref_pass_delete_walk_list))
  )

  ;; ---------------------------------
  ;; USUN ORYGINALNY MODEL I ORYGINALNE OSIE
  ;; ---------------------------------

  (command "_layout" "_S" "Model")

  ;; Obiekty pochodzace z BIND moga nadal lezec na warstwach, ktore byly
  ;; zablokowane w pliku XREF. Odblokowujemy wszystko tylko na czas sprzatania.
  (command "_.-LAYER" "_UNLOCK" "*" "")

  (if taz_s_orig_cleanup_ss
    (command "_.ERASE" taz_s_orig_cleanup_ss "")
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
;; ORGANIZER WIDOKOW - PLIK ZEWNETRZNY
;; ============================================================================
;; Funkcje porzadkujace widoki, w tym m.in.:
;;   - taz_s_create_drawings_execution_design_organize (wywolywana wyzej),
;;   - taz_s_create_steel_table (wywolywana wyzej, zestawienie stali),
;; zostaly wydzielone do osobnego pliku:
;;   taz_s_create_drawings_execution_design_organize.lsp
;; Ponizej nastepuje jego automatyczne wczytanie, jesli nie jest on jeszcze
;; zaladowany w biezacej sesji (plik musi byc dostepny w sciezce wsparcia
;; programu, tak samo jak taz_s_create_drawings_execution_design.dcl).
;; ============================================================================

;;(if (not (boundp 'taz_s_create_drawings_execution_design_organize))
  ;;(if (findfile "taz_s_create_drawings_execution_design_organize.lsp")
    ;;(load "taz_s_create_drawings_execution_design_organize.lsp")
  ;;)
;;)
