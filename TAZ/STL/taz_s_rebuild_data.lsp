;; ============================================================
;; TAZ_S_REBUILD_data.LSP  (wersja prosta - bez vl-, zmienne globalne)
;;
;; Co robi:
;;  1. Wczytuje plik taz_s_data_file (setq wykonuja sie po kolei,
;;     wiec w pamieci zostaja same aktualne/ostatnie wartosci)
;;  2. Szuka na rysunku bryl 3D na warstwie "taz_s_beam"
;;  3. Czysci plik i zapisuje go od nowa - dla kazdej bryly z
;;     rysunku, dane pobrane z pamieci, w tej samej kolejnosci
;;     jak w oryginalnym kodzie zapisujacym
;;     (attr1..attr10, angle, position, p1, p2)
;; ============================================================

(defun c:taz_s_rebuild_data ()

  ;; -- jesli nie ma jeszcze ustawionej sciezki do pliku danych, pytamy --
  (if (= taz_s_data_file nil)
    (setq taz_s_data_file (getfiled "Wskaz plik danych taz_s (txt)" "" "txt" 4))
  )

  (if (= taz_s_data_file nil)

    (princ "\nAnulowano - brak pliku danych.")

    (progn

      ;; -- 1. wczytujemy plik - w pamieci zostana tylko ostatnie wartosci --
      (load taz_s_data_file)

      ;; -- 2. szukamy bryl 3D na warstwie taz_s_beam --
      (setq taz_s_selection_set (ssget "_X" '((0 . "3DSOLID") (8 . "taz_s_beam"))))

      (if (= taz_s_selection_set nil)

        (princ "\nNie znaleziono zadnych bryl 3D na warstwie taz_s_beam.")

        (progn

          ;; -- otwieramy plik do zapisu, tryb "w" czysci cala zawartosc --
          (setq taz_s_output_file (open taz_s_data_file "w"))

          (setq taz_s_ok_count 0)
          (setq taz_s_selection_count (sslength taz_s_selection_set))
          (setq taz_s_selection_index 0)

          (while (< taz_s_selection_index taz_s_selection_count)

            (setq taz_s_entity_name (ssname taz_s_selection_set taz_s_selection_index))
            (setq taz_s_entity_data (entget taz_s_entity_name))
            (setq taz_s_entity_handle (cdr (assoc 5 taz_s_entity_data)))

            ;; -- pobieramy z pamieci aktualne wartosci tej bryly --
            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_attr1")))
            (setq taz_s_current_attr1 (eval taz_s_var_symbol))

            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_attr2")))
            (setq taz_s_current_attr2 (eval taz_s_var_symbol))

            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_attr3")))
            (setq taz_s_current_attr3 (eval taz_s_var_symbol))

            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_attr4")))
            (setq taz_s_current_attr4 (eval taz_s_var_symbol))

            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_attr5")))
            (setq taz_s_current_attr5 (eval taz_s_var_symbol))

            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_attr6")))
            (setq taz_s_current_attr6 (eval taz_s_var_symbol))

            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_attr7")))
            (setq taz_s_current_attr7 (eval taz_s_var_symbol))

            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_attr8")))
            (setq taz_s_current_attr8 (eval taz_s_var_symbol))

            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_attr9")))
            (setq taz_s_current_attr9 (eval taz_s_var_symbol))

            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_attr10")))
            (setq taz_s_current_attr10 (eval taz_s_var_symbol))

            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_section_angle")))
            (setq taz_s_current_section_angle (eval taz_s_var_symbol))

            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_section_position")))
            (setq taz_s_current_section_position (eval taz_s_var_symbol))

            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_sweep_p1")))
            (setq taz_s_current_sweep_p1 (eval taz_s_var_symbol))

            (setq taz_s_var_symbol (read (strcat "taz_s_" taz_s_entity_handle "_sweep_p2")))
            (setq taz_s_current_sweep_p2 (eval taz_s_var_symbol))

            ;; -- zapisujemy komplet danych tej bryly do pliku --
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_attr1 \"" taz_s_current_attr1 "\")") taz_s_output_file)
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_attr2 \"" taz_s_current_attr2 "\")") taz_s_output_file)
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_attr3 \"" taz_s_current_attr3 "\")") taz_s_output_file)
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_attr4 \"" taz_s_current_attr4 "\")") taz_s_output_file)
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_attr5 \"" taz_s_current_attr5 "\")") taz_s_output_file)
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_attr6 \"" taz_s_current_attr6 "\")") taz_s_output_file)
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_attr7 \"" taz_s_current_attr7 "\")") taz_s_output_file)
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_attr8 \"" taz_s_current_attr8 "\")") taz_s_output_file)
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_attr9 \"" taz_s_current_attr9 "\")") taz_s_output_file)
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_attr10 \"" taz_s_current_attr10 "\")") taz_s_output_file)
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_section_angle " (itoa taz_s_current_section_angle) ")") taz_s_output_file)
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_section_position " (itoa taz_s_current_section_position) ")") taz_s_output_file)

            (setq taz_s_p1x (car taz_s_current_sweep_p1))
            (setq taz_s_p1y (cadr taz_s_current_sweep_p1))
            (setq taz_s_p1z (caddr taz_s_current_sweep_p1))
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_sweep_p1 (list " (rtos taz_s_p1x 2 6) " " (rtos taz_s_p1y 2 6) " " (rtos taz_s_p1z 2 6) "))") taz_s_output_file)

            (setq taz_s_p2x (car taz_s_current_sweep_p2))
            (setq taz_s_p2y (cadr taz_s_current_sweep_p2))
            (setq taz_s_p2z (caddr taz_s_current_sweep_p2))
            (write-line (strcat "(setq taz_s_" taz_s_entity_handle "_sweep_p2 (list " (rtos taz_s_p2x 2 6) " " (rtos taz_s_p2y 2 6) " " (rtos taz_s_p2z 2 6) "))") taz_s_output_file)

            (setq taz_s_ok_count (+ taz_s_ok_count 1))

            (setq taz_s_selection_index (+ taz_s_selection_index 1))
          )

          (close taz_s_output_file)

          (princ (strcat "\nGotowe. Zapisano danych dla: " (itoa taz_s_ok_count) " bryl."))
        )
      )
    )
  )

  (princ)
)
(princ)
