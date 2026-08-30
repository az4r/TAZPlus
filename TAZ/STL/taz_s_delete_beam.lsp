;; ============================================================
;; TAZ_S_DELETE_BEAM.LSP  (wersja uproszczona)
;;
;; Co robi:
;;  1. Pobiera zaznaczony obiekt (bryla 3D) i jego uchwyt (handle)
;;  2. Wczytuje plik taz_s_data_file (taz_s_beam_data.txt)
;;  3. Czyta plik linia po linii i pomija linie dotyczace
;;     zaznaczonej bryly (rozpoznawane po handle w nazwie zmiennej)
;;  4. Zapisuje plik ponownie - juz bez wpisow usuwanej bryly
;;  5. Czysci rowniez odpowiadajace zmienne w pamieci (ustawia na nil)
;;  6. Usuwa (entdel) zaznaczona bryle z rysunku
;;
;; Uwaga: to NIE jest pelne "sprzatanie" bazy (np. wpisow po
;; brylach usunietych recznie z rysunku bez tego skryptu) -
;; do tego sluzy taz_s_rebuild_data. Jesli chcemy pelne
;; posprzatanie, wystarczy na koncu odpalic (c:taz_s_rebuild_data)
;; - patrz zakomentowana linia na dole.
;; ============================================================

(defun c:taz_s_delete_beam ()

  (taz_s_current_settings_save)

  ;; ---------------------------------------------------------
  ;; SCIEZKA DO PLIKU DANYCH
  ;; ---------------------------------------------------------

  (setq taz_s_data_file
    (strcat (taz_s_path) "taz_s_beam_data.txt"))

  ;; ---------------------------------------------------------
  ;; POBIERZ ZAZNACZENIE
  ;; ---------------------------------------------------------

  (setq taz_s_delete_selection (ssget "_I"))

  (if (and taz_s_delete_selection
           (> (sslength taz_s_delete_selection) 1))
    (progn
      (sssetfirst nil nil)
      (setq taz_s_delete_selection (ssget "_+.:E:S"))
    )
  )

  (if (null taz_s_delete_selection)
    (setq taz_s_delete_selection (ssget "_+.:E:S"))
  )

  (if (null taz_s_delete_selection)
    (progn
      (print "Nie wybrano obiektu.")
      (taz_s_current_settings_restore)
      (exit)
    )
  )

  (setq taz_s_delete_object (ssname taz_s_delete_selection 0))

  ;; sprawdz typ
  (if (/= (cdr (assoc 0 (entget taz_s_delete_object))) "3DSOLID")
    (progn
      (print "Wybrany obiekt nie jest bryla 3D.")
      (taz_s_current_settings_restore)
      (exit)
    )
  )

  ;; uchwyt bryly do usuniecia + fragment po ktorym rozpoznamy jej linie w pliku
  (setq taz_s_delete_object_handle
        (cdr (assoc 5 (entget taz_s_delete_object))))
  (setq taz_s_delete_tag
        (strcat "taz_s_" taz_s_delete_object_handle "_"))

  ;; ---------------------------------------------------------
  ;; ZALADUJ BAZE (jak dotychczas - dane trafiaja do pamieci)
  ;; ---------------------------------------------------------

  (load taz_s_data_file)

  ;; ---------------------------------------------------------
  ;; WCZYTAJ PLIK LINIA PO LINII, POMIJAJAC WPISY USUWANEJ BRYLY
  ;; ---------------------------------------------------------

  (setq taz_s_lines_kept nil)
  (setq taz_s_f_read (open taz_s_data_file "r"))

  (while (setq taz_s_line (read-line taz_s_f_read))
    (if (not (wcmatch taz_s_line (strcat "*" taz_s_delete_tag "*")))
      (setq taz_s_lines_kept (cons taz_s_line taz_s_lines_kept))
    )
  )

  (close taz_s_f_read)
  (setq taz_s_lines_kept (reverse taz_s_lines_kept))

  ;; ---------------------------------------------------------
  ;; ZAPISZ PLIK PONOWNIE - BEZ WPISOW USUWANEJ BRYLY
  ;; ---------------------------------------------------------

  (setq taz_s_f_write (open taz_s_data_file "w"))

  (foreach taz_s_line taz_s_lines_kept
    (write-line taz_s_line taz_s_f_write)
  )

  (close taz_s_f_write)

  ;; ---------------------------------------------------------
  ;; WYCZYSC DANE USUWANEJ BRYLY TAKZE Z PAMIECI
  ;; ---------------------------------------------------------

  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_attr1"))  nil)
  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_attr2"))  nil)
  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_attr3"))  nil)
  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_attr4"))  nil)
  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_attr5"))  nil)
  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_attr6"))  nil)
  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_attr7"))  nil)
  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_attr8"))  nil)
  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_attr9"))  nil)
  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_attr10")) nil)
  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_section_angle"))    nil)
  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_section_position")) nil)
  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_sweep_p1")) nil)
  (set (read (strcat "taz_s_" taz_s_delete_object_handle "_sweep_p2")) nil)

  ;; ---------------------------------------------------------
  ;; USUN BRYLE Z RYSUNKU
  ;; ---------------------------------------------------------

  (command "_LAYER" "_U" "taz_s_beam" "")
  (if (and taz_s_delete_object (entget taz_s_delete_object))
    (entdel taz_s_delete_object)
  )
  (command "_LAYER" "_LO" "taz_s_beam" "")

  ;; ---------------------------------------------------------
  ;; SPRZATANIE ZMIENNYCH POMOCNICZYCH
  ;; ---------------------------------------------------------

  (setq taz_s_delete_selection nil)
  (setq taz_s_delete_object nil)
  (setq taz_s_delete_object_handle nil)
  (setq taz_s_delete_tag nil)
  (setq taz_s_lines_kept nil)

  ;; -- opcjonalnie: pelne posprzatanie bazy na podstawie stanu modelu --
  ;; (c:taz_s_rebuild_data)

  (taz_s_current_settings_restore)

  (princ)
)
(princ)
