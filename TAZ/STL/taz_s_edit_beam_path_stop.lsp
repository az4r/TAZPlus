;; =============================================================
;; taz_s_edit_beam_path_stop
;; Zamknięcie trybu edycji ścieżki wyciągnięcia belki.
;; Przywraca przezroczystość bryły, usuwa linię sterującą,
;; wyłącza reaktor i czyści zmienne trybu edycji.
;; Czyści też warstwę taz_s_editing_layer ze wszystkich nadmiarowych
;; obiektów (np. kopii linii sterującej wykonanych komendą COPY).
;; =============================================================

(defun c:taz_s_edit_beam_path_stop ( / )

  ;; przywróć normalną przezroczystość bryły
  ;;(if (and taz_s_attribs_object (entget taz_s_attribs_object))
    ;;(command "_CHPROP" taz_s_attribs_object "" "_P" "_TR" "0" "")
  ;;)

  ;; usuń czerwoną linię sterującą
  (if (and taz_s_attribs_line (entget taz_s_attribs_line))
    (entdel taz_s_attribs_line)
  )

  ;; wyczyść zmienne trybu edycji
  (setq taz_s_attribs_line nil)
  (setq taz_s_attribs_object_old nil)
  (setq taz_s_edit_mode nil)
  (setq taz_s_edit_beam_path_mode nil)

  ;; ---------------------------------------------------------
  ;; Wyłącz reaktor
  ;; ---------------------------------------------------------

  (if taz_s_edit_beam_path_reactor
    (progn
      (vlr-remove taz_s_edit_beam_path_reactor)
      (setq taz_s_edit_beam_path_reactor nil)
    )
  )

  ;; ---------------------------------------------------------
  ;; Posprzątaj warstwę edycji
  ;; Usuń wszystko, co zostało na warstwie taz_s_editing_layer
  ;; (np. kopie linii sterującej wykonane komendą COPY, OFFSET,
  ;; MIRROR, ARRAY albo kopiowaniem uchwytem). Główna linia
  ;; sterująca jest już usunięta wyżej, więc tu trafiają same
  ;; nadmiarowe obiekty. Używamy entdel (bez command), żeby
  ;; sprzątanie nie wywoływało żadnych zdarzeń komend.
  ;; ---------------------------------------------------------

  (setq taz_s_stop_cleanup_selection (ssget "_X" (list (cons 8 "taz_s_editing_layer"))))

  (if taz_s_stop_cleanup_selection
    (progn
      (setq taz_s_stop_cleanup_count (sslength taz_s_stop_cleanup_selection))
      (setq taz_s_stop_cleanup_index 0)

      (while (< taz_s_stop_cleanup_index taz_s_stop_cleanup_count)
        (setq taz_s_stop_cleanup_entity (ssname taz_s_stop_cleanup_selection taz_s_stop_cleanup_index))
        (entdel taz_s_stop_cleanup_entity)
        (setq taz_s_stop_cleanup_index (+ taz_s_stop_cleanup_index 1))
      )

      (princ (strcat "\nUsunięto nadmiarowe obiekty z warstwy edycji: " (itoa taz_s_stop_cleanup_count)))
    )
    (princ)
  )

  (setq taz_s_stop_cleanup_selection nil)
  (setq taz_s_stop_cleanup_count nil)
  (setq taz_s_stop_cleanup_index nil)
  (setq taz_s_stop_cleanup_entity nil)

  (princ "\nEdycja ścieżki zamknięta – linia usunięta, bryła przywrócona.")

  (command "_LAYER" "_LO" "taz_s_editing_layer" "")
  ;;(taz_s_current_settings_restore)
  
  ;; WARSTWA
  (setvar "CLAYER" taz_s_current_layer)
  
  ;; UCS
  (if taz_s_current_ucs_exist
    (command "_.UCS" "_NA" "_R" "taz_s_current_ucs")
    (princ)
  )

  (princ)

)
