;; =============================================================
;; taz_s_edit_beam_path_stop
;; Zamknięcie trybu edycji ścieżki wyciągnięcia belki.
;; Przywraca przezroczystość bryły, usuwa linię sterującą,
;; wyłącza reaktor i czyści zmienne trybu edycji.
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
