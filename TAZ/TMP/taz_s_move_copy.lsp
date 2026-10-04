;; =========================================================
;; taz_s_move_copy.lsp
;; Komenda: TAZ_S_MOVE_COPY
;;
;; ETAP 1 - wersja robocza:
;;   1. Wybór obiektów. Można zaznaczyć zarówno obiekty modelu
;;      (z atrybutami), jak i obiekty bez atrybutów.
;;   2. Z zaznaczenia powstaje lista ename.
;;   3. Okno DCL (plik taz_s_move_copy.dcl):
;;        - radio button: Move / Copy
;;        - pola tekstowe: X, Y, Z
;;        - przycisk Point (na razie bez działania)
;;        - przyciski OK i Anuluj
;;   4. OK     -> alert z ename obiektów, trybem (Move / Copy)
;;                oraz wartościami X, Y, Z z okna DCL
;;      Anuluj -> przerwanie działania skryptu
;;
;; Na tym etapie skrypt niczego nie zmienia w rysunku.
;; =========================================================

(defun c:taz_s_move_copy ()

  ;; ---------------------------------------------------------
  ;; ZMIENNE GLOBALNE - WARTOŚCI POCZĄTKOWE
  ;; ---------------------------------------------------------

  (setq taz_s_move_copy_can_continue T)

  (setq taz_s_move_copy_selection_set nil)
  (setq taz_s_move_copy_ename_list nil)
  (setq taz_s_move_copy_current_ename nil)
  (setq taz_s_move_copy_index 0)

  ;; tryb: "1" = Move (przesuń), "0" = Copy (kopiuj)
  (setq taz_s_move_copy_mode "1")

  ;; wartości przesunięcia - jako tekst, tak jak w polach okna DCL
  (setq taz_s_move_copy_x "0")
  (setq taz_s_move_copy_y "0")
  (setq taz_s_move_copy_z "0")

  (setq taz_s_move_copy_dcl_id 0)

  ;; wynik okna DCL: 0 = Anuluj, 1 = OK
  (setq taz_s_move_copy_dialog_result 0)

  ;; ---------------------------------------------------------
  ;; WYBÓR OBIEKTÓW
  ;; Bez żadnego filtra - można wybrać dowolne obiekty:
  ;; obiekty modelu (z atrybutami) i obiekty bez atrybutów.
  ;; ---------------------------------------------------------

  (setq taz_s_move_copy_selection_set (ssget))

  (if (null taz_s_move_copy_selection_set)
    (progn
      (alert "Nie wybrano żadnych obiektów.")
      (setq taz_s_move_copy_can_continue nil)
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; LISTA ENAME ZAZNACZONYCH OBIEKTÓW
  ;; Lista powstaje od razu po wyborze, przed oknem dialogowym.
  ;; ---------------------------------------------------------

  (if taz_s_move_copy_can_continue
    (progn

      (setq taz_s_move_copy_ename_list nil)
      (setq taz_s_move_copy_index 0)

      (while (< taz_s_move_copy_index (sslength taz_s_move_copy_selection_set))
        (setq taz_s_move_copy_current_ename (ssname taz_s_move_copy_selection_set taz_s_move_copy_index))
        (setq taz_s_move_copy_ename_list (append taz_s_move_copy_ename_list (list taz_s_move_copy_current_ename)))
        (setq taz_s_move_copy_index (+ taz_s_move_copy_index 1))
      )

      ;; zbiór zaznaczenia nie jest już potrzebny - dalej pracujemy na liście
      (setq taz_s_move_copy_selection_set nil)

    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; OKNO DIALOGOWE
  ;; Wynik: 0 = Anuluj, 1 = OK
  ;; ---------------------------------------------------------

  (if taz_s_move_copy_can_continue
    (progn

      (setq taz_s_move_copy_dcl_id (load_dialog "taz_s_move_copy.dcl"))
      (new_dialog "taz_s_move_copy_dialog" taz_s_move_copy_dcl_id)

      ;; stan początkowy radio buttonów
      (if (= taz_s_move_copy_mode "1")
        (progn
          (set_tile "taz_s_mode_move" "1")
          (set_tile "taz_s_mode_copy" "0")
        )
        (progn
          (set_tile "taz_s_mode_move" "0")
          (set_tile "taz_s_mode_copy" "1")
        )
      )

      ;; wartości początkowe pól tekstowych
      (set_tile "taz_s_x" taz_s_move_copy_x)
      (set_tile "taz_s_y" taz_s_move_copy_y)
      (set_tile "taz_s_z" taz_s_move_copy_z)

      ;; wybór trybu
      (action_tile "taz_s_mode_move" "(setq taz_s_move_copy_mode \"1\")")
      (action_tile "taz_s_mode_copy" "(setq taz_s_move_copy_mode \"0\")")

      ;; przycisk Point - na razie bez logiki, kliknięcie niczego nie robi
      (action_tile "taz_s_point" "(princ)")

      ;; OK - odczytaj pola i zamknij okno z wynikiem 1
      (action_tile "accept" "(taz_s_move_copy_read_values)(done_dialog 1)")

      ;; Anuluj - zamknij okno z wynikiem 0
      (action_tile "cancel" "(done_dialog 0)")

      (setq taz_s_move_copy_dialog_result (start_dialog))

      (unload_dialog taz_s_move_copy_dcl_id)

    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; OK -> ALERT Z PODSUMOWANIEM
  ;; ---------------------------------------------------------

  (if (and taz_s_move_copy_can_continue (= taz_s_move_copy_dialog_result 1))
    (taz_s_move_copy_show_alert)
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; ANULUJ -> PRZERWANIE DZIAŁANIA SKRYPTU
  ;; ---------------------------------------------------------

  (if (and taz_s_move_copy_can_continue (= taz_s_move_copy_dialog_result 0))
    (princ "\nPrzerwano działanie skryptu (Anuluj).")
    (princ)
  )

  (princ)

)

;; ---------------------------------------------------------
;; ODCZYT WARTOŚCI Z OKNA DIALOGOWEGO
;; Wywoływane po kliknięciu OK (z wnętrza action_tile).
;; Puste pole oznacza wartość 0.
;; ---------------------------------------------------------

(defun taz_s_move_copy_read_values ()

  (setq taz_s_move_copy_x (get_tile "taz_s_x"))
  (setq taz_s_move_copy_y (get_tile "taz_s_y"))
  (setq taz_s_move_copy_z (get_tile "taz_s_z"))

  (if (= taz_s_move_copy_x "") (setq taz_s_move_copy_x "0") (princ))
  (if (= taz_s_move_copy_y "") (setq taz_s_move_copy_y "0") (princ))
  (if (= taz_s_move_copy_z "") (setq taz_s_move_copy_z "0") (princ))

  (princ)

)

;; ---------------------------------------------------------
;; ALERT Z PODSUMOWANIEM
;; Wypisuje: ename zaznaczonych obiektów, tryb (Move / Copy)
;; oraz wartości przesunięcia X, Y, Z pobrane z okna DCL.
;; ---------------------------------------------------------

(defun taz_s_move_copy_show_alert ()

  ;; liczba obiektów na liście
  (setq taz_s_move_copy_alert_count (length taz_s_move_copy_ename_list))
  (setq taz_s_move_copy_alert_count_text (itoa taz_s_move_copy_alert_count))

  ;; tryb jako tekst
  (if (= taz_s_move_copy_mode "1")
    (setq taz_s_move_copy_alert_mode_text "MOVE (przesunięcie)")
    (setq taz_s_move_copy_alert_mode_text "COPY (kopiowanie)")
  )

  ;; budowanie tekstu alertu - jeden krok = jeden fragment tekstu
  (setq taz_s_move_copy_alert_text "")

  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text "Liczba zaznaczonych obiektów: "))
  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text taz_s_move_copy_alert_count_text))
  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text "\n\n"))

  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text "Ename zaznaczonych obiektów:\n"))

  ;; ename każdego obiektu - każdy w osobnej linii
  ;; vl-princ-to-string to jedyna funkcja VL w tym skrypcie:
  ;; alert przyjmuje tylko tekst, a zwykłe funkcje AutoLISP
  ;; nie potrafią zamienić ename na tekst.
  (setq taz_s_move_copy_alert_index 0)

  (while (< taz_s_move_copy_alert_index taz_s_move_copy_alert_count)
    (setq taz_s_move_copy_alert_ename (nth taz_s_move_copy_alert_index taz_s_move_copy_ename_list))
    (setq taz_s_move_copy_alert_ename_text (vl-princ-to-string taz_s_move_copy_alert_ename))
    (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text taz_s_move_copy_alert_ename_text))
    (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text "\n"))
    (setq taz_s_move_copy_alert_index (+ taz_s_move_copy_alert_index 1))
  )

  ;; tryb
  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text "\nTryb: "))
  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text taz_s_move_copy_alert_mode_text))
  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text "\n\n"))

  ;; wartości przesunięcia
  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text "Wartości przesunięcia:\n"))

  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text "X = "))
  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text taz_s_move_copy_x))
  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text "\n"))

  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text "Y = "))
  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text taz_s_move_copy_y))
  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text "\n"))

  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text "Z = "))
  (setq taz_s_move_copy_alert_text (strcat taz_s_move_copy_alert_text taz_s_move_copy_z))

  ;; wyświetlenie alertu
  (alert taz_s_move_copy_alert_text)

  (princ)

)
