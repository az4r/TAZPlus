;; ===========================================================
;; RAMKA RYSUNKOWA
;; ===========================================================
;;
;; Wersja poczatkowa:
;; - wybor skali ramki
;; - formaty A0, A1, A2
;; - punkt wskazany przez uzytkownika jest srodkiem arkusza
;; - ramka zewnetrzna = wymiar papieru
;; - ramka wewnetrzna = 5 mm od krawedzi papieru
;; - uskok po lewej stronie:
;;   287 mm w gore od dolnej ramki wewnetrznej
;;   i 25 mm w prawo
;; - wszystko powstaje na warstwie taz_s_frame
;;
;; ===========================================================

(defun c:taz_s_frame ()

  ;; ---------------------------------------------------------
  ;; ZAPISZ AKTUALNE USTAWIENIA
  ;; ---------------------------------------------------------

  (taz_s_current_settings_save)

  ;; ---------------------------------------------------------
  ;; ZMIENNE GLOBALNE
  ;; ---------------------------------------------------------

  (setq taz_s_frame_dcl_id nil)
  (setq taz_s_frame_selected_index 1)
  (setq taz_s_frame_scale_selected_index 0)
  (setq taz_s_frame_dialog_result nil)
  (setq taz_s_frame_format "A1")
  (setq taz_s_frame_scale "1:1")
  (setq taz_s_frame_scale_factor 1.0)

  (setq taz_s_frame_width nil)
  (setq taz_s_frame_height nil)

  (setq taz_s_frame_insert_point nil)

  ;; Jesli organizer przekazal znany format i skale,
  ;; zapamietaj je lokalnie dla tego uruchomienia ramki
  ;; i od razu wyczysc zmienne przekazujace.
  (setq taz_s_frame_known_format_local nil)
  (setq taz_s_frame_known_scale_factor_local nil)

  (if
    (and
      (boundp 'taz_s_frame_known_format)
      taz_s_frame_known_format
    )
    (progn
      (setq taz_s_frame_known_format_local
        taz_s_frame_known_format
      )
      (setq taz_s_frame_known_format nil)
    )
  )

  (if
    (and
      (boundp 'taz_s_frame_known_scale_factor)
      taz_s_frame_known_scale_factor
    )
    (progn
      (setq taz_s_frame_known_scale_factor_local
        taz_s_frame_known_scale_factor
      )
      (setq taz_s_frame_known_scale_factor nil)
    )
  )

  (if taz_s_frame_known_format_local
    (setq taz_s_frame_format taz_s_frame_known_format_local)
  )

  (if taz_s_frame_known_scale_factor_local
    (progn
      (setq taz_s_frame_scale_factor
        taz_s_frame_known_scale_factor_local
      )
      (setq taz_s_frame_scale
        (strcat
          "1:"
          (itoa (fix taz_s_frame_scale_factor))
        )
      )
    )
  )

  ;; Jesli organizer przekazal znany srodek przypadku,
  ;; zapamietaj go lokalnie dla tego uruchomienia ramki
  ;; i od razu wyczysc zmienna przekazujaca.
  (setq taz_s_frame_known_insert_point_local nil)

  (if
    (and
      (boundp 'taz_s_frame_known_insert_point)
      taz_s_frame_known_insert_point
    )
    (progn
      (setq taz_s_frame_known_insert_point_local
        taz_s_frame_known_insert_point
      )
      (setq taz_s_frame_known_insert_point nil)
    )
  )

  (setq taz_s_frame_x nil)
  (setq taz_s_frame_y nil)
  (setq taz_s_frame_z nil)
  (setq taz_s_frame_x0 nil)
  (setq taz_s_frame_y0 nil)

  (setq taz_s_frame_outer_p1 nil)
  (setq taz_s_frame_outer_p2 nil)
  (setq taz_s_frame_outer_p3 nil)
  (setq taz_s_frame_outer_p4 nil)

  (setq taz_s_frame_inner_p1 nil)
  (setq taz_s_frame_inner_p2 nil)
  (setq taz_s_frame_inner_p3 nil)
  (setq taz_s_frame_inner_p4 nil)
  (setq taz_s_frame_inner_p5 nil)
  (setq taz_s_frame_inner_p6 nil)

  ;; ---------------------------------------------------------
  ;; OKNO DCL TYLKO GDY FORMAT LUB SKALA NIE SA ZNANE
  ;; ---------------------------------------------------------
  ;; Przy wywolaniu z organizera oba parametry sa przekazane,
  ;; dlatego okno taz_s_frame.dcl nie jest wtedy wyswietlane.
  ;; Przy recznym uruchomieniu taz_s_frame zachowanie pozostaje stare.

  (if
    (or
      (= taz_s_frame_known_format_local nil)
      (= taz_s_frame_known_scale_factor_local nil)
    )
    (progn
      ;; ---------------------------------------------------------
      ;; WCZYTANIE PLIKU DCL
      ;; ---------------------------------------------------------

      (setq taz_s_frame_dcl_id (load_dialog "taz_s_frame.dcl"))

      (if (< taz_s_frame_dcl_id 0)
        (progn
          (alert "Nie moge zaladowac pliku taz_s_frame.dcl.")
          (taz_s_current_settings_restore)
          (taz_s_frame_exit)
        )
      )

      (if (not (new_dialog "taz_s_frame_dialog" taz_s_frame_dcl_id))
        (progn
          (alert "Nie moge zaladowac okienka DCL.")
          (unload_dialog taz_s_frame_dcl_id)
          (taz_s_current_settings_restore)
          (taz_s_frame_exit)
        )
      )

      ;; ---------------------------------------------------------
      ;; LISTA FORMATOW
      ;; ---------------------------------------------------------

      (start_list "taz_s_frame_format_popup")
      (mapcar 'add_list '("A0"
                          "A0+1" "A0+2" "A0+3" "A0+4"
                          "A1" "A1+1" "A1+2" "A1+3" "A1+4"
                          "A2" "A2+1" "A2+2" "A2+3" "A2+4"
                          "A3" "A3+1" "A3+2" "A3+3" "A3+4"
                          "A4"))
      (end_list)

      ;; Domyslnie A1, czyli indeks 5
      (set_tile "taz_s_frame_format_popup" "5")

      ;; ---------------------------------------------------------
      ;; LISTA SKAL
      ;; ---------------------------------------------------------

      (start_list "taz_s_frame_scale_popup")
      (mapcar 'add_list '("1:1" "1:2" "1:5" "1:10" "1:20"
                          "1:25" "1:50" "1:100" "1:200"))
      (end_list)

      ;; Domyslnie 1:1, czyli indeks 0
      (set_tile "taz_s_frame_scale_popup" "0")

      ;; ---------------------------------------------------------
      ;; OBSLUGA OK
      ;; ---------------------------------------------------------

      (action_tile "accept"
        "(progn
            (setq taz_s_frame_selected_index (atoi (get_tile \"taz_s_frame_format_popup\")))
            (setq taz_s_frame_scale_selected_index (atoi (get_tile \"taz_s_frame_scale_popup\")))

            (if (= taz_s_frame_selected_index 0) (setq taz_s_frame_format \"A0\"))
            (if (= taz_s_frame_selected_index 1) (setq taz_s_frame_format \"A0+1\"))
            (if (= taz_s_frame_selected_index 2) (setq taz_s_frame_format \"A0+2\"))
            (if (= taz_s_frame_selected_index 3) (setq taz_s_frame_format \"A0+3\"))
            (if (= taz_s_frame_selected_index 4) (setq taz_s_frame_format \"A0+4\"))
            (if (= taz_s_frame_selected_index 5) (setq taz_s_frame_format \"A1\"))
            (if (= taz_s_frame_selected_index 6) (setq taz_s_frame_format \"A1+1\"))
            (if (= taz_s_frame_selected_index 7) (setq taz_s_frame_format \"A1+2\"))
            (if (= taz_s_frame_selected_index 8) (setq taz_s_frame_format \"A1+3\"))
            (if (= taz_s_frame_selected_index 9) (setq taz_s_frame_format \"A1+4\"))
            (if (= taz_s_frame_selected_index 10) (setq taz_s_frame_format \"A2\"))
            (if (= taz_s_frame_selected_index 11) (setq taz_s_frame_format \"A2+1\"))
            (if (= taz_s_frame_selected_index 12) (setq taz_s_frame_format \"A2+2\"))
            (if (= taz_s_frame_selected_index 13) (setq taz_s_frame_format \"A2+3\"))
            (if (= taz_s_frame_selected_index 14) (setq taz_s_frame_format \"A2+4\"))
            (if (= taz_s_frame_selected_index 15) (setq taz_s_frame_format \"A3\"))
            (if (= taz_s_frame_selected_index 16) (setq taz_s_frame_format \"A3+1\"))
            (if (= taz_s_frame_selected_index 17) (setq taz_s_frame_format \"A3+2\"))
            (if (= taz_s_frame_selected_index 18) (setq taz_s_frame_format \"A3+3\"))
            (if (= taz_s_frame_selected_index 19) (setq taz_s_frame_format \"A3+4\"))
            (if (= taz_s_frame_selected_index 20) (setq taz_s_frame_format \"A4\"))

            (if (= taz_s_frame_scale_selected_index 0) (progn (setq taz_s_frame_scale \"1:1\") (setq taz_s_frame_scale_factor 1.0)))
            (if (= taz_s_frame_scale_selected_index 1) (progn (setq taz_s_frame_scale \"1:2\") (setq taz_s_frame_scale_factor 2.0)))
            (if (= taz_s_frame_scale_selected_index 2) (progn (setq taz_s_frame_scale \"1:5\") (setq taz_s_frame_scale_factor 5.0)))
            (if (= taz_s_frame_scale_selected_index 3) (progn (setq taz_s_frame_scale \"1:10\") (setq taz_s_frame_scale_factor 10.0)))
            (if (= taz_s_frame_scale_selected_index 4) (progn (setq taz_s_frame_scale \"1:20\") (setq taz_s_frame_scale_factor 20.0)))
            (if (= taz_s_frame_scale_selected_index 5) (progn (setq taz_s_frame_scale \"1:25\") (setq taz_s_frame_scale_factor 25.0)))
            (if (= taz_s_frame_scale_selected_index 6) (progn (setq taz_s_frame_scale \"1:50\") (setq taz_s_frame_scale_factor 50.0)))
            (if (= taz_s_frame_scale_selected_index 7) (progn (setq taz_s_frame_scale \"1:100\") (setq taz_s_frame_scale_factor 100.0)))
            (if (= taz_s_frame_scale_selected_index 8) (progn (setq taz_s_frame_scale \"1:200\") (setq taz_s_frame_scale_factor 200.0)))

            (done_dialog 1)
        )"
      )

      ;; ---------------------------------------------------------
      ;; OBSLUGA ANULUJ
      ;; ---------------------------------------------------------

      (action_tile "cancel"
        "(progn
            (done_dialog 0)
        )"
      )

      ;; ---------------------------------------------------------
      ;; URUCHOM OKNO DCL
      ;; ---------------------------------------------------------

      (setq taz_s_frame_dialog_result (start_dialog))

      (unload_dialog taz_s_frame_dcl_id)

      ;; Jesli anulowano - przywroc ustawienia i przerwij skrypt
      (if (= taz_s_frame_dialog_result 0)
        (progn
          (taz_s_current_settings_restore)
          (taz_s_frame_exit)
        )
      )

    )
  )

  ;; ---------------------------------------------------------
  ;; WYMIARY ARKUSZA
  ;; ---------------------------------------------------------
  ;; Wszystkie formaty sa poziomo.
  ;;
  ;; A0 = 1189 x 841
  ;; A1 =  841 x 594
  ;; A2 =  594 x 420
  ;; ---------------------------------------------------------

  (if (= taz_s_frame_format "A0")
    (progn
      (setq taz_s_frame_width 1189.0)
      (setq taz_s_frame_height 841.0)
    )
  )

  (if (= taz_s_frame_format "A1")
    (progn
      (setq taz_s_frame_width 841.0)
      (setq taz_s_frame_height 594.0)
    )
  )

  (if (= taz_s_frame_format "A2")
    (progn
      (setq taz_s_frame_width 594.0)
      (setq taz_s_frame_height 420.0)
    )
  )

  (if (= taz_s_frame_format "A3")
    (progn
      (setq taz_s_frame_width 420.0)
      (setq taz_s_frame_height 297.0)
    )
  )

  (if (= taz_s_frame_format "A4")
    (progn
      (setq taz_s_frame_width 210.0)
      (setq taz_s_frame_height 297.0)
    )
  )

  ;; ---------------------------------------------------------
  ;; FORMATY PRZEDLUZONE
  ;; Kazde +1 wydluza arkusz poziomo o 210 mm.
  ;; ---------------------------------------------------------

  (if (= taz_s_frame_format "A0+1")
    (progn
      (setq taz_s_frame_width (+ 1189.0 (* 1.0 210.0)))
      (setq taz_s_frame_height 841.0)
    )
  )

  (if (= taz_s_frame_format "A0+2")
    (progn
      (setq taz_s_frame_width (+ 1189.0 (* 2.0 210.0)))
      (setq taz_s_frame_height 841.0)
    )
  )

  (if (= taz_s_frame_format "A0+3")
    (progn
      (setq taz_s_frame_width (+ 1189.0 (* 3.0 210.0)))
      (setq taz_s_frame_height 841.0)
    )
  )

  (if (= taz_s_frame_format "A0+4")
    (progn
      (setq taz_s_frame_width (+ 1189.0 (* 4.0 210.0)))
      (setq taz_s_frame_height 841.0)
    )
  )

  (if (= taz_s_frame_format "A1+1")
    (progn
      (setq taz_s_frame_width (+ 841.0 (* 1.0 210.0)))
      (setq taz_s_frame_height 594.0)
    )
  )

  (if (= taz_s_frame_format "A1+2")
    (progn
      (setq taz_s_frame_width (+ 841.0 (* 2.0 210.0)))
      (setq taz_s_frame_height 594.0)
    )
  )

  (if (= taz_s_frame_format "A1+3")
    (progn
      (setq taz_s_frame_width (+ 841.0 (* 3.0 210.0)))
      (setq taz_s_frame_height 594.0)
    )
  )

  (if (= taz_s_frame_format "A1+4")
    (progn
      (setq taz_s_frame_width (+ 841.0 (* 4.0 210.0)))
      (setq taz_s_frame_height 594.0)
    )
  )

  (if (= taz_s_frame_format "A2+1")
    (progn
      (setq taz_s_frame_width (+ 594.0 (* 1.0 210.0)))
      (setq taz_s_frame_height 420.0)
    )
  )

  (if (= taz_s_frame_format "A2+2")
    (progn
      (setq taz_s_frame_width (+ 594.0 (* 2.0 210.0)))
      (setq taz_s_frame_height 420.0)
    )
  )

  (if (= taz_s_frame_format "A2+3")
    (progn
      (setq taz_s_frame_width (+ 594.0 (* 3.0 210.0)))
      (setq taz_s_frame_height 420.0)
    )
  )

  (if (= taz_s_frame_format "A2+4")
    (progn
      (setq taz_s_frame_width (+ 594.0 (* 4.0 210.0)))
      (setq taz_s_frame_height 420.0)
    )
  )

  (if (= taz_s_frame_format "A3+1")
    (progn
      (setq taz_s_frame_width (+ 420.0 (* 1.0 210.0)))
      (setq taz_s_frame_height 297.0)
    )
  )

  (if (= taz_s_frame_format "A3+2")
    (progn
      (setq taz_s_frame_width (+ 420.0 (* 2.0 210.0)))
      (setq taz_s_frame_height 297.0)
    )
  )

  (if (= taz_s_frame_format "A3+3")
    (progn
      (setq taz_s_frame_width (+ 420.0 (* 3.0 210.0)))
      (setq taz_s_frame_height 297.0)
    )
  )

  (if (= taz_s_frame_format "A3+4")
    (progn
      (setq taz_s_frame_width (+ 420.0 (* 4.0 210.0)))
      (setq taz_s_frame_height 297.0)
    )
  )

  ;; ---------------------------------------------------------
  ;; SKALA RAMKI
  ;; ---------------------------------------------------------

  (setq taz_s_frame_width (* taz_s_frame_width taz_s_frame_scale_factor))
  (setq taz_s_frame_height (* taz_s_frame_height taz_s_frame_scale_factor))

  ;; ---------------------------------------------------------
  ;; PUNKT WSTAWIENIA = SRODEK ARKUSZA
  ;; ---------------------------------------------------------

  (if taz_s_frame_known_insert_point_local
    (setq taz_s_frame_insert_point taz_s_frame_known_insert_point_local)
    (setq taz_s_frame_insert_point
      (getpoint
        (strcat
          "\nWskaz srodek ramki "
          taz_s_frame_format
          " w skali "
          taz_s_frame_scale
          ": "
        )
      )
    )
  )

  (if (null taz_s_frame_insert_point)
    (progn
      (taz_s_current_settings_restore)
      (taz_s_frame_exit)
    )
  )

  (setq taz_s_frame_x (car taz_s_frame_insert_point))
  (setq taz_s_frame_y (cadr taz_s_frame_insert_point))
  (setq taz_s_frame_z (caddr taz_s_frame_insert_point))

  ;; Lewy dolny naroznik arkusza
  (setq taz_s_frame_x0 (- taz_s_frame_x (/ taz_s_frame_width 2.0)))
  (setq taz_s_frame_y0 (- taz_s_frame_y (/ taz_s_frame_height 2.0)))

  ;; ---------------------------------------------------------
  ;; PUNKTY RAMKI ZEWNĘTRZNEJ
  ;; ---------------------------------------------------------
  ;; Kolejnosc taka sama jak w przeslanym przykladzie A1:
  ;; lewy gorny -> prawy gorny -> prawy dolny -> lewy dolny
  ;; ---------------------------------------------------------

  (setq taz_s_frame_outer_p1
        (list
          taz_s_frame_x0
          (+ taz_s_frame_y0 taz_s_frame_height)
          taz_s_frame_z
        )
  )

  (setq taz_s_frame_outer_p2
        (list
          (+ taz_s_frame_x0 taz_s_frame_width)
          (+ taz_s_frame_y0 taz_s_frame_height)
          taz_s_frame_z
        )
  )

  (setq taz_s_frame_outer_p3
        (list
          (+ taz_s_frame_x0 taz_s_frame_width)
          taz_s_frame_y0
          taz_s_frame_z
        )
  )

  (setq taz_s_frame_outer_p4
        (list
          taz_s_frame_x0
          taz_s_frame_y0
          taz_s_frame_z
        )
  )

  ;; ---------------------------------------------------------
  ;; PUNKTY RAMKI WEWNĘTRZNEJ
  ;; ---------------------------------------------------------
  ;; Offset od papieru = 5 mm.
  ;;
  ;; Uskok NIE jest zmieniany razem z formatem:
  ;; - od lewej ramki wewnetrznej 25 mm w prawo
  ;; - od dolnej ramki wewnetrznej 287 mm w gore
  ;;
  ;; Wszystkie te wymiary sa mnozone przez wybrana skale.
  ;; Dla A1 w skali 1:1 daje to dokladnie:
  ;; (30,5) ... (5,297) (30,297)
  ;; gdy lewy dolny naroznik papieru jest w (0,0).
  ;; ---------------------------------------------------------

  (setq taz_s_frame_inner_p1
        (list
          (+ taz_s_frame_x0 (* 30.0 taz_s_frame_scale_factor))
          (+ taz_s_frame_y0 (* 5.0 taz_s_frame_scale_factor))
          taz_s_frame_z
        )
  )

  (setq taz_s_frame_inner_p2
        (list
          (- (+ taz_s_frame_x0 taz_s_frame_width) (* 5.0 taz_s_frame_scale_factor))
          (+ taz_s_frame_y0 (* 5.0 taz_s_frame_scale_factor))
          taz_s_frame_z
        )
  )

  (setq taz_s_frame_inner_p3
        (list
          (- (+ taz_s_frame_x0 taz_s_frame_width) (* 5.0 taz_s_frame_scale_factor))
          (- (+ taz_s_frame_y0 taz_s_frame_height) (* 5.0 taz_s_frame_scale_factor))
          taz_s_frame_z
        )
  )

  (setq taz_s_frame_inner_p4
        (list
          (if (or (= taz_s_frame_format "A3")
                  (= taz_s_frame_format "A3+1")
                  (= taz_s_frame_format "A3+2")
                  (= taz_s_frame_format "A3+3")
                  (= taz_s_frame_format "A3+4")
                  (= taz_s_frame_format "A4"))
            (+ taz_s_frame_x0 (* 30.0 taz_s_frame_scale_factor))
            (+ taz_s_frame_x0 (* 5.0 taz_s_frame_scale_factor))
          )
          (- (+ taz_s_frame_y0 taz_s_frame_height) (* 5.0 taz_s_frame_scale_factor))
          taz_s_frame_z
        )
  )

  (setq taz_s_frame_inner_p5
        (list
          (if (or (= taz_s_frame_format "A3")
                  (= taz_s_frame_format "A3+1")
                  (= taz_s_frame_format "A3+2")
                  (= taz_s_frame_format "A3+3")
                  (= taz_s_frame_format "A3+4")
                  (= taz_s_frame_format "A4"))
            (+ taz_s_frame_x0 (* 30.0 taz_s_frame_scale_factor))
            (+ taz_s_frame_x0 (* 5.0 taz_s_frame_scale_factor))
          )
          (+ taz_s_frame_y0
             (* 5.0 taz_s_frame_scale_factor)
             (* 287.0 taz_s_frame_scale_factor))
          taz_s_frame_z
        )
  )

  (setq taz_s_frame_inner_p6
        (list
          (+ taz_s_frame_x0
             (* 5.0 taz_s_frame_scale_factor)
             (* 25.0 taz_s_frame_scale_factor))
          (+ taz_s_frame_y0
             (* 5.0 taz_s_frame_scale_factor)
             (* 287.0 taz_s_frame_scale_factor))
          taz_s_frame_z
        )
  )

  ;; ---------------------------------------------------------
  ;; WARSTWA RAMKI
  ;; ---------------------------------------------------------

  (command "_LAYER" "_U" "taz_s_frame" "")
  (command "_LAYER" "_S" "taz_s_frame" "")
  
  ;; ---------------------------------------------------------
  ;; USTAWIENIE KAMERY ###
  ;; ---------------------------------------------------------
  (command "_LINE" '(-50 -50 0) '(50 50 0) "")
  (command "_PLAN" "_C")
  (command "_ZOOM" "_OBJECT" (entlast) "")
  (entdel (entlast))
  (command "_ZOOM" "_SCALE" "1000X")
  (command "REGEN")

  ;; ---------------------------------------------------------
  ;; RYSOWANIE RAMKI ZEWNĘTRZNEJ
  ;; ---------------------------------------------------------

  (command
    "_PLINE"
    taz_s_frame_outer_p1
    taz_s_frame_outer_p2
    taz_s_frame_outer_p3
    taz_s_frame_outer_p4
    "_C"
  )
  
  ;; ---------------------------------------------------------
  ;; USTAWIENIE KAMERY ###
  ;; ---------------------------------------------------------
  (command "_LINE" '(-50 -50 0) '(50 50 0) "")
  (command "_PLAN" "_C")
  (command "_ZOOM" "_OBJECT" (entlast) "")
  (entdel (entlast))
  (command "_ZOOM" "_SCALE" "1000X")
  (command "REGEN")

  ;; ---------------------------------------------------------
  ;; RYSOWANIE RAMKI WEWNĘTRZNEJ
  ;; ---------------------------------------------------------

  (command
    "_PLINE"
    taz_s_frame_inner_p1
    taz_s_frame_inner_p2
    taz_s_frame_inner_p3
    taz_s_frame_inner_p4
    taz_s_frame_inner_p5
    taz_s_frame_inner_p6
    "_C"
  )

  ;; ---------------------------------------------------------
  ;; ZABLOKUJ WARSTWE I PRZYWROC USTAWIENIA
  ;; ---------------------------------------------------------

  (command "_LAYER" "_LO" "taz_s_frame" "")

  (taz_s_current_settings_restore)

  (princ (strcat "\nWstawiono ramke rysunkowa " taz_s_frame_format " w skali " taz_s_frame_scale "."))
  (princ)
)

;; ===========================================================
;; CICHE PRZERWANIE SKRYPTU
;; ===========================================================
;; Ta sama idea jak w taz_s_annotation_scale:
;; podmieniamy na chwile *error*, wywolujemy exit i nie pokazujemy
;; komunikatu "quit / exit abort".
;; Po przerwaniu przywracamy poprzedni *error*.
;; ===========================================================

(defun taz_s_frame_exit ()

  (setq taz_s_frame_old_error *error*)

  (setq *error*
    (lambda (taz_s_frame_error_message)
      (setq *error* taz_s_frame_old_error)
      (princ "")
    )
  )

  (exit)
)
