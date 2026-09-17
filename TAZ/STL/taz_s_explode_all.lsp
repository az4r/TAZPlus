;; ============================================================
;; TAZ_S_EXPLODE_ALL.LSP
;;
;; Tworzy kopie aktualnego rysunku z dopiskiem "_EXPLODED"
;; i usuwa z pamieci sesji dane TAZ przypisane do obiektow.
;;
;; Kolejnosc:
;;
;; 1. Zapisuje aktualny rysunek.
;; 2. Tworzy kopie rysunku z dopiskiem "_EXPLODED".
;; 3. Przeglada wszystkie obiekty w aktualnym rysunku.
;; 4. Pobiera HANDLE kazdego obiektu.
;; 5. Sprawdza, czy dla HANDLE istnieja dane TAZ.
;; 6. Jezeli dane istnieja, ustawia wszystkie 14 zmiennych
;;    przypisanych do tego HANDLE na nil.
;;
;; Skrypt NIE:
;;
;; - wykonuje polecenia EXPLODE,
;; - usuwa zadnych obiektow,
;; - zmienia geometrii,
;; - zmienia warstw,
;; - zmienia wlasciwosci obiektow,
;; - usuwa danych osi,
;; - usuwa danych parametrów projektu,
;; - modyfikuje taz_s_beam_data.txt.
;;
;; Plik _EXPLODED nie otrzymuje osobnego pliku danych.
;;
;; ============================================================


(defun taz_s_explode_all_clear_handle_data (taz_s_explode_all_handle)

  ;; ----------------------------------------------------------
  ;; SPRAWDZ CZY ISTNIEJA DANE DLA HANDLE
  ;; ----------------------------------------------------------

  (setq taz_s_explode_all_handle_found nil)


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_attr1"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_attr2"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_attr3"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_attr4"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_attr5"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_attr6"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_attr7"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_attr8"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_attr9"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_attr10"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_section_angle"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_section_position"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_sweep_p1"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  (if
    (boundp
      (read
        (strcat
          "taz_s_"
          taz_s_explode_all_handle
          "_sweep_p2"
        )
      )
    )
    (setq taz_s_explode_all_handle_found T)
  )


  ;; ----------------------------------------------------------
  ;; JEZELI DANE ISTNIEJA - WYCZYSC CALY KOMPLET
  ;; ----------------------------------------------------------

  (if taz_s_explode_all_handle_found

    (progn

      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_attr1"
          )
        )
        nil
      )


      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_attr2"
          )
        )
        nil
      )


      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_attr3"
          )
        )
        nil
      )


      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_attr4"
          )
        )
        nil
      )


      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_attr5"
          )
        )
        nil
      )


      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_attr6"
          )
        )
        nil
      )


      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_attr7"
          )
        )
        nil
      )


      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_attr8"
          )
        )
        nil
      )


      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_attr9"
          )
        )
        nil
      )


      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_attr10"
          )
        )
        nil
      )


      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_section_angle"
          )
        )
        nil
      )


      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_section_position"
          )
        )
        nil
      )


      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_sweep_p1"
          )
        )
        nil
      )


      (set
        (read
          (strcat
            "taz_s_"
            taz_s_explode_all_handle
            "_sweep_p2"
          )
        )
        nil
      )


      (setq taz_s_explode_all_cleared_count
        (+ taz_s_explode_all_cleared_count 1)
      )

    )
  )


  (setq taz_s_explode_all_handle_found nil)

)


;; ============================================================
;; GLOWNA FUNKCJA
;; ============================================================


(defun c:taz_s_explode_all ()

  ;; ----------------------------------------------------------
  ;; ZAPISZ AKTUALNE USTAWIENIA
  ;; ----------------------------------------------------------

  (taz_s_current_settings_save)


  ;; ----------------------------------------------------------
  ;; ZAPISZ ORYGINALNY RYSUNEK
  ;; ----------------------------------------------------------

  (command "_.QSAVE")


  ;; ----------------------------------------------------------
  ;; POBIERZ NAZWE I SCIEZKE ORYGINALNEGO RYSUNKU
  ;; ----------------------------------------------------------

  (setq taz_s_explode_all_original_dwg_name
    (getvar "DWGNAME")
  )


  (setq taz_s_explode_all_original_dwg_path
    (getvar "DWGPREFIX")
  )


  ;; ----------------------------------------------------------
  ;; USUN ROZSZERZENIE .DWG Z NAZWY
  ;; ----------------------------------------------------------

  (setq taz_s_explode_all_original_dwg_name_no_ext
    (substr
      taz_s_explode_all_original_dwg_name
      1
      (- (strlen taz_s_explode_all_original_dwg_name) 4)
    )
  )


  ;; ----------------------------------------------------------
  ;; ZBUDUJ NAZWE PLIKU _EXPLODED
  ;; ----------------------------------------------------------

  (setq taz_s_explode_all_target_file
    (strcat
      taz_s_explode_all_original_dwg_path
      taz_s_explode_all_original_dwg_name_no_ext
      "_EXPLODED.dwg"
    )
  )


  ;; ----------------------------------------------------------
  ;; ZAPISZ KOPIE
  ;; ----------------------------------------------------------

  (if (findfile taz_s_explode_all_target_file)

    (command
      "_.SAVEAS"
      ""
      taz_s_explode_all_target_file
      "_Y"
    )

    (command
      "_.SAVEAS"
      ""
      taz_s_explode_all_target_file
    )
  )


  ;; ----------------------------------------------------------
  ;; INICJALIZACJA LICZNIKA
  ;; ----------------------------------------------------------

  (setq taz_s_explode_all_cleared_count 0)


  (setq taz_s_explode_all_object_count 0)


  ;; ----------------------------------------------------------
  ;; POBIERZ WSZYSTKIE OBIEKTY Z RYSUNKU
  ;; ----------------------------------------------------------

  (setq taz_s_explode_all_selection
    (ssget "_X")
  )


  ;; ----------------------------------------------------------
  ;; PRZEJDZ PRZEZ WSZYSTKIE OBIEKTY
  ;; ----------------------------------------------------------

  (if taz_s_explode_all_selection

    (progn

      (setq taz_s_explode_all_i 0)


      (setq taz_s_explode_all_selection_length
        (sslength taz_s_explode_all_selection)
      )


      (while
        (<
          taz_s_explode_all_i
          taz_s_explode_all_selection_length
        )

        ;; ----------------------------------------------------
        ;; POBIERZ OBIEKT
        ;; ----------------------------------------------------

        (setq taz_s_explode_all_object
          (ssname
            taz_s_explode_all_selection
            taz_s_explode_all_i
          )
        )


        ;; ----------------------------------------------------
        ;; POBIERZ HANDLE
        ;; ----------------------------------------------------

        (setq taz_s_explode_all_object_data
          (entget taz_s_explode_all_object)
        )


        (setq taz_s_explode_all_handle
          (cdr
            (assoc
              5
              taz_s_explode_all_object_data
            )
          )
        )


        ;; ----------------------------------------------------
        ;; JEZELI OBIEKT MA HANDLE - SPRAWDZ DANE
        ;; ----------------------------------------------------

        (if taz_s_explode_all_handle

          (progn

            (setq taz_s_explode_all_object_count
              (+ taz_s_explode_all_object_count 1)
            )


            (taz_s_explode_all_clear_handle_data
              taz_s_explode_all_handle
            )

          )
        )


        ;; ----------------------------------------------------
        ;; NASTEPNY OBIEKT
        ;; ----------------------------------------------------

        (setq taz_s_explode_all_i
          (+ taz_s_explode_all_i 1)
        )

      )
    )
  )


  ;; ----------------------------------------------------------
  ;; ZAPISZ KOPIE PO WYCZYSZCZENIU PAMIECI
  ;;
  ;; UWAGA:
  ;; SAVEAS zostal wykonany przed czyszczeniem pamieci.
  ;; Dane AutoLISP nie sa czescia zapisywanego DWG,
  ;; dlatego nie ma potrzeby ponownego QSAVE.
  ;; ----------------------------------------------------------

  ;; ----------------------------------------------------------
  ;; INFORMACJA
  ;; ----------------------------------------------------------

  (print
    (strcat
      "Utworzono plik: "
      taz_s_explode_all_target_file
    )
  )


  (print
    (strcat
      "Sprawdzono obiektow: "
      (itoa taz_s_explode_all_object_count)
    )
  )


  (print
    (strcat
      "Wyczyszczono obiektow z danymi TAZ: "
      (itoa taz_s_explode_all_cleared_count)
    )
  )


  (print
    "Geometria i obiekty rysunku nie zostaly zmienione."
  )


  ;; ----------------------------------------------------------
  ;; CZYSZCZENIE ZMIENNYCH ROBOCZYCH
  ;; ----------------------------------------------------------

  (setq taz_s_explode_all_original_dwg_name nil)

  (setq taz_s_explode_all_original_dwg_path nil)

  (setq taz_s_explode_all_original_dwg_name_no_ext nil)

  (setq taz_s_explode_all_target_file nil)

  (setq taz_s_explode_all_selection nil)

  (setq taz_s_explode_all_selection_length nil)

  (setq taz_s_explode_all_i nil)

  (setq taz_s_explode_all_object nil)

  (setq taz_s_explode_all_object_data nil)

  (setq taz_s_explode_all_handle nil)

  (setq taz_s_explode_all_handle_found nil)

  (setq taz_s_explode_all_object_count nil)

  (setq taz_s_explode_all_cleared_count nil)


  ;; ----------------------------------------------------------
  ;; PRZYWROC USTAWIENIA
  ;; ----------------------------------------------------------

  (taz_s_current_settings_restore)


  (princ)
)


(princ)