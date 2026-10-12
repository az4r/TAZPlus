;; ============================================================
;; TAZ_S_ADD_ATTRIBUTES.LSP  (wersja prosta - bez vl-, zmienne globalne)
;;
;; Co robi:
;;  1. Pozwala wskazac dowolny obiekt (albo uzyc obiektow zaznaczonych
;;     wczesniej) - bez zadnego filtra typu obiektu
;;  2. Dla kazdego obiektu, ktory nie ma jeszcze atrybutow, dopisuje
;;     na koncu pliku taz_s_objects_data.txt komplet pustych danych,
;;     w tej samej kolejnosci i w tym samym formacie co taz_s_create_beam:
;;     attr1..attr10 (puste), section_angle (0), section_position (5),
;;     sweep_p1 i sweep_p2 (0 0 0)
;;  3. Wczytuje plik do pamieci (load), wiec dane dzialaja od razu
;;     w biezacej sesji
;;
;; UWAGA: skrypt celowo NIE wywoluje c:taz_s_rebuild_data.
;; Rebuild przepisuje plik od nowa i zapisuje w nim tylko bryly 3DSOLID
;; z warstw taz_s_beam oraz taz_s_plate. Dane pozostalych obiektow
;; znikna z pliku przy najblizszym wywolaniu rebuild (w pamieci
;; zostaja do konca sesji).
;; ============================================================

(defun c:taz_s_add_attributes ()

  ;; ---------------------------------------------------------
  ;; SCIEZKA DO PLIKU DANYCH
  ;; taz_s_path zwraca <folder rysunku><nazwa rysunku>/
  ;; ---------------------------------------------------------

  (setq taz_s_add_attributes_folder (taz_s_path))
  (setq taz_s_add_attributes_data_file (strcat taz_s_add_attributes_folder "taz_s_objects_data.txt"))

  ;; Znacznik: T = mozna isc dalej, nil = ktorys krok sie nie udal
  (setq taz_s_add_attributes_ok T)

  ;; ---------------------------------------------------------
  ;; SELEKCJA OBIEKTOW - tylko 3DSOLID - bez filtra typu obiektu zakomentowane
  ;; Najpierw obiekty zaznaczone wczesniej, potem wskazanie nowego zbioru
  ;; ---------------------------------------------------------

  ;;(setq taz_s_add_attributes_selection (ssget "_I"))
  (setq taz_s_add_attributes_selection (ssget "_I" '((0 . "3DSOLID"))))

  (if (not taz_s_add_attributes_selection)
    ;;(setq taz_s_add_attributes_selection (ssget "_+.:E:S"))
    ;;(setq taz_s_add_attributes_selection (ssget "_+.:E:S" '((0 . "3DSOLID"))))
    (setq taz_s_add_attributes_selection (ssget '((0 . "3DSOLID"))))
    (princ)
  )

  (if (not taz_s_add_attributes_selection)
    (progn
      (alert "Nie wybrano obiektu.")
      (setq taz_s_add_attributes_ok nil)
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; WCZYTANIE ISTNIEJACYCH DANYCH (jezeli plik juz istnieje)
  ;; Dzieki temu pamiec zgadza sie z plikiem i wiemy, ktore
  ;; obiekty maja juz atrybuty
  ;; ---------------------------------------------------------

  (setq taz_s_add_attributes_file_exists nil)
  (if taz_s_add_attributes_ok
    (setq taz_s_add_attributes_file_exists (findfile taz_s_add_attributes_data_file))
    (princ)
  )

  (if taz_s_add_attributes_file_exists
    (load taz_s_add_attributes_data_file)
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; OTWARCIE PLIKU DO DOPISYWANIA
  ;; "a" oznacza dopisywanie na koniec - poprzednie dane nie znikaja
  ;; Jezeli folder danych nie istnieje, open zwroci nil
  ;; ---------------------------------------------------------

  (setq taz_s_add_attributes_file nil)
  (if taz_s_add_attributes_ok
    (setq taz_s_add_attributes_file (open taz_s_add_attributes_data_file "a"))
    (princ)
  )

  (if (and taz_s_add_attributes_ok (not taz_s_add_attributes_file))
    (progn
      (alert "Nie mozna otworzyc pliku danych projektu. Sprawdz czy projekt zostal zapisany i czy istnieje jego folder danych.")
      (setq taz_s_add_attributes_ok nil)
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; PETLA PO WYBRANYCH OBIEKTACH
  ;; ---------------------------------------------------------

  (if taz_s_add_attributes_ok
    (progn

      (setq taz_s_add_attributes_count (sslength taz_s_add_attributes_selection))
      (setq taz_s_add_attributes_index 0)
      (setq taz_s_add_attributes_added 0)
      (setq taz_s_add_attributes_skipped 0)

      (while (< taz_s_add_attributes_index taz_s_add_attributes_count)

        (setq taz_s_add_attributes_object (ssname taz_s_add_attributes_selection taz_s_add_attributes_index))
        (setq taz_s_add_attributes_object_data (entget taz_s_add_attributes_object))
        (setq taz_s_add_attributes_handle (cdr (assoc 5 taz_s_add_attributes_object_data)))

        ;; domyslnie dopisujemy dane - ponizsze kroki moga to wylaczyc
        (setq taz_s_add_attributes_write T)

        ;; brak handle - nie mamy pod jaka nazwa zapisac danych
        (if taz_s_add_attributes_handle
          (princ)
          (setq taz_s_add_attributes_write nil)
        )

        ;; obiekt ma juz atrybuty (atrybut 1 w pamieci) - nie nadpisujemy ich pustymi
        (setq taz_s_add_attributes_existing nil)
        (if taz_s_add_attributes_write
          (setq taz_s_add_attributes_existing (eval (read (strcat "taz_s_" taz_s_add_attributes_handle "_attr1"))))
          (princ)
        )
        (if taz_s_add_attributes_existing
          (setq taz_s_add_attributes_write nil)
          (princ)
        )

        ;; zapis kompletu danych tego obiektu do pliku
        (if taz_s_add_attributes_write
          (progn

            ;; -- atrybuty ogolne (wszystkie puste) --
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_attr1 \"\")") taz_s_add_attributes_file)
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_attr2 \"\")") taz_s_add_attributes_file)
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_attr3 \"\")") taz_s_add_attributes_file)
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_attr4 \"\")") taz_s_add_attributes_file)
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_attr5 \"\")") taz_s_add_attributes_file)
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_attr6 \"\")") taz_s_add_attributes_file)
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_attr7 \"\")") taz_s_add_attributes_file)
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_attr8 \"\")") taz_s_add_attributes_file)
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_attr9 \"\")") taz_s_add_attributes_file)
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_attr10 \"\")") taz_s_add_attributes_file)

            ;; -- kat obrotu przekroju (wartosc neutralna, liczba calkowita) --
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_section_angle 0)") taz_s_add_attributes_file)

            ;; -- pozycja przekroju wzgledem osi (wartosc neutralna, liczba calkowita) --
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_section_position 5)") taz_s_add_attributes_file)

            ;; -- punkt poczatkowy i koncowy sciezki (wartosc neutralna 0 0 0) --
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_sweep_p1 (list 0.000000 0.000000 0.000000))") taz_s_add_attributes_file)
            (write-line (strcat "(setq taz_s_" taz_s_add_attributes_handle "_sweep_p2 (list 0.000000 0.000000 0.000000))") taz_s_add_attributes_file)

          )
          (princ)
        )

        ;; licznik: dopisane albo pominiete
        (if taz_s_add_attributes_write
          (setq taz_s_add_attributes_added (+ taz_s_add_attributes_added 1))
          (setq taz_s_add_attributes_skipped (+ taz_s_add_attributes_skipped 1))
        )

        (setq taz_s_add_attributes_index (+ taz_s_add_attributes_index 1))

      )

    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; ZAMKNIECIE PLIKU I WCZYTANIE DANYCH DO PAMIECI
  ;; Plik musi byc zamkniety przed load - inaczej dane moga
  ;; nie zdazyc sie zapisac na dysk
  ;; ---------------------------------------------------------

  (if taz_s_add_attributes_ok
    (progn
      (close taz_s_add_attributes_file)
      (load taz_s_add_attributes_data_file)
      (princ (strcat "\nDodano atrybuty do obiektow (pamietaj aby zmienic warstwe na taz_s_beam lub taz_s_plate): " (itoa taz_s_add_attributes_added) "."))
      (princ (strcat "\nPominieto (maja juz atrybuty lub brak handle): " (itoa taz_s_add_attributes_skipped) "."))
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; WYCZYSZCZENIE ZMIENNYCH TYMCZASOWYCH
  ;; ---------------------------------------------------------

  (setq taz_s_add_attributes_folder nil)
  (setq taz_s_add_attributes_data_file nil)
  (setq taz_s_add_attributes_ok nil)
  (setq taz_s_add_attributes_selection nil)
  (setq taz_s_add_attributes_file_exists nil)
  (setq taz_s_add_attributes_file nil)
  (setq taz_s_add_attributes_count nil)
  (setq taz_s_add_attributes_index nil)
  (setq taz_s_add_attributes_added nil)
  (setq taz_s_add_attributes_skipped nil)
  (setq taz_s_add_attributes_object nil)
  (setq taz_s_add_attributes_object_data nil)
  (setq taz_s_add_attributes_handle nil)
  (setq taz_s_add_attributes_write nil)
  (setq taz_s_add_attributes_existing nil)

  (princ)

)
(princ)
