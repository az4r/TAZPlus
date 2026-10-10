;; =========================================================
;; taz_s_move_copy.lsp
;; Komenda: TAZ_S_MOVE_COPY
;;
;; ETAP 2 - wersja robocza:
;;   1. Wybór obiektów. Można zaznaczyć zarówno obiekty modelu
;;      (z atrybutami), jak i obiekty bez atrybutów.
;;   2. Z zaznaczenia powstaje lista ename.
;;   3. Okno DCL (plik taz_s_move_copy.dcl):
;;        - radio button: Move / Copy
;;        - pola tekstowe: X, Y, Z
;;        - przycisk Point: okno zamyka się, użytkownik wskazuje dwa
;;          punkty w rysunku, a ich różnica (punkt 2 minus punkt 1)
;;          trafia do pól X, Y, Z; potem okno otwiera się ponownie
;;        - przyciski OK i Anuluj
;;   4. OK     -> dla każdego zaznaczonego obiektu:
;;                - obiekt z atrybutami w pliku taz_s_beam_data.txt
;;                  (section_angle, section_position, sweep_p1, sweep_p2):
;;                  do sweep_p1 i sweep_p2 dodawane są X, Y, Z z okna DCL,
;;                  z nowych punktów taz_s_create_beam tworzy nową bryłę,
;;                  a atrybuty starej bryły są przenoszone na nową
;;                  (w pamięci i w pliku taz_s_beam_data.txt),
;;                - obiekt bez atrybutów: komenda COPY o wektor X, Y, Z.
;;      Anuluj -> przerwanie działania skryptu
;;
;; Move: po utworzeniu nowego obiektu (nowa bryła albo kopia) obiekt
;; bazowy jest usuwany. Copy: obiekty bazowe zostają nietknięte.
;;
;; UCS: wektor X, Y, Z (wpisany ręcznie albo wskazany przyciskiem
;; Point) jest podawany w UCS, który był aktywny tuż przed uruchomieniem
;; komendy. Dalsza praca odbywa się w World, więc wektor jest
;; przeliczany z tego UCS do World, a po zakończeniu pracy UCS sprzed
;; uruchomienia komendy jest przywracany.
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

  ;; licznik cykli przycisku Point (obejście "widmowego" punktu
  ;; przy drugim cyklu - patrz taz_s_move_copy_pick_vector)
  (setq taz_s_move_copy_pick_counter 1)

  ;; punkty wskazane przyciskiem Point
  (setq taz_s_move_copy_pick_p1 nil)
  (setq taz_s_move_copy_pick_p2 nil)

  (setq taz_s_move_copy_dcl_id 0)

  ;; wynik okna DCL: 0 = Anuluj, 1 = OK, 2 = kliknięto Point
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
  ;; Wynik: 0 = Anuluj, 1 = OK, 2 = kliknięto "Point"
  ;; Jeśli 2 -> pobieramy punkty, liczymy wektor i otwieramy
  ;; okno ponownie z uzupełnionymi polami.
  ;; ---------------------------------------------------------

  (if taz_s_move_copy_can_continue
    (progn

      (setq taz_s_move_copy_dialog_result 2)

      (while (= taz_s_move_copy_dialog_result 2)

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

        ;; przycisk Point - odczytaj pola i zamknij okno z wynikiem 2
        ;; (odczyt pól, żeby wpisane wartości nie zginęły po ponownym
        ;; otwarciu okna)
        (action_tile "taz_s_point" "(taz_s_move_copy_read_values)(done_dialog 2)")

        ;; OK - odczytaj pola i zamknij okno z wynikiem 1
        (action_tile "accept" "(taz_s_move_copy_read_values)(done_dialog 1)")

        ;; Anuluj - zamknij okno z wynikiem 0
        (action_tile "cancel" "(done_dialog 0)")

        (setq taz_s_move_copy_dialog_result (start_dialog))

        (unload_dialog taz_s_move_copy_dcl_id)

        ;; kliknięto Point - pobranie wektora z dwóch punktów
        (if (= taz_s_move_copy_dialog_result 2)
          (taz_s_move_copy_pick_vector)
          (princ)
        )

      )

    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; OK -> PRZESUNIĘCIE / KOPIOWANIE OBIEKTÓW
  ;; ---------------------------------------------------------

  (if (and taz_s_move_copy_can_continue (= taz_s_move_copy_dialog_result 1))
    (taz_s_move_copy_run)
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
;; Wywoływane po kliknięciu OK albo Point (z wnętrza action_tile).
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
;; POBRANIE WEKTORA PRZESUNIĘCIA Z DWÓCH PUNKTÓW
;; Wywoływane po kliknięciu przycisku Point (wynik okna = 2).
;; Użytkownik wskazuje dwa punkty w rysunku, a ich różnica
;; (punkt 2 minus punkt 1) trafia do pól X, Y, Z jako tekst.
;; Punkty są podane w UCS aktywnym w tej chwili, czyli w UCS sprzed
;; uruchomienia komendy - wektor jest więc w tym samym UCS.
;; Enter przy wskazywaniu punktu = rezygnacja: pola zostają takie,
;; jakie były przed zamknięciem okna.
;; ---------------------------------------------------------

(defun taz_s_move_copy_pick_vector ()

  (setq taz_s_move_copy_pick_p1 nil)
  (setq taz_s_move_copy_pick_p2 nil)

  ;; -----------------------------------------------------------
  ;; Obejście problemu z "widmowym" punktem przy DRUGIM cyklu:
  ;; jeśli licznik ma wartość 2, "pochłaniamy" zalegające
  ;; zdarzenie pustym getpoint przed właściwym pobraniem punktów.
  ;; -----------------------------------------------------------

  ;;(if (= taz_s_move_copy_pick_counter 2)
    ;;(getpoint)
    ;;(princ)
  ;;)

  (setq taz_s_move_copy_pick_p1 (getpoint "\nPodaj pierwszy punkt wektora przesunięcia: "))

  (if taz_s_move_copy_pick_p1
    (setq taz_s_move_copy_pick_p2 (getpoint taz_s_move_copy_pick_p1 "\nPodaj drugi punkt wektora przesunięcia: "))
    (princ)
  )

  (if (and taz_s_move_copy_pick_p1 taz_s_move_copy_pick_p2)
    (progn
      (setq taz_s_move_copy_pick_dx (- (car taz_s_move_copy_pick_p2) (car taz_s_move_copy_pick_p1)))
      (setq taz_s_move_copy_pick_dy (- (cadr taz_s_move_copy_pick_p2) (cadr taz_s_move_copy_pick_p1)))
      (setq taz_s_move_copy_pick_dz (- (caddr taz_s_move_copy_pick_p2) (caddr taz_s_move_copy_pick_p1)))

      (setq taz_s_move_copy_x (rtos taz_s_move_copy_pick_dx 2 6))
      (setq taz_s_move_copy_y (rtos taz_s_move_copy_pick_dy 2 6))
      (setq taz_s_move_copy_z (rtos taz_s_move_copy_pick_dz 2 6))
    )
    (princ)
  )

  ;; licznik cykli - inkrementacja na końcu każdego przebiegu
  (setq taz_s_move_copy_pick_counter (+ taz_s_move_copy_pick_counter 1))

  (princ)

)

;; ---------------------------------------------------------
;; ZAMIANA PRZECINKA NA KROPKĘ
;; Tekst z globalnej zmiennej taz_s_move_copy_comma_input
;; zamienia na tekst w zmiennej taz_s_move_copy_comma_output
;; (każdy przecinek zamieniony na kropkę).
;; Potrzebne, bo atof kończy odczyt na przecinku: "12,5" da 12.
;; ---------------------------------------------------------

(defun taz_s_move_copy_replace_comma ()

  (setq taz_s_move_copy_comma_output "")
  (setq taz_s_move_copy_comma_length (strlen taz_s_move_copy_comma_input))
  (setq taz_s_move_copy_comma_index 1)

  (while (<= taz_s_move_copy_comma_index taz_s_move_copy_comma_length)

    (setq taz_s_move_copy_comma_char (substr taz_s_move_copy_comma_input taz_s_move_copy_comma_index 1))

    (if (= taz_s_move_copy_comma_char ",")
      (setq taz_s_move_copy_comma_char ".")
      (princ)
    )

    (setq taz_s_move_copy_comma_output (strcat taz_s_move_copy_comma_output taz_s_move_copy_comma_char))
    (setq taz_s_move_copy_comma_index (+ taz_s_move_copy_comma_index 1))

  )

  (princ)

)

;; ---------------------------------------------------------
;; ODCZYT HANDLE I ATRYBUTÓW OBIEKTU
;; Dla obiektu z globalnej zmiennej taz_s_move_copy_run_ename:
;;   1. pobiera handle obiektu (kod DXF 5) - z niego zbudowane
;;      są nazwy zmiennych w pliku taz_s_beam_data.txt,
;;   2. odczytuje zmienne:
;;        taz_s_<handle>_section_angle    -> taz_s_move_copy_attr_angle
;;        taz_s_<handle>_section_position -> taz_s_move_copy_attr_position
;;        taz_s_<handle>_sweep_p1         -> taz_s_move_copy_attr_p1
;;        taz_s_<handle>_sweep_p2         -> taz_s_move_copy_attr_p2
;;      (wartość nil oznacza, że taka zmienna nie istnieje).
;; Wartości zmiennych muszą być już wczytane z pliku danych
;; (robi to taz_s_move_copy_run przed pętlą).
;; ---------------------------------------------------------

(defun taz_s_move_copy_read_attributes ()

  ;; na początku wszystko puste - żeby nic nie zostało
  ;; po poprzednio sprawdzanym obiekcie
  (setq taz_s_move_copy_attr_handle nil)
  (setq taz_s_move_copy_attr_angle nil)
  (setq taz_s_move_copy_attr_position nil)
  (setq taz_s_move_copy_attr_p1 nil)
  (setq taz_s_move_copy_attr_p2 nil)

  ;; handle obiektu (kod DXF 5)
  (setq taz_s_move_copy_attr_entity_data (entget taz_s_move_copy_run_ename))
  (setq taz_s_move_copy_attr_handle (cdr (assoc 5 taz_s_move_copy_attr_entity_data)))

  ;; nazwy zmiennych i ich wartości
  ;; (wartość nil oznacza, że taka zmienna nie istnieje)
  (if taz_s_move_copy_attr_handle
    (progn

      (setq taz_s_move_copy_attr_name_angle (strcat "taz_s_" taz_s_move_copy_attr_handle "_section_angle"))
      (setq taz_s_move_copy_attr_name_position (strcat "taz_s_" taz_s_move_copy_attr_handle "_section_position"))
      (setq taz_s_move_copy_attr_name_p1 (strcat "taz_s_" taz_s_move_copy_attr_handle "_sweep_p1"))
      (setq taz_s_move_copy_attr_name_p2 (strcat "taz_s_" taz_s_move_copy_attr_handle "_sweep_p2"))

      (setq taz_s_move_copy_attr_angle (eval (read taz_s_move_copy_attr_name_angle)))
      (setq taz_s_move_copy_attr_position (eval (read taz_s_move_copy_attr_name_position)))
      (setq taz_s_move_copy_attr_p1 (eval (read taz_s_move_copy_attr_name_p1)))
      (setq taz_s_move_copy_attr_p2 (eval (read taz_s_move_copy_attr_name_p2)))

    )
    (princ)
  )

  (princ)

)

;; ---------------------------------------------------------
;; KATEGORIA PRZEKROJU Z RODZINY PROFILU
;; Na podstawie globalnej zmiennej taz_s_family ustawia
;; taz_s_category (tak samo jak skrypty edycji, bo w trybie
;; edycji taz_s_create_beam pomija taz_s_select_section).
;; Nieznana rodzina -> taz_s_category = nil.
;; ---------------------------------------------------------

(defun taz_s_move_copy_set_category ()

  (setq taz_s_category nil)

  (if (= taz_s_family "HEA")
    (setq taz_s_category "Dwuteowniki")
    (princ)
  )
  (if (= taz_s_family "HEB")
    (setq taz_s_category "Dwuteowniki")
    (princ)
  )
  (if (= taz_s_family "IPE")
    (setq taz_s_category "Dwuteowniki")
    (princ)
  )
  (if (= taz_s_family "IPN")
    (setq taz_s_category "Dwuteowniki")
    (princ)
  )
  (if (= taz_s_family "UPE")
    (setq taz_s_category "Ceowniki")
    (princ)
  )
  (if (= taz_s_family "UPN")
    (setq taz_s_category "Ceowniki")
    (princ)
  )
  (if (= taz_s_family "LR")
    (setq taz_s_category "Katowniki")
    (princ)
  )
  (if (= taz_s_family "LN")
    (setq taz_s_category "Katowniki")
    (princ)
  )
  (if (= taz_s_family "SHS")
    (setq taz_s_category "Rury")
    (princ)
  )
  (if (= taz_s_family "RHS")
    (setq taz_s_category "Rury")
    (princ)
  )
  (if (= taz_s_family "CHS")
    (setq taz_s_category "Rury")
    (princ)
  )

  (princ)

)

;; ---------------------------------------------------------
;; ZAPIS ATRYBUTÓW NOWEJ BRYŁY DO PLIKU
;; Dopisuje na końcu pliku taz_s_data_file linie dla handle
;; nowej bryły (taz_s_move_copy_new_handle): attr1-attr10,
;; section_angle, section_position (stare wartości) oraz
;; sweep_p1 i sweep_p2 (nowe punkty). Format taki sam jak
;; w taz_s_create_beam i w skryptach edycji.
;; ---------------------------------------------------------

(defun taz_s_move_copy_write_beam_data ()

  ;; liczby jako tekst (6 miejsc po przecinku, tak jak w taz_s_create_beam)
  (setq taz_s_move_copy_text_angle (rtos taz_s_move_copy_old_angle 2 6))
  (setq taz_s_move_copy_text_position (rtos taz_s_move_copy_old_position 2 0))

  (setq taz_s_move_copy_text_p1_x (rtos taz_s_move_copy_new_p1_x 2 6))
  (setq taz_s_move_copy_text_p1_y (rtos taz_s_move_copy_new_p1_y 2 6))
  (setq taz_s_move_copy_text_p1_z (rtos taz_s_move_copy_new_p1_z 2 6))

  (setq taz_s_move_copy_text_p2_x (rtos taz_s_move_copy_new_p2_x 2 6))
  (setq taz_s_move_copy_text_p2_y (rtos taz_s_move_copy_new_p2_y 2 6))
  (setq taz_s_move_copy_text_p2_z (rtos taz_s_move_copy_new_p2_z 2 6))

  ;; "a" oznacza dopisywanie na koniec - poprzednie dane nie znikają
  (setq taz_s_move_copy_file (open taz_s_data_file "a"))

  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_attr1 \"" taz_s_move_copy_old_attr1 "\")") taz_s_move_copy_file)
  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_attr2 \"" taz_s_move_copy_old_attr2 "\")") taz_s_move_copy_file)
  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_attr3 \"" taz_s_move_copy_old_attr3 "\")") taz_s_move_copy_file)
  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_attr4 \"" taz_s_move_copy_old_attr4 "\")") taz_s_move_copy_file)
  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_attr5 \"" taz_s_move_copy_old_attr5 "\")") taz_s_move_copy_file)
  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_attr6 \"" taz_s_move_copy_old_attr6 "\")") taz_s_move_copy_file)
  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_attr7 \"" taz_s_move_copy_old_attr7 "\")") taz_s_move_copy_file)
  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_attr8 \"" taz_s_move_copy_old_attr8 "\")") taz_s_move_copy_file)
  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_attr9 \"" taz_s_move_copy_old_attr9 "\")") taz_s_move_copy_file)
  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_attr10 \"" taz_s_move_copy_old_attr10 "\")") taz_s_move_copy_file)

  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_section_angle " taz_s_move_copy_text_angle ")") taz_s_move_copy_file)
  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_section_position " taz_s_move_copy_text_position ")") taz_s_move_copy_file)

  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_sweep_p1 (list " taz_s_move_copy_text_p1_x " " taz_s_move_copy_text_p1_y " " taz_s_move_copy_text_p1_z "))") taz_s_move_copy_file)
  (write-line (strcat "(setq taz_s_" taz_s_move_copy_new_handle "_sweep_p2 (list " taz_s_move_copy_text_p2_x " " taz_s_move_copy_text_p2_y " " taz_s_move_copy_text_p2_z "))") taz_s_move_copy_file)

  (close taz_s_move_copy_file)

  (princ)

)

;; ---------------------------------------------------------
;; USUNIĘCIE OBIEKTU BAZOWEGO (tylko tryb Move)
;; Usuwa obiekt z globalnej zmiennej taz_s_move_copy_run_ename,
;; czyli ten, z którego powstała nowa bryła albo kopia.
;; Wywoływane dopiero wtedy, gdy nowy obiekt już istnieje.
;; Najpierw sprawdza, czy obiekt jeszcze istnieje (entdel na już
;; usuniętym obiekcie PRZYWRÓCIŁBY go), potem usuwa i kontroluje
;; wynik. Warstwy są odblokowane przez taz_s_move_copy_run.
;; ---------------------------------------------------------

(defun taz_s_move_copy_delete_base ()

  ;; czy obiekt bazowy jeszcze istnieje
  (setq taz_s_move_copy_delete_data (entget taz_s_move_copy_run_ename))

  (if taz_s_move_copy_delete_data
    (progn

      (entdel taz_s_move_copy_run_ename)

      ;; kontrola: po usunięciu entget nie powinien już nic zwracać
      (setq taz_s_move_copy_delete_check (entget taz_s_move_copy_run_ename))

      (if taz_s_move_copy_delete_check
        (princ (strcat "\nNie usunięto obiektu bazowego (handle " taz_s_move_copy_attr_handle ")."))
        (setq taz_s_move_copy_count_deleted (+ taz_s_move_copy_count_deleted 1))
      )

    )
    (princ)
  )

  (princ)

)

;; ---------------------------------------------------------
;; OBIEKT Z ATRYBUTAMI -> NOWA BRYŁA
;; Dla obiektu z globalnej zmiennej taz_s_move_copy_run_ename
;; (atrybuty już odczytane przez taz_s_move_copy_read_attributes):
;;   1. odczytuje atrybuty starej bryły (przed utworzeniem nowej),
;;   2. nowe punkty = stare sweep_p1 i sweep_p2 + X, Y, Z z okna DCL,
;;   3. tworzy nową bryłę przez taz_s_create_beam w trybie edycji
;;      ścieżki (nowe punkty przekazywane zmiennymi
;;      taz_s_edit_new_path_p1 i taz_s_edit_new_path_p2),
;;   4. przenosi atrybuty ze starej bryły na nową - w pamięci
;;      i w pliku taz_s_beam_data.txt.
;; Tryb Move: stara bryła jest usuwana PRZED zapisem pliku, tak jak
;; w skryptach edycji (dzięki temu rebuild jest tuż po zamknięciu
;; pliku). Tryb Copy: stara bryła zostaje.
;; ---------------------------------------------------------

(defun taz_s_move_copy_new_beam ()

  (setq taz_s_move_copy_beam_ok T)

  ;; --- stara bryła: handle i atrybuty (PRZED utworzeniem nowej) ---

  (setq taz_s_move_copy_old_handle taz_s_move_copy_attr_handle)
  (setq taz_s_move_copy_old_angle taz_s_move_copy_attr_angle)
  (setq taz_s_move_copy_old_position taz_s_move_copy_attr_position)
  (setq taz_s_move_copy_old_p1 taz_s_move_copy_attr_p1)
  (setq taz_s_move_copy_old_p2 taz_s_move_copy_attr_p2)

  (setq taz_s_move_copy_old_attr1 (eval (read (strcat "taz_s_" taz_s_move_copy_old_handle "_attr1"))))
  (setq taz_s_move_copy_old_attr2 (eval (read (strcat "taz_s_" taz_s_move_copy_old_handle "_attr2"))))
  (setq taz_s_move_copy_old_attr3 (eval (read (strcat "taz_s_" taz_s_move_copy_old_handle "_attr3"))))
  (setq taz_s_move_copy_old_attr4 (eval (read (strcat "taz_s_" taz_s_move_copy_old_handle "_attr4"))))
  (setq taz_s_move_copy_old_attr5 (eval (read (strcat "taz_s_" taz_s_move_copy_old_handle "_attr5"))))
  (setq taz_s_move_copy_old_attr6 (eval (read (strcat "taz_s_" taz_s_move_copy_old_handle "_attr6"))))
  (setq taz_s_move_copy_old_attr7 (eval (read (strcat "taz_s_" taz_s_move_copy_old_handle "_attr7"))))
  (setq taz_s_move_copy_old_attr8 (eval (read (strcat "taz_s_" taz_s_move_copy_old_handle "_attr8"))))
  (setq taz_s_move_copy_old_attr9 (eval (read (strcat "taz_s_" taz_s_move_copy_old_handle "_attr9"))))
  (setq taz_s_move_copy_old_attr10 (eval (read (strcat "taz_s_" taz_s_move_copy_old_handle "_attr10"))))

  ;; brakujący atrybut (nil) zamieniamy na pusty tekst - inaczej strcat
  ;; przy zapisie do pliku zgłosiłby błąd
  (if (null taz_s_move_copy_old_attr1) (setq taz_s_move_copy_old_attr1 "") (princ))
  (if (null taz_s_move_copy_old_attr2) (setq taz_s_move_copy_old_attr2 "") (princ))
  (if (null taz_s_move_copy_old_attr3) (setq taz_s_move_copy_old_attr3 "") (princ))
  (if (null taz_s_move_copy_old_attr4) (setq taz_s_move_copy_old_attr4 "") (princ))
  (if (null taz_s_move_copy_old_attr5) (setq taz_s_move_copy_old_attr5 "") (princ))
  (if (null taz_s_move_copy_old_attr6) (setq taz_s_move_copy_old_attr6 "") (princ))
  (if (null taz_s_move_copy_old_attr7) (setq taz_s_move_copy_old_attr7 "") (princ))
  (if (null taz_s_move_copy_old_attr8) (setq taz_s_move_copy_old_attr8 "") (princ))
  (if (null taz_s_move_copy_old_attr9) (setq taz_s_move_copy_old_attr9 "") (princ))
  (if (null taz_s_move_copy_old_attr10) (setq taz_s_move_copy_old_attr10 "") (princ))

  ;; --- nowe punkty ścieżki: stare punkty + X, Y, Z z okna DCL ---

  (setq taz_s_move_copy_new_p1_x (+ (car taz_s_move_copy_old_p1) taz_s_move_copy_offset_x))
  (setq taz_s_move_copy_new_p1_y (+ (cadr taz_s_move_copy_old_p1) taz_s_move_copy_offset_y))
  (setq taz_s_move_copy_new_p1_z (+ (caddr taz_s_move_copy_old_p1) taz_s_move_copy_offset_z))
  (setq taz_s_move_copy_new_p1 (list taz_s_move_copy_new_p1_x taz_s_move_copy_new_p1_y taz_s_move_copy_new_p1_z))

  (setq taz_s_move_copy_new_p2_x (+ (car taz_s_move_copy_old_p2) taz_s_move_copy_offset_x))
  (setq taz_s_move_copy_new_p2_y (+ (cadr taz_s_move_copy_old_p2) taz_s_move_copy_offset_y))
  (setq taz_s_move_copy_new_p2_z (+ (caddr taz_s_move_copy_old_p2) taz_s_move_copy_offset_z))
  (setq taz_s_move_copy_new_p2 (list taz_s_move_copy_new_p2_x taz_s_move_copy_new_p2_y taz_s_move_copy_new_p2_z))

  ;; --- rodzina, typ i kategoria przekroju (jak w skryptach edycji) ---

  (setq taz_s_family taz_s_move_copy_old_attr6)
  (setq taz_s_type taz_s_move_copy_old_attr7)
  (taz_s_move_copy_set_category)

  (if (null taz_s_category)
    (progn
      (setq taz_s_move_copy_beam_ok nil)
      (princ (strcat "\nPominięto belkę (handle " taz_s_move_copy_old_handle "): nieznana rodzina profilu."))
      (setq taz_s_move_copy_count_errors (+ taz_s_move_copy_count_errors 1))
    )
    (princ)
  )

  ;; --- nowa bryła: taz_s_create_beam w trybie edycji ścieżki ---

  (if taz_s_move_copy_beam_ok
    (progn

      ;; ostatni obiekt w rysunku PRZED wywołaniem (do sprawdzenia, czy coś powstało)
      (setq taz_s_move_copy_last_before (entlast))

      ;; dane dla taz_s_create_beam
      ;; taz_s_attribs_object_name = handle STAREJ bryły (stąd czytany jest
      ;; kąt i pozycja; taz_s_create_beam sam podmieni go na handle nowej)
      (setq taz_s_attribs_object_name taz_s_move_copy_old_handle)
      (setq taz_s_edit_new_path_p1 taz_s_move_copy_new_p1)
      (setq taz_s_edit_new_path_p2 taz_s_move_copy_new_p2)

      (setq taz_s_edit_section_angle_mode nil)
      (setq taz_s_edit_section_position_mode nil)
      (setq taz_s_edit_beam_path_mode T)
      (setq taz_s_edit_mode T)

      (c:taz_s_create_beam)

      (setq taz_s_edit_mode nil)
      (setq taz_s_edit_beam_path_mode nil)

      ;; --- sprawdzenie: czy powstała NOWA bryła 3DSOLID ---

      (setq taz_s_move_copy_new_ename (entlast))
      (setq taz_s_move_copy_new_type (cdr (assoc 0 (entget taz_s_move_copy_new_ename))))

      (setq taz_s_move_copy_created_ok T)

      (if (equal taz_s_move_copy_new_ename taz_s_move_copy_last_before)
        (setq taz_s_move_copy_created_ok nil)
        (princ)
      )

      (if (/= taz_s_move_copy_new_type "3DSOLID")
        (setq taz_s_move_copy_created_ok nil)
        (princ)
      )

      (if taz_s_move_copy_created_ok
        (princ)
        (progn
          (setq taz_s_move_copy_beam_ok nil)
          (princ (strcat "\nPominięto belkę (handle " taz_s_move_copy_old_handle "): taz_s_create_beam nie utworzył nowej bryły 3DSOLID, dane nie zostały zapisane."))
          (setq taz_s_move_copy_count_errors (+ taz_s_move_copy_count_errors 1))
        )
      )

    )
    (princ)
  )

  ;; --- atrybuty nowej bryły: pamięć, plik ---

  (if taz_s_move_copy_beam_ok
    (progn

      ;; handle nowej bryły
      (setq taz_s_move_copy_new_handle (cdr (assoc 5 (entget taz_s_move_copy_new_ename))))

      ;; pamięć: atrybuty ze starej bryły na nową
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_attr1")) taz_s_move_copy_old_attr1)
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_attr2")) taz_s_move_copy_old_attr2)
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_attr3")) taz_s_move_copy_old_attr3)
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_attr4")) taz_s_move_copy_old_attr4)
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_attr5")) taz_s_move_copy_old_attr5)
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_attr6")) taz_s_move_copy_old_attr6)
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_attr7")) taz_s_move_copy_old_attr7)
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_attr8")) taz_s_move_copy_old_attr8)
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_attr9")) taz_s_move_copy_old_attr9)
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_attr10")) taz_s_move_copy_old_attr10)
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_section_angle")) taz_s_move_copy_old_angle)
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_section_position")) taz_s_move_copy_old_position)

      ;; pamięć: nowe punkty ścieżki
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_sweep_p1")) taz_s_move_copy_new_p1)
      (set (read (strcat "taz_s_" taz_s_move_copy_new_handle "_sweep_p2")) taz_s_move_copy_new_p2)

      ;; tryb Move: usunięcie starej bryły - tak jak w skryptach edycji
      ;; PRZED zapisem pliku (dane starej bryły są już w zmiennych old)
      (if taz_s_move_copy_is_move
        (taz_s_move_copy_delete_base)
        (princ)
      )

      ;; plik: dopisanie linii dla nowej bryły i przebudowa pliku
      (taz_s_move_copy_write_beam_data)
      (c:taz_s_rebuild_data)

      (setq taz_s_move_copy_count_beams (+ taz_s_move_copy_count_beams 1))

    )
    (princ)
  )

  (princ)

)

;; ---------------------------------------------------------
;; OBIEKT BEZ ATRYBUTÓW -> KOMENDA COPY
;; Kopiuje obiekt z globalnej zmiennej taz_s_move_copy_run_ename
;; o wektor X, Y, Z z okna DCL (od punktu 0,0,0 do punktu X,Y,Z).
;; UCS = World ustawia taz_s_move_copy_run, a "_non" wyłącza osnapy.
;; ---------------------------------------------------------

(defun taz_s_move_copy_plain_copy ()

  ;; ostatni obiekt w rysunku PRZED kopiowaniem
  (setq taz_s_move_copy_last_before (entlast))

  ;; wektor przesunięcia
  (setq taz_s_move_copy_copy_from (list 0.0 0.0 0.0))
  (setq taz_s_move_copy_copy_to (list taz_s_move_copy_offset_x taz_s_move_copy_offset_y taz_s_move_copy_offset_z))

  (command "_.COPY" taz_s_move_copy_run_ename "" "_non" taz_s_move_copy_copy_from "_non" taz_s_move_copy_copy_to)

  ;; COPY może czekać na kolejny punkt (tryb wielokrotny) - kończymy ją
  ;; Enterem, ale tylko wtedy, gdy komenda nadal jest aktywna.
  ;; Licznik chroni przed pętlą nieskończoną.
  (setq taz_s_move_copy_copy_guard 0)

  (while (and (> (getvar "CMDACTIVE") 0) (< taz_s_move_copy_copy_guard 10))
    (command "")
    (setq taz_s_move_copy_copy_guard (+ taz_s_move_copy_copy_guard 1))
  )

  ;; sprawdzenie: czy powstał nowy obiekt
  (setq taz_s_move_copy_copy_new_ename (entlast))

  (if (equal taz_s_move_copy_copy_new_ename taz_s_move_copy_last_before)
    (progn
      (princ (strcat "\nNie skopiowano obiektu (handle " taz_s_move_copy_attr_handle ")."))
      (setq taz_s_move_copy_count_errors (+ taz_s_move_copy_count_errors 1))
    )
    (setq taz_s_move_copy_count_copies (+ taz_s_move_copy_count_copies 1))
  )

  ;; tryb Move: jeżeli kopia powstała, usuwamy obiekt bazowy
  (if taz_s_move_copy_is_move
    (if (equal taz_s_move_copy_copy_new_ename taz_s_move_copy_last_before)
      (princ)
      (taz_s_move_copy_delete_base)
    )
    (princ)
  )

  (princ)

)

;; ---------------------------------------------------------
;; ZAKOŃCZENIE NIEDOKOŃCZONEJ KOMENDY
;; Jeśli po poleceniu UCS nadal jest aktywna jakaś komenda (np. UCS
;; nie rozpoznał opcji i czeka na dalsze dane), kończy ją tak jak
;; klawisz Esc. Gdy żadna komenda nie jest aktywna - nic nie robi.
;; Licznik chroni przed pętlą nieskończoną.
;; ---------------------------------------------------------

(defun taz_s_move_copy_ucs_cancel_pending ()

  (setq taz_s_move_copy_ucs_guard 0)

  (while (and (> (getvar "CMDACTIVE") 0) (< taz_s_move_copy_ucs_guard 10))
    (command)
    (setq taz_s_move_copy_ucs_guard (+ taz_s_move_copy_ucs_guard 1))
  )

  (princ)

)

;; ---------------------------------------------------------
;; ZAPIS UCS SPRZED URUCHOMIENIA KOMENDY
;; Wywoływane na samym początku taz_s_move_copy_run, zanim cokolwiek
;; zmieni UCS. Zapamiętuje, jak przywrócić aktywny UCS
;; (zmienna taz_s_move_copy_ucs_kind):
;;   "world"   - aktywny jest UCS World -> na końcu: UCS World,
;;   "named"   - aktywny jest UCS z nazwą -> na końcu: przywrócenie
;;               go po nazwie (nic nie jest zapisywane w rysunku),
;;   "unnamed" - aktywny jest UCS bez nazwy -> zapis pod nazwą
;;               tymczasową, na końcu przywrócenie i usunięcie tej nazwy,
;;   "failed"  - zapis się nie udał -> UCS nie zostanie przywrócony.
;; Dodatkowo zapamiętuje początek i kierunki osi UCS (we współrzędnych
;; World), żeby po przywróceniu sprawdzić, czy UCS jest ten sam.
;; ---------------------------------------------------------

(defun taz_s_move_copy_ucs_save ()

  (setq taz_s_move_copy_ucs_temp_name "taz_s_move_copy_ucs_temp")

  ;; geometria aktywnego UCS - do sprawdzenia po przywróceniu
  (setq taz_s_move_copy_ucs_origin (getvar "UCSORG"))
  (setq taz_s_move_copy_ucs_xdir (getvar "UCSXDIR"))
  (setq taz_s_move_copy_ucs_ydir (getvar "UCSYDIR"))

  ;; nazwa aktywnego UCS (pusty tekst = UCS bez nazwy albo World)
  (setq taz_s_move_copy_ucs_name (getvar "UCSNAME"))

  (if (null taz_s_move_copy_ucs_name)
    (setq taz_s_move_copy_ucs_name "")
    (princ)
  )

  ;; sposób przywrócenia - domyślnie UCS bez nazwy
  (setq taz_s_move_copy_ucs_kind "unnamed")

  ;; UCS z nazwą - tylko jeśli taka nazwa naprawdę istnieje w rysunku
  (setq taz_s_move_copy_ucs_found nil)

  (if (/= taz_s_move_copy_ucs_name "")
    (setq taz_s_move_copy_ucs_found (tblsearch "UCS" taz_s_move_copy_ucs_name))
    (princ)
  )

  (if taz_s_move_copy_ucs_found
    (setq taz_s_move_copy_ucs_kind "named")
    (princ)
  )

  ;; UCS World (ma pierwszeństwo przed pozostałymi)
  (if (= (getvar "WORLDUCS") 1)
    (setq taz_s_move_copy_ucs_kind "world")
    (princ)
  )

  ;; UCS bez nazwy: zapis pod nazwą tymczasową
  (if (= taz_s_move_copy_ucs_kind "unnamed")
    (progn

      ;; pozostałość po przerwanym uruchomieniu - usuwamy
      (if (tblsearch "UCS" taz_s_move_copy_ucs_temp_name)
        (progn
          (command "_.UCS" "_D" taz_s_move_copy_ucs_temp_name)
          (taz_s_move_copy_ucs_cancel_pending)
        )
        (princ)
      )

      (command "_.UCS" "_S" taz_s_move_copy_ucs_temp_name)
      (taz_s_move_copy_ucs_cancel_pending)

      ;; kontrola: czy nazwa tymczasowa powstała
      (if (tblsearch "UCS" taz_s_move_copy_ucs_temp_name)
        (princ)
        (progn
          (setq taz_s_move_copy_ucs_kind "failed")
          (princ "\nUWAGA: nie udało się zapisać aktualnego UCS - po zakończeniu skryptu nie zostanie przywrócony.")
        )
      )

    )
    (princ)
  )

  (princ)

)

;; ---------------------------------------------------------
;; PRZYWRÓCENIE UCS SPRZED URUCHOMIENIA KOMENDY
;; Wywoływane na samym końcu taz_s_move_copy_run, po wszystkich
;; operacjach, które zmieniają UCS. Przywraca UCS zapamiętany przez
;; taz_s_move_copy_ucs_save (sposób zależy od taz_s_move_copy_ucs_kind),
;; a potem porównuje początek i osie UCS z zapamiętanymi.
;; Jeśli UCS nie jest taki sam - wypisuje ostrzeżenie.
;; ---------------------------------------------------------

(defun taz_s_move_copy_ucs_restore ()

  ;; World
  (if (= taz_s_move_copy_ucs_kind "world")
    (progn
      (command "_.UCS" "_W")
      (taz_s_move_copy_ucs_cancel_pending)
    )
    (princ)
  )

  ;; UCS z nazwą
  (if (= taz_s_move_copy_ucs_kind "named")
    (progn
      (command "_.UCS" "_R" taz_s_move_copy_ucs_name)
      (taz_s_move_copy_ucs_cancel_pending)
    )
    (princ)
  )

  ;; UCS bez nazwy: przywrócenie z nazwy tymczasowej i usunięcie tej nazwy
  (if (= taz_s_move_copy_ucs_kind "unnamed")
    (progn
      (command "_.UCS" "_R" taz_s_move_copy_ucs_temp_name)
      (taz_s_move_copy_ucs_cancel_pending)
      (command "_.UCS" "_D" taz_s_move_copy_ucs_temp_name)
      (taz_s_move_copy_ucs_cancel_pending)
    )
    (princ)
  )

  ;; zapis się nie udał - nic nie przywrócono
  (if (= taz_s_move_copy_ucs_kind "failed")
    (princ "\nUWAGA: UCS sprzed uruchomienia skryptu nie został przywrócony.")
    (princ)
  )

  ;; kontrola: czy aktywny UCS jest taki sam jak zapamiętany
  (if (/= taz_s_move_copy_ucs_kind "failed")
    (progn

      (setq taz_s_move_copy_ucs_same T)

      (if (equal (getvar "UCSORG") taz_s_move_copy_ucs_origin 0.000001)
        (princ)
        (setq taz_s_move_copy_ucs_same nil)
      )

      (if (equal (getvar "UCSXDIR") taz_s_move_copy_ucs_xdir 0.000001)
        (princ)
        (setq taz_s_move_copy_ucs_same nil)
      )

      (if (equal (getvar "UCSYDIR") taz_s_move_copy_ucs_ydir 0.000001)
        (princ)
        (setq taz_s_move_copy_ucs_same nil)
      )

      (if taz_s_move_copy_ucs_same
        (princ)
        (princ "\nUWAGA: UCS po zakończeniu skryptu różni się od UCS sprzed uruchomienia.")
      )

    )
    (princ)
  )

  (princ)

)

;; ---------------------------------------------------------
;; PRZESUNIĘCIE / KOPIOWANIE ZAZNACZONYCH OBIEKTÓW
;; Wywoływane po kliknięciu OK. Dla każdego obiektu z listy
;; taz_s_move_copy_ename_list:
;;   - obiekt z kompletem atrybutów (section_angle, section_position,
;;     sweep_p1, sweep_p2) -> nowa bryła (taz_s_move_copy_new_beam),
;;   - pozostałe obiekty -> komenda COPY (taz_s_move_copy_plain_copy).
;; Tryb Move (radio w oknie DCL): po utworzeniu nowego obiektu obiekt
;; bazowy jest usuwany. Tryb Copy: obiekty bazowe zostają.
;; UCS: na początku zapisywany jest UCS sprzed uruchomienia komendy,
;; wektor X, Y, Z jest przeliczany z tego UCS do World, dalsza praca
;; odbywa się w World, a na końcu UCS sprzed uruchomienia komendy
;; jest przywracany.
;; ---------------------------------------------------------

(defun taz_s_move_copy_run ()

  ;; --- UCS: zapis UCS sprzed uruchomienia komendy ---
  ;; Musi być pierwszym krokiem, zanim cokolwiek zmieni UCS.
  ;; Przywraca go taz_s_move_copy_ucs_restore na samym końcu.

  (taz_s_move_copy_ucs_save)

  ;; --- wartości przesunięcia: tekst z okna DCL -> liczby ---

  (setq taz_s_move_copy_comma_input taz_s_move_copy_x)
  (taz_s_move_copy_replace_comma)
  (setq taz_s_move_copy_offset_x (atof taz_s_move_copy_comma_output))

  (setq taz_s_move_copy_comma_input taz_s_move_copy_y)
  (taz_s_move_copy_replace_comma)
  (setq taz_s_move_copy_offset_y (atof taz_s_move_copy_comma_output))

  (setq taz_s_move_copy_comma_input taz_s_move_copy_z)
  (taz_s_move_copy_replace_comma)
  (setq taz_s_move_copy_offset_z (atof taz_s_move_copy_comma_output))

  ;; --- wektor przesunięcia: z UCS użytkownika do World ---
  ;; Wartości X, Y, Z (wpisane ręcznie albo z przycisku Point) są podane
  ;; w UCS, który był aktywny przed uruchomieniem komendy. Dalsza praca
  ;; odbywa się w World, więc wektor trzeba przeliczyć teraz, dopóki
  ;; aktywny jest jeszcze UCS użytkownika.
  ;; trans: 1 = aktywny UCS, 0 = World, T = przeliczany jest wektor
  ;; (przesunięcie), a nie punkt - bez przesunięcia początku układu.

  (setq taz_s_move_copy_vector_ucs (list taz_s_move_copy_offset_x taz_s_move_copy_offset_y taz_s_move_copy_offset_z))
  (setq taz_s_move_copy_vector_world (trans taz_s_move_copy_vector_ucs 1 0 T))

  (setq taz_s_move_copy_offset_x (car taz_s_move_copy_vector_world))
  (setq taz_s_move_copy_offset_y (cadr taz_s_move_copy_vector_world))
  (setq taz_s_move_copy_offset_z (caddr taz_s_move_copy_vector_world))

  ;; --- plik z danymi belek: ścieżka i wczytanie (jeżeli istnieje) ---
  ;; Dzięki temu zmienne z atrybutami są w pamięci także dla belek
  ;; utworzonych w tej sesji. Zmienna taz_s_data_file ustawiana jest tak
  ;; samo jak w skryptach edycji (korzysta z niej też taz_s_rebuild_data).

  (setq taz_s_move_copy_data_folder (taz_s_path))
  (setq taz_s_data_file (strcat taz_s_move_copy_data_folder "taz_s_beam_data.txt"))

  (if (findfile taz_s_data_file)
    (load taz_s_data_file)
    (princ)
  )

  ;; --- przygotowanie (tak jak w skryptach edycji) ---
  ;; zapis ustawień, odblokowanie warstw (COPY nie działa na zablokowanych
  ;; warstwach), warstwa edycji jako aktualna, UCS = World

  (taz_s_current_settings_save)
  (taz_s_unlock_all_layers)
  (command "_LAYER" "_S" "taz_s_editing_layer" "")
  (command "_.UCS" "_W")

  ;; --- pętla po zaznaczonych obiektach ---

  (setq taz_s_move_copy_count_beams 0)
  (setq taz_s_move_copy_count_copies 0)
  (setq taz_s_move_copy_count_errors 0)
  (setq taz_s_move_copy_count_deleted 0)

  ;; tryb Move ("1" z radio w oknie DCL): obiekty bazowe są usuwane
  (setq taz_s_move_copy_is_move nil)
  (if (= taz_s_move_copy_mode "1")
    (setq taz_s_move_copy_is_move T)
    (princ)
  )

  (setq taz_s_move_copy_run_index 0)

  (while (< taz_s_move_copy_run_index (length taz_s_move_copy_ename_list))

    (setq taz_s_move_copy_run_ename (nth taz_s_move_copy_run_index taz_s_move_copy_ename_list))

    ;; handle i atrybuty obiektu
    (taz_s_move_copy_read_attributes)

    ;; obiekt z atrybutami = istnieją wszystkie cztery
    (setq taz_s_move_copy_is_beam T)
    (if (null taz_s_move_copy_attr_angle) (setq taz_s_move_copy_is_beam nil) (princ))
    (if (null taz_s_move_copy_attr_position) (setq taz_s_move_copy_is_beam nil) (princ))
    (if (null taz_s_move_copy_attr_p1) (setq taz_s_move_copy_is_beam nil) (princ))
    (if (null taz_s_move_copy_attr_p2) (setq taz_s_move_copy_is_beam nil) (princ))

    (if taz_s_move_copy_is_beam
      (taz_s_move_copy_new_beam)
      (taz_s_move_copy_plain_copy)
    )

    (setq taz_s_move_copy_run_index (+ taz_s_move_copy_run_index 1))

  )

  ;; --- sprzątanie (tak jak w skryptach edycji) ---
  ;; zablokowanie warstw i przywrócenie ustawień

  (taz_s_lock_all_layers)
  (taz_s_current_settings_restore)

  ;; --- UCS: przywrócenie UCS sprzed uruchomienia komendy ---
  ;; Po wszystkich operacjach, które zmieniają UCS.

  (taz_s_move_copy_ucs_restore)

  ;; --- podsumowanie w wierszu poleceń ---

  (setq taz_s_move_copy_summary "\nGotowe. Utworzone belki: ")
  (setq taz_s_move_copy_summary (strcat taz_s_move_copy_summary (itoa taz_s_move_copy_count_beams)))
  (setq taz_s_move_copy_summary (strcat taz_s_move_copy_summary ", skopiowane obiekty: "))
  (setq taz_s_move_copy_summary (strcat taz_s_move_copy_summary (itoa taz_s_move_copy_count_copies)))
  (setq taz_s_move_copy_summary (strcat taz_s_move_copy_summary ", pominięte: "))
  (setq taz_s_move_copy_summary (strcat taz_s_move_copy_summary (itoa taz_s_move_copy_count_errors)))
  (setq taz_s_move_copy_summary (strcat taz_s_move_copy_summary ", usunięte obiekty bazowe: "))
  (setq taz_s_move_copy_summary (strcat taz_s_move_copy_summary (itoa taz_s_move_copy_count_deleted)))
  (setq taz_s_move_copy_summary (strcat taz_s_move_copy_summary "."))

  (princ taz_s_move_copy_summary)

  (princ)

)
