;; ===========================================================
;; ZMIANA PROFILU ISTNIEJACYCH BELEK (bryl 3D)
;; Z ZACHOWANIEM POZYCJI I KATA WZGLEDEM SCIEZKI STERUJACEJ
;; ===========================================================
;;
;; Dziala na tej samej zasadzie co taz_s_move_copy oraz
;; taz_s_edit_section_angle / taz_s_edit_section_position:
;; - dla kazdej zaznaczonej bryly odtwarzamy linie sterujaca
;;   na podstawie zapisanych punktow taz_s_..._sweep_p1 / p2
;; - wywolujemy c:taz_s_create_beam w trybie edycji
;;   (taz_s_edit_beam_path_mode), dzieki czemu kat i pozycja
;;   przekroju wzgledem osi zostaja takie same jak wczesniej
;; - jedyna roznica jest taka, ze taz_s_family / taz_s_type /
;;   taz_s_category ustawiamy na NOWY profil wybrany raz,
;;   dla wszystkich zaznaczonych elementow
;; - argumenty attr1 i attr2 (numer elementu / numer wysylkowy)
;;   sa czyszczone, bo dotycza starego profilu
;;
;; ===========================================================

(defun c:taz_s_change_profile ()

  ;; ---------------------------------------------------------
  ;; ZMIENNE GLOBALNE
  ;; ---------------------------------------------------------

  (setq taz_s_change_profile_selection nil)
  (setq taz_s_change_profile_ok T)
  (setq taz_s_change_profile_i 0)
  (setq taz_s_change_profile_ent nil)
  (setq taz_s_change_profile_type nil)
  (setq taz_s_change_profile_list nil)
  (setq taz_s_change_profile_obj nil)

  (setq taz_s_change_profile_new_category nil)
  (setq taz_s_change_profile_new_family nil)
  (setq taz_s_change_profile_new_type nil)

  ;; ---------------------------------------------------------
  ;; WYBOR OBIEKTOW (jeden lub kilka)
  ;; ---------------------------------------------------------

  (setq taz_s_change_profile_selection (ssget))

  (if (null taz_s_change_profile_selection)
    (progn
      (alert "Nie wybrano żadnych obiektów.")
      (setq taz_s_change_profile_ok nil)
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; SPRAWDZENIE CZY WSZYSTKIE OBIEKTY TO 3DSOLID
  ;; ---------------------------------------------------------

  (if taz_s_change_profile_ok
    (progn
      (setq taz_s_change_profile_i 0)
      (while (< taz_s_change_profile_i (sslength taz_s_change_profile_selection))

        (setq taz_s_change_profile_ent (ssname taz_s_change_profile_selection taz_s_change_profile_i))
        (setq taz_s_change_profile_type (cdr (assoc 0 (entget taz_s_change_profile_ent))))

        (if (/= taz_s_change_profile_type "3DSOLID")
          (setq taz_s_change_profile_ok nil)
          (princ)
        )

        (setq taz_s_change_profile_i (+ taz_s_change_profile_i 1))
      )
    )
    (princ)
  )

  (if (not taz_s_change_profile_ok)
    (progn
      (if taz_s_change_profile_selection
        (alert "Wszystkie zaznaczone obiekty muszą być bryłami 3D (3DSOLID).")
        (princ)
      )
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; SCIEZKA DO PLIKU DANYCH I WCZYTANIE
  ;; ---------------------------------------------------------

  (if taz_s_change_profile_ok
    (progn
      (setq taz_s_data_file (strcat (taz_s_path) "taz_s_beam_data.txt"))
      (load taz_s_data_file)
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; WARSTWA ROBOCZA
  ;; ---------------------------------------------------------

  (if taz_s_change_profile_ok
    (progn
      (taz_s_current_settings_save)
      (command "_LAYER" "_U" "taz_s_editing_layer" "")
      (command "_LAYER" "_S" "taz_s_editing_layer" "")
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; WYBOR NOWEGO PROFILU (okno DCL) - TYLKO RAZ DLA WSZYSTKICH
  ;; ---------------------------------------------------------
  ;; Uwaga: jesli uzytkownik anuluje okno, taz_s_select_section
  ;; sama sprzata za soba (odblokowuje warstwy, przywraca
  ;; ustawienia) i konczy dzialanie skryptu poleceniem (exit).

  (if taz_s_change_profile_ok
    (taz_s_select_section)
    (princ)
  )

  ;; Zapamietaj wybrany nowy profil
  (if taz_s_change_profile_ok
    (progn
      (setq taz_s_change_profile_new_category taz_s_category)
      (setq taz_s_change_profile_new_family taz_s_family)
      (setq taz_s_change_profile_new_type taz_s_type)
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; ZAPAMIETAJ LISTE ENCJI PRZED JAKAKOLWIEK EDYCJA
  ;; ---------------------------------------------------------

  (if taz_s_change_profile_ok
    (progn
      (setq taz_s_change_profile_list nil)
      (setq taz_s_change_profile_i 0)
      (while (< taz_s_change_profile_i (sslength taz_s_change_profile_selection))
        (setq taz_s_change_profile_ent (ssname taz_s_change_profile_selection taz_s_change_profile_i))
        (setq taz_s_change_profile_list (append taz_s_change_profile_list (list taz_s_change_profile_ent)))
        (setq taz_s_change_profile_i (+ taz_s_change_profile_i 1))
      )
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; PETLA PO WSZYSTKICH ZAZNACZONYCH OBIEKTACH
  ;; ---------------------------------------------------------

  (if taz_s_change_profile_ok
    (progn
      (setq taz_s_change_profile_i 0)
      (while (< taz_s_change_profile_i (length taz_s_change_profile_list))

        (setq taz_s_change_profile_obj (nth taz_s_change_profile_i taz_s_change_profile_list))

        (taz_s_change_profile_process_object)

        (setq taz_s_change_profile_i (+ taz_s_change_profile_i 1))
      )
    )
    (princ)
  )

  ;; ---------------------------------------------------------
  ;; PRZYWROC WARSTWE I USTAWIENIA
  ;; ---------------------------------------------------------

  (if taz_s_change_profile_ok
    (progn
      (command "_LAYER" "_LO" "taz_s_editing_layer" "")
      (taz_s_current_settings_restore)
    )
    (princ)
  )

  (princ)

)

;; ---------------------------------------------------------
;; PRZETWORZENIE JEDNEGO OBIEKTU (taz_s_change_profile_obj)
;; ---------------------------------------------------------

(defun taz_s_change_profile_process_object ()

  (setq taz_s_attribs_line nil)

  ;; TRYB EDYCJI - zachowaj stary kat i pozycje przekroju,
  ;; ale zmien profil na nowo wybrany
  (setq taz_s_edit_mode T)
  (setq taz_s_edit_beam_path_mode T)

  ;; Reset UCS do World
  (command "_.UCS" "_W")

  ;; sprawdz czy obiekt nadal istnieje
  (if (and taz_s_change_profile_obj (entget taz_s_change_profile_obj))
    (progn

      ;; pobierz obiekt
      (setq taz_s_attribs_object taz_s_change_profile_obj)
      (setq taz_s_attribs_object_old taz_s_attribs_object)

      ;; pobierz nazwe obiektu (handle)
      (setq taz_s_attribs_object_name
            (cdr (assoc 5 (entget taz_s_attribs_object))))

      ;; pobierz zapisane punkty sciezki
      (setq taz_s_edit_p1
            (eval (read
                   (strcat "taz_s_"
                           taz_s_attribs_object_name
                           "_sweep_p1"))))

      (setq taz_s_edit_p2
            (eval (read
                   (strcat "taz_s_"
                           taz_s_attribs_object_name
                           "_sweep_p2"))))

      ;; narysuj linie sterujaca w tym samym miejscu co poprzednio
      (command "_ZOOM" "_SCALE" "10000X")
      (command "_LINE" taz_s_edit_p1 taz_s_edit_p2 "")
      (command "_ZOOM" "_SCALE" "0.0001X")
      (setq taz_s_attribs_line (entlast))

      ;; ustaw kolor czerwony
      (command "_CHPROP" taz_s_attribs_line "" "_P" "_C" "1" "")

      ;; -------------------------------------------------------
      ;; POBIERZ ATRYBUTY STAREJ BRYLY (PRZED create_beam!)
      ;; -------------------------------------------------------
      ;; attr1 i attr2 (numer elementu / numer wysylkowy) CZYSCIMY,
      ;; bo dotycza starego profilu

      (setq taz_s_attr1_old  "")
      (setq taz_s_attr2_old  "")
      (setq taz_s_attr3_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr3"))))
      (setq taz_s_attr4_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr4"))))
      (setq taz_s_attr5_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr5"))))
      (setq taz_s_attr6_old  taz_s_change_profile_new_family)
      (setq taz_s_attr7_old  taz_s_change_profile_new_type)
      (setq taz_s_attr8_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr8"))))
      (setq taz_s_attr9_old  (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr9"))))
      (setq taz_s_attr10_old (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_attr10"))))
      (setq taz_s_section_position_old (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_position"))))
      (setq taz_s_section_angle_old (eval (read (strcat "taz_s_" taz_s_attribs_object_name "_section_angle"))))

      ;; -------------------------------------------------------
      ;; USTAW NOWY PROFIL DO ZMIENNYCH GLOBALNYCH
      ;; UZYWANYCH PRZEZ FUNKCJE RYSUJACE WEWNATRZ create_beam
      ;; -------------------------------------------------------

      (setq taz_s_family taz_s_change_profile_new_family)
      (setq taz_s_type taz_s_change_profile_new_type)
      (setq taz_s_category taz_s_change_profile_new_category)

      ;; -------------------------------------------------------
      ;; Zapisz nowe punkty sciezki (linia sterujaca zostaje
      ;; w tym samym miejscu co stara, wiec sa to te same punkty)
      ;; -------------------------------------------------------

      (setq taz_s_edit_new_path_p1 (cdr (assoc 10 (entget taz_s_attribs_line))))
      (setq taz_s_edit_new_path_p2 (cdr (assoc 11 (entget taz_s_attribs_line))))

      ;; -------------------------------------------------------
      ;; GENERUJ NOWA BRYLE (nowy profil, stary kat i pozycja)
      ;; -------------------------------------------------------

      (c:taz_s_create_beam)

      ;; -------------------------------------------------------
      ;; NOWA BRYLA - pobierz jej handle
      ;; -------------------------------------------------------

      (setq taz_s_attribs_object_new (entlast))
      (setq taz_s_attribs_object_name_new
            (cdr (assoc 5 (entget taz_s_attribs_object_new))))

      ;; -------------------------------------------------------
      ;; PRZENIES ATRYBUTY ZE STAREJ BRYLY NA NOWA
      ;; (attr1 i attr2 juz wyczyszczone powyzej)
      ;; -------------------------------------------------------

      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr1"))  taz_s_attr1_old)
      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr2"))  taz_s_attr2_old)
      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr3"))  taz_s_attr3_old)
      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr4"))  taz_s_attr4_old)
      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr5"))  taz_s_attr5_old)
      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr6"))  taz_s_attr6_old)
      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr7"))  taz_s_attr7_old)
      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr8"))  taz_s_attr8_old)
      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr9"))  taz_s_attr9_old)
      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_attr10")) taz_s_attr10_old)
      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_section_angle")) taz_s_section_angle_old)
      (set (read (strcat "taz_s_" taz_s_attribs_object_name_new "_section_position")) taz_s_section_position_old)

      ;; -------------------------------------------------------
      ;; USUN STARA BRYLE
      ;; -------------------------------------------------------

      (command "_LAYER" "_U" "taz_s_beam" "")
      (if (and taz_s_attribs_object_old (entget taz_s_attribs_object_old))
        (entdel taz_s_attribs_object_old)
      )
      (command "_LAYER" "_LO" "taz_s_beam" "")

      ;; -------------------------------------------------------
      ;; USUN LINIE POMOCNICZA
      ;; -------------------------------------------------------

      (if (and taz_s_attribs_line (entget taz_s_attribs_line))
        (entdel taz_s_attribs_line)
      )

      ;; -------------------------------------------------------
      ;; ZAPIS DANYCH DO PLIKU
      ;; -------------------------------------------------------

      (setq taz_s_f_beam_data (open taz_s_data_file "a"))

      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr1 \""           taz_s_attr1_old "\")") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr2 \""           taz_s_attr2_old "\")") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr3 \""           taz_s_attr3_old "\")") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr4 \""           taz_s_attr4_old "\")") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr5 \""           taz_s_attr5_old "\")") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr6 \""           taz_s_attr6_old "\")") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr7 \""           taz_s_attr7_old "\")") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr8 \""           taz_s_attr8_old "\")") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr9 \""           taz_s_attr9_old "\")") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_attr10 \""          taz_s_attr10_old "\")") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_section_angle " (rtos taz_s_section_angle_old 2 6) ")") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_section_position "(itoa taz_s_section_position_old) ")") taz_s_f_beam_data)

      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_sweep_p1 (list " (rtos taz_s_p1x 2 6) " " (rtos taz_s_p1y 2 6) " " (rtos taz_s_p1z 2 6) "))") taz_s_f_beam_data)
      (write-line (strcat "(setq taz_s_" taz_s_attribs_object_name_new "_sweep_p2 (list " (rtos taz_s_p2x 2 6) " " (rtos taz_s_p2y 2 6) " " (rtos taz_s_p2z 2 6) "))") taz_s_f_beam_data)

      (close taz_s_f_beam_data)

      ;; wyczysc zmienne obiektu
      (setq taz_s_attribs_line nil)
      (setq taz_s_attribs_object_old nil)

    )
    (princ)
  )

  ;; wylacz tryb edycji
  (setq taz_s_edit_mode nil)
  (setq taz_s_edit_beam_path_mode nil)

  (princ)

)
