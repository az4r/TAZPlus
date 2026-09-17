;; ============================================================
;; TAZ_S_EXPLODE.LSP
;;
;; Usuwa dane TAZ przypisane do wskazanego obiektu.
;;
;; Skrypt NIE zmienia geometrii ani wlasciwosci obiektu.
;; Nie wykonuje polecenia AutoCAD/GstarCAD EXPLODE.
;;
;; Dane sa rozpoznawane na podstawie HANDLE obiektu:
;;
;;   taz_s_<HANDLE>_attr1
;;   taz_s_<HANDLE>_attr2
;;   taz_s_<HANDLE>_attr3
;;   taz_s_<HANDLE>_attr4
;;   taz_s_<HANDLE>_attr5
;;   taz_s_<HANDLE>_attr6
;;   taz_s_<HANDLE>_attr7
;;   taz_s_<HANDLE>_attr8
;;   taz_s_<HANDLE>_attr9
;;   taz_s_<HANDLE>_attr10
;;   taz_s_<HANDLE>_section_angle
;;   taz_s_<HANDLE>_section_position
;;   taz_s_<HANDLE>_sweep_p1
;;   taz_s_<HANDLE>_sweep_p2
;;
;; Dane sa usuwane:
;;
;;   1. z biezacej sesji AutoLISP
;;   2. z pliku taz_s_beam_data.txt
;;
;; Sam obiekt pozostaje bez zmian.
;;
;; ============================================================


(defun c:taz_s_explode ()

  ;; ----------------------------------------------------------
  ;; ZAPISZ AKTUALNE USTAWIENIA
  ;; ----------------------------------------------------------

  (taz_s_current_settings_save)


  ;; ----------------------------------------------------------
  ;; POBIERZ OBIEKT
  ;; ----------------------------------------------------------

  (setq taz_s_explode_selection
        (entsel "\nWskaz obiekt do usuniecia danych: "))


  ;; ----------------------------------------------------------
  ;; SPRAWDZ CZY OBIEKT ZOSTAL WSKAZANY
  ;; ----------------------------------------------------------

  (if (null taz_s_explode_selection)

    (progn

      (print "Nie wybrano obiektu.")

      (taz_s_current_settings_restore)

      (princ)
    )

    (progn

      ;; ------------------------------------------------------
      ;; POBIERZ ENAME OBIEKTU
      ;; ------------------------------------------------------

      (setq taz_s_explode_object
            (car taz_s_explode_selection))


      ;; ------------------------------------------------------
      ;; POBIERZ DANE OBIEKTU
      ;; ------------------------------------------------------

      (setq taz_s_explode_object_data
            (entget taz_s_explode_object))


      ;; ------------------------------------------------------
      ;; POBIERZ HANDLE OBIEKTU
      ;; ------------------------------------------------------

      (setq taz_s_explode_object_handle
            (cdr (assoc 5 taz_s_explode_object_data)))


      ;; ------------------------------------------------------
      ;; SPRAWDZ CZY HANDLE ISTNIEJE
      ;; ------------------------------------------------------

      (if (null taz_s_explode_object_handle)

        (progn

          (print
            "Nie mozna odczytac HANDLE wskazanego obiektu."
          )

          (taz_s_current_settings_restore)

          (princ)
        )

        (progn

          ;; --------------------------------------------------
          ;; ZBUDUJ PREFIKS DANYCH DLA HANDLE
          ;; --------------------------------------------------

          (setq taz_s_explode_tag
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_"
                )
          )


          ;; --------------------------------------------------
          ;; ZBUDUJ SCIEZKE DO PLIKU DANYCH
          ;; --------------------------------------------------

          (setq taz_s_explode_data_file
                (strcat
                  (taz_s_path)
                  "taz_s_beam_data.txt"
                )
          )


          ;; --------------------------------------------------
          ;; SPRAWDZ DANE W BIEZACEJ SESJI
          ;; --------------------------------------------------

          (setq taz_s_explode_memory_found nil)


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_attr1"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_attr2"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_attr3"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_attr4"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_attr5"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_attr6"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_attr7"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_attr8"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_attr9"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_attr10"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_section_angle"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_section_position"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_sweep_p1"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          (if
            (boundp
              (read
                (strcat
                  "taz_s_"
                  taz_s_explode_object_handle
                  "_sweep_p2"
                )
              )
            )
            (setq taz_s_explode_memory_found T)
          )


          ;; --------------------------------------------------
          ;; SPRAWDZ DANE W PLIKU TXT
          ;; --------------------------------------------------

          (setq taz_s_explode_file_found nil)

          (setq taz_s_explode_lines_kept nil)

          (setq taz_s_explode_f_read
                (open
                  taz_s_explode_data_file
                  "r"
                )
          )


          ;; --------------------------------------------------
          ;; JEZELI PLIK ISTNIEJE - CZYTAJ GO LINIA PO LINII
          ;; --------------------------------------------------

          (if taz_s_explode_f_read

            (progn

              (while
                (setq taz_s_explode_line
                      (read-line
                        taz_s_explode_f_read
                      )
                )

                (if
                  (wcmatch
                    taz_s_explode_line
                    (strcat
                      "*"
                      taz_s_explode_tag
                      "*"
                    )
                  )

                  (setq taz_s_explode_file_found T)

                  (setq taz_s_explode_lines_kept
                        (cons
                          taz_s_explode_line
                          taz_s_explode_lines_kept
                        )
                  )
                )
              )


              (close taz_s_explode_f_read)


              (setq taz_s_explode_lines_kept
                    (reverse
                      taz_s_explode_lines_kept
                    )
              )
            )
          )


          ;; --------------------------------------------------
          ;; SPRAWDZ CZY DANE W OGOLE ISTNIEJA
          ;; --------------------------------------------------

          (if
            (and
              (not taz_s_explode_memory_found)
              (not taz_s_explode_file_found)
            )

            (progn

              (print
                (strcat
                  "Obiekt o HANDLE "
                  taz_s_explode_object_handle
                  " nie ma danych TAZ."
                )
              )
            )

            (progn

              ;; ----------------------------------------------
              ;; USUN DANE Z BIEZACEJ SESJI
              ;; ----------------------------------------------

              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_attr1"
                  )
                )
                nil
              )


              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_attr2"
                  )
                )
                nil
              )


              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_attr3"
                  )
                )
                nil
              )


              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_attr4"
                  )
                )
                nil
              )


              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_attr5"
                  )
                )
                nil
              )


              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_attr6"
                  )
                )
                nil
              )


              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_attr7"
                  )
                )
                nil
              )


              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_attr8"
                  )
                )
                nil
              )


              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_attr9"
                  )
                )
                nil
              )


              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_attr10"
                  )
                )
                nil
              )


              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_section_angle"
                  )
                )
                nil
              )


              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_section_position"
                  )
                )
                nil
              )


              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_sweep_p1"
                  )
                )
                nil
              )


              (set
                (read
                  (strcat
                    "taz_s_"
                    taz_s_explode_object_handle
                    "_sweep_p2"
                  )
                )
                nil
              )


              ;; ----------------------------------------------
              ;; USUN DANE Z PLIKU TXT
              ;; ----------------------------------------------

              (if taz_s_explode_file_found

                (progn

                  (setq taz_s_explode_f_write
                        (open
                          taz_s_explode_data_file
                          "w"
                        )
                  )


                  (if taz_s_explode_f_write

                    (progn

                      (foreach
                        taz_s_explode_line
                        taz_s_explode_lines_kept

                        (write-line
                          taz_s_explode_line
                          taz_s_explode_f_write
                        )
                      )


                      (close taz_s_explode_f_write)
                    )
                  )
                )
              )


              ;; ----------------------------------------------
              ;; INFORMACJA
              ;; ----------------------------------------------

              (print
                (strcat
                  "Usunieto dane TAZ dla obiektu o HANDLE "
                  taz_s_explode_object_handle
                  "."
                )
              )
            )
          )


          ;; --------------------------------------------------
          ;; CZYSZCZENIE ZMIENNYCH ROBOCZYCH
          ;; --------------------------------------------------

          (setq taz_s_explode_selection nil)

          (setq taz_s_explode_object nil)

          (setq taz_s_explode_object_data nil)

          (setq taz_s_explode_object_handle nil)

          (setq taz_s_explode_tag nil)

          (setq taz_s_explode_data_file nil)

          (setq taz_s_explode_memory_found nil)

          (setq taz_s_explode_file_found nil)

          (setq taz_s_explode_lines_kept nil)

          (setq taz_s_explode_f_read nil)

          (setq taz_s_explode_f_write nil)

          (setq taz_s_explode_line nil)


          ;; --------------------------------------------------
          ;; PRZYWROC USTAWIENIA
          ;; --------------------------------------------------

          (taz_s_current_settings_restore)

          (princ)
        )
      )
    )
  )
)


(princ)