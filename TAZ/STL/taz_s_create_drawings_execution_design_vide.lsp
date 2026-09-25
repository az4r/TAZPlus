;; =====================================================================
;; TAZ_S_CREATE_DRAWINGS_EXECUTION_DESIGN_VIDE.LSP
;; =====================================================================
;;
;; Ten plik dodaje nowa logike opisana w pliku KONCEPCJA.txt.
;;
;; TLO PROBLEMU:
;; W pliku taz_s_create_drawings_execution_design.lsp wykonywane sa
;; dla kazdego przypadku (IZO, PRZYPADKI X, PRZYPADKI Y, PRZYPADKI Z)
;; dwa przebiegi SOLPROF:
;;   - jeden tylko dla obiektow podkladu (warstwa
;;     taz_s_xref_editing_layer) - wynik trafia na warstwy
;;     taz_s_xref_visible oraz taz_s_xref_hidden,
;;   - drugi wspolny, dla obiektow podkladu i obiektow modelu razem
;;     (warstwy taz_s_execution_design oraz taz_s_xref_editing_layer)
;;     - wynik trafia na warstwy taz_s_visible oraz taz_s_hidden.
;;
;; Funkcja SOLPROF nie zapamietuje z jakiego obiektu wejsciowego
;; powstala dana krzywa wynikowa. Dlatego czesc krzywych na warstwach
;; taz_s_visible / taz_s_hidden w rzeczywistosci geometrycznie
;; pochodzi z podkladu, a nie z modelu.
;;
;; TA LOGIKA ODZYSKUJE TA INFORMACJE PRZEZ POROWNANIE GEOMETRYCZNE:
;;
;; Jezeli obiekt z warstwy taz_s_visible pokrywa sie (co najmniej
;; dwoma roznymi punktami, z tolerancja taz_s_vide_tolerance) z
;; jakakolwiek krzywa z warstwy, ktorej nazwa zaczyna sie od
;; "taz_s_xref_", to ten obiekt jest przenoszony na warstwe
;; taz_s_xref_visible.
;;
;; Analogicznie obiekt z warstwy taz_s_hidden jest przenoszony na
;; warstwe taz_s_xref_hidden.
;;
;; Test pokrywania wykonywany jest funkcja VL vlax-curve-getClosestPointTo,
;; tak jak opisano w pliku KONCEPCJA.txt.
;;
;; USTALONE DECYZJE (potwierdzone przez uzytkownika):
;;   1) Zasieg porownania jest zawezony do jednego przypadku (IZO
;;      albo jednej iteracji petli PRZYPADKI X / Y / Z), a nie do
;;      calego rysunku. Do tego sluzy mechanizm znacznikow "przed
;;      przebiegiem", ktory jest juz uzywany w pliku
;;      taz_s_create_drawings_execution_design.lsp.
;;   2) Funkcje VL (vlax-curve-...) sa uzywane takze do generowania
;;      punktow probkujacych na obiekcie bazowym - nie tylko do testu
;;      pokrywania.
;;   3) Sprawdzanie czy warstwa jest warstwa podkladu wykonywane jest
;;      przez sprawdzenie przedrostka nazwy warstwy "taz_s_xref_",
;;      tak jak w pliku KONCEPCJA.txt, a nie przez wpisanie na trwale
;;      dwoch konkretnych nazw warstw.
;;
;; PROTOKOL WYWOLANIA Z PLIKU
;; taz_s_create_drawings_execution_design.lsp (dla kazdego przypadku):
;;
;;   1. Przed przebiegiem SOLPROF dla samego podkladu, niezaleznie
;;      od tego czy ten przebieg faktycznie sie wykona:
;;        (setq taz_s_vide_before_xref_pass nil)
;;
;;   2. Jezeli przebieg dla samego podkladu faktycznie sie wykonuje,
;;      jako pierwsza linia wewnatrz tego przebiegu:
;;        (setq taz_s_vide_before_xref_pass
;;          (taz_s_execution_design_get_last_entity)
;;        )
;;
;;   3. Jezeli wspolny przebieg SOLPROF (podklad + model razem)
;;      faktycznie sie wykonuje, jako pierwsza linia wewnatrz tego
;;      przebiegu:
;;        (setq taz_s_vide_before_combined_pass
;;          (taz_s_execution_design_get_last_entity)
;;        )
;;
;;   4. Bezposrednio po wywolaniu (taz_s_merge_solprof_layers) dla
;;      wspolnego przebiegu, jako ostatnia linia wewnatrz tego
;;      przebiegu:
;;        (taz_s_vide_correct_layers)
;;
;; Ten plik korzysta z funkcji taz_s_execution_design_get_last_entity
;; oraz taz_s_execution_design_collect_new_entities, ktore sa
;; zdefiniowane na poziomie glownym w pliku
;; taz_s_create_drawings_execution_design.lsp. Nie sa one w zaden
;; sposob modyfikowane przez ten plik - sa tylko wywolywane.
;;
;; =====================================================================

(vl-load-com)

;; ---------------------------------------------------------------
;; TOLERANCJA UZYWANA PRZY SPRAWDZANIU CZY PUNKT LEZY NA KRZYWEJ
;; PODKLADU. Zgodnie z KONCEPCJA.txt.
;; ---------------------------------------------------------------

(setq taz_s_vide_tolerance 0.001)

;; ---------------------------------------------------------------
;; ILE PUNKTOW PROBKUJACYCH PRZYPADA NA JEDEN SEGMENT OBIEKTU TYPU
;; LWPOLYLINE ALBO POLYLINE.
;; ---------------------------------------------------------------

(setq taz_s_vide_points_per_segment 10)


;; =====================================================================
;; FUNKCJA: taz_s_vide_get_polyline_point_count
;;
;; Ustala ile punktow probkujacych wygenerowac dla obiektu typu
;; LWPOLYLINE albo POLYLINE. Liczba segmentow jest odczytywana z
;; parametru krzywej (funkcje VL), a nie z rozbioru wierzcholkow, bo
;; zgodnie z Decyzja 2 korzystamy tutaj z funkcji VL.
;;
;; Wejscie (zmienna globalna):
;;   taz_s_vide_base_entity
;;
;; Wyjscie (zmienna globalna):
;;   taz_s_vide_polyline_point_count
;; =====================================================================

(defun taz_s_vide_get_polyline_point_count ()

  (setq taz_s_vide_polyline_start_param
    (vlax-curve-getStartParam taz_s_vide_base_entity)
  )

  (setq taz_s_vide_polyline_end_param
    (vlax-curve-getEndParam taz_s_vide_base_entity)
  )

  (setq taz_s_vide_polyline_param_span
    (- taz_s_vide_polyline_end_param taz_s_vide_polyline_start_param)
  )

  (setq taz_s_vide_polyline_segment_count
    (fix (+ 0.5 taz_s_vide_polyline_param_span))
  )

  (setq taz_s_vide_polyline_point_count
    (* taz_s_vide_polyline_segment_count taz_s_vide_points_per_segment)
  )

  (if (< taz_s_vide_polyline_point_count 3)
    (setq taz_s_vide_polyline_point_count 3)
  )

  taz_s_vide_polyline_point_count
)


;; =====================================================================
;; FUNKCJA: taz_s_vide_get_point_count
;;
;; Ustala ile punktow probkujacych wygenerowac na aktualnym obiekcie
;; bazowym (taz_s_vide_base_entity), w zaleznosci od jego typu,
;; zgodnie z zasadami z pliku KONCEPCJA.txt:
;;   LINE        -> 3 punkty
;;   ARC         -> 20 punktow
;;   CIRCLE      -> 30 punktow
;;   ELLIPSE     -> 30 punktow
;;   SPLINE      -> 50 punktow
;;   LWPOLYLINE  -> zalezy od liczby segmentow
;;   POLYLINE    -> zalezy od liczby segmentow
;;   kazdy inny typ -> nil (nie probkujemy tego obiektu)
;;
;; Wejscie (zmienna globalna):
;;   taz_s_vide_base_entity_type
;;
;; Wyjscie (zmienna globalna):
;;   taz_s_vide_point_count
;; =====================================================================

(defun taz_s_vide_get_point_count ()

  (setq taz_s_vide_point_count nil)

  (if (= taz_s_vide_base_entity_type "LINE")
    (setq taz_s_vide_point_count 3)
  )

  (if (= taz_s_vide_base_entity_type "ARC")
    (setq taz_s_vide_point_count 20)
  )

  (if (= taz_s_vide_base_entity_type "CIRCLE")
    (setq taz_s_vide_point_count 30)
  )

  (if (= taz_s_vide_base_entity_type "ELLIPSE")
    (setq taz_s_vide_point_count 30)
  )

  (if (= taz_s_vide_base_entity_type "SPLINE")
    (setq taz_s_vide_point_count 50)
  )

  (if (= taz_s_vide_base_entity_type "LWPOLYLINE")
    (setq taz_s_vide_point_count (taz_s_vide_get_polyline_point_count))
  )

  (if (= taz_s_vide_base_entity_type "POLYLINE")
    (setq taz_s_vide_point_count (taz_s_vide_get_polyline_point_count))
  )

  taz_s_vide_point_count
)


;; =====================================================================
;; FUNKCJA: taz_s_vide_generate_points_on_base_entity
;;
;; Generuje liste punktow rozlozonych rownomiernie po parametrze
;; krzywej taz_s_vide_base_entity, od parametru startowego do
;; parametru koncowego, wlacznie z obydwoma koncami. Liczba punktow
;; to taz_s_vide_point_count.
;;
;; Dziala jednakowo dla kazdego typu krzywej (LINE, ARC, CIRCLE,
;; ELLIPSE, LWPOLYLINE, POLYLINE, SPLINE), poniewaz korzystamy z
;; funkcji VL dzialajacych na dowolnym typie krzywej (Decyzja 2).
;;
;; Wejscie (zmienna globalna):
;;   taz_s_vide_base_entity
;;   taz_s_vide_point_count
;;
;; Wyjscie (zmienna globalna):
;;   taz_s_vide_point_list
;; =====================================================================

(defun taz_s_vide_generate_points_on_base_entity ()

  (setq taz_s_vide_point_list '())

  (setq taz_s_vide_start_param
    (vlax-curve-getStartParam taz_s_vide_base_entity)
  )

  (setq taz_s_vide_end_param
    (vlax-curve-getEndParam taz_s_vide_base_entity)
  )

  (setq taz_s_vide_param_span
    (- taz_s_vide_end_param taz_s_vide_start_param)
  )

  (setq taz_s_vide_point_index 0)

  (repeat taz_s_vide_point_count

    (setq taz_s_vide_param_fraction
      (/
        (float taz_s_vide_point_index)
        (float (- taz_s_vide_point_count 1))
      )
    )

    (setq taz_s_vide_current_param
      (+ taz_s_vide_start_param (* taz_s_vide_param_span taz_s_vide_param_fraction))
    )

    (setq taz_s_vide_current_point nil)

    (setq taz_s_vide_current_point
      (vlax-curve-getPointAtParam taz_s_vide_base_entity taz_s_vide_current_param)
    )

    (if taz_s_vide_current_point
      (setq taz_s_vide_point_list
        (append taz_s_vide_point_list (list taz_s_vide_current_point))
      )
    )

    (setq taz_s_vide_point_index (+ taz_s_vide_point_index 1))

  )

  taz_s_vide_point_list
)


;; =====================================================================
;; FUNKCJA: taz_s_vide_is_point_on_current_xref_entity
;;
;; Sprawdza czy punkt taz_s_vide_current_point lezy na krzywej
;; taz_s_vide_current_xref_entity, z tolerancja taz_s_vide_tolerance.
;;
;; Test wykonywany jest funkcja VL vlax-curve-getClosestPointTo,
;; tak jak opisano w pliku KONCEPCJA.txt:
;;   Q = vlax-curve-getClosestPointTo(objXref, P)
;;   dist = distance(P, Q)
;;   jesli dist < tolerancja -> punkt P lezy na krzywej.
;;
;; Wejscie (zmienna globalna):
;;   taz_s_vide_current_point
;;   taz_s_vide_current_xref_entity
;;   taz_s_vide_tolerance
;;
;; Wyjscie (zmienna globalna):
;;   taz_s_vide_point_on_curve_result
;; =====================================================================

(defun taz_s_vide_is_point_on_current_xref_entity ()

  (setq taz_s_vide_point_on_curve_result nil)

  (setq taz_s_vide_closest_point
    (vlax-curve-getClosestPointTo
      taz_s_vide_current_xref_entity
      taz_s_vide_current_point
    )
  )

  (if taz_s_vide_closest_point
    (progn

      (setq taz_s_vide_distance_value
        (distance taz_s_vide_current_point taz_s_vide_closest_point)
      )

      (if (< taz_s_vide_distance_value taz_s_vide_tolerance)
        (setq taz_s_vide_point_on_curve_result T)
      )

    )
  )

  taz_s_vide_point_on_curve_result
)


;; =====================================================================
;; FUNKCJA: taz_s_vide_test_point_against_xref_entities
;;
;; Sprawdza czy punkt taz_s_vide_current_point lezy na jakiejkolwiek
;; krzywej z listy taz_s_vide_filtered_xref_entities. Sprawdzanie
;; jest przerywane jak tylko znajdzie sie pierwsze trafienie, bo dla
;; jednego punktu potrzebna jest tylko odpowiedz "trafiono / nie
;; trafiono".
;;
;; Wejscie (zmienna globalna):
;;   taz_s_vide_current_point
;;   taz_s_vide_filtered_xref_entities
;;
;; Wyjscie (zmienna globalna):
;;   taz_s_vide_point_matched_result
;; =====================================================================

(defun taz_s_vide_test_point_against_xref_entities ()

  (setq taz_s_vide_point_matched_result nil)

  (setq taz_s_vide_xref_entity_index 0)

  (setq taz_s_vide_xref_entity_count
    (length taz_s_vide_filtered_xref_entities)
  )

  (while
    (and
      (< taz_s_vide_xref_entity_index taz_s_vide_xref_entity_count)
      (not taz_s_vide_point_matched_result)
    )

    (setq taz_s_vide_current_xref_entity
      (nth taz_s_vide_xref_entity_index taz_s_vide_filtered_xref_entities)
    )

    (if (taz_s_vide_is_point_on_current_xref_entity)
      (setq taz_s_vide_point_matched_result T)
    )

    (setq taz_s_vide_xref_entity_index (+ taz_s_vide_xref_entity_index 1))

  )

  taz_s_vide_point_matched_result
)


;; =====================================================================
;; FUNKCJA: taz_s_vide_count_matching_points
;;
;; Zlicza ile punktow z listy taz_s_vide_point_list trafia na
;; jakakolwiek krzywa z listy taz_s_vide_filtered_xref_entities.
;;
;; Wejscie (zmienna globalna):
;;   taz_s_vide_point_list
;;   taz_s_vide_filtered_xref_entities
;;
;; Wyjscie (zmienna globalna):
;;   taz_s_vide_hit_count
;; =====================================================================

(defun taz_s_vide_count_matching_points ()

  (setq taz_s_vide_hit_count 0)

  (setq taz_s_vide_point_list_index 0)

  (setq taz_s_vide_point_list_count (length taz_s_vide_point_list))

  (while (< taz_s_vide_point_list_index taz_s_vide_point_list_count)

    (setq taz_s_vide_current_point
      (nth taz_s_vide_point_list_index taz_s_vide_point_list)
    )

    (if (taz_s_vide_test_point_against_xref_entities)
      (setq taz_s_vide_hit_count (+ taz_s_vide_hit_count 1))
    )

    (setq taz_s_vide_point_list_index (+ taz_s_vide_point_list_index 1))

  )

  taz_s_vide_hit_count
)


;; =====================================================================
;; FUNKCJA: taz_s_vide_layer_name_starts_with_xref_prefix
;;
;; Sprawdza czy nazwa warstwy taz_s_vide_layer_name_to_check zaczyna
;; sie od przedrostka "taz_s_xref_", tak jak opisano w pliku
;; KONCEPCJA.txt (Decyzja 3 - przedrostek, a nie dwie zapisane na
;; trwale nazwy warstw).
;;
;; Wejscie (zmienna globalna):
;;   taz_s_vide_layer_name_to_check
;;
;; Wyjscie (zmienna globalna):
;;   taz_s_vide_layer_prefix_result
;; =====================================================================

(defun taz_s_vide_layer_name_starts_with_xref_prefix ()

  (setq taz_s_vide_layer_prefix_result nil)

  (setq taz_s_vide_layer_name_prefix_text "taz_s_xref_")

  (setq taz_s_vide_layer_name_prefix_length
    (strlen taz_s_vide_layer_name_prefix_text)
  )

  (setq taz_s_vide_layer_name_actual_length
    (strlen taz_s_vide_layer_name_to_check)
  )

  (if (>= taz_s_vide_layer_name_actual_length taz_s_vide_layer_name_prefix_length)
    (progn

      (setq taz_s_vide_layer_name_actual_prefix
        (substr
          taz_s_vide_layer_name_to_check
          1
          taz_s_vide_layer_name_prefix_length
        )
      )

      (if
        (=
          (strcase taz_s_vide_layer_name_actual_prefix)
          (strcase taz_s_vide_layer_name_prefix_text)
        )
        (setq taz_s_vide_layer_prefix_result T)
      )

    )
  )

  taz_s_vide_layer_prefix_result
)


;; =====================================================================
;; FUNKCJA: taz_s_vide_filter_xref_entities
;;
;; Z listy taz_s_vide_new_xref_entities (obiekty nowo utworzone w
;; przebiegu SOLPROF dla samego podkladu) wybiera tylko te obiekty,
;; ktorych aktualna warstwa faktycznie zaczyna sie od przedrostka
;; "taz_s_xref_". Wynik to lista obiektow, wzgledem ktorych bedziemy
;; testowac obiekty bazowe.
;;
;; Wejscie (zmienna globalna):
;;   taz_s_vide_new_xref_entities
;;
;; Wyjscie (zmienna globalna):
;;   taz_s_vide_filtered_xref_entities
;; =====================================================================

(defun taz_s_vide_filter_xref_entities ()

  (setq taz_s_vide_filtered_xref_entities '())

  (setq taz_s_vide_xref_candidate_index 0)

  (setq taz_s_vide_xref_candidate_count
    (length taz_s_vide_new_xref_entities)
  )

  (while (< taz_s_vide_xref_candidate_index taz_s_vide_xref_candidate_count)

    (setq taz_s_vide_xref_candidate_entity
      (nth taz_s_vide_xref_candidate_index taz_s_vide_new_xref_entities)
    )

    (setq taz_s_vide_xref_candidate_data
      (entget taz_s_vide_xref_candidate_entity)
    )

    (if taz_s_vide_xref_candidate_data

      (progn

        (setq taz_s_vide_layer_name_to_check
          (cdr (assoc 8 taz_s_vide_xref_candidate_data))
        )

        (if (taz_s_vide_layer_name_starts_with_xref_prefix)
          (setq taz_s_vide_filtered_xref_entities
            (append
              taz_s_vide_filtered_xref_entities
              (list taz_s_vide_xref_candidate_entity)
            )
          )
        )

      )

    )

    (setq taz_s_vide_xref_candidate_index (+ taz_s_vide_xref_candidate_index 1))

  )

  taz_s_vide_filtered_xref_entities
)


;; =====================================================================
;; FUNKCJA: taz_s_vide_get_target_layer_name
;;
;; Na podstawie aktualnej warstwy obiektu bazowego
;; (taz_s_vide_base_entity_layer_name) ustala nazwe warstwy
;; docelowej:
;;   taz_s_visible -> taz_s_xref_visible
;;   taz_s_hidden  -> taz_s_xref_hidden
;;   kazda inna warstwa -> nil (ten obiekt nas nie interesuje)
;;
;; Wejscie (zmienna globalna):
;;   taz_s_vide_base_entity_layer_name
;;
;; Wyjscie (zmienna globalna):
;;   taz_s_vide_target_layer_name
;; =====================================================================

(defun taz_s_vide_get_target_layer_name ()

  (setq taz_s_vide_target_layer_name nil)

  (if (= (strcase taz_s_vide_base_entity_layer_name) (strcase "taz_s_visible"))
    (setq taz_s_vide_target_layer_name "taz_s_xref_visible")
  )

  (if (= (strcase taz_s_vide_base_entity_layer_name) (strcase "taz_s_hidden"))
    (setq taz_s_vide_target_layer_name "taz_s_xref_hidden")
  )

  taz_s_vide_target_layer_name
)


;; =====================================================================
;; FUNKCJA: taz_s_vide_move_base_entity_to_target_layer
;;
;; Przenosi aktualny obiekt bazowy na warstwe docelowa. Uzywa tego
;; samego sposobu co funkcja taz_s_merge_solprof_layers w pliku
;; taz_s_create_drawings_execution_design.lsp, czyli entmod na
;; kodzie DXF 8 (warstwa).
;;
;; Wejscie (zmienna globalna):
;;   taz_s_vide_base_entity_data
;;   taz_s_vide_target_layer_name
;; =====================================================================

(defun taz_s_vide_move_base_entity_to_target_layer ()

  (setq taz_s_vide_new_entity_data
    (subst
      (cons 8 taz_s_vide_target_layer_name)
      (assoc 8 taz_s_vide_base_entity_data)
      taz_s_vide_base_entity_data
    )
  )

  (entmod taz_s_vide_new_entity_data)

  (princ)
)


;; =====================================================================
;; FUNKCJA: taz_s_vide_process_one_base_entity
;;
;; Przetwarza jeden obiekt bazowy (taz_s_vide_base_entity):
;;   1. odczytuje jego dane oraz aktualna warstwe,
;;   2. ustala warstwe docelowa; jesli nie ma warstwy docelowej, nie
;;      robi nic wiecej z tym obiektem,
;;   3. ustala liczbe punktow probkujacych dla jego typu; jesli typ
;;      nie jest obslugiwany, nie robi nic wiecej z tym obiektem,
;;   4. generuje punkty probkujace,
;;   5. zlicza ile z tych punktow trafia na krzywe podkladu,
;;   6. jesli trafily co najmniej dwa rozne punkty, przenosi obiekt
;;      na warstwe docelowa.
;;
;; Wejscie (zmienna globalna):
;;   taz_s_vide_base_entity
;;   taz_s_vide_filtered_xref_entities
;; =====================================================================

(defun taz_s_vide_process_one_base_entity ()

  (setq taz_s_vide_base_entity_data (entget taz_s_vide_base_entity))

  (if taz_s_vide_base_entity_data

    (progn

      (setq taz_s_vide_base_entity_type
        (cdr (assoc 0 taz_s_vide_base_entity_data))
      )

      (setq taz_s_vide_base_entity_layer_name
        (cdr (assoc 8 taz_s_vide_base_entity_data))
      )

      (setq taz_s_vide_target_layer_name (taz_s_vide_get_target_layer_name))

      (if taz_s_vide_target_layer_name

        (progn

          (setq taz_s_vide_point_count (taz_s_vide_get_point_count))

          (if taz_s_vide_point_count

            (progn

              (setq taz_s_vide_point_list
                (taz_s_vide_generate_points_on_base_entity)
              )

              (setq taz_s_vide_hit_count (taz_s_vide_count_matching_points))

              (if (>= taz_s_vide_hit_count 2)
                (taz_s_vide_move_base_entity_to_target_layer)
              )

            )

          )

        )

      )

    )

  )

  (princ)
)


;; =====================================================================
;; FUNKCJA: taz_s_vide_correct_layers
;;
;; Glowna funkcja tego pliku. Wywolywana z pliku
;; taz_s_create_drawings_execution_design.lsp bezposrednio po
;; zakonczeniu wspolnego przebiegu SOLPROF (podklad + model razem),
;; zgodnie z protokolem opisanym na poczatku tego pliku.
;;
;; Wejscie (zmienna globalna, ustawiane przez plik wywolujacy):
;;   taz_s_vide_before_xref_pass     (moze byc nil, jesli w tym
;;                                    przypadku nie bylo przebiegu
;;                                    SOLPROF dla samego podkladu)
;;   taz_s_vide_before_combined_pass
;; =====================================================================

(defun taz_s_vide_correct_layers ()

  (setq taz_s_vide_new_xref_entities nil)

  (if taz_s_vide_before_xref_pass
    (setq taz_s_vide_new_xref_entities
      (taz_s_execution_design_collect_new_entities
        taz_s_vide_before_xref_pass
      )
    )
  )

  (setq taz_s_vide_new_base_entities nil)

  (if taz_s_vide_before_combined_pass
    (setq taz_s_vide_new_base_entities
      (taz_s_execution_design_collect_new_entities
        taz_s_vide_before_combined_pass
      )
    )
  )

  (setq taz_s_vide_filtered_xref_entities (taz_s_vide_filter_xref_entities))

  (if taz_s_vide_filtered_xref_entities

    (progn

      (setq taz_s_vide_base_entity_index 0)

      (setq taz_s_vide_base_entity_count
        (length taz_s_vide_new_base_entities)
      )

      (while (< taz_s_vide_base_entity_index taz_s_vide_base_entity_count)

        (setq taz_s_vide_base_entity
          (nth taz_s_vide_base_entity_index taz_s_vide_new_base_entities)
        )

        (taz_s_vide_process_one_base_entity)

        (setq taz_s_vide_base_entity_index (+ taz_s_vide_base_entity_index 1))

      )

    )

  )

  (princ)
)

(princ)
