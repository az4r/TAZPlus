;; ============================================================================
;; TAZ_S_CREATE_DRAWINGS_EXECUTION_DESIGN_VIDE.LSP
;;
;; Ten plik dotyczy wylacznie SOLPROF w taz_s_create_drawings_execution_design.lsp.
;; Tam kazdy przypadek (IZO / X / Y / Z) przechodzi przez SOLPROF DWA razy:
;;
;;   1. przebieg - tylko obiekty podkladu (taz_s_xref_editing_layer)
;;      -> wynik trafia na warstwy taz_s_xref_visible / taz_s_xref_hidden
;;
;;   2. przebieg - podklad razem z belkami / plytami
;;      (taz_s_execution_design + taz_s_xref_editing_layer razem)
;;      -> wynik trafia na warstwy taz_s_visible / taz_s_hidden
;;
;; Ten sam podklad bierze udzial w obu przebiegach. Krawedz podkladu, ktorej
;; nie zaslania zadna belka / plyta, wychodzi z obu przebiegow jako ten sam
;; odcinek (lub luk) w przestrzeni WCS - tylko raz trafia na warstwe podkladu
;; (taz_s_xref_visible / taz_s_xref_hidden), a drugi raz na zwykla warstwe
;; (taz_s_visible / taz_s_hidden), tak jakby byla elementem modelu.
;;
;; Funkcja taz_s_vide_reclassify_edges sprawdza kazda encje z 2. przebiegu
;; (aktualnie na taz_s_visible lub taz_s_hidden) i szuka wsrod encji z
;; 1. przebiegu (taz_s_xref_visible + taz_s_xref_hidden razem, symetrycznie -
;; bez wzgledu na to, czy sprawdzana encja jest widoczna czy ukryta) takiej,
;; ktora jest z nia rownolegla (wspolliniowa) i nakladajaca sie. Jesli tak,
;; encja z 2. przebiegu wraca na wlasciwa warstwe podkladu:
;;   taz_s_visible -> taz_s_xref_visible
;;   taz_s_hidden  -> taz_s_xref_hidden
;;
;; Wywolanie (z taz_s_create_drawings_execution_design.lsp, dla kazdego z 4
;; przypadkow osobno, zaraz po drugim / wspolnym przebiegu SOLPROF):
;;
;;   (taz_s_vide_reclassify_edges taz_s_vide_combined_entities taz_s_vide_xref_entities)
;;
;; gdzie:
;;   taz_s_vide_combined_entities - lista enames utworzonych przez 2. przebieg
;;   taz_s_vide_xref_entities     - lista enames utworzonych przez 1. przebieg
;;
;; Obie listy sa zbierane przez juz istniejace funkcje pomocnicze z pliku
;; taz_s_create_drawings_execution_design.lsp:
;;   taz_s_execution_design_get_last_entity
;;   taz_s_execution_design_collect_new_entities
;;
;; Obslugiwane typy encji SOLPROF: LINE, ARC, CIRCLE, ELLIPSE, LWPOLYLINE,
;; POLYLINE, SPLINE (ten sam zestaw co w taz_s_is_sweepable_xref_curve).
;; Kazda taka encja jest rozbijana na liste prostych "prymitywow" - tylko
;; do celow POROWNANIA. Sama encja nigdy nie jest dzielona - jesli
;; ktorykolwiek jej prymityw sie pokrywa, cala encja zmienia warstwe.
;;
;; Prymityw ma postac listy:
;;   ("LIN" p1 p2)                                  - odcinek prosty
;;   ("CIR" srodek promien normalna kat1 kat2)       - luk / okrag (LINE nie, ARC/CIRCLE tak)
;;   ("ELL" srodek wektor_osi_glownej normalna param1 param2) - luk eliptyczny
;;
;; ============================================================================


;; ----------------------------------------------------------------------------
;; TOLERANCJE
;; ----------------------------------------------------------------------------

(setq taz_s_vide_parallel_tol 1e-6)
(setq taz_s_vide_offset_tol 0.05)
(setq taz_s_vide_min_overlap 0.05)
(setq taz_s_vide_radius_tol 0.05)
(setq taz_s_vide_min_overlap_ang 0.0005)
(setq taz_s_vide_full_circle_angle (* 2.0 pi))


;; ----------------------------------------------------------------------------
;; POMOCNICZE - PODSTAWOWE DZIALANIA NA WEKTORACH 3D (WSPOLRZEDNE WCS)
;; ----------------------------------------------------------------------------

;; Zwraca wektor prowadzacy od taz_s_vide_point_a do taz_s_vide_point_b.
;; Ta sama funkcja sluzy tez do odejmowania dwoch wektorow (np. dwoch
;; wektorow osi glownej elipsy) - wtedy argumenty nie sa punktami tylko
;; wektorami, ale odejmowanie wspolrzednych dziala tak samo.

(defun taz_s_vide_vector_between_points (taz_s_vide_point_a taz_s_vide_point_b)

  (setq taz_s_vide_vector_result
    (list
      (- (car taz_s_vide_point_b) (car taz_s_vide_point_a))
      (- (cadr taz_s_vide_point_b) (cadr taz_s_vide_point_a))
      (- (caddr taz_s_vide_point_b) (caddr taz_s_vide_point_a))
    )
  )

  taz_s_vide_vector_result
)


(defun taz_s_vide_dot_product (taz_s_vide_vector_a taz_s_vide_vector_b)

  (setq taz_s_vide_dot_result
    (+
      (* (car taz_s_vide_vector_a) (car taz_s_vide_vector_b))
      (* (cadr taz_s_vide_vector_a) (cadr taz_s_vide_vector_b))
      (* (caddr taz_s_vide_vector_a) (caddr taz_s_vide_vector_b))
    )
  )

  taz_s_vide_dot_result
)


(defun taz_s_vide_cross_product (taz_s_vide_vector_a taz_s_vide_vector_b)

  (setq taz_s_vide_cross_x
    (-
      (* (cadr taz_s_vide_vector_a) (caddr taz_s_vide_vector_b))
      (* (caddr taz_s_vide_vector_a) (cadr taz_s_vide_vector_b))
    )
  )

  (setq taz_s_vide_cross_y
    (-
      (* (caddr taz_s_vide_vector_a) (car taz_s_vide_vector_b))
      (* (car taz_s_vide_vector_a) (caddr taz_s_vide_vector_b))
    )
  )

  (setq taz_s_vide_cross_z
    (-
      (* (car taz_s_vide_vector_a) (cadr taz_s_vide_vector_b))
      (* (cadr taz_s_vide_vector_a) (car taz_s_vide_vector_b))
    )
  )

  (setq taz_s_vide_cross_result
    (list taz_s_vide_cross_x taz_s_vide_cross_y taz_s_vide_cross_z)
  )

  taz_s_vide_cross_result
)


(defun taz_s_vide_vector_length (taz_s_vide_vector_a)

  (setq taz_s_vide_length_result
    (sqrt
      (+
        (* (car taz_s_vide_vector_a) (car taz_s_vide_vector_a))
        (* (cadr taz_s_vide_vector_a) (cadr taz_s_vide_vector_a))
        (* (caddr taz_s_vide_vector_a) (caddr taz_s_vide_vector_a))
      )
    )
  )

  taz_s_vide_length_result
)


(defun taz_s_vide_minimum_of_two (taz_s_vide_value_a taz_s_vide_value_b)

  (setq taz_s_vide_minimum_result taz_s_vide_value_a)
  (if (< taz_s_vide_value_b taz_s_vide_minimum_result)
    (setq taz_s_vide_minimum_result taz_s_vide_value_b)
  )

  taz_s_vide_minimum_result
)


(defun taz_s_vide_maximum_of_two (taz_s_vide_value_a taz_s_vide_value_b)

  (setq taz_s_vide_maximum_result taz_s_vide_value_a)
  (if (> taz_s_vide_value_b taz_s_vide_maximum_result)
    (setq taz_s_vide_maximum_result taz_s_vide_value_b)
  )

  taz_s_vide_maximum_result
)


;; ----------------------------------------------------------------------------
;; POMOCNICZE - ROWNOLEGLOSC WEKTOROW
;; ----------------------------------------------------------------------------

;; Dwa wektory sa rownolegle, jesli sinus kata miedzy nimi (dlugosc ich
;; znormalizowanego iloczynu wektorowego) jest ponizej tolerancji.

(defun taz_s_vide_vectors_parallel_p (taz_s_vide_vector_a taz_s_vide_vector_b)

  (setq taz_s_vide_parallel_result nil)

  (setq taz_s_vide_length_a (taz_s_vide_vector_length taz_s_vide_vector_a))
  (setq taz_s_vide_length_b (taz_s_vide_vector_length taz_s_vide_vector_b))

  (if (and (> taz_s_vide_length_a 1e-9) (> taz_s_vide_length_b 1e-9))
    (progn
      (setq taz_s_vide_cross_vector (taz_s_vide_cross_product taz_s_vide_vector_a taz_s_vide_vector_b))
      (setq taz_s_vide_cross_length (taz_s_vide_vector_length taz_s_vide_cross_vector))
      (setq taz_s_vide_sine_of_angle (/ taz_s_vide_cross_length (* taz_s_vide_length_a taz_s_vide_length_b)))
      (if (< taz_s_vide_sine_of_angle taz_s_vide_parallel_tol)
        (setq taz_s_vide_parallel_result T)
      )
    )
  )

  taz_s_vide_parallel_result
)


;; Rownolegle I zwrocone w te sama strone (iloczyn skalarny dodatni).
;; Uzywane do porownywania normalnych (210) dwoch lukow / okregow / elips -
;; przeciwnie zwrocona normalna oznacza inna orientacje przekroju, wiec to
;; NIE jest ten sam luk.

(defun taz_s_vide_vectors_same_direction_p (taz_s_vide_vector_a taz_s_vide_vector_b)

  (setq taz_s_vide_same_direction_result nil)

  (if (taz_s_vide_vectors_parallel_p taz_s_vide_vector_a taz_s_vide_vector_b)
    (progn
      (setq taz_s_vide_dot_check (taz_s_vide_dot_product taz_s_vide_vector_a taz_s_vide_vector_b))
      (if (> taz_s_vide_dot_check 0.0)
        (setq taz_s_vide_same_direction_result T)
      )
    )
  )

  taz_s_vide_same_direction_result
)


;; ----------------------------------------------------------------------------
;; POMOCNICZE - KATY (DLA LUKOW / OKREGOW / ELIPS)
;; ----------------------------------------------------------------------------

;; Sprowadza kat do przedzialu [0, 2*pi).

(defun taz_s_vide_normalize_angle (taz_s_vide_angle_value)

  (while (>= taz_s_vide_angle_value taz_s_vide_full_circle_angle)
    (setq taz_s_vide_angle_value (- taz_s_vide_angle_value taz_s_vide_full_circle_angle))
  )

  (while (< taz_s_vide_angle_value 0.0)
    (setq taz_s_vide_angle_value (+ taz_s_vide_angle_value taz_s_vide_full_circle_angle))
  )

  taz_s_vide_angle_value
)


;; Rozwartosc luku liczona od kata poczatkowego do koncowego, zawsze
;; dodatnia (w kierunku przeciwnym do wskazowek zegara, tak jak ARC/CIRCLE
;; w DXF). Gdy kat koncowy wychodzi "przed" poczatkowy po normalizacji,
;; dodajemy pelny obrot.

(defun taz_s_vide_angular_sweep (taz_s_vide_sweep_start_angle taz_s_vide_sweep_end_angle)

  (setq taz_s_vide_sweep_start_normalized (taz_s_vide_normalize_angle taz_s_vide_sweep_start_angle))
  (setq taz_s_vide_sweep_end_normalized (taz_s_vide_normalize_angle taz_s_vide_sweep_end_angle))

  (setq taz_s_vide_sweep_result (- taz_s_vide_sweep_end_normalized taz_s_vide_sweep_start_normalized))

  (if (<= taz_s_vide_sweep_result 1e-12)
    (setq taz_s_vide_sweep_result (+ taz_s_vide_sweep_result taz_s_vide_full_circle_angle))
  )

  taz_s_vide_sweep_result
)


;; Sprawdza, czy dwa zakresy katowe (start + rozwartosc, obydwa w radianach,
;; obydwa dodatnie i rosnace) naprawde sie naklada. Zakres B jest sprawdzany
;; przesuniety o -2*pi, 0 i +2*pi, zeby poprawnie zlapac przypadek zawiniecia
;; przez 0/360 stopni.

(defun taz_s_vide_angular_ranges_overlap_p
  (taz_s_vide_start_a taz_s_vide_sweep_a taz_s_vide_start_b taz_s_vide_sweep_b)

  (setq taz_s_vide_angular_overlap_found nil)

  (setq taz_s_vide_end_a (+ taz_s_vide_start_a taz_s_vide_sweep_a))

  (setq taz_s_vide_wrap_index -1)
  (while (and (not taz_s_vide_angular_overlap_found) (<= taz_s_vide_wrap_index 1))

    (setq taz_s_vide_start_b_shifted
      (+ taz_s_vide_start_b (* taz_s_vide_wrap_index taz_s_vide_full_circle_angle))
    )
    (setq taz_s_vide_end_b_shifted (+ taz_s_vide_start_b_shifted taz_s_vide_sweep_b))

    (setq taz_s_vide_overlap_low (taz_s_vide_maximum_of_two taz_s_vide_start_a taz_s_vide_start_b_shifted))
    (setq taz_s_vide_overlap_high (taz_s_vide_minimum_of_two taz_s_vide_end_a taz_s_vide_end_b_shifted))

    (if (> (- taz_s_vide_overlap_high taz_s_vide_overlap_low) taz_s_vide_min_overlap_ang)
      (setq taz_s_vide_angular_overlap_found T)
    )

    (setq taz_s_vide_wrap_index (1+ taz_s_vide_wrap_index))
  )

  taz_s_vide_angular_overlap_found
)


;; ----------------------------------------------------------------------------
;; POMOCNICZE - "TEN SAM OKRAG / LUK" ORAZ "TA SAMA ELIPSA"
;; ----------------------------------------------------------------------------

;; Argumenty to dane prymitywu CIR BEZ znacznika typu, czyli lista
;; (srodek promien normalna kat1 kat2).

(defun taz_s_vide_same_circle_p (taz_s_vide_circle_data_a taz_s_vide_circle_data_b)

  (setq taz_s_vide_same_circle_result nil)

  (setq taz_s_vide_center_a (nth 0 taz_s_vide_circle_data_a))
  (setq taz_s_vide_radius_a (nth 1 taz_s_vide_circle_data_a))
  (setq taz_s_vide_normal_a (nth 2 taz_s_vide_circle_data_a))

  (setq taz_s_vide_center_b (nth 0 taz_s_vide_circle_data_b))
  (setq taz_s_vide_radius_b (nth 1 taz_s_vide_circle_data_b))
  (setq taz_s_vide_normal_b (nth 2 taz_s_vide_circle_data_b))

  (if
    (and
      (< (abs (- taz_s_vide_radius_a taz_s_vide_radius_b)) taz_s_vide_radius_tol)
      (taz_s_vide_vectors_same_direction_p taz_s_vide_normal_a taz_s_vide_normal_b)
      (< (distance taz_s_vide_center_a taz_s_vide_center_b) taz_s_vide_offset_tol)
    )
    (setq taz_s_vide_same_circle_result T)
  )

  taz_s_vide_same_circle_result
)


;; Argumenty to dane prymitywu ELL bez znacznika typu, czyli lista
;; (srodek wektor_osi_glownej normalna param1 param2). Os mala nie jest
;; potrzebna do porownania - wynika z osi glownej, normalnej i promienia
;; DXF 40, wiec przy tej samej osi glownej i normalnej jest tez taka sama.

(defun taz_s_vide_same_ellipse_p (taz_s_vide_ellipse_data_a taz_s_vide_ellipse_data_b)

  (setq taz_s_vide_same_ellipse_result nil)

  (setq taz_s_vide_ell_center_a (nth 0 taz_s_vide_ellipse_data_a))
  (setq taz_s_vide_ell_major_a (nth 1 taz_s_vide_ellipse_data_a))
  (setq taz_s_vide_ell_normal_a (nth 2 taz_s_vide_ellipse_data_a))

  (setq taz_s_vide_ell_center_b (nth 0 taz_s_vide_ellipse_data_b))
  (setq taz_s_vide_ell_major_b (nth 1 taz_s_vide_ellipse_data_b))
  (setq taz_s_vide_ell_normal_b (nth 2 taz_s_vide_ellipse_data_b))

  (setq taz_s_vide_major_axis_difference
    (taz_s_vide_vector_between_points taz_s_vide_ell_major_a taz_s_vide_ell_major_b)
  )
  (setq taz_s_vide_major_axis_difference_length (taz_s_vide_vector_length taz_s_vide_major_axis_difference))

  (if
    (and
      (taz_s_vide_vectors_same_direction_p taz_s_vide_ell_normal_a taz_s_vide_ell_normal_b)
      (< (distance taz_s_vide_ell_center_a taz_s_vide_ell_center_b) taz_s_vide_offset_tol)
      (< taz_s_vide_major_axis_difference_length taz_s_vide_offset_tol)
    )
    (setq taz_s_vide_same_ellipse_result T)
  )

  taz_s_vide_same_ellipse_result
)


;; ----------------------------------------------------------------------------
;; PRZELICZENIE BULGE (LWPOLYLINE / POLYLINE) NA LUK
;; ----------------------------------------------------------------------------
;; taz_s_vide_bulge_point_a / b - konce segmentu, jako punkty 2D (x y)
;; taz_s_vide_bulge_value       - wartosc bulge (DXF 42)
;;
;; Zwraca liste (srodek_2d promien kat1 kat2) albo nil, jesli segment jest
;; zdegenerowany (zerowa cieciwa albo bulge praktycznie rowny zero).
;; ----------------------------------------------------------------------------

(defun taz_s_vide_bulge_to_arc (taz_s_vide_bulge_point_a taz_s_vide_bulge_point_b taz_s_vide_bulge_value)

  (setq taz_s_vide_bulge_arc_result nil)

  (setq taz_s_vide_bulge_chord_length (distance taz_s_vide_bulge_point_a taz_s_vide_bulge_point_b))
  (setq taz_s_vide_bulge_included_angle (* 4.0 (atan taz_s_vide_bulge_value)))

  (if
    (and
      (> taz_s_vide_bulge_chord_length 1e-9)
      (> (abs (sin (/ taz_s_vide_bulge_included_angle 2.0))) 1e-9)
    )
    (progn

      (setq taz_s_vide_bulge_radius
        (/ taz_s_vide_bulge_chord_length (* 2.0 (sin (/ taz_s_vide_bulge_included_angle 2.0))))
      )
      (setq taz_s_vide_bulge_radius (abs taz_s_vide_bulge_radius))

      (setq taz_s_vide_bulge_chord_angle (angle taz_s_vide_bulge_point_a taz_s_vide_bulge_point_b))

      (setq taz_s_vide_bulge_center
        (polar
          taz_s_vide_bulge_point_a
          (+ taz_s_vide_bulge_chord_angle (- (/ pi 2.0) (/ taz_s_vide_bulge_included_angle 2.0)))
          taz_s_vide_bulge_radius
        )
      )

      (setq taz_s_vide_bulge_angle_1 (angle taz_s_vide_bulge_center taz_s_vide_bulge_point_a))
      (setq taz_s_vide_bulge_angle_2 (angle taz_s_vide_bulge_center taz_s_vide_bulge_point_b))

      (if (< taz_s_vide_bulge_value 0.0)
        (progn
          (setq taz_s_vide_bulge_angle_swap taz_s_vide_bulge_angle_1)
          (setq taz_s_vide_bulge_angle_1 taz_s_vide_bulge_angle_2)
          (setq taz_s_vide_bulge_angle_2 taz_s_vide_bulge_angle_swap)
        )
      )

      (setq taz_s_vide_bulge_arc_result
        (list
          taz_s_vide_bulge_center
          taz_s_vide_bulge_radius
          taz_s_vide_bulge_angle_1
          taz_s_vide_bulge_angle_2
        )
      )
    )
  )

  taz_s_vide_bulge_arc_result
)


;; ----------------------------------------------------------------------------
;; NAKLADANIE SIE DWOCH PRYMITYWOW TEGO SAMEGO TYPU
;; ----------------------------------------------------------------------------

;; Argumenty - dane BEZ znacznika typu: (p1 p2).

(defun taz_s_vide_linear_primitives_overlap_p (taz_s_vide_line_data_a taz_s_vide_line_data_b)

  (setq taz_s_vide_linear_overlap_result nil)

  (setq taz_s_vide_line_a_point_1 (nth 0 taz_s_vide_line_data_a))
  (setq taz_s_vide_line_a_point_2 (nth 1 taz_s_vide_line_data_a))
  (setq taz_s_vide_line_b_point_1 (nth 0 taz_s_vide_line_data_b))
  (setq taz_s_vide_line_b_point_2 (nth 1 taz_s_vide_line_data_b))

  (setq taz_s_vide_direction_a
    (taz_s_vide_vector_between_points taz_s_vide_line_a_point_1 taz_s_vide_line_a_point_2)
  )
  (setq taz_s_vide_direction_b
    (taz_s_vide_vector_between_points taz_s_vide_line_b_point_1 taz_s_vide_line_b_point_2)
  )
  (setq taz_s_vide_direction_a_length (taz_s_vide_vector_length taz_s_vide_direction_a))

  (setq taz_s_vide_vector_between_starts
    (taz_s_vide_vector_between_points taz_s_vide_line_a_point_1 taz_s_vide_line_b_point_1)
  )

  ;; Warunek 1: wektory rownolegle.
  ;; Warunek 2: poczatek odcinka B lezy na tej samej prostej co odcinek A
  ;; (to daje prawdziwa wspolliniowosc, a nie tylko rownoleglosc z przesunieciem).
  (if
    (and
      (> taz_s_vide_direction_a_length 1e-9)
      (taz_s_vide_vectors_parallel_p taz_s_vide_direction_a taz_s_vide_direction_b)
      (taz_s_vide_vectors_parallel_p taz_s_vide_direction_a taz_s_vide_vector_between_starts)
    )
    (progn

      (setq taz_s_vide_unit_x (/ (car taz_s_vide_direction_a) taz_s_vide_direction_a_length))
      (setq taz_s_vide_unit_y (/ (cadr taz_s_vide_direction_a) taz_s_vide_direction_a_length))
      (setq taz_s_vide_unit_z (/ (caddr taz_s_vide_direction_a) taz_s_vide_direction_a_length))
      (setq taz_s_vide_unit_direction (list taz_s_vide_unit_x taz_s_vide_unit_y taz_s_vide_unit_z))

      ;; Rzut punktow obu odcinkow na wspolna os (poczatek = pierwszy punkt A).
      (setq taz_s_vide_param_a1 0.0)
      (setq taz_s_vide_param_a2 taz_s_vide_direction_a_length)

      (setq taz_s_vide_vector_to_b1
        (taz_s_vide_vector_between_points taz_s_vide_line_a_point_1 taz_s_vide_line_b_point_1)
      )
      (setq taz_s_vide_vector_to_b2
        (taz_s_vide_vector_between_points taz_s_vide_line_a_point_1 taz_s_vide_line_b_point_2)
      )

      (setq taz_s_vide_param_b1 (taz_s_vide_dot_product taz_s_vide_unit_direction taz_s_vide_vector_to_b1))
      (setq taz_s_vide_param_b2 (taz_s_vide_dot_product taz_s_vide_unit_direction taz_s_vide_vector_to_b2))

      (setq taz_s_vide_range_a_min (taz_s_vide_minimum_of_two taz_s_vide_param_a1 taz_s_vide_param_a2))
      (setq taz_s_vide_range_a_max (taz_s_vide_maximum_of_two taz_s_vide_param_a1 taz_s_vide_param_a2))
      (setq taz_s_vide_range_b_min (taz_s_vide_minimum_of_two taz_s_vide_param_b1 taz_s_vide_param_b2))
      (setq taz_s_vide_range_b_max (taz_s_vide_maximum_of_two taz_s_vide_param_b1 taz_s_vide_param_b2))

      (setq taz_s_vide_overlap_low (taz_s_vide_maximum_of_two taz_s_vide_range_a_min taz_s_vide_range_b_min))
      (setq taz_s_vide_overlap_high (taz_s_vide_minimum_of_two taz_s_vide_range_a_max taz_s_vide_range_b_max))

      (if (> (- taz_s_vide_overlap_high taz_s_vide_overlap_low) taz_s_vide_min_overlap)
        (setq taz_s_vide_linear_overlap_result T)
      )
    )
  )

  taz_s_vide_linear_overlap_result
)


;; Argumenty - dane BEZ znacznika typu: (srodek promien normalna kat1 kat2).

(defun taz_s_vide_circular_primitives_overlap_p (taz_s_vide_circle_data_a taz_s_vide_circle_data_b)

  (setq taz_s_vide_circular_overlap_result nil)

  (if (taz_s_vide_same_circle_p taz_s_vide_circle_data_a taz_s_vide_circle_data_b)
    (progn
      (setq taz_s_vide_start_a (taz_s_vide_normalize_angle (nth 3 taz_s_vide_circle_data_a)))
      (setq taz_s_vide_sweep_a
        (taz_s_vide_angular_sweep (nth 3 taz_s_vide_circle_data_a) (nth 4 taz_s_vide_circle_data_a))
      )
      (setq taz_s_vide_start_b (taz_s_vide_normalize_angle (nth 3 taz_s_vide_circle_data_b)))
      (setq taz_s_vide_sweep_b
        (taz_s_vide_angular_sweep (nth 3 taz_s_vide_circle_data_b) (nth 4 taz_s_vide_circle_data_b))
      )

      (setq taz_s_vide_circular_overlap_result
        (taz_s_vide_angular_ranges_overlap_p taz_s_vide_start_a taz_s_vide_sweep_a taz_s_vide_start_b taz_s_vide_sweep_b)
      )
    )
  )

  taz_s_vide_circular_overlap_result
)


;; Argumenty - dane BEZ znacznika typu: (srodek wektor_osi_glownej normalna param1 param2).

(defun taz_s_vide_elliptical_primitives_overlap_p (taz_s_vide_ellipse_data_a taz_s_vide_ellipse_data_b)

  (setq taz_s_vide_elliptical_overlap_result nil)

  (if (taz_s_vide_same_ellipse_p taz_s_vide_ellipse_data_a taz_s_vide_ellipse_data_b)
    (progn
      (setq taz_s_vide_start_a (taz_s_vide_normalize_angle (nth 3 taz_s_vide_ellipse_data_a)))
      (setq taz_s_vide_sweep_a
        (taz_s_vide_angular_sweep (nth 3 taz_s_vide_ellipse_data_a) (nth 4 taz_s_vide_ellipse_data_a))
      )
      (setq taz_s_vide_start_b (taz_s_vide_normalize_angle (nth 3 taz_s_vide_ellipse_data_b)))
      (setq taz_s_vide_sweep_b
        (taz_s_vide_angular_sweep (nth 3 taz_s_vide_ellipse_data_b) (nth 4 taz_s_vide_ellipse_data_b))
      )

      (setq taz_s_vide_elliptical_overlap_result
        (taz_s_vide_angular_ranges_overlap_p taz_s_vide_start_a taz_s_vide_sweep_a taz_s_vide_start_b taz_s_vide_sweep_b)
      )
    )
  )

  taz_s_vide_elliptical_overlap_result
)


;; Glowny "rozdzielacz" - dwa PELNE prymitywy (ze znacznikiem typu na
;; pierwszym miejscu). Porownywane sa tylko prymitywy tego samego typu.

(defun taz_s_vide_primitives_overlap_p (taz_s_vide_primitive_a taz_s_vide_primitive_b)

  (setq taz_s_vide_primitive_overlap_result nil)

  (setq taz_s_vide_primitive_type_a (car taz_s_vide_primitive_a))
  (setq taz_s_vide_primitive_type_b (car taz_s_vide_primitive_b))

  (if (= taz_s_vide_primitive_type_a taz_s_vide_primitive_type_b)
    (progn

      (if (= taz_s_vide_primitive_type_a "LIN")
        (setq taz_s_vide_primitive_overlap_result
          (taz_s_vide_linear_primitives_overlap_p (cdr taz_s_vide_primitive_a) (cdr taz_s_vide_primitive_b))
        )
      )

      (if (= taz_s_vide_primitive_type_a "CIR")
        (setq taz_s_vide_primitive_overlap_result
          (taz_s_vide_circular_primitives_overlap_p (cdr taz_s_vide_primitive_a) (cdr taz_s_vide_primitive_b))
        )
      )

      (if (= taz_s_vide_primitive_type_a "ELL")
        (setq taz_s_vide_primitive_overlap_result
          (taz_s_vide_elliptical_primitives_overlap_p (cdr taz_s_vide_primitive_a) (cdr taz_s_vide_primitive_b))
        )
      )
    )
  )

  taz_s_vide_primitive_overlap_result
)


;; ----------------------------------------------------------------------------
;; ROZBICIE ENCJI NA LISTE PRYMITYWOW - JEDNA FUNKCJA NA TYP DXF
;;
;; Sposob wyciagania punktow WCS / OCS jest wzorowany na juz istniejacej
;; funkcji taz_s_get_xref_curve_point z taz_s_create_drawings_execution_design.lsp.
;; ----------------------------------------------------------------------------

;; LINE - punkty 10 i 11 sa zawsze zapisane wprost w WCS.

(defun taz_s_vide_primitives_of_line (taz_s_vide_line_entity)

  (setq taz_s_vide_line_data (entget taz_s_vide_line_entity))

  (setq taz_s_vide_line_start (cdr (assoc 10 taz_s_vide_line_data)))
  (setq taz_s_vide_line_end (cdr (assoc 11 taz_s_vide_line_data)))

  (setq taz_s_vide_line_primitive (list "LIN" taz_s_vide_line_start taz_s_vide_line_end))

  (list taz_s_vide_line_primitive)
)


;; ARC - srodek (10) jest w OCS luku, trzeba go przeliczyc do WCS.
;; Kat poczatkowy (50) i koncowy (51) zostaja bez zmian - to kierunki w OCS.

(defun taz_s_vide_primitives_of_arc (taz_s_vide_arc_entity)

  (setq taz_s_vide_arc_data (entget taz_s_vide_arc_entity))

  (setq taz_s_vide_arc_normal (cdr (assoc 210 taz_s_vide_arc_data)))
  (if (not taz_s_vide_arc_normal)
    (setq taz_s_vide_arc_normal (list 0.0 0.0 1.0))
  )

  (setq taz_s_vide_arc_center_ocs (cdr (assoc 10 taz_s_vide_arc_data)))
  (setq taz_s_vide_arc_center (trans taz_s_vide_arc_center_ocs taz_s_vide_arc_entity 0))

  (setq taz_s_vide_arc_radius (cdr (assoc 40 taz_s_vide_arc_data)))
  (setq taz_s_vide_arc_start_angle (cdr (assoc 50 taz_s_vide_arc_data)))
  (setq taz_s_vide_arc_end_angle (cdr (assoc 51 taz_s_vide_arc_data)))

  (setq taz_s_vide_arc_primitive
    (list
      "CIR"
      taz_s_vide_arc_center
      taz_s_vide_arc_radius
      taz_s_vide_arc_normal
      taz_s_vide_arc_start_angle
      taz_s_vide_arc_end_angle
    )
  )

  (list taz_s_vide_arc_primitive)
)


;; CIRCLE - jak ARC, tylko rozwartosc to zawsze pelny obrot (0 .. 2*pi).
;; Dzieki temu CIRCLE nie jest pomijany - jest po prostu lukiem pelnym.

(defun taz_s_vide_primitives_of_circle (taz_s_vide_circle_entity)

  (setq taz_s_vide_circle_data (entget taz_s_vide_circle_entity))

  (setq taz_s_vide_circle_normal (cdr (assoc 210 taz_s_vide_circle_data)))
  (if (not taz_s_vide_circle_normal)
    (setq taz_s_vide_circle_normal (list 0.0 0.0 1.0))
  )

  (setq taz_s_vide_circle_center_ocs (cdr (assoc 10 taz_s_vide_circle_data)))
  (setq taz_s_vide_circle_center (trans taz_s_vide_circle_center_ocs taz_s_vide_circle_entity 0))

  (setq taz_s_vide_circle_radius (cdr (assoc 40 taz_s_vide_circle_data)))

  (setq taz_s_vide_circle_primitive
    (list
      "CIR"
      taz_s_vide_circle_center
      taz_s_vide_circle_radius
      taz_s_vide_circle_normal
      0.0
      taz_s_vide_full_circle_angle
    )
  )

  (list taz_s_vide_circle_primitive)
)


;; ELLIPSE - srodek (10) jest juz zapisany wprost w WCS (tak jak w
;; taz_s_get_xref_curve_point), podobnie jak wektor osi glownej (11).
;; Parametry 41/42 to zakres katowy (pelna elipsa, gdy ich brak).

(defun taz_s_vide_primitives_of_ellipse (taz_s_vide_ellipse_entity)

  (setq taz_s_vide_ellipse_data (entget taz_s_vide_ellipse_entity))

  (setq taz_s_vide_ellipse_primitives nil)

  (setq taz_s_vide_ellipse_center (cdr (assoc 10 taz_s_vide_ellipse_data)))
  (setq taz_s_vide_ellipse_major_axis_vector (cdr (assoc 11 taz_s_vide_ellipse_data)))

  (setq taz_s_vide_ellipse_normal (cdr (assoc 210 taz_s_vide_ellipse_data)))
  (if (not taz_s_vide_ellipse_normal)
    (setq taz_s_vide_ellipse_normal (list 0.0 0.0 1.0))
  )

  (setq taz_s_vide_ellipse_start_param (cdr (assoc 41 taz_s_vide_ellipse_data)))
  (setq taz_s_vide_ellipse_end_param (cdr (assoc 42 taz_s_vide_ellipse_data)))
  (if (not taz_s_vide_ellipse_start_param)
    (setq taz_s_vide_ellipse_start_param 0.0)
  )
  (if (not taz_s_vide_ellipse_end_param)
    (setq taz_s_vide_ellipse_end_param taz_s_vide_full_circle_angle)
  )

  (if (and taz_s_vide_ellipse_center taz_s_vide_ellipse_major_axis_vector)
    (progn
      (setq taz_s_vide_ellipse_primitive
        (list
          "ELL"
          taz_s_vide_ellipse_center
          taz_s_vide_ellipse_major_axis_vector
          taz_s_vide_ellipse_normal
          taz_s_vide_ellipse_start_param
          taz_s_vide_ellipse_end_param
        )
      )
      (setq taz_s_vide_ellipse_primitives (list taz_s_vide_ellipse_primitive))
    )
  )

  taz_s_vide_ellipse_primitives
)


;; LWPOLYLINE - kazdy segment (wierzcholek do wierzcholka) staje sie
;; osobnym prymitywem LIN (bulge = 0) albo CIR (bulge <> 0, przez
;; taz_s_vide_bulge_to_arc). Zamknieta polilinia dostaje dodatkowy
;; segment domykajacy (ostatni wierzcholek -> pierwszy).
;; Punkty 10 sa w OCS (elewacja w 38), wiec kazdy trzeba przeliczyc do WCS.

(defun taz_s_vide_primitives_of_lwpolyline (taz_s_vide_lwpoly_entity)

  (setq taz_s_vide_lwpoly_data (entget taz_s_vide_lwpoly_entity))

  (setq taz_s_vide_lwpoly_flags (cdr (assoc 70 taz_s_vide_lwpoly_data)))
  (if (not taz_s_vide_lwpoly_flags)
    (setq taz_s_vide_lwpoly_flags 0)
  )
  (setq taz_s_vide_lwpoly_closed nil)
  (if (= 1 (logand 1 taz_s_vide_lwpoly_flags))
    (setq taz_s_vide_lwpoly_closed T)
  )

  (setq taz_s_vide_lwpoly_elevation (cdr (assoc 38 taz_s_vide_lwpoly_data)))
  (if (not taz_s_vide_lwpoly_elevation)
    (setq taz_s_vide_lwpoly_elevation 0.0)
  )

  (setq taz_s_vide_lwpoly_normal (cdr (assoc 210 taz_s_vide_lwpoly_data)))
  (if (not taz_s_vide_lwpoly_normal)
    (setq taz_s_vide_lwpoly_normal (list 0.0 0.0 1.0))
  )

  ;; Zbierz wierzcholki (10) oraz przypisane im bulge (42) - bulge zawsze
  ;; opisuje segment WYCHODZACY z danego wierzcholka.
  (setq taz_s_vide_lwpoly_vertex_list nil)
  (setq taz_s_vide_lwpoly_bulge_list nil)

  (setq taz_s_vide_lwpoly_dxf_walk taz_s_vide_lwpoly_data)
  (while taz_s_vide_lwpoly_dxf_walk

    (setq taz_s_vide_lwpoly_dxf_pair (car taz_s_vide_lwpoly_dxf_walk))

    (if (= (car taz_s_vide_lwpoly_dxf_pair) 10)
      (progn
        (setq taz_s_vide_lwpoly_vertex_list (cons (cdr taz_s_vide_lwpoly_dxf_pair) taz_s_vide_lwpoly_vertex_list))
        (setq taz_s_vide_lwpoly_bulge_list (cons 0.0 taz_s_vide_lwpoly_bulge_list))
      )
    )

    (if (and (= (car taz_s_vide_lwpoly_dxf_pair) 42) taz_s_vide_lwpoly_bulge_list)
      (setq taz_s_vide_lwpoly_bulge_list
        (cons (cdr taz_s_vide_lwpoly_dxf_pair) (cdr taz_s_vide_lwpoly_bulge_list))
      )
    )

    (setq taz_s_vide_lwpoly_dxf_walk (cdr taz_s_vide_lwpoly_dxf_walk))
  )

  (setq taz_s_vide_lwpoly_vertex_list (reverse taz_s_vide_lwpoly_vertex_list))
  (setq taz_s_vide_lwpoly_bulge_list (reverse taz_s_vide_lwpoly_bulge_list))
  (setq taz_s_vide_lwpoly_vertex_count (length taz_s_vide_lwpoly_vertex_list))

  (setq taz_s_vide_lwpoly_segment_count taz_s_vide_lwpoly_vertex_count)
  (if (not taz_s_vide_lwpoly_closed)
    (setq taz_s_vide_lwpoly_segment_count (1- taz_s_vide_lwpoly_vertex_count))
  )

  (setq taz_s_vide_lwpoly_result nil)
  (setq taz_s_vide_lwpoly_index 0)

  (while (< taz_s_vide_lwpoly_index taz_s_vide_lwpoly_segment_count)

    (setq taz_s_vide_lwpoly_vertex_a (nth taz_s_vide_lwpoly_index taz_s_vide_lwpoly_vertex_list))
    (setq taz_s_vide_lwpoly_next_index (rem (1+ taz_s_vide_lwpoly_index) taz_s_vide_lwpoly_vertex_count))
    (setq taz_s_vide_lwpoly_vertex_b (nth taz_s_vide_lwpoly_next_index taz_s_vide_lwpoly_vertex_list))
    (setq taz_s_vide_lwpoly_bulge (nth taz_s_vide_lwpoly_index taz_s_vide_lwpoly_bulge_list))

    (if (< (abs taz_s_vide_lwpoly_bulge) 1e-9)

      (progn
        (setq taz_s_vide_lwpoly_point_a
          (trans
            (list (car taz_s_vide_lwpoly_vertex_a) (cadr taz_s_vide_lwpoly_vertex_a) taz_s_vide_lwpoly_elevation)
            taz_s_vide_lwpoly_entity
            0
          )
        )
        (setq taz_s_vide_lwpoly_point_b
          (trans
            (list (car taz_s_vide_lwpoly_vertex_b) (cadr taz_s_vide_lwpoly_vertex_b) taz_s_vide_lwpoly_elevation)
            taz_s_vide_lwpoly_entity
            0
          )
        )
        (setq taz_s_vide_lwpoly_primitive (list "LIN" taz_s_vide_lwpoly_point_a taz_s_vide_lwpoly_point_b))
        (setq taz_s_vide_lwpoly_result (cons taz_s_vide_lwpoly_primitive taz_s_vide_lwpoly_result))
      )

      (progn
        (setq taz_s_vide_lwpoly_arc_data
          (taz_s_vide_bulge_to_arc taz_s_vide_lwpoly_vertex_a taz_s_vide_lwpoly_vertex_b taz_s_vide_lwpoly_bulge)
        )
        (if taz_s_vide_lwpoly_arc_data
          (progn
            (setq taz_s_vide_lwpoly_arc_center_2d (nth 0 taz_s_vide_lwpoly_arc_data))
            (setq taz_s_vide_lwpoly_arc_center
              (trans
                (list
                  (car taz_s_vide_lwpoly_arc_center_2d)
                  (cadr taz_s_vide_lwpoly_arc_center_2d)
                  taz_s_vide_lwpoly_elevation
                )
                taz_s_vide_lwpoly_entity
                0
              )
            )
            (setq taz_s_vide_lwpoly_arc_radius (nth 1 taz_s_vide_lwpoly_arc_data))
            (setq taz_s_vide_lwpoly_arc_angle_1 (nth 2 taz_s_vide_lwpoly_arc_data))
            (setq taz_s_vide_lwpoly_arc_angle_2 (nth 3 taz_s_vide_lwpoly_arc_data))
            (setq taz_s_vide_lwpoly_primitive
              (list
                "CIR"
                taz_s_vide_lwpoly_arc_center
                taz_s_vide_lwpoly_arc_radius
                taz_s_vide_lwpoly_normal
                taz_s_vide_lwpoly_arc_angle_1
                taz_s_vide_lwpoly_arc_angle_2
              )
            )
            (setq taz_s_vide_lwpoly_result (cons taz_s_vide_lwpoly_primitive taz_s_vide_lwpoly_result))
          )
        )
      )
    )

    (setq taz_s_vide_lwpoly_index (1+ taz_s_vide_lwpoly_index))
  )

  taz_s_vide_lwpoly_result
)


;; POLYLINE (stary format, wierzcholki jako osobne encje VERTEX) - ta sama
;; zasada co LWPOLYLINE, tylko wierzcholki trzeba pobrac przez ENTNEXT.
;; Dla starej polilinii 3D (flaga 8) bulge nie ma zastosowania - traktujemy
;; ja jako zbior samych odcinkow prostych.

(defun taz_s_vide_primitives_of_polyline (taz_s_vide_poly_entity)

  (setq taz_s_vide_poly_data (entget taz_s_vide_poly_entity))

  (setq taz_s_vide_poly_flags (cdr (assoc 70 taz_s_vide_poly_data)))
  (if (not taz_s_vide_poly_flags)
    (setq taz_s_vide_poly_flags 0)
  )
  (setq taz_s_vide_poly_closed nil)
  (if (= 1 (logand 1 taz_s_vide_poly_flags))
    (setq taz_s_vide_poly_closed T)
  )
  (setq taz_s_vide_poly_is_3d nil)
  (if (= 8 (logand 8 taz_s_vide_poly_flags))
    (setq taz_s_vide_poly_is_3d T)
  )

  (setq taz_s_vide_poly_normal (cdr (assoc 210 taz_s_vide_poly_data)))
  (if (not taz_s_vide_poly_normal)
    (setq taz_s_vide_poly_normal (list 0.0 0.0 1.0))
  )

  (setq taz_s_vide_poly_vertex_list nil)
  (setq taz_s_vide_poly_bulge_list nil)

  (setq taz_s_vide_poly_vertex_entity (entnext taz_s_vide_poly_entity))
  (setq taz_s_vide_poly_vertex_type nil)
  (if taz_s_vide_poly_vertex_entity
    (setq taz_s_vide_poly_vertex_type (cdr (assoc 0 (entget taz_s_vide_poly_vertex_entity))))
  )

  (while (and taz_s_vide_poly_vertex_entity (= taz_s_vide_poly_vertex_type "VERTEX"))

    (setq taz_s_vide_poly_vertex_data (entget taz_s_vide_poly_vertex_entity))
    (setq taz_s_vide_poly_vertex_list
      (cons (cdr (assoc 10 taz_s_vide_poly_vertex_data)) taz_s_vide_poly_vertex_list)
    )

    (setq taz_s_vide_poly_bulge (cdr (assoc 42 taz_s_vide_poly_vertex_data)))
    (if (not taz_s_vide_poly_bulge)
      (setq taz_s_vide_poly_bulge 0.0)
    )
    (if taz_s_vide_poly_is_3d
      (setq taz_s_vide_poly_bulge 0.0)
    )
    (setq taz_s_vide_poly_bulge_list (cons taz_s_vide_poly_bulge taz_s_vide_poly_bulge_list))

    (setq taz_s_vide_poly_vertex_entity (entnext taz_s_vide_poly_vertex_entity))
    (setq taz_s_vide_poly_vertex_type nil)
    (if taz_s_vide_poly_vertex_entity
      (setq taz_s_vide_poly_vertex_type (cdr (assoc 0 (entget taz_s_vide_poly_vertex_entity))))
    )
  )

  (setq taz_s_vide_poly_vertex_list (reverse taz_s_vide_poly_vertex_list))
  (setq taz_s_vide_poly_bulge_list (reverse taz_s_vide_poly_bulge_list))
  (setq taz_s_vide_poly_vertex_count (length taz_s_vide_poly_vertex_list))

  (setq taz_s_vide_poly_segment_count taz_s_vide_poly_vertex_count)
  (if (not taz_s_vide_poly_closed)
    (setq taz_s_vide_poly_segment_count (1- taz_s_vide_poly_vertex_count))
  )

  (setq taz_s_vide_poly_result nil)
  (setq taz_s_vide_poly_index 0)

  (while (< taz_s_vide_poly_index taz_s_vide_poly_segment_count)

    (setq taz_s_vide_poly_vertex_a (nth taz_s_vide_poly_index taz_s_vide_poly_vertex_list))
    (setq taz_s_vide_poly_next_index (rem (1+ taz_s_vide_poly_index) taz_s_vide_poly_vertex_count))
    (setq taz_s_vide_poly_vertex_b (nth taz_s_vide_poly_next_index taz_s_vide_poly_vertex_list))
    (setq taz_s_vide_poly_bulge (nth taz_s_vide_poly_index taz_s_vide_poly_bulge_list))

    (if (< (abs taz_s_vide_poly_bulge) 1e-9)

      (progn
        (setq taz_s_vide_poly_point_a (trans taz_s_vide_poly_vertex_a taz_s_vide_poly_entity 0))
        (setq taz_s_vide_poly_point_b (trans taz_s_vide_poly_vertex_b taz_s_vide_poly_entity 0))
        (setq taz_s_vide_poly_primitive (list "LIN" taz_s_vide_poly_point_a taz_s_vide_poly_point_b))
        (setq taz_s_vide_poly_result (cons taz_s_vide_poly_primitive taz_s_vide_poly_result))
      )

      (progn
        (setq taz_s_vide_poly_vertex_a_2d (list (car taz_s_vide_poly_vertex_a) (cadr taz_s_vide_poly_vertex_a)))
        (setq taz_s_vide_poly_vertex_b_2d (list (car taz_s_vide_poly_vertex_b) (cadr taz_s_vide_poly_vertex_b)))

        (setq taz_s_vide_poly_arc_data
          (taz_s_vide_bulge_to_arc taz_s_vide_poly_vertex_a_2d taz_s_vide_poly_vertex_b_2d taz_s_vide_poly_bulge)
        )
        (if taz_s_vide_poly_arc_data
          (progn
            (setq taz_s_vide_poly_arc_center_2d (nth 0 taz_s_vide_poly_arc_data))
            (setq taz_s_vide_poly_arc_elevation 0.0)
            (if (caddr taz_s_vide_poly_vertex_a)
              (setq taz_s_vide_poly_arc_elevation (caddr taz_s_vide_poly_vertex_a))
            )
            (setq taz_s_vide_poly_arc_center
              (trans
                (list
                  (car taz_s_vide_poly_arc_center_2d)
                  (cadr taz_s_vide_poly_arc_center_2d)
                  taz_s_vide_poly_arc_elevation
                )
                taz_s_vide_poly_entity
                0
              )
            )
            (setq taz_s_vide_poly_arc_radius (nth 1 taz_s_vide_poly_arc_data))
            (setq taz_s_vide_poly_arc_angle_1 (nth 2 taz_s_vide_poly_arc_data))
            (setq taz_s_vide_poly_arc_angle_2 (nth 3 taz_s_vide_poly_arc_data))
            (setq taz_s_vide_poly_primitive
              (list
                "CIR"
                taz_s_vide_poly_arc_center
                taz_s_vide_poly_arc_radius
                taz_s_vide_poly_normal
                taz_s_vide_poly_arc_angle_1
                taz_s_vide_poly_arc_angle_2
              )
            )
            (setq taz_s_vide_poly_result (cons taz_s_vide_poly_primitive taz_s_vide_poly_result))
          )
        )
      )
    )

    (setq taz_s_vide_poly_index (1+ taz_s_vide_poly_index))
  )

  taz_s_vide_poly_result
)


;; SPLINE - sprowadzony do jednego prymitywu LIN: pierwszy i ostatni fit
;; point (kod 11), a gdy ich brak - pierwszy i ostatni control point
;; (kod 10). To jest uproszczenie (cieciwa zamiast prawdziwej krzywej),
;; taka sama zasada jak juz istniejaca taz_s_get_xref_curve_point.

(defun taz_s_vide_primitives_of_spline (taz_s_vide_spline_entity)

  (setq taz_s_vide_spline_data (entget taz_s_vide_spline_entity))
  (setq taz_s_vide_spline_result nil)

  (setq taz_s_vide_spline_point_list nil)
  (setq taz_s_vide_spline_dxf_walk taz_s_vide_spline_data)
  (while taz_s_vide_spline_dxf_walk
    (setq taz_s_vide_spline_dxf_pair (car taz_s_vide_spline_dxf_walk))
    (if (= (car taz_s_vide_spline_dxf_pair) 11)
      (setq taz_s_vide_spline_point_list (cons (cdr taz_s_vide_spline_dxf_pair) taz_s_vide_spline_point_list))
    )
    (setq taz_s_vide_spline_dxf_walk (cdr taz_s_vide_spline_dxf_walk))
  )
  (setq taz_s_vide_spline_point_list (reverse taz_s_vide_spline_point_list))

  (if (not taz_s_vide_spline_point_list)
    (progn
      (setq taz_s_vide_spline_dxf_walk taz_s_vide_spline_data)
      (while taz_s_vide_spline_dxf_walk
        (setq taz_s_vide_spline_dxf_pair (car taz_s_vide_spline_dxf_walk))
        (if (= (car taz_s_vide_spline_dxf_pair) 10)
          (setq taz_s_vide_spline_point_list (cons (cdr taz_s_vide_spline_dxf_pair) taz_s_vide_spline_point_list))
        )
        (setq taz_s_vide_spline_dxf_walk (cdr taz_s_vide_spline_dxf_walk))
      )
      (setq taz_s_vide_spline_point_list (reverse taz_s_vide_spline_point_list))
    )
  )

  (if (>= (length taz_s_vide_spline_point_list) 2)
    (progn
      (setq taz_s_vide_spline_first_point (car taz_s_vide_spline_point_list))
      (setq taz_s_vide_spline_reversed_list (reverse taz_s_vide_spline_point_list))
      (setq taz_s_vide_spline_last_point (car taz_s_vide_spline_reversed_list))
      (setq taz_s_vide_spline_primitive (list "LIN" taz_s_vide_spline_first_point taz_s_vide_spline_last_point))
      (setq taz_s_vide_spline_result (list taz_s_vide_spline_primitive))
    )
  )

  taz_s_vide_spline_result
)


;; ----------------------------------------------------------------------------
;; ROZDZIELACZ TYPOW DXF -> LISTA PRYMITYWOW
;;
;; Obslugiwany zestaw typow jest taki sam jak w taz_s_is_sweepable_xref_curve.
;; Kazdy inny typ (np. TEXT, MTEXT, INSERT) daje pusta liste - encja jest
;; wtedy po prostu pomijana przy dopasowywaniu.
;; ----------------------------------------------------------------------------

(defun taz_s_vide_entity_primitives (taz_s_vide_source_entity)

  (setq taz_s_vide_source_primitives nil)

  (if (and taz_s_vide_source_entity (entget taz_s_vide_source_entity))
    (progn

      (setq taz_s_vide_source_entity_type (cdr (assoc 0 (entget taz_s_vide_source_entity))))

      (if (= taz_s_vide_source_entity_type "LINE")
        (setq taz_s_vide_source_primitives (taz_s_vide_primitives_of_line taz_s_vide_source_entity))
      )

      (if (= taz_s_vide_source_entity_type "ARC")
        (setq taz_s_vide_source_primitives (taz_s_vide_primitives_of_arc taz_s_vide_source_entity))
      )

      (if (= taz_s_vide_source_entity_type "CIRCLE")
        (setq taz_s_vide_source_primitives (taz_s_vide_primitives_of_circle taz_s_vide_source_entity))
      )

      (if (= taz_s_vide_source_entity_type "ELLIPSE")
        (setq taz_s_vide_source_primitives (taz_s_vide_primitives_of_ellipse taz_s_vide_source_entity))
      )

      (if (= taz_s_vide_source_entity_type "LWPOLYLINE")
        (setq taz_s_vide_source_primitives (taz_s_vide_primitives_of_lwpolyline taz_s_vide_source_entity))
      )

      (if (= taz_s_vide_source_entity_type "POLYLINE")
        (setq taz_s_vide_source_primitives (taz_s_vide_primitives_of_polyline taz_s_vide_source_entity))
      )

      (if (= taz_s_vide_source_entity_type "SPLINE")
        (setq taz_s_vide_source_primitives (taz_s_vide_primitives_of_spline taz_s_vide_source_entity))
      )
    )
  )

  taz_s_vide_source_primitives
)


;; ----------------------------------------------------------------------------
;; CZY ENCJA POKRYWA SIE Z KTORYMKOLWIEK PRYMITYWEM Z LISTY KANDYDATOW
;; ----------------------------------------------------------------------------

(defun taz_s_vide_entity_matches_any_p (taz_s_vide_check_entity taz_s_vide_candidate_primitive_list)

  (setq taz_s_vide_found_match nil)

  (setq taz_s_vide_own_primitive_list (taz_s_vide_entity_primitives taz_s_vide_check_entity))

  (setq taz_s_vide_own_walk_list taz_s_vide_own_primitive_list)
  (while (and taz_s_vide_own_walk_list (not taz_s_vide_found_match))

    (setq taz_s_vide_own_current_primitive (car taz_s_vide_own_walk_list))

    (setq taz_s_vide_candidate_walk_list taz_s_vide_candidate_primitive_list)
    (while (and taz_s_vide_candidate_walk_list (not taz_s_vide_found_match))

      (setq taz_s_vide_candidate_current_primitive (car taz_s_vide_candidate_walk_list))

      (if (taz_s_vide_primitives_overlap_p taz_s_vide_own_current_primitive taz_s_vide_candidate_current_primitive)
        (setq taz_s_vide_found_match T)
      )

      (setq taz_s_vide_candidate_walk_list (cdr taz_s_vide_candidate_walk_list))
    )

    (setq taz_s_vide_own_walk_list (cdr taz_s_vide_own_walk_list))
  )

  taz_s_vide_found_match
)


;; ----------------------------------------------------------------------------
;; GLOWNA FUNKCJA - WYWOLYWANA Z taz_s_create_drawings_execution_design.lsp
;; ----------------------------------------------------------------------------
;; taz_s_vide_combined_entity_list - encje utworzone przez 2. (wspolny)
;;                                    przebieg SOLPROF danego przypadku
;;                                    (aktualnie na taz_s_visible / taz_s_hidden)
;; taz_s_vide_xref_entity_list      - encje utworzone przez 1. (sam podklad)
;;                                    przebieg SOLPROF tego samego przypadku
;;                                    (na taz_s_xref_visible / taz_s_xref_hidden)
;;
;; Porownanie jest symetryczne: zarowno encje widoczne jak i ukryte z
;; przebiegu 2. sa sprawdzane wzgledem CALEJ listy taz_s_vide_xref_entity_list
;; (bez wzgledu na to, czy dany kandydat trafil na xref_visible czy
;; xref_hidden). Warstwa docelowa zalezy tylko od AKTUALNEJ warstwy
;; sprawdzanej encji.
;; ----------------------------------------------------------------------------

(defun taz_s_vide_reclassify_edges (taz_s_vide_combined_entity_list taz_s_vide_xref_entity_list)

  ;; Zbuduj jedna, wspolna liste prymitywow ze WSZYSTKICH encji 1. przebiegu.
  (setq taz_s_vide_reference_primitive_list nil)

  (setq taz_s_vide_xref_walk_list taz_s_vide_xref_entity_list)
  (while taz_s_vide_xref_walk_list

    (setq taz_s_vide_xref_current_entity (car taz_s_vide_xref_walk_list))
    (setq taz_s_vide_reference_primitive_list
      (append taz_s_vide_reference_primitive_list (taz_s_vide_entity_primitives taz_s_vide_xref_current_entity))
    )

    (setq taz_s_vide_xref_walk_list (cdr taz_s_vide_xref_walk_list))
  )

  (if taz_s_vide_reference_primitive_list
    (progn

      (setq taz_s_vide_combined_walk_list taz_s_vide_combined_entity_list)
      (while taz_s_vide_combined_walk_list

        (setq taz_s_vide_combined_current_entity (car taz_s_vide_combined_walk_list))

        (if (and taz_s_vide_combined_current_entity (entget taz_s_vide_combined_current_entity))
          (progn

            (setq taz_s_vide_combined_current_data (entget taz_s_vide_combined_current_entity))
            (setq taz_s_vide_combined_current_layer (cdr (assoc 8 taz_s_vide_combined_current_data)))

            (setq taz_s_vide_target_layer nil)
            (if (= taz_s_vide_combined_current_layer "taz_s_visible")
              (setq taz_s_vide_target_layer "taz_s_xref_visible")
            )
            (if (= taz_s_vide_combined_current_layer "taz_s_hidden")
              (setq taz_s_vide_target_layer "taz_s_xref_hidden")
            )

            (if taz_s_vide_target_layer
              (progn
                (setq taz_s_vide_match_found
                  (taz_s_vide_entity_matches_any_p taz_s_vide_combined_current_entity taz_s_vide_reference_primitive_list)
                )
                (if taz_s_vide_match_found
                  (entmod
                    (subst
                      (cons 8 taz_s_vide_target_layer)
                      (assoc 8 taz_s_vide_combined_current_data)
                      taz_s_vide_combined_current_data
                    )
                  )
                )
              )
            )

          )
        )

        (setq taz_s_vide_combined_walk_list (cdr taz_s_vide_combined_walk_list))
      )
    )
  )

  (princ)
)

(princ)
