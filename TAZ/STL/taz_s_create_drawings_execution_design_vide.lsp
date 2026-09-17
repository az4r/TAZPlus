;; ============================================================================
;; taz_s_create_drawings_execution_design_vide.lsp
;; ============================================================================
;; Wykrywanie duplikatow SOLPROF miedzy dwoma przebiegami wykonywanymi w
;; taz_s_create_drawings_execution_design.lsp:
;;
;;   1. SOLPROF wykonany na samych "obiektach podkladu" (warstwa
;;      taz_s_xref_editing_layer) -> wynik na warstwach taz_s_xref_visible
;;      / taz_s_xref_hidden.
;;   2. SOLPROF wykonany wspolnie na "obiektach podkladu" + "obiektach
;;      modelu" (taz_s_execution_design) -> wynik na warstwach
;;      taz_s_visible / taz_s_hidden.
;;
;; Jesli obiekt z przebiegu 2 jest wspolliniowy / wspolosiowy z obiektem
;; z przebiegu 1 i ich zakresy (odcinek albo luk) nakladaja sie, to znaczy,
;; ze ten fragment wyniku wspolnego pochodzi w rzeczywistosci z podkladu.
;; Taki obiekt przenosimy: taz_s_visible -> taz_s_xref_visible, analogicznie
;; taz_s_hidden -> taz_s_xref_hidden.
;;
;; Kategorie dopasowania widocznosci:
;;   - obiekt WIDOCZNY z przebiegu wspolnego dopasowujemy TYLKO do
;;     obiektow WIDOCZNYCH z przebiegu podkladu,
;;   - obiekt UKRYTY z przebiegu wspolnego dopasowujemy do obiektow
;;     UKRYTYCH ORAZ WIDOCZNYCH z przebiegu podkladu (krawedz mogla
;;     zostac dodatkowo zaslonieta przez model).
;;
;; OBSLUGIWANE TYPY ENCJI (ten sam zestaw, ktory projekt juz uznaje za
;; standardowy - patrz taz_s_is_sweepable_xref_curve):
;;   LINE, ARC, CIRCLE, ELLIPSE, LWPOLYLINE, POLYLINE (2D i 3D), SPLINE.
;;
;; ARCHITEKTURA: kazda encja jest rozkladana na liste "prymitywow":
;;   ("LIN" p1 p2)                                   - odcinek prosty
;;   ("CIR" centrum promien normalna kat1 kat2)       - luk / okrag
;;   ("ELL" centrum os_glowna os_mala normalna p1 p2) - luk eliptyczny / elipsa
;; LINE/ARC/CIRCLE/ELLIPSE daja zawsze 1 prymityw. LWPOLYLINE i POLYLINE sa
;; rozbijane na N prymitywow (jeden na segment - prosty albo luk z bulge).
;; SPLINE jest - tak jak w istniejacej juz w projekcie funkcji
;; taz_s_get_xref_curve_point - traktowany bardzo pobieznie: bierzemy
;; pierwszy i ostatni punkt definiujacy krzywa (fit point, a w ich braku
;; control point) i redukujemy splina do jednego prymitywu LIN (cieciwy).
;;
;; Obiekt zlozony (Polyline / 3D Polyline) jest przenoszony na warstwe xref
;; W CALOSCI, jesli KTORYKOLWIEK jego segment (prymityw) sie pokrywa -
;; bez rozbijania geometrii na kawalki.
;;
;; Glowne wejscie (wywolywane z taz_s_create_drawings_execution_design.lsp,
;; interfejs bez zmian wzgledem poprzedniej wersji):
;;
;;   (taz_s_solprof_mark_xref_duplicates
;;     taz_s_vide_combined_ents   ; nowe encje z przebiegu WSPOLNEGO
;;     taz_s_vide_xref_ents       ; nowe encje z przebiegu SAMEGO PODKLADU
;;   )
;; ============================================================================


;; ---------------------------------------------------------
;; TOLERANCJE - do ewentualnej korekty
;; ---------------------------------------------------------

(setq taz_s_vide_parallel_tol    1e-6)    ; sin kata miedzy wektorami / normalnymi
(setq taz_s_vide_offset_tol      0.05)    ; [mm] odleglosc punktu od prostej / srodka / dlugosc roznicy wektorow osi
(setq taz_s_vide_min_overlap     0.05)    ; [mm] min. nakladanie odcinkow (LIN, w tym SPLINE-jako-cieciwa)
(setq taz_s_vide_radius_tol      0.05)    ; [mm] tolerancja promienia CIR
(setq taz_s_vide_min_overlap_ang 0.0005)  ; [rad] min. nakladanie katowe CIR i ELL
(setq taz_s_vide_2pi (* 2.0 pi))


;; ---------------------------------------------------------
;; POMOCNICZE: MIN / MAX DWOCH LICZB
;; (standardowy AutoLISP nie ma wbudowanych MIN / MAX)
;; ---------------------------------------------------------

(defun taz_s_vide_min2 (taz_s_vide_a taz_s_vide_b)
  (if (< taz_s_vide_a taz_s_vide_b) taz_s_vide_a taz_s_vide_b)
)

(defun taz_s_vide_max2 (taz_s_vide_a taz_s_vide_b)
  (if (> taz_s_vide_a taz_s_vide_b) taz_s_vide_a taz_s_vide_b)
)


;; ---------------------------------------------------------
;; PODSTAWOWE OPERACJE WEKTOROWE (3D, WCS)
;; ---------------------------------------------------------

(defun taz_s_vide_vec (taz_s_vide_p1 taz_s_vide_p2)
  (list
    (- (car   taz_s_vide_p2) (car   taz_s_vide_p1))
    (- (cadr  taz_s_vide_p2) (cadr  taz_s_vide_p1))
    (- (caddr taz_s_vide_p2) (caddr taz_s_vide_p1))
  )
)

(defun taz_s_vide_dot (taz_s_vide_v1 taz_s_vide_v2)
  (+
    (* (car   taz_s_vide_v1) (car   taz_s_vide_v2))
    (* (cadr  taz_s_vide_v1) (cadr  taz_s_vide_v2))
    (* (caddr taz_s_vide_v1) (caddr taz_s_vide_v2))
  )
)

(defun taz_s_vide_cross (taz_s_vide_v1 taz_s_vide_v2)
  (list
    (- (* (cadr  taz_s_vide_v1) (caddr taz_s_vide_v2)) (* (caddr taz_s_vide_v1) (cadr  taz_s_vide_v2)))
    (- (* (caddr taz_s_vide_v1) (car   taz_s_vide_v2)) (* (car   taz_s_vide_v1) (caddr taz_s_vide_v2)))
    (- (* (car   taz_s_vide_v1) (cadr  taz_s_vide_v2)) (* (cadr  taz_s_vide_v1) (car   taz_s_vide_v2)))
  )
)

(defun taz_s_vide_len (taz_s_vide_v)
  (sqrt
    (+
      (* (car   taz_s_vide_v) (car   taz_s_vide_v))
      (* (cadr  taz_s_vide_v) (cadr  taz_s_vide_v))
      (* (caddr taz_s_vide_v) (caddr taz_s_vide_v))
    )
  )
)

(defun taz_s_vide_dist (taz_s_vide_p1 taz_s_vide_p2)
  (taz_s_vide_len (taz_s_vide_vec taz_s_vide_p1 taz_s_vide_p2))
)

;; Wektory rownolegle w dowolnym z dwoch zwrotow (test przez znormalizowany
;; modul iloczynu wektorowego, czyli sinus kata miedzy nimi).
(defun taz_s_vide_parallel_p (taz_s_vide_v1 taz_s_vide_v2 / taz_s_vide_l1 taz_s_vide_l2)
  (setq taz_s_vide_l1 (taz_s_vide_len taz_s_vide_v1))
  (setq taz_s_vide_l2 (taz_s_vide_len taz_s_vide_v2))
  (and
    (> taz_s_vide_l1 1e-9)
    (> taz_s_vide_l2 1e-9)
    (<
      (/ (taz_s_vide_len (taz_s_vide_cross taz_s_vide_v1 taz_s_vide_v2)) (* taz_s_vide_l1 taz_s_vide_l2))
      taz_s_vide_parallel_tol
    )
  )
)

;; Wektory rownolegle I zgodnie zwrocone (iloczyn skalarny dodatni) -
;; potrzebne do porownywania normalnych CIR / ELL.
(defun taz_s_vide_same_dir_p (taz_s_vide_v1 taz_s_vide_v2)
  (and
    (taz_s_vide_parallel_p taz_s_vide_v1 taz_s_vide_v2)
    (> (taz_s_vide_dot taz_s_vide_v1 taz_s_vide_v2) 0.0)
  )
)


;; ---------------------------------------------------------
;; BULGE -> LUK (dla segmentow LWPOLYLINE / POLYLINE)
;; ---------------------------------------------------------

;; taz_s_vide_p1 / taz_s_vide_p2: punkty 2D (lokalne, w plaszczyznie OCS
;; polilinii - bez trans). Zwraca (centrum_2D promien kat_start kat_koniec)
;; w konwencji "od kat_start do kat_koniec licza sie CCW" - tak jak grupy
;; DXF 50/51 dla ARC. Zwraca nil, jesli nie da sie wyznaczyc luku
;; (zdegenerowany przypadek - segment powinien byc wtedy traktowany jako
;; prosty, co obsluguje wolajacy sprawdzajac bulge ~ 0 wczesniej).
(defun taz_s_vide_bulge_arc
  (taz_s_vide_p1 taz_s_vide_p2 taz_s_vide_bulge
    / taz_s_vide_d taz_s_vide_theta taz_s_vide_r taz_s_vide_chord_ang
      taz_s_vide_center taz_s_vide_a1 taz_s_vide_a2
  )
  (setq taz_s_vide_d (distance taz_s_vide_p1 taz_s_vide_p2))
  (setq taz_s_vide_theta (* 4.0 (atan taz_s_vide_bulge)))
  (if (and (> taz_s_vide_d 1e-9) (> (abs (sin (/ taz_s_vide_theta 2.0))) 1e-9))
    (progn
      (setq taz_s_vide_r (/ taz_s_vide_d (* 2.0 (sin (/ taz_s_vide_theta 2.0)))))
      (setq taz_s_vide_chord_ang (angle taz_s_vide_p1 taz_s_vide_p2))
      (setq taz_s_vide_center
        (polar taz_s_vide_p1
          (+ taz_s_vide_chord_ang (- (/ pi 2.0) (/ taz_s_vide_theta 2.0)))
          taz_s_vide_r
        )
      )
      ;; Dla bulge ujemnego (CW) trzeba zamienic role p1/p2, aby zakres
      ;; "kat_start -> kat_koniec liczony CCW" odpowiadal wlasciwemu (a nie
      ;; dopelniajacemu) lukowi.
      (if (< taz_s_vide_bulge 0.0)
        (progn
          (setq taz_s_vide_a1 (angle taz_s_vide_center taz_s_vide_p2))
          (setq taz_s_vide_a2 (angle taz_s_vide_center taz_s_vide_p1))
        )
        (progn
          (setq taz_s_vide_a1 (angle taz_s_vide_center taz_s_vide_p1))
          (setq taz_s_vide_a2 (angle taz_s_vide_center taz_s_vide_p2))
        )
      )
      (list taz_s_vide_center (abs taz_s_vide_r) taz_s_vide_a1 taz_s_vide_a2)
    )
    nil
  )
)


;; ---------------------------------------------------------
;; KATY: normalizacja, sweep, nakladanie zakresow (uzywane wspolnie
;; przez CIR i ELL - obie sa okresowe z okresem 2*pi)
;; ---------------------------------------------------------

(defun taz_s_vide_norm_ang (taz_s_vide_a)
  (while (>= taz_s_vide_a taz_s_vide_2pi) (setq taz_s_vide_a (- taz_s_vide_a taz_s_vide_2pi)))
  (while (< taz_s_vide_a 0.0) (setq taz_s_vide_a (+ taz_s_vide_a taz_s_vide_2pi)))
  taz_s_vide_a
)

;; Dodatni zakres katowy start->koniec (z obsluga zawijania przez 0).
(defun taz_s_vide_sweep (taz_s_vide_a1 taz_s_vide_a2 / taz_s_vide_s)
  (setq taz_s_vide_s (- (taz_s_vide_norm_ang taz_s_vide_a2) (taz_s_vide_norm_ang taz_s_vide_a1)))
  (if (<= taz_s_vide_s 1e-12) (setq taz_s_vide_s (+ taz_s_vide_s taz_s_vide_2pi)))
  taz_s_vide_s
)

;; Nakladanie sie dwoch zakresow katowych (start + dodatni sweep) na tym
;; samym okregu / elipsie nosnej - B sprawdzane z przesunieciem -2pi/0/+2pi,
;; zeby poprawnie zlapac przypadki "zawiniete" przez 0/2pi.
(defun taz_s_vide_ang_overlap_p
  (taz_s_vide_sa taz_s_vide_swa taz_s_vide_sb taz_s_vide_swb
    / taz_s_vide_ea taz_s_vide_k taz_s_vide_sb2 taz_s_vide_eb2 taz_s_vide_result
  )
  (setq taz_s_vide_ea (+ taz_s_vide_sa taz_s_vide_swa))
  (setq taz_s_vide_result nil)
  (setq taz_s_vide_k -1)
  (while (and (not taz_s_vide_result) (<= taz_s_vide_k 1))
    (setq taz_s_vide_sb2 (+ taz_s_vide_sb (* taz_s_vide_k taz_s_vide_2pi)))
    (setq taz_s_vide_eb2 (+ taz_s_vide_sb2 taz_s_vide_swb))
    (if
      (>
        (- (taz_s_vide_min2 taz_s_vide_ea taz_s_vide_eb2) (taz_s_vide_max2 taz_s_vide_sa taz_s_vide_sb2))
        taz_s_vide_min_overlap_ang
      )
      (setq taz_s_vide_result T)
    )
    (setq taz_s_vide_k (1+ taz_s_vide_k))
  )
  taz_s_vide_result
)


;; ---------------------------------------------------------
;; "TEN SAM KSZTALT NOSNY" - CIR i ELL
;; ---------------------------------------------------------

;; taz_s_vide_da / db = (centrum promien normalna kat1 kat2)
(defun taz_s_vide_same_circle_p (taz_s_vide_da taz_s_vide_db)
  (and
    (< (abs (- (cadr taz_s_vide_da) (cadr taz_s_vide_db))) taz_s_vide_radius_tol)
    (taz_s_vide_same_dir_p (caddr taz_s_vide_da) (caddr taz_s_vide_db))
    (< (taz_s_vide_dist (car taz_s_vide_da) (car taz_s_vide_db)) taz_s_vide_offset_tol)
  )
)

;; taz_s_vide_da / db = (centrum os_glowna os_mala normalna param1 param2).
;; Os mala wynika z osi glownej + normalnej + stosunku, wiec nie trzeba jej
;; dodatkowo sprawdzac - wystarczy centrum + os glowna (kierunek I dlugosc)
;; + normalna.
(defun taz_s_vide_same_ellipse_p (taz_s_vide_da taz_s_vide_db)
  (and
    (taz_s_vide_same_dir_p (nth 3 taz_s_vide_da) (nth 3 taz_s_vide_db))
    (< (taz_s_vide_dist (nth 0 taz_s_vide_da) (nth 0 taz_s_vide_db)) taz_s_vide_offset_tol)
    (<
      (taz_s_vide_len (taz_s_vide_vec (nth 1 taz_s_vide_da) (nth 1 taz_s_vide_db)))
      taz_s_vide_offset_tol
    )
  )
)


;; ---------------------------------------------------------
;; NAKLADANIE PER RODZAJ PRYMITYWU (dzialaja na SUROWYCH DANYCH,
;; nie na encjach - ekstrakcja z encji jest zrobiona wczesniej, raz)
;; ---------------------------------------------------------

;; taz_s_vide_da / db = (p1 p2), juz WCS.
(defun taz_s_vide_lin_overlap_p
  (taz_s_vide_da taz_s_vide_db
    / taz_s_vide_pa1 taz_s_vide_pa2 taz_s_vide_pb1 taz_s_vide_pb2
      taz_s_vide_va taz_s_vide_vb taz_s_vide_len_a taz_s_vide_unit
      taz_s_vide_result taz_s_vide_ta1 taz_s_vide_ta2 taz_s_vide_tb1 taz_s_vide_tb2
      taz_s_vide_mina taz_s_vide_maxa taz_s_vide_minb taz_s_vide_maxb
  )
  (setq taz_s_vide_pa1 (car taz_s_vide_da))
  (setq taz_s_vide_pa2 (cadr taz_s_vide_da))
  (setq taz_s_vide_pb1 (car taz_s_vide_db))
  (setq taz_s_vide_pb2 (cadr taz_s_vide_db))
  (setq taz_s_vide_result nil)

  (setq taz_s_vide_va (taz_s_vide_vec taz_s_vide_pa1 taz_s_vide_pa2))
  (setq taz_s_vide_vb (taz_s_vide_vec taz_s_vide_pb1 taz_s_vide_pb2))
  (setq taz_s_vide_len_a (taz_s_vide_len taz_s_vide_va))

  (if
    (and
      (> taz_s_vide_len_a 1e-9)
      (taz_s_vide_parallel_p taz_s_vide_va taz_s_vide_vb)
      ;; wspolliniowosc: wektor miedzy poczatkami rowniez rownolegly do va
      (taz_s_vide_parallel_p taz_s_vide_va (taz_s_vide_vec taz_s_vide_pa1 taz_s_vide_pb1))
    )
    (progn
      (setq taz_s_vide_unit
        (list
          (/ (car   taz_s_vide_va) taz_s_vide_len_a)
          (/ (cadr  taz_s_vide_va) taz_s_vide_len_a)
          (/ (caddr taz_s_vide_va) taz_s_vide_len_a)
        )
      )
      (setq taz_s_vide_ta1 0.0)
      (setq taz_s_vide_ta2 taz_s_vide_len_a)
      (setq taz_s_vide_tb1 (taz_s_vide_dot taz_s_vide_unit (taz_s_vide_vec taz_s_vide_pa1 taz_s_vide_pb1)))
      (setq taz_s_vide_tb2 (taz_s_vide_dot taz_s_vide_unit (taz_s_vide_vec taz_s_vide_pa1 taz_s_vide_pb2)))

      (setq taz_s_vide_mina (taz_s_vide_min2 taz_s_vide_ta1 taz_s_vide_ta2))
      (setq taz_s_vide_maxa (taz_s_vide_max2 taz_s_vide_ta1 taz_s_vide_ta2))
      (setq taz_s_vide_minb (taz_s_vide_min2 taz_s_vide_tb1 taz_s_vide_tb2))
      (setq taz_s_vide_maxb (taz_s_vide_max2 taz_s_vide_tb1 taz_s_vide_tb2))

      (if
        (>
          (- (taz_s_vide_min2 taz_s_vide_maxa taz_s_vide_maxb) (taz_s_vide_max2 taz_s_vide_mina taz_s_vide_minb))
          taz_s_vide_min_overlap
        )
        (setq taz_s_vide_result T)
      )
    )
  )
  taz_s_vide_result
)

;; taz_s_vide_da / db = (centrum promien normalna kat1 kat2)
(defun taz_s_vide_cir_overlap_p (taz_s_vide_da taz_s_vide_db)
  (if (taz_s_vide_same_circle_p taz_s_vide_da taz_s_vide_db)
    (taz_s_vide_ang_overlap_p
      (taz_s_vide_norm_ang (nth 3 taz_s_vide_da))
      (taz_s_vide_sweep (nth 3 taz_s_vide_da) (nth 4 taz_s_vide_da))
      (taz_s_vide_norm_ang (nth 3 taz_s_vide_db))
      (taz_s_vide_sweep (nth 3 taz_s_vide_db) (nth 4 taz_s_vide_db))
    )
    nil
  )
)

;; taz_s_vide_da / db = (centrum os_glowna os_mala normalna param1 param2)
(defun taz_s_vide_ell_overlap_p (taz_s_vide_da taz_s_vide_db)
  (if (taz_s_vide_same_ellipse_p taz_s_vide_da taz_s_vide_db)
    (taz_s_vide_ang_overlap_p
      (taz_s_vide_norm_ang (nth 4 taz_s_vide_da))
      (taz_s_vide_sweep (nth 4 taz_s_vide_da) (nth 5 taz_s_vide_da))
      (taz_s_vide_norm_ang (nth 4 taz_s_vide_db))
      (taz_s_vide_sweep (nth 4 taz_s_vide_db) (nth 5 taz_s_vide_db))
    )
    nil
  )
)

;; Prymityw = (cons "LIN"/"CIR"/"ELL" dane...). Dopasowanie tylko miedzy
;; prymitywami TEGO SAMEGO rodzaju.
(defun taz_s_vide_prims_overlap_p (taz_s_vide_pa taz_s_vide_pb)
  (if (= (car taz_s_vide_pa) (car taz_s_vide_pb))
    (cond
      ((= (car taz_s_vide_pa) "LIN") (taz_s_vide_lin_overlap_p (cdr taz_s_vide_pa) (cdr taz_s_vide_pb)))
      ((= (car taz_s_vide_pa) "CIR") (taz_s_vide_cir_overlap_p (cdr taz_s_vide_pa) (cdr taz_s_vide_pb)))
      ((= (car taz_s_vide_pa) "ELL") (taz_s_vide_ell_overlap_p (cdr taz_s_vide_pa) (cdr taz_s_vide_pb)))
      (T nil)
    )
    nil
  )
)


;; ---------------------------------------------------------
;; EKSTRAKCJA PRYMITYWOW Z ENCJI - jeden raz na encje, wynik w WCS
;; ---------------------------------------------------------

(defun taz_s_vide_prims_from_line (taz_s_vide_ent / taz_s_vide_ed)
  (setq taz_s_vide_ed (entget taz_s_vide_ent))
  (list
    (cons "LIN"
      (list
        (trans (cdr (assoc 10 taz_s_vide_ed)) taz_s_vide_ent 0)
        (trans (cdr (assoc 11 taz_s_vide_ed)) taz_s_vide_ent 0)
      )
    )
  )
)

(defun taz_s_vide_prims_from_arc (taz_s_vide_ent / taz_s_vide_ed taz_s_vide_n)
  (setq taz_s_vide_ed (entget taz_s_vide_ent))
  (setq taz_s_vide_n (cdr (assoc 210 taz_s_vide_ed)))
  (if (not taz_s_vide_n) (setq taz_s_vide_n (list 0.0 0.0 1.0)))
  (list
    (cons "CIR"
      (list
        (trans (cdr (assoc 10 taz_s_vide_ed)) taz_s_vide_ent 0)
        (cdr (assoc 40 taz_s_vide_ed))
        taz_s_vide_n
        (cdr (assoc 50 taz_s_vide_ed))
        (cdr (assoc 51 taz_s_vide_ed))
      )
    )
  )
)

(defun taz_s_vide_prims_from_circle (taz_s_vide_ent / taz_s_vide_ed taz_s_vide_n)
  (setq taz_s_vide_ed (entget taz_s_vide_ent))
  (setq taz_s_vide_n (cdr (assoc 210 taz_s_vide_ed)))
  (if (not taz_s_vide_n) (setq taz_s_vide_n (list 0.0 0.0 1.0)))
  (list
    (cons "CIR"
      (list
        (trans (cdr (assoc 10 taz_s_vide_ed)) taz_s_vide_ent 0)
        (cdr (assoc 40 taz_s_vide_ed))
        taz_s_vide_n
        0.0
        taz_s_vide_2pi
      )
    )
  )
)

;; ELLIPSE jest wyjatkiem: centrum (10) i wektor osi glownej (11) sa w DXF
;; dane bezposrednio w WCS (bez OCS) - tak samo traktuje to juz istniejace
;; taz_s_get_xref_curve_point (brak "trans" w jego galezi ELLIPSE).
(defun taz_s_vide_prims_from_ellipse
  (taz_s_vide_ent
    / taz_s_vide_ed taz_s_vide_c taz_s_vide_major taz_s_vide_ratio
      taz_s_vide_n taz_s_vide_minor taz_s_vide_p1 taz_s_vide_p2
  )
  (setq taz_s_vide_ed (entget taz_s_vide_ent))
  (setq taz_s_vide_c (cdr (assoc 10 taz_s_vide_ed)))
  (setq taz_s_vide_major (cdr (assoc 11 taz_s_vide_ed)))
  (setq taz_s_vide_ratio (cdr (assoc 40 taz_s_vide_ed)))
  (setq taz_s_vide_n (cdr (assoc 210 taz_s_vide_ed)))
  (if (not taz_s_vide_n) (setq taz_s_vide_n (list 0.0 0.0 1.0)))
  (setq taz_s_vide_p1 (cdr (assoc 41 taz_s_vide_ed)))
  (setq taz_s_vide_p2 (cdr (assoc 42 taz_s_vide_ed)))
  (if (not taz_s_vide_p1) (setq taz_s_vide_p1 0.0))
  (if (not taz_s_vide_p2) (setq taz_s_vide_p2 taz_s_vide_2pi))
  (if (and taz_s_vide_c taz_s_vide_major taz_s_vide_ratio)
    (progn
      (setq taz_s_vide_minor
        (mapcar
          '(lambda (taz_s_vide_q) (* taz_s_vide_q taz_s_vide_ratio))
          (taz_s_vide_cross taz_s_vide_n taz_s_vide_major)
        )
      )
      (list
        (cons "ELL"
          (list taz_s_vide_c taz_s_vide_major taz_s_vide_minor taz_s_vide_n taz_s_vide_p1 taz_s_vide_p2)
        )
      )
    )
    nil
  )
)

;; LWPOLYLINE: wierzcholki (10, 2D) + bulge (42) sa w jednej encji, w
;; kolejnosci. Kazdy 42 nalezy do WCZESNIEJSZEGO wierzcholka (10).
(defun taz_s_vide_prims_from_lwpolyline
  (taz_s_vide_ent
    / taz_s_vide_ed taz_s_vide_closed taz_s_vide_elev taz_s_vide_n
      taz_s_vide_verts taz_s_vide_bulges taz_s_vide_pair
      taz_s_vide_n_verts taz_s_vide_i taz_s_vide_p1 taz_s_vide_p2
      taz_s_vide_b taz_s_vide_arc taz_s_vide_result taz_s_vide_flags
  )
  (setq taz_s_vide_ed (entget taz_s_vide_ent))
  (setq taz_s_vide_flags (cdr (assoc 70 taz_s_vide_ed)))
  (if (not taz_s_vide_flags) (setq taz_s_vide_flags 0))
  (setq taz_s_vide_closed (= 1 (logand 1 taz_s_vide_flags)))
  (setq taz_s_vide_elev (cdr (assoc 38 taz_s_vide_ed)))
  (if (not taz_s_vide_elev) (setq taz_s_vide_elev 0.0))
  (setq taz_s_vide_n (cdr (assoc 210 taz_s_vide_ed)))
  (if (not taz_s_vide_n) (setq taz_s_vide_n (list 0.0 0.0 1.0)))

  (setq taz_s_vide_verts nil)
  (setq taz_s_vide_bulges nil)
  (foreach taz_s_vide_pair taz_s_vide_ed
    (cond
      ((= (car taz_s_vide_pair) 10)
        (setq taz_s_vide_verts (cons (cdr taz_s_vide_pair) taz_s_vide_verts))
        (setq taz_s_vide_bulges (cons 0.0 taz_s_vide_bulges))
      )
      ((and (= (car taz_s_vide_pair) 42) taz_s_vide_bulges)
        (setq taz_s_vide_bulges (cons (cdr taz_s_vide_pair) (cdr taz_s_vide_bulges)))
      )
    )
  )
  (setq taz_s_vide_verts (reverse taz_s_vide_verts))
  (setq taz_s_vide_bulges (reverse taz_s_vide_bulges))
  (setq taz_s_vide_n_verts (length taz_s_vide_verts))

  (setq taz_s_vide_result nil)
  (setq taz_s_vide_i 0)
  (while (< taz_s_vide_i (if taz_s_vide_closed taz_s_vide_n_verts (1- taz_s_vide_n_verts)))
    (setq taz_s_vide_p1 (nth taz_s_vide_i taz_s_vide_verts))
    (setq taz_s_vide_p2 (nth (rem (1+ taz_s_vide_i) taz_s_vide_n_verts) taz_s_vide_verts))
    (setq taz_s_vide_b (nth taz_s_vide_i taz_s_vide_bulges))

    (if (< (abs taz_s_vide_b) 1e-9)
      (setq taz_s_vide_result
        (cons
          (cons "LIN"
            (list
              (trans (list (car taz_s_vide_p1) (cadr taz_s_vide_p1) taz_s_vide_elev) taz_s_vide_ent 0)
              (trans (list (car taz_s_vide_p2) (cadr taz_s_vide_p2) taz_s_vide_elev) taz_s_vide_ent 0)
            )
          )
          taz_s_vide_result
        )
      )
      (progn
        (setq taz_s_vide_arc (taz_s_vide_bulge_arc taz_s_vide_p1 taz_s_vide_p2 taz_s_vide_b))
        (if taz_s_vide_arc
          (setq taz_s_vide_result
            (cons
              (cons "CIR"
                (list
                  (trans
                    (list (car (car taz_s_vide_arc)) (cadr (car taz_s_vide_arc)) taz_s_vide_elev)
                    taz_s_vide_ent 0
                  )
                  (nth 1 taz_s_vide_arc)
                  taz_s_vide_n
                  (nth 2 taz_s_vide_arc)
                  (nth 3 taz_s_vide_arc)
                )
              )
              taz_s_vide_result
            )
          )
        )
      )
    )
    (setq taz_s_vide_i (1+ taz_s_vide_i))
  )
  taz_s_vide_result
)

;; Stary typ POLYLINE: wierzcholki to osobne sub-encje VERTEX (entnext od
;; POLYLINE do SEQEND). Bit 70&8 = to jest 3D polyline (same odcinki, bulge
;; sie nie liczy). Kazdy VERTEX niesie swoj wlasny bulge (42).
(defun taz_s_vide_prims_from_polyline
  (taz_s_vide_ent
    / taz_s_vide_ed taz_s_vide_closed taz_s_vide_is3d taz_s_vide_n
      taz_s_vide_vtx taz_s_vide_ved taz_s_vide_verts taz_s_vide_bulges
      taz_s_vide_n_verts taz_s_vide_i taz_s_vide_p1 taz_s_vide_p2
      taz_s_vide_b taz_s_vide_arc taz_s_vide_result taz_s_vide_flags
  )
  (setq taz_s_vide_ed (entget taz_s_vide_ent))
  (setq taz_s_vide_flags (cdr (assoc 70 taz_s_vide_ed)))
  (if (not taz_s_vide_flags) (setq taz_s_vide_flags 0))
  (setq taz_s_vide_closed (= 1 (logand 1 taz_s_vide_flags)))
  (setq taz_s_vide_is3d (= 8 (logand 8 taz_s_vide_flags)))
  (setq taz_s_vide_n (cdr (assoc 210 taz_s_vide_ed)))
  (if (not taz_s_vide_n) (setq taz_s_vide_n (list 0.0 0.0 1.0)))

  (setq taz_s_vide_verts nil)
  (setq taz_s_vide_bulges nil)
  (setq taz_s_vide_vtx (entnext taz_s_vide_ent))
  (while
    (and taz_s_vide_vtx (= (cdr (assoc 0 (entget taz_s_vide_vtx))) "VERTEX"))
    (setq taz_s_vide_ved (entget taz_s_vide_vtx))
    (setq taz_s_vide_verts (cons (cdr (assoc 10 taz_s_vide_ved)) taz_s_vide_verts))
    (setq taz_s_vide_b (cdr (assoc 42 taz_s_vide_ved)))
    (if (or (not taz_s_vide_b) taz_s_vide_is3d) (setq taz_s_vide_b 0.0))
    (setq taz_s_vide_bulges (cons taz_s_vide_b taz_s_vide_bulges))
    (setq taz_s_vide_vtx (entnext taz_s_vide_vtx))
  )
  (setq taz_s_vide_verts (reverse taz_s_vide_verts))
  (setq taz_s_vide_bulges (reverse taz_s_vide_bulges))
  (setq taz_s_vide_n_verts (length taz_s_vide_verts))

  (setq taz_s_vide_result nil)
  (setq taz_s_vide_i 0)
  (while (< taz_s_vide_i (if taz_s_vide_closed taz_s_vide_n_verts (1- taz_s_vide_n_verts)))
    (setq taz_s_vide_p1 (nth taz_s_vide_i taz_s_vide_verts))
    (setq taz_s_vide_p2 (nth (rem (1+ taz_s_vide_i) taz_s_vide_n_verts) taz_s_vide_verts))
    (setq taz_s_vide_b (nth taz_s_vide_i taz_s_vide_bulges))

    (if (< (abs taz_s_vide_b) 1e-9)
      (setq taz_s_vide_result
        (cons
          (cons "LIN"
            (list
              (trans taz_s_vide_p1 taz_s_vide_ent 0)
              (trans taz_s_vide_p2 taz_s_vide_ent 0)
            )
          )
          taz_s_vide_result
        )
      )
      (progn
        (setq taz_s_vide_arc
          (taz_s_vide_bulge_arc
            (list (car taz_s_vide_p1) (cadr taz_s_vide_p1))
            (list (car taz_s_vide_p2) (cadr taz_s_vide_p2))
            taz_s_vide_b
          )
        )
        (if taz_s_vide_arc
          (setq taz_s_vide_result
            (cons
              (cons "CIR"
                (list
                  (trans
                    (list
                      (car (car taz_s_vide_arc))
                      (cadr (car taz_s_vide_arc))
                      (caddr taz_s_vide_p1)
                    )
                    taz_s_vide_ent 0
                  )
                  (nth 1 taz_s_vide_arc)
                  taz_s_vide_n
                  (nth 2 taz_s_vide_arc)
                  (nth 3 taz_s_vide_arc)
                )
              )
              taz_s_vide_result
            )
          )
        )
      )
    )
    (setq taz_s_vide_i (1+ taz_s_vide_i))
  )
  taz_s_vide_result
)

;; SPLINE - takie samo uproszczenie jak w istniejacym
;; taz_s_get_xref_curve_point (fit point, a w ich braku control point),
;; tyle ze bierzemy PIERWSZY i OSTATNI taki punkt i redukujemy splina do
;; jednego prymitywu LIN (cieciwy). Zadny nowy "rodzaj" nie jest potrzebny -
;; dalej idzie przez ten sam, juz istniejacy test odcinkow.
(defun taz_s_vide_prims_from_spline
  (taz_s_vide_ent
    / taz_s_vide_ed taz_s_vide_pts taz_s_vide_pair taz_s_vide_p1 taz_s_vide_p2
  )
  (setq taz_s_vide_ed (entget taz_s_vide_ent))
  (setq taz_s_vide_pts nil)
  (foreach taz_s_vide_pair taz_s_vide_ed
    (if (= (car taz_s_vide_pair) 11)
      (setq taz_s_vide_pts (cons (cdr taz_s_vide_pair) taz_s_vide_pts))
    )
  )
  (setq taz_s_vide_pts (reverse taz_s_vide_pts))
  (if (not taz_s_vide_pts)
    (progn
      (foreach taz_s_vide_pair taz_s_vide_ed
        (if (= (car taz_s_vide_pair) 10)
          (setq taz_s_vide_pts (cons (cdr taz_s_vide_pair) taz_s_vide_pts))
        )
      )
      (setq taz_s_vide_pts (reverse taz_s_vide_pts))
    )
  )
  (if (>= (length taz_s_vide_pts) 2)
    (progn
      (setq taz_s_vide_p1 (car taz_s_vide_pts))
      (setq taz_s_vide_p2 (car (reverse taz_s_vide_pts)))
      (list (cons "LIN" (list taz_s_vide_p1 taz_s_vide_p2)))
    )
    nil
  )
)

;; Dyspozytor: encja -> lista prymitywow (pusta/nil dla nieobslugiwanego typu)
(defun taz_s_vide_entity_primitives (taz_s_vide_ent / taz_s_vide_ty)
  (setq taz_s_vide_ty (cdr (assoc 0 (entget taz_s_vide_ent))))
  (cond
    ((= taz_s_vide_ty "LINE") (taz_s_vide_prims_from_line taz_s_vide_ent))
    ((= taz_s_vide_ty "ARC") (taz_s_vide_prims_from_arc taz_s_vide_ent))
    ((= taz_s_vide_ty "CIRCLE") (taz_s_vide_prims_from_circle taz_s_vide_ent))
    ((= taz_s_vide_ty "ELLIPSE") (taz_s_vide_prims_from_ellipse taz_s_vide_ent))
    ((= taz_s_vide_ty "LWPOLYLINE") (taz_s_vide_prims_from_lwpolyline taz_s_vide_ent))
    ((= taz_s_vide_ty "POLYLINE") (taz_s_vide_prims_from_polyline taz_s_vide_ent))
    ((= taz_s_vide_ty "SPLINE") (taz_s_vide_prims_from_spline taz_s_vide_ent))
    (T nil)
  )
)


;; ---------------------------------------------------------
;; DOPASOWANIE: czy KTORYKOLWIEK prymityw danej encji pasuje do
;; KTOREGOKOLWIEK prymitywu z puli kandydatow?
;; ---------------------------------------------------------

(defun taz_s_vide_entity_matches_p
  (taz_s_vide_ent taz_s_vide_candidate_prims
    / taz_s_vide_my_prims taz_s_vide_p taz_s_vide_c taz_s_vide_found
  )
  (setq taz_s_vide_my_prims (taz_s_vide_entity_primitives taz_s_vide_ent))
  (setq taz_s_vide_found nil)
  (setq taz_s_vide_p taz_s_vide_my_prims)
  (while (and taz_s_vide_p (not taz_s_vide_found))
    (setq taz_s_vide_c taz_s_vide_candidate_prims)
    (while (and taz_s_vide_c (not taz_s_vide_found))
      (if (taz_s_vide_prims_overlap_p (car taz_s_vide_p) (car taz_s_vide_c))
        (setq taz_s_vide_found T)
      )
      (setq taz_s_vide_c (cdr taz_s_vide_c))
    )
    (setq taz_s_vide_p (cdr taz_s_vide_p))
  )
  taz_s_vide_found
)


;; ---------------------------------------------------------
;; GLOWNA FUNKCJA: przeklasyfikowanie duplikatow
;;
;; taz_s_vide_combined_ents - nowe encje z przebiegu WSPOLNEGO (aktualnie
;;                             na taz_s_visible / taz_s_hidden)
;; taz_s_vide_xref_ents     - nowe encje z przebiegu SAMEGO PODKLADU
;;                             (aktualnie na taz_s_xref_visible /
;;                             taz_s_xref_hidden)
;;
;; Kategorie dopasowania:
;;   widoczny (wspolny) <- tylko widoczny (podklad)
;;   ukryty   (wspolny) <- ukryty LUB widoczny (podklad)
;;
;; Obiekt zlozony (Polyline/3D Polyline) jest przenoszony W CALOSCI, jesli
;; KTORYKOLWIEK jego segment sie pokrywa.
;; ---------------------------------------------------------

(defun taz_s_solprof_mark_xref_duplicates
  (taz_s_vide_combined_ents taz_s_vide_xref_ents
    / taz_s_vide_xref_vis_prims taz_s_vide_xref_hid_prims taz_s_vide_tmp
      taz_s_vide_ent taz_s_vide_ed taz_s_vide_layer
      taz_s_vide_candidates taz_s_vide_new_layer
  )

  (if (and taz_s_vide_combined_ents taz_s_vide_xref_ents)
    (progn

      ;; Rozbij encje podkladu na prymitywy, podzielone wg aktualnej warstwy.
      (setq taz_s_vide_xref_vis_prims nil)
      (setq taz_s_vide_xref_hid_prims nil)
      (setq taz_s_vide_tmp taz_s_vide_xref_ents)
      (while taz_s_vide_tmp
        (setq taz_s_vide_ent (car taz_s_vide_tmp))
        (setq taz_s_vide_layer (cdr (assoc 8 (entget taz_s_vide_ent))))
        (cond
          ((= taz_s_vide_layer "taz_s_xref_visible")
            (setq taz_s_vide_xref_vis_prims
              (append (taz_s_vide_entity_primitives taz_s_vide_ent) taz_s_vide_xref_vis_prims)
            )
          )
          ((= taz_s_vide_layer "taz_s_xref_hidden")
            (setq taz_s_vide_xref_hid_prims
              (append (taz_s_vide_entity_primitives taz_s_vide_ent) taz_s_vide_xref_hid_prims)
            )
          )
        )
        (setq taz_s_vide_tmp (cdr taz_s_vide_tmp))
      )

      ;; Dla kazdej nowej encji przebiegu wspolnego sprawdz dopasowanie.
      (setq taz_s_vide_tmp taz_s_vide_combined_ents)
      (while taz_s_vide_tmp
        (setq taz_s_vide_ent (car taz_s_vide_tmp))
        (setq taz_s_vide_ed (entget taz_s_vide_ent))
        (setq taz_s_vide_layer (cdr (assoc 8 taz_s_vide_ed)))
        (setq taz_s_vide_candidates nil)
        (setq taz_s_vide_new_layer nil)

        (cond
          ((= taz_s_vide_layer "taz_s_visible")
            (setq taz_s_vide_candidates taz_s_vide_xref_vis_prims)
            (setq taz_s_vide_new_layer "taz_s_xref_visible")
          )
          ((= taz_s_vide_layer "taz_s_hidden")
            (setq taz_s_vide_candidates (append taz_s_vide_xref_hid_prims taz_s_vide_xref_vis_prims))
            (setq taz_s_vide_new_layer "taz_s_xref_hidden")
          )
        )

        (if
          (and
            taz_s_vide_new_layer
            (taz_s_vide_entity_matches_p taz_s_vide_ent taz_s_vide_candidates)
          )
          (entmod
            (subst (cons 8 taz_s_vide_new_layer) (assoc 8 taz_s_vide_ed) taz_s_vide_ed)
          )
        )

        (setq taz_s_vide_tmp (cdr taz_s_vide_tmp))
      )
    )
  )
  (princ)
)

(princ)
