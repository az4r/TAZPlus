(defun taz_s_section_ibeam_draw ()
  
  (setq taz_s_r1 1)
  (setq taz_s_r2 1)
  
  (if (= taz_s_family "HEA")
    (taz_s_section_ibeam_draw_parametres_hea)
    (princ)
  )
  (if (= taz_s_family "HEB")
    (taz_s_section_ibeam_draw_parametres_heb)
    (princ)
  )
  (if (= taz_s_family "IPE")
    (taz_s_section_ibeam_draw_parametres_ipe)
    (princ)
  )
  (if (= taz_s_family "IPN")
    (taz_s_section_ibeam_draw_parametres_ipn)
    (princ)
  )

  ;; zapisz widok
  (command "-VIEW" "_S" "taz_s_temp_view")

  (command "_ZOOM" "_SCALE" "10000X")
  
  ;; punkt bazowy
  (setq taz_s_p '(0 0 0))

  ;; obliczenia geometryczne
  (setq taz_s_x1 (- (car taz_s_p) (/ taz_s_b 2.0)))
  (setq taz_s_x2 (+ (car taz_s_p) (/ taz_s_b 2.0)))
  (setq taz_s_y1 (- (cadr taz_s_p) (/ taz_s_h 2.0)))
  (setq taz_s_y2 (+ (cadr taz_s_p) (/ taz_s_h 2.0)))
  
  ;; środnik
  (setq taz_s_xw1 (- (car taz_s_p) (/ taz_s_tw 2.0)))
  (setq taz_s_xw2 (+ (car taz_s_p) (/ taz_s_tw 2.0)))

  ;; wysokości półek przy zewnętrznej krawędzi
  (setq taz_s_yf1 (+ taz_s_y1 taz_s_tf))
  (setq taz_s_yf2 (- taz_s_y2 taz_s_tf))
  
  ;; prawa krawędź półki przy środniku
  (setq taz_s_x_inner_r taz_s_xw2)

  ;; lewa krawędź półki przy środniku
  (setq taz_s_x_inner_l taz_s_xw1)

  ;; >>> IPN SLOPE LOGIC <<<
  (if (= taz_s_family "IPN")
    (progn
      ;; długość jednej półki od środnika do krawędzi
      (setq taz_s_dx (/ (- taz_s_b taz_s_tw) 2.0))

      ;; kąt ze spadku %
      (setq taz_s_alpha (atan (/ taz_s_sf 100.0)))

      ;; różnica wysokości
      (setq taz_s_dy (* taz_s_dx (tan taz_s_alpha)))

      ;; górna półka – wyżej przy środniku
      (setq taz_s_yf2_in (- taz_s_yf2 taz_s_dy))

      ;; dolna półka – niżej przy środniku
      (setq taz_s_yf1_in (+ taz_s_yf1 taz_s_dy))
    )
    (progn
      ;; HEA / HEB / IPE – poziome półki
      (setq taz_s_yf2_in taz_s_yf2)
      (setq taz_s_yf1_in taz_s_yf1)
    )
  )
  ;; >>> END IPN SLOPE <<<
  
  ;; rysowanie konturu
  ;; ustawienie kamery
  (command "_LINE" '(-50 -50 0) '(50 50 0) "")
  (command "_PLAN" "_C")
  (command "_ZOOM" "_OBJECT" (entlast) "")
  (entdel (entlast))
  (command "_ZOOM" "_SCALE" "1000X")
  (command "REGEN")

  ;; punkty obrysu dwuteownika
  (setq taz_s_plp1  (list taz_s_x1  taz_s_y1))       ;; dół lewo
  (setq taz_s_plp2  (list taz_s_x2  taz_s_y1))       ;; dół prawo
  (setq taz_s_plp3  (list taz_s_x2  taz_s_yf1))      ;; prawa dolna krawędź
  (setq taz_s_plp4  (list taz_s_xw2 taz_s_yf1_in))   ;; dolna półka przy środniku
  (setq taz_s_plp5  (list taz_s_xw2 taz_s_yf2_in))   ;; górna półka przy środniku
  (setq taz_s_plp6  (list taz_s_x2  taz_s_yf2))      ;; prawa górna krawędź
  (setq taz_s_plp7  (list taz_s_x2  taz_s_y2))       ;; góra prawo
  (setq taz_s_plp8  (list taz_s_x1  taz_s_y2))       ;; góra lewo
  (setq taz_s_plp9  (list taz_s_x1  taz_s_yf2))      ;; lewa górna krawędź
  (setq taz_s_plp10 (list taz_s_xw1 taz_s_yf2_in))   ;; górna półka przy środniku
  (setq taz_s_plp11 (list taz_s_xw1 taz_s_yf1_in))   ;; dolna półka przy środniku
  (setq taz_s_plp12 (list taz_s_x1  taz_s_yf1))      ;; lewa dolna krawędź
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; DODATKOWE PUNKTY DLA LUKOW (PUNKT 3)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; 1. DANE WEJSCIOWE
  (setq taz_s_arc_X_A1 (car taz_s_plp2))
  (setq taz_s_arc_Y_A1 (cadr taz_s_plp2))
  (setq taz_s_arc_X_A2 (car taz_s_plp3))
  (setq taz_s_arc_Y_A2 (cadr taz_s_plp3))

  (setq taz_s_arc_X_B1 (car taz_s_plp3))
  (setq taz_s_arc_Y_B1 (cadr taz_s_plp3))
  (setq taz_s_arc_X_B2 (car taz_s_plp4))
  (setq taz_s_arc_Y_B2 (cadr taz_s_plp4))
    
  (setq taz_s_arc_A_1 (list taz_s_arc_X_A1 taz_s_arc_Y_A1)) 
  (setq taz_s_arc_A_2 (list taz_s_arc_X_A2 taz_s_arc_Y_A2)) 
  (setq taz_s_arc_B_1 (list taz_s_arc_X_B1 taz_s_arc_Y_B1)) 
  (setq taz_s_arc_B_2 (list taz_s_arc_X_B2 taz_s_arc_Y_B2)) 

  ;; PROMIEŃ
  (setq taz_s_arc_R taz_s_r2)

  ;; PUNKT PRZECIĘCIA PROSTYCH
  (setq taz_s_arc_S_X taz_s_arc_X_A2)
  (setq taz_s_arc_S_Y taz_s_arc_Y_A2)

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ A
  ;; ----------------------------------------------------

  ;; kierunek
  (setq taz_s_arc_AX (- taz_s_arc_X_A2 taz_s_arc_X_A1))
  (setq taz_s_arc_AY (- taz_s_arc_Y_A2 taz_s_arc_Y_A1))

  ;; normalna
  (setq taz_s_arc_ANX (- taz_s_arc_AY))
  (setq taz_s_arc_ANY taz_s_arc_AX)

  ;; długość normalnej
  (setq taz_s_arc_ALEN (sqrt (+ (* taz_s_arc_ANX taz_s_arc_ANX) (* taz_s_arc_ANY taz_s_arc_ANY))))

  ;; współczynnik
  (setq taz_s_arc_AK (/ taz_s_arc_R taz_s_arc_ALEN))

  ;; offset +R
  (setq taz_s_arc_A1_plus (list (+ taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_plus (list (+ taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; offset -R
  (setq taz_s_arc_A1_minus (list (- taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_minus (list (- taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ B
  ;; ----------------------------------------------------

  (setq taz_s_arc_BX (- taz_s_arc_X_B2 taz_s_arc_X_B1))
  (setq taz_s_arc_BY (- taz_s_arc_Y_B2 taz_s_arc_Y_B1))

  (setq taz_s_arc_BNX (- taz_s_arc_BY))
  (setq taz_s_arc_BNY taz_s_arc_BX)

  (setq taz_s_arc_BLEN (sqrt (+ (* taz_s_arc_BNX taz_s_arc_BNX) (* taz_s_arc_BNY taz_s_arc_BNY))))
  (setq taz_s_arc_BK (/ taz_s_arc_R taz_s_arc_BLEN))

  (setq taz_s_arc_B1_plus (list (+ taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_plus (list (+ taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  (setq taz_s_arc_B1_minus (list (- taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_minus (list (- taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  ;; PODGLĄD
  (print "Offset A +R:")
  (print taz_s_arc_A1_plus)
  (print taz_s_arc_A2_plus)

  (print "Offset A -R:")
  (print taz_s_arc_A1_minus)
  (print taz_s_arc_A2_minus)

  (print "Offset B +R:")
  (print taz_s_arc_B1_plus)
  (print taz_s_arc_B2_plus)

  (print "Offset B -R:")
  (print taz_s_arc_B1_minus)
  (print taz_s_arc_B2_minus)

  ;; PUNKT PRZECIECIA OFFSETOW +R
  (setq taz_s_arc_x1 (car taz_s_arc_A1_plus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_plus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_plus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_plus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_plus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_plus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_plus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_plus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_plus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_plus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów +R:")
  (print taz_s_arc_P_plus)

  (setq taz_s_arc_x1 (car taz_s_arc_A1_minus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_minus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_minus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_minus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_minus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_minus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_minus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_minus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_minus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_minus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów -R:")
  (print taz_s_arc_P_minus)

  ;; długość łuku A_1 -> P_plus -> B_2
  (if taz_s_arc_P_plus
    (setq taz_s_arc_L_plus (+ (distance taz_s_arc_A_1 taz_s_arc_P_plus) (distance taz_s_arc_P_plus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_plus -> B_2:")
  (print taz_s_arc_L_plus)

  ;; długość łuku A_1 -> P_minus -> B_2
  (if taz_s_arc_P_minus
    (setq taz_s_arc_L_minus (+ (distance taz_s_arc_A_1 taz_s_arc_P_minus) (distance taz_s_arc_P_minus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_minus -> B_2:")
  (print taz_s_arc_L_minus)

  (if (> taz_s_arc_L_plus taz_s_arc_L_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_plus)
  )

  ;; ----------------------------------------------------
  ;; PUNKTY ODDALONE O R WZDŁUŻ NORMALNYCH (Z WYBOREM ZWROTU)
  ;; ----------------------------------------------------

  ;; normalna A
  (setq taz_s_arc_nAx (/ taz_s_arc_ANX taz_s_arc_ALEN))
  (setq taz_s_arc_nAy (/ taz_s_arc_ANY taz_s_arc_ALEN))

  ;; dwie opcje punktu dla A
  (setq taz_s_arc_P1a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (setq taz_s_arc_P1b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P1a taz_s_arc_A_2) (distance taz_s_arc_P1b taz_s_arc_A_2))
    (setq taz_s_arc_P1 taz_s_arc_P1a)
    (setq taz_s_arc_P1 taz_s_arc_P1b)
  )

  ;; normalna B
  (setq taz_s_arc_nBx (/ taz_s_arc_BNX taz_s_arc_BLEN))
  (setq taz_s_arc_nBy (/ taz_s_arc_BNY taz_s_arc_BLEN))

  (setq taz_s_arc_P2a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (setq taz_s_arc_P2b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P2a taz_s_arc_B_1) (distance taz_s_arc_P2b taz_s_arc_B_1))
    (setq taz_s_arc_P2 taz_s_arc_P2a)
    (setq taz_s_arc_P2 taz_s_arc_P2b)
  )

  (print "Punkt od A:")
  (print taz_s_arc_P1)

  (print "Punkt od B:")
  (print taz_s_arc_P2)
    
  ;; ----------------------------------------------------
  ;; PUNKT W KIERUNKU S NA ODLEGŁOŚĆ R
  ;; ----------------------------------------------------

  ;; wektor od P_plusminus do S
  (setq taz_s_arc_vx (- taz_s_arc_S_X (car taz_s_arc_P_plusminus)))
  (setq taz_s_arc_vy (- taz_s_arc_S_Y (cadr taz_s_arc_P_plusminus)))

  ;; długość wektora
  (setq taz_s_arc_vlen (sqrt (+ (* taz_s_arc_vx taz_s_arc_vx)
                                (* taz_s_arc_vy taz_s_arc_vy))))

  ;; współczynnik skalujący
  (setq taz_s_arc_k (/ taz_s_arc_R taz_s_arc_vlen))

  ;; nowy punkt
  (setq taz_s_arc_P3
    (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_vx taz_s_arc_k))
          (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_vy taz_s_arc_k)))
  )

  (print "Punkt w kierunku S o długości R:")
  (print taz_s_arc_P3)
  
  (setq taz_s_plp3_1 taz_s_arc_P1)
  (setq taz_s_plp3_2 taz_s_arc_P3)
  (setq taz_s_plp3_3 taz_s_arc_P2)
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; KONIEC DODATKOWYCH PUNKTOW (PUNKT 3)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; DODATKOWE PUNKTY DLA LUKOW (PUNKT 4)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; 1. DANE WEJSCIOWE
  (setq taz_s_arc_X_A1 (car taz_s_plp3))
  (setq taz_s_arc_Y_A1 (cadr taz_s_plp3))
  (setq taz_s_arc_X_A2 (car taz_s_plp4))
  (setq taz_s_arc_Y_A2 (cadr taz_s_plp4))

  (setq taz_s_arc_X_B1 (car taz_s_plp4))
  (setq taz_s_arc_Y_B1 (cadr taz_s_plp4))
  (setq taz_s_arc_X_B2 (car taz_s_plp5))
  (setq taz_s_arc_Y_B2 (cadr taz_s_plp5))
    
  (setq taz_s_arc_A_1 (list taz_s_arc_X_A1 taz_s_arc_Y_A1)) 
  (setq taz_s_arc_A_2 (list taz_s_arc_X_A2 taz_s_arc_Y_A2)) 
  (setq taz_s_arc_B_1 (list taz_s_arc_X_B1 taz_s_arc_Y_B1)) 
  (setq taz_s_arc_B_2 (list taz_s_arc_X_B2 taz_s_arc_Y_B2)) 

  ;; PROMIEŃ
  (setq taz_s_arc_R taz_s_r1)

  ;; PUNKT PRZECIĘCIA PROSTYCH
  (setq taz_s_arc_S_X taz_s_arc_X_A2)
  (setq taz_s_arc_S_Y taz_s_arc_Y_A2)

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ A
  ;; ----------------------------------------------------

  ;; kierunek
  (setq taz_s_arc_AX (- taz_s_arc_X_A2 taz_s_arc_X_A1))
  (setq taz_s_arc_AY (- taz_s_arc_Y_A2 taz_s_arc_Y_A1))

  ;; normalna
  (setq taz_s_arc_ANX (- taz_s_arc_AY))
  (setq taz_s_arc_ANY taz_s_arc_AX)

  ;; długość normalnej
  (setq taz_s_arc_ALEN (sqrt (+ (* taz_s_arc_ANX taz_s_arc_ANX) (* taz_s_arc_ANY taz_s_arc_ANY))))

  ;; współczynnik
  (setq taz_s_arc_AK (/ taz_s_arc_R taz_s_arc_ALEN))

  ;; offset +R
  (setq taz_s_arc_A1_plus (list (+ taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_plus (list (+ taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; offset -R
  (setq taz_s_arc_A1_minus (list (- taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_minus (list (- taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ B
  ;; ----------------------------------------------------

  (setq taz_s_arc_BX (- taz_s_arc_X_B2 taz_s_arc_X_B1))
  (setq taz_s_arc_BY (- taz_s_arc_Y_B2 taz_s_arc_Y_B1))

  (setq taz_s_arc_BNX (- taz_s_arc_BY))
  (setq taz_s_arc_BNY taz_s_arc_BX)

  (setq taz_s_arc_BLEN (sqrt (+ (* taz_s_arc_BNX taz_s_arc_BNX) (* taz_s_arc_BNY taz_s_arc_BNY))))
  (setq taz_s_arc_BK (/ taz_s_arc_R taz_s_arc_BLEN))

  (setq taz_s_arc_B1_plus (list (+ taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_plus (list (+ taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  (setq taz_s_arc_B1_minus (list (- taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_minus (list (- taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  ;; PODGLĄD
  (print "Offset A +R:")
  (print taz_s_arc_A1_plus)
  (print taz_s_arc_A2_plus)

  (print "Offset A -R:")
  (print taz_s_arc_A1_minus)
  (print taz_s_arc_A2_minus)

  (print "Offset B +R:")
  (print taz_s_arc_B1_plus)
  (print taz_s_arc_B2_plus)

  (print "Offset B -R:")
  (print taz_s_arc_B1_minus)
  (print taz_s_arc_B2_minus)

  ;; PUNKT PRZECIECIA OFFSETOW +R
  (setq taz_s_arc_x1 (car taz_s_arc_A1_plus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_plus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_plus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_plus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_plus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_plus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_plus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_plus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_plus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_plus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów +R:")
  (print taz_s_arc_P_plus)

  (setq taz_s_arc_x1 (car taz_s_arc_A1_minus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_minus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_minus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_minus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_minus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_minus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_minus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_minus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_minus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_minus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów -R:")
  (print taz_s_arc_P_minus)

  ;; długość łuku A_1 -> P_plus -> B_2
  (if taz_s_arc_P_plus
    (setq taz_s_arc_L_plus (+ (distance taz_s_arc_A_1 taz_s_arc_P_plus) (distance taz_s_arc_P_plus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_plus -> B_2:")
  (print taz_s_arc_L_plus)

  ;; długość łuku A_1 -> P_minus -> B_2
  (if taz_s_arc_P_minus
    (setq taz_s_arc_L_minus (+ (distance taz_s_arc_A_1 taz_s_arc_P_minus) (distance taz_s_arc_P_minus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_minus -> B_2:")
  (print taz_s_arc_L_minus)

  (if (> taz_s_arc_L_plus taz_s_arc_L_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_plus)
  )

  ;; ----------------------------------------------------
  ;; PUNKTY ODDALONE O R WZDŁUŻ NORMALNYCH (Z WYBOREM ZWROTU)
  ;; ----------------------------------------------------

  ;; normalna A
  (setq taz_s_arc_nAx (/ taz_s_arc_ANX taz_s_arc_ALEN))
  (setq taz_s_arc_nAy (/ taz_s_arc_ANY taz_s_arc_ALEN))

  ;; dwie opcje punktu dla A
  (setq taz_s_arc_P1a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (setq taz_s_arc_P1b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P1a taz_s_arc_A_2) (distance taz_s_arc_P1b taz_s_arc_A_2))
    (setq taz_s_arc_P1 taz_s_arc_P1a)
    (setq taz_s_arc_P1 taz_s_arc_P1b)
  )

  ;; normalna B
  (setq taz_s_arc_nBx (/ taz_s_arc_BNX taz_s_arc_BLEN))
  (setq taz_s_arc_nBy (/ taz_s_arc_BNY taz_s_arc_BLEN))

  (setq taz_s_arc_P2a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (setq taz_s_arc_P2b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P2a taz_s_arc_B_1) (distance taz_s_arc_P2b taz_s_arc_B_1))
    (setq taz_s_arc_P2 taz_s_arc_P2a)
    (setq taz_s_arc_P2 taz_s_arc_P2b)
  )

  (print "Punkt od A:")
  (print taz_s_arc_P1)

  (print "Punkt od B:")
  (print taz_s_arc_P2)
    
  ;; ----------------------------------------------------
  ;; PUNKT W KIERUNKU S NA ODLEGŁOŚĆ R
  ;; ----------------------------------------------------

  ;; wektor od P_plusminus do S
  (setq taz_s_arc_vx (- taz_s_arc_S_X (car taz_s_arc_P_plusminus)))
  (setq taz_s_arc_vy (- taz_s_arc_S_Y (cadr taz_s_arc_P_plusminus)))

  ;; długość wektora
  (setq taz_s_arc_vlen (sqrt (+ (* taz_s_arc_vx taz_s_arc_vx)
                                (* taz_s_arc_vy taz_s_arc_vy))))

  ;; współczynnik skalujący
  (setq taz_s_arc_k (/ taz_s_arc_R taz_s_arc_vlen))

  ;; nowy punkt
  (setq taz_s_arc_P3
    (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_vx taz_s_arc_k))
          (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_vy taz_s_arc_k)))
  )

  (print "Punkt w kierunku S o długości R:")
  (print taz_s_arc_P3)
  
  (setq taz_s_plp4_1 taz_s_arc_P1)
  (setq taz_s_plp4_2 taz_s_arc_P3)
  (setq taz_s_plp4_3 taz_s_arc_P2)
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; KONIEC DODATKOWYCH PUNKTOW (PUNKT 4)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; DODATKOWE PUNKTY DLA LUKOW (PUNKT 5)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; 1. DANE WEJSCIOWE
  (setq taz_s_arc_X_A1 (car taz_s_plp4))
  (setq taz_s_arc_Y_A1 (cadr taz_s_plp4))
  (setq taz_s_arc_X_A2 (car taz_s_plp5))
  (setq taz_s_arc_Y_A2 (cadr taz_s_plp5))

  (setq taz_s_arc_X_B1 (car taz_s_plp5))
  (setq taz_s_arc_Y_B1 (cadr taz_s_plp5))
  (setq taz_s_arc_X_B2 (car taz_s_plp6))
  (setq taz_s_arc_Y_B2 (cadr taz_s_plp6))
    
  (setq taz_s_arc_A_1 (list taz_s_arc_X_A1 taz_s_arc_Y_A1)) 
  (setq taz_s_arc_A_2 (list taz_s_arc_X_A2 taz_s_arc_Y_A2)) 
  (setq taz_s_arc_B_1 (list taz_s_arc_X_B1 taz_s_arc_Y_B1)) 
  (setq taz_s_arc_B_2 (list taz_s_arc_X_B2 taz_s_arc_Y_B2)) 

  ;; PROMIEŃ
  (setq taz_s_arc_R taz_s_r1)

  ;; PUNKT PRZECIĘCIA PROSTYCH
  (setq taz_s_arc_S_X taz_s_arc_X_A2)
  (setq taz_s_arc_S_Y taz_s_arc_Y_A2)

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ A
  ;; ----------------------------------------------------

  ;; kierunek
  (setq taz_s_arc_AX (- taz_s_arc_X_A2 taz_s_arc_X_A1))
  (setq taz_s_arc_AY (- taz_s_arc_Y_A2 taz_s_arc_Y_A1))

  ;; normalna
  (setq taz_s_arc_ANX (- taz_s_arc_AY))
  (setq taz_s_arc_ANY taz_s_arc_AX)

  ;; długość normalnej
  (setq taz_s_arc_ALEN (sqrt (+ (* taz_s_arc_ANX taz_s_arc_ANX) (* taz_s_arc_ANY taz_s_arc_ANY))))

  ;; współczynnik
  (setq taz_s_arc_AK (/ taz_s_arc_R taz_s_arc_ALEN))

  ;; offset +R
  (setq taz_s_arc_A1_plus (list (+ taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_plus (list (+ taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; offset -R
  (setq taz_s_arc_A1_minus (list (- taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_minus (list (- taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ B
  ;; ----------------------------------------------------

  (setq taz_s_arc_BX (- taz_s_arc_X_B2 taz_s_arc_X_B1))
  (setq taz_s_arc_BY (- taz_s_arc_Y_B2 taz_s_arc_Y_B1))

  (setq taz_s_arc_BNX (- taz_s_arc_BY))
  (setq taz_s_arc_BNY taz_s_arc_BX)

  (setq taz_s_arc_BLEN (sqrt (+ (* taz_s_arc_BNX taz_s_arc_BNX) (* taz_s_arc_BNY taz_s_arc_BNY))))
  (setq taz_s_arc_BK (/ taz_s_arc_R taz_s_arc_BLEN))

  (setq taz_s_arc_B1_plus (list (+ taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_plus (list (+ taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  (setq taz_s_arc_B1_minus (list (- taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_minus (list (- taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  ;; PODGLĄD
  (print "Offset A +R:")
  (print taz_s_arc_A1_plus)
  (print taz_s_arc_A2_plus)

  (print "Offset A -R:")
  (print taz_s_arc_A1_minus)
  (print taz_s_arc_A2_minus)

  (print "Offset B +R:")
  (print taz_s_arc_B1_plus)
  (print taz_s_arc_B2_plus)

  (print "Offset B -R:")
  (print taz_s_arc_B1_minus)
  (print taz_s_arc_B2_minus)

  ;; PUNKT PRZECIECIA OFFSETOW +R
  (setq taz_s_arc_x1 (car taz_s_arc_A1_plus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_plus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_plus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_plus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_plus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_plus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_plus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_plus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_plus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_plus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów +R:")
  (print taz_s_arc_P_plus)

  (setq taz_s_arc_x1 (car taz_s_arc_A1_minus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_minus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_minus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_minus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_minus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_minus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_minus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_minus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_minus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_minus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów -R:")
  (print taz_s_arc_P_minus)

  ;; długość łuku A_1 -> P_plus -> B_2
  (if taz_s_arc_P_plus
    (setq taz_s_arc_L_plus (+ (distance taz_s_arc_A_1 taz_s_arc_P_plus) (distance taz_s_arc_P_plus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_plus -> B_2:")
  (print taz_s_arc_L_plus)

  ;; długość łuku A_1 -> P_minus -> B_2
  (if taz_s_arc_P_minus
    (setq taz_s_arc_L_minus (+ (distance taz_s_arc_A_1 taz_s_arc_P_minus) (distance taz_s_arc_P_minus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_minus -> B_2:")
  (print taz_s_arc_L_minus)

  (if (> taz_s_arc_L_plus taz_s_arc_L_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_plus)
  )

  ;; ----------------------------------------------------
  ;; PUNKTY ODDALONE O R WZDŁUŻ NORMALNYCH (Z WYBOREM ZWROTU)
  ;; ----------------------------------------------------

  ;; normalna A
  (setq taz_s_arc_nAx (/ taz_s_arc_ANX taz_s_arc_ALEN))
  (setq taz_s_arc_nAy (/ taz_s_arc_ANY taz_s_arc_ALEN))

  ;; dwie opcje punktu dla A
  (setq taz_s_arc_P1a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (setq taz_s_arc_P1b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P1a taz_s_arc_A_2) (distance taz_s_arc_P1b taz_s_arc_A_2))
    (setq taz_s_arc_P1 taz_s_arc_P1a)
    (setq taz_s_arc_P1 taz_s_arc_P1b)
  )

  ;; normalna B
  (setq taz_s_arc_nBx (/ taz_s_arc_BNX taz_s_arc_BLEN))
  (setq taz_s_arc_nBy (/ taz_s_arc_BNY taz_s_arc_BLEN))

  (setq taz_s_arc_P2a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (setq taz_s_arc_P2b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P2a taz_s_arc_B_1) (distance taz_s_arc_P2b taz_s_arc_B_1))
    (setq taz_s_arc_P2 taz_s_arc_P2a)
    (setq taz_s_arc_P2 taz_s_arc_P2b)
  )

  (print "Punkt od A:")
  (print taz_s_arc_P1)

  (print "Punkt od B:")
  (print taz_s_arc_P2)
    
  ;; ----------------------------------------------------
  ;; PUNKT W KIERUNKU S NA ODLEGŁOŚĆ R
  ;; ----------------------------------------------------

  ;; wektor od P_plusminus do S
  (setq taz_s_arc_vx (- taz_s_arc_S_X (car taz_s_arc_P_plusminus)))
  (setq taz_s_arc_vy (- taz_s_arc_S_Y (cadr taz_s_arc_P_plusminus)))

  ;; długość wektora
  (setq taz_s_arc_vlen (sqrt (+ (* taz_s_arc_vx taz_s_arc_vx)
                                (* taz_s_arc_vy taz_s_arc_vy))))

  ;; współczynnik skalujący
  (setq taz_s_arc_k (/ taz_s_arc_R taz_s_arc_vlen))

  ;; nowy punkt
  (setq taz_s_arc_P3
    (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_vx taz_s_arc_k))
          (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_vy taz_s_arc_k)))
  )

  (print "Punkt w kierunku S o długości R:")
  (print taz_s_arc_P3)
  
  (setq taz_s_plp5_1 taz_s_arc_P1)
  (setq taz_s_plp5_2 taz_s_arc_P3)
  (setq taz_s_plp5_3 taz_s_arc_P2)
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; KONIEC DODATKOWYCH PUNKTOW (PUNKT 5)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; DODATKOWE PUNKTY DLA LUKOW (PUNKT 6)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; 1. DANE WEJSCIOWE
  (setq taz_s_arc_X_A1 (car taz_s_plp5))
  (setq taz_s_arc_Y_A1 (cadr taz_s_plp5))
  (setq taz_s_arc_X_A2 (car taz_s_plp6))
  (setq taz_s_arc_Y_A2 (cadr taz_s_plp6))

  (setq taz_s_arc_X_B1 (car taz_s_plp6))
  (setq taz_s_arc_Y_B1 (cadr taz_s_plp6))
  (setq taz_s_arc_X_B2 (car taz_s_plp7))
  (setq taz_s_arc_Y_B2 (cadr taz_s_plp7))
    
  (setq taz_s_arc_A_1 (list taz_s_arc_X_A1 taz_s_arc_Y_A1)) 
  (setq taz_s_arc_A_2 (list taz_s_arc_X_A2 taz_s_arc_Y_A2)) 
  (setq taz_s_arc_B_1 (list taz_s_arc_X_B1 taz_s_arc_Y_B1)) 
  (setq taz_s_arc_B_2 (list taz_s_arc_X_B2 taz_s_arc_Y_B2)) 

  ;; PROMIEŃ
  (setq taz_s_arc_R taz_s_r2)

  ;; PUNKT PRZECIĘCIA PROSTYCH
  (setq taz_s_arc_S_X taz_s_arc_X_A2)
  (setq taz_s_arc_S_Y taz_s_arc_Y_A2)

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ A
  ;; ----------------------------------------------------

  ;; kierunek
  (setq taz_s_arc_AX (- taz_s_arc_X_A2 taz_s_arc_X_A1))
  (setq taz_s_arc_AY (- taz_s_arc_Y_A2 taz_s_arc_Y_A1))

  ;; normalna
  (setq taz_s_arc_ANX (- taz_s_arc_AY))
  (setq taz_s_arc_ANY taz_s_arc_AX)

  ;; długość normalnej
  (setq taz_s_arc_ALEN (sqrt (+ (* taz_s_arc_ANX taz_s_arc_ANX) (* taz_s_arc_ANY taz_s_arc_ANY))))

  ;; współczynnik
  (setq taz_s_arc_AK (/ taz_s_arc_R taz_s_arc_ALEN))

  ;; offset +R
  (setq taz_s_arc_A1_plus (list (+ taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_plus (list (+ taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; offset -R
  (setq taz_s_arc_A1_minus (list (- taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_minus (list (- taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ B
  ;; ----------------------------------------------------

  (setq taz_s_arc_BX (- taz_s_arc_X_B2 taz_s_arc_X_B1))
  (setq taz_s_arc_BY (- taz_s_arc_Y_B2 taz_s_arc_Y_B1))

  (setq taz_s_arc_BNX (- taz_s_arc_BY))
  (setq taz_s_arc_BNY taz_s_arc_BX)

  (setq taz_s_arc_BLEN (sqrt (+ (* taz_s_arc_BNX taz_s_arc_BNX) (* taz_s_arc_BNY taz_s_arc_BNY))))
  (setq taz_s_arc_BK (/ taz_s_arc_R taz_s_arc_BLEN))

  (setq taz_s_arc_B1_plus (list (+ taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_plus (list (+ taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  (setq taz_s_arc_B1_minus (list (- taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_minus (list (- taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  ;; PODGLĄD
  (print "Offset A +R:")
  (print taz_s_arc_A1_plus)
  (print taz_s_arc_A2_plus)

  (print "Offset A -R:")
  (print taz_s_arc_A1_minus)
  (print taz_s_arc_A2_minus)

  (print "Offset B +R:")
  (print taz_s_arc_B1_plus)
  (print taz_s_arc_B2_plus)

  (print "Offset B -R:")
  (print taz_s_arc_B1_minus)
  (print taz_s_arc_B2_minus)

  ;; PUNKT PRZECIECIA OFFSETOW +R
  (setq taz_s_arc_x1 (car taz_s_arc_A1_plus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_plus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_plus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_plus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_plus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_plus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_plus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_plus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_plus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_plus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów +R:")
  (print taz_s_arc_P_plus)

  (setq taz_s_arc_x1 (car taz_s_arc_A1_minus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_minus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_minus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_minus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_minus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_minus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_minus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_minus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_minus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_minus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów -R:")
  (print taz_s_arc_P_minus)

  ;; długość łuku A_1 -> P_plus -> B_2
  (if taz_s_arc_P_plus
    (setq taz_s_arc_L_plus (+ (distance taz_s_arc_A_1 taz_s_arc_P_plus) (distance taz_s_arc_P_plus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_plus -> B_2:")
  (print taz_s_arc_L_plus)

  ;; długość łuku A_1 -> P_minus -> B_2
  (if taz_s_arc_P_minus
    (setq taz_s_arc_L_minus (+ (distance taz_s_arc_A_1 taz_s_arc_P_minus) (distance taz_s_arc_P_minus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_minus -> B_2:")
  (print taz_s_arc_L_minus)

  (if (> taz_s_arc_L_plus taz_s_arc_L_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_plus)
  )

  ;; ----------------------------------------------------
  ;; PUNKTY ODDALONE O R WZDŁUŻ NORMALNYCH (Z WYBOREM ZWROTU)
  ;; ----------------------------------------------------

  ;; normalna A
  (setq taz_s_arc_nAx (/ taz_s_arc_ANX taz_s_arc_ALEN))
  (setq taz_s_arc_nAy (/ taz_s_arc_ANY taz_s_arc_ALEN))

  ;; dwie opcje punktu dla A
  (setq taz_s_arc_P1a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (setq taz_s_arc_P1b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P1a taz_s_arc_A_2) (distance taz_s_arc_P1b taz_s_arc_A_2))
    (setq taz_s_arc_P1 taz_s_arc_P1a)
    (setq taz_s_arc_P1 taz_s_arc_P1b)
  )

  ;; normalna B
  (setq taz_s_arc_nBx (/ taz_s_arc_BNX taz_s_arc_BLEN))
  (setq taz_s_arc_nBy (/ taz_s_arc_BNY taz_s_arc_BLEN))

  (setq taz_s_arc_P2a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (setq taz_s_arc_P2b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P2a taz_s_arc_B_1) (distance taz_s_arc_P2b taz_s_arc_B_1))
    (setq taz_s_arc_P2 taz_s_arc_P2a)
    (setq taz_s_arc_P2 taz_s_arc_P2b)
  )

  (print "Punkt od A:")
  (print taz_s_arc_P1)

  (print "Punkt od B:")
  (print taz_s_arc_P2)
    
  ;; ----------------------------------------------------
  ;; PUNKT W KIERUNKU S NA ODLEGŁOŚĆ R
  ;; ----------------------------------------------------

  ;; wektor od P_plusminus do S
  (setq taz_s_arc_vx (- taz_s_arc_S_X (car taz_s_arc_P_plusminus)))
  (setq taz_s_arc_vy (- taz_s_arc_S_Y (cadr taz_s_arc_P_plusminus)))

  ;; długość wektora
  (setq taz_s_arc_vlen (sqrt (+ (* taz_s_arc_vx taz_s_arc_vx)
                                (* taz_s_arc_vy taz_s_arc_vy))))

  ;; współczynnik skalujący
  (setq taz_s_arc_k (/ taz_s_arc_R taz_s_arc_vlen))

  ;; nowy punkt
  (setq taz_s_arc_P3
    (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_vx taz_s_arc_k))
          (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_vy taz_s_arc_k)))
  )

  (print "Punkt w kierunku S o długości R:")
  (print taz_s_arc_P3)
  
  (setq taz_s_plp6_1 taz_s_arc_P1)
  (setq taz_s_plp6_2 taz_s_arc_P3)
  (setq taz_s_plp6_3 taz_s_arc_P2)
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; KONIEC DODATKOWYCH PUNKTOW (PUNKT 6)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; DODATKOWE PUNKTY DLA LUKOW (PUNKT 9)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; 1. DANE WEJSCIOWE
  (setq taz_s_arc_X_A1 (car taz_s_plp8))
  (setq taz_s_arc_Y_A1 (cadr taz_s_plp8))
  (setq taz_s_arc_X_A2 (car taz_s_plp9))
  (setq taz_s_arc_Y_A2 (cadr taz_s_plp9))

  (setq taz_s_arc_X_B1 (car taz_s_plp9))
  (setq taz_s_arc_Y_B1 (cadr taz_s_plp9))
  (setq taz_s_arc_X_B2 (car taz_s_plp10))
  (setq taz_s_arc_Y_B2 (cadr taz_s_plp10))
    
  (setq taz_s_arc_A_1 (list taz_s_arc_X_A1 taz_s_arc_Y_A1)) 
  (setq taz_s_arc_A_2 (list taz_s_arc_X_A2 taz_s_arc_Y_A2)) 
  (setq taz_s_arc_B_1 (list taz_s_arc_X_B1 taz_s_arc_Y_B1)) 
  (setq taz_s_arc_B_2 (list taz_s_arc_X_B2 taz_s_arc_Y_B2)) 

  ;; PROMIEŃ
  (setq taz_s_arc_R taz_s_r2)

  ;; PUNKT PRZECIĘCIA PROSTYCH
  (setq taz_s_arc_S_X taz_s_arc_X_A2)
  (setq taz_s_arc_S_Y taz_s_arc_Y_A2)

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ A
  ;; ----------------------------------------------------

  ;; kierunek
  (setq taz_s_arc_AX (- taz_s_arc_X_A2 taz_s_arc_X_A1))
  (setq taz_s_arc_AY (- taz_s_arc_Y_A2 taz_s_arc_Y_A1))

  ;; normalna
  (setq taz_s_arc_ANX (- taz_s_arc_AY))
  (setq taz_s_arc_ANY taz_s_arc_AX)

  ;; długość normalnej
  (setq taz_s_arc_ALEN (sqrt (+ (* taz_s_arc_ANX taz_s_arc_ANX) (* taz_s_arc_ANY taz_s_arc_ANY))))

  ;; współczynnik
  (setq taz_s_arc_AK (/ taz_s_arc_R taz_s_arc_ALEN))

  ;; offset +R
  (setq taz_s_arc_A1_plus (list (+ taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_plus (list (+ taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; offset -R
  (setq taz_s_arc_A1_minus (list (- taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_minus (list (- taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ B
  ;; ----------------------------------------------------

  (setq taz_s_arc_BX (- taz_s_arc_X_B2 taz_s_arc_X_B1))
  (setq taz_s_arc_BY (- taz_s_arc_Y_B2 taz_s_arc_Y_B1))

  (setq taz_s_arc_BNX (- taz_s_arc_BY))
  (setq taz_s_arc_BNY taz_s_arc_BX)

  (setq taz_s_arc_BLEN (sqrt (+ (* taz_s_arc_BNX taz_s_arc_BNX) (* taz_s_arc_BNY taz_s_arc_BNY))))
  (setq taz_s_arc_BK (/ taz_s_arc_R taz_s_arc_BLEN))

  (setq taz_s_arc_B1_plus (list (+ taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_plus (list (+ taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  (setq taz_s_arc_B1_minus (list (- taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_minus (list (- taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  ;; PODGLĄD
  (print "Offset A +R:")
  (print taz_s_arc_A1_plus)
  (print taz_s_arc_A2_plus)

  (print "Offset A -R:")
  (print taz_s_arc_A1_minus)
  (print taz_s_arc_A2_minus)

  (print "Offset B +R:")
  (print taz_s_arc_B1_plus)
  (print taz_s_arc_B2_plus)

  (print "Offset B -R:")
  (print taz_s_arc_B1_minus)
  (print taz_s_arc_B2_minus)

  ;; PUNKT PRZECIECIA OFFSETOW +R
  (setq taz_s_arc_x1 (car taz_s_arc_A1_plus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_plus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_plus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_plus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_plus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_plus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_plus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_plus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_plus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_plus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów +R:")
  (print taz_s_arc_P_plus)

  (setq taz_s_arc_x1 (car taz_s_arc_A1_minus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_minus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_minus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_minus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_minus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_minus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_minus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_minus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_minus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_minus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów -R:")
  (print taz_s_arc_P_minus)

  ;; długość łuku A_1 -> P_plus -> B_2
  (if taz_s_arc_P_plus
    (setq taz_s_arc_L_plus (+ (distance taz_s_arc_A_1 taz_s_arc_P_plus) (distance taz_s_arc_P_plus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_plus -> B_2:")
  (print taz_s_arc_L_plus)

  ;; długość łuku A_1 -> P_minus -> B_2
  (if taz_s_arc_P_minus
    (setq taz_s_arc_L_minus (+ (distance taz_s_arc_A_1 taz_s_arc_P_minus) (distance taz_s_arc_P_minus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_minus -> B_2:")
  (print taz_s_arc_L_minus)

  (if (> taz_s_arc_L_plus taz_s_arc_L_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_plus)
  )

  ;; ----------------------------------------------------
  ;; PUNKTY ODDALONE O R WZDŁUŻ NORMALNYCH (Z WYBOREM ZWROTU)
  ;; ----------------------------------------------------

  ;; normalna A
  (setq taz_s_arc_nAx (/ taz_s_arc_ANX taz_s_arc_ALEN))
  (setq taz_s_arc_nAy (/ taz_s_arc_ANY taz_s_arc_ALEN))

  ;; dwie opcje punktu dla A
  (setq taz_s_arc_P1a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (setq taz_s_arc_P1b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P1a taz_s_arc_A_2) (distance taz_s_arc_P1b taz_s_arc_A_2))
    (setq taz_s_arc_P1 taz_s_arc_P1a)
    (setq taz_s_arc_P1 taz_s_arc_P1b)
  )

  ;; normalna B
  (setq taz_s_arc_nBx (/ taz_s_arc_BNX taz_s_arc_BLEN))
  (setq taz_s_arc_nBy (/ taz_s_arc_BNY taz_s_arc_BLEN))

  (setq taz_s_arc_P2a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (setq taz_s_arc_P2b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P2a taz_s_arc_B_1) (distance taz_s_arc_P2b taz_s_arc_B_1))
    (setq taz_s_arc_P2 taz_s_arc_P2a)
    (setq taz_s_arc_P2 taz_s_arc_P2b)
  )

  (print "Punkt od A:")
  (print taz_s_arc_P1)

  (print "Punkt od B:")
  (print taz_s_arc_P2)
    
  ;; ----------------------------------------------------
  ;; PUNKT W KIERUNKU S NA ODLEGŁOŚĆ R
  ;; ----------------------------------------------------

  ;; wektor od P_plusminus do S
  (setq taz_s_arc_vx (- taz_s_arc_S_X (car taz_s_arc_P_plusminus)))
  (setq taz_s_arc_vy (- taz_s_arc_S_Y (cadr taz_s_arc_P_plusminus)))

  ;; długość wektora
  (setq taz_s_arc_vlen (sqrt (+ (* taz_s_arc_vx taz_s_arc_vx)
                                (* taz_s_arc_vy taz_s_arc_vy))))

  ;; współczynnik skalujący
  (setq taz_s_arc_k (/ taz_s_arc_R taz_s_arc_vlen))

  ;; nowy punkt
  (setq taz_s_arc_P3
    (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_vx taz_s_arc_k))
          (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_vy taz_s_arc_k)))
  )

  (print "Punkt w kierunku S o długości R:")
  (print taz_s_arc_P3)
  
  (setq taz_s_plp9_1 taz_s_arc_P1)
  (setq taz_s_plp9_2 taz_s_arc_P3)
  (setq taz_s_plp9_3 taz_s_arc_P2)
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; KONIEC DODATKOWYCH PUNKTOW (PUNKT 9)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; DODATKOWE PUNKTY DLA LUKOW (PUNKT 10)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; 1. DANE WEJSCIOWE
  (setq taz_s_arc_X_A1 (car taz_s_plp9))
  (setq taz_s_arc_Y_A1 (cadr taz_s_plp9))
  (setq taz_s_arc_X_A2 (car taz_s_plp10))
  (setq taz_s_arc_Y_A2 (cadr taz_s_plp10))

  (setq taz_s_arc_X_B1 (car taz_s_plp10))
  (setq taz_s_arc_Y_B1 (cadr taz_s_plp10))
  (setq taz_s_arc_X_B2 (car taz_s_plp11))
  (setq taz_s_arc_Y_B2 (cadr taz_s_plp11))
    
  (setq taz_s_arc_A_1 (list taz_s_arc_X_A1 taz_s_arc_Y_A1)) 
  (setq taz_s_arc_A_2 (list taz_s_arc_X_A2 taz_s_arc_Y_A2)) 
  (setq taz_s_arc_B_1 (list taz_s_arc_X_B1 taz_s_arc_Y_B1)) 
  (setq taz_s_arc_B_2 (list taz_s_arc_X_B2 taz_s_arc_Y_B2)) 

  ;; PROMIEŃ
  (setq taz_s_arc_R taz_s_r1)

  ;; PUNKT PRZECIĘCIA PROSTYCH
  (setq taz_s_arc_S_X taz_s_arc_X_A2)
  (setq taz_s_arc_S_Y taz_s_arc_Y_A2)

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ A
  ;; ----------------------------------------------------

  ;; kierunek
  (setq taz_s_arc_AX (- taz_s_arc_X_A2 taz_s_arc_X_A1))
  (setq taz_s_arc_AY (- taz_s_arc_Y_A2 taz_s_arc_Y_A1))

  ;; normalna
  (setq taz_s_arc_ANX (- taz_s_arc_AY))
  (setq taz_s_arc_ANY taz_s_arc_AX)

  ;; długość normalnej
  (setq taz_s_arc_ALEN (sqrt (+ (* taz_s_arc_ANX taz_s_arc_ANX) (* taz_s_arc_ANY taz_s_arc_ANY))))

  ;; współczynnik
  (setq taz_s_arc_AK (/ taz_s_arc_R taz_s_arc_ALEN))

  ;; offset +R
  (setq taz_s_arc_A1_plus (list (+ taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_plus (list (+ taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; offset -R
  (setq taz_s_arc_A1_minus (list (- taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_minus (list (- taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ B
  ;; ----------------------------------------------------

  (setq taz_s_arc_BX (- taz_s_arc_X_B2 taz_s_arc_X_B1))
  (setq taz_s_arc_BY (- taz_s_arc_Y_B2 taz_s_arc_Y_B1))

  (setq taz_s_arc_BNX (- taz_s_arc_BY))
  (setq taz_s_arc_BNY taz_s_arc_BX)

  (setq taz_s_arc_BLEN (sqrt (+ (* taz_s_arc_BNX taz_s_arc_BNX) (* taz_s_arc_BNY taz_s_arc_BNY))))
  (setq taz_s_arc_BK (/ taz_s_arc_R taz_s_arc_BLEN))

  (setq taz_s_arc_B1_plus (list (+ taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_plus (list (+ taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  (setq taz_s_arc_B1_minus (list (- taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_minus (list (- taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  ;; PODGLĄD
  (print "Offset A +R:")
  (print taz_s_arc_A1_plus)
  (print taz_s_arc_A2_plus)

  (print "Offset A -R:")
  (print taz_s_arc_A1_minus)
  (print taz_s_arc_A2_minus)

  (print "Offset B +R:")
  (print taz_s_arc_B1_plus)
  (print taz_s_arc_B2_plus)

  (print "Offset B -R:")
  (print taz_s_arc_B1_minus)
  (print taz_s_arc_B2_minus)

  ;; PUNKT PRZECIECIA OFFSETOW +R
  (setq taz_s_arc_x1 (car taz_s_arc_A1_plus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_plus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_plus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_plus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_plus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_plus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_plus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_plus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_plus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_plus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów +R:")
  (print taz_s_arc_P_plus)

  (setq taz_s_arc_x1 (car taz_s_arc_A1_minus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_minus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_minus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_minus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_minus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_minus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_minus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_minus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_minus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_minus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów -R:")
  (print taz_s_arc_P_minus)

  ;; długość łuku A_1 -> P_plus -> B_2
  (if taz_s_arc_P_plus
    (setq taz_s_arc_L_plus (+ (distance taz_s_arc_A_1 taz_s_arc_P_plus) (distance taz_s_arc_P_plus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_plus -> B_2:")
  (print taz_s_arc_L_plus)

  ;; długość łuku A_1 -> P_minus -> B_2
  (if taz_s_arc_P_minus
    (setq taz_s_arc_L_minus (+ (distance taz_s_arc_A_1 taz_s_arc_P_minus) (distance taz_s_arc_P_minus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_minus -> B_2:")
  (print taz_s_arc_L_minus)

  (if (> taz_s_arc_L_plus taz_s_arc_L_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_plus)
  )

  ;; ----------------------------------------------------
  ;; PUNKTY ODDALONE O R WZDŁUŻ NORMALNYCH (Z WYBOREM ZWROTU)
  ;; ----------------------------------------------------

  ;; normalna A
  (setq taz_s_arc_nAx (/ taz_s_arc_ANX taz_s_arc_ALEN))
  (setq taz_s_arc_nAy (/ taz_s_arc_ANY taz_s_arc_ALEN))

  ;; dwie opcje punktu dla A
  (setq taz_s_arc_P1a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (setq taz_s_arc_P1b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P1a taz_s_arc_A_2) (distance taz_s_arc_P1b taz_s_arc_A_2))
    (setq taz_s_arc_P1 taz_s_arc_P1a)
    (setq taz_s_arc_P1 taz_s_arc_P1b)
  )

  ;; normalna B
  (setq taz_s_arc_nBx (/ taz_s_arc_BNX taz_s_arc_BLEN))
  (setq taz_s_arc_nBy (/ taz_s_arc_BNY taz_s_arc_BLEN))

  (setq taz_s_arc_P2a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (setq taz_s_arc_P2b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P2a taz_s_arc_B_1) (distance taz_s_arc_P2b taz_s_arc_B_1))
    (setq taz_s_arc_P2 taz_s_arc_P2a)
    (setq taz_s_arc_P2 taz_s_arc_P2b)
  )

  (print "Punkt od A:")
  (print taz_s_arc_P1)

  (print "Punkt od B:")
  (print taz_s_arc_P2)
    
  ;; ----------------------------------------------------
  ;; PUNKT W KIERUNKU S NA ODLEGŁOŚĆ R
  ;; ----------------------------------------------------

  ;; wektor od P_plusminus do S
  (setq taz_s_arc_vx (- taz_s_arc_S_X (car taz_s_arc_P_plusminus)))
  (setq taz_s_arc_vy (- taz_s_arc_S_Y (cadr taz_s_arc_P_plusminus)))

  ;; długość wektora
  (setq taz_s_arc_vlen (sqrt (+ (* taz_s_arc_vx taz_s_arc_vx)
                                (* taz_s_arc_vy taz_s_arc_vy))))

  ;; współczynnik skalujący
  (setq taz_s_arc_k (/ taz_s_arc_R taz_s_arc_vlen))

  ;; nowy punkt
  (setq taz_s_arc_P3
    (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_vx taz_s_arc_k))
          (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_vy taz_s_arc_k)))
  )

  (print "Punkt w kierunku S o długości R:")
  (print taz_s_arc_P3)
  
  (setq taz_s_plp10_1 taz_s_arc_P1)
  (setq taz_s_plp10_2 taz_s_arc_P3)
  (setq taz_s_plp10_3 taz_s_arc_P2)
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; KONIEC DODATKOWYCH PUNKTOW (PUNKT 10)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; DODATKOWE PUNKTY DLA LUKOW (PUNKT 11)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; 1. DANE WEJSCIOWE
  (setq taz_s_arc_X_A1 (car taz_s_plp10))
  (setq taz_s_arc_Y_A1 (cadr taz_s_plp10))
  (setq taz_s_arc_X_A2 (car taz_s_plp11))
  (setq taz_s_arc_Y_A2 (cadr taz_s_plp11))

  (setq taz_s_arc_X_B1 (car taz_s_plp11))
  (setq taz_s_arc_Y_B1 (cadr taz_s_plp11))
  (setq taz_s_arc_X_B2 (car taz_s_plp12))
  (setq taz_s_arc_Y_B2 (cadr taz_s_plp12))
    
  (setq taz_s_arc_A_1 (list taz_s_arc_X_A1 taz_s_arc_Y_A1)) 
  (setq taz_s_arc_A_2 (list taz_s_arc_X_A2 taz_s_arc_Y_A2)) 
  (setq taz_s_arc_B_1 (list taz_s_arc_X_B1 taz_s_arc_Y_B1)) 
  (setq taz_s_arc_B_2 (list taz_s_arc_X_B2 taz_s_arc_Y_B2)) 

  ;; PROMIEŃ
  (setq taz_s_arc_R taz_s_r1)

  ;; PUNKT PRZECIĘCIA PROSTYCH
  (setq taz_s_arc_S_X taz_s_arc_X_A2)
  (setq taz_s_arc_S_Y taz_s_arc_Y_A2)

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ A
  ;; ----------------------------------------------------

  ;; kierunek
  (setq taz_s_arc_AX (- taz_s_arc_X_A2 taz_s_arc_X_A1))
  (setq taz_s_arc_AY (- taz_s_arc_Y_A2 taz_s_arc_Y_A1))

  ;; normalna
  (setq taz_s_arc_ANX (- taz_s_arc_AY))
  (setq taz_s_arc_ANY taz_s_arc_AX)

  ;; długość normalnej
  (setq taz_s_arc_ALEN (sqrt (+ (* taz_s_arc_ANX taz_s_arc_ANX) (* taz_s_arc_ANY taz_s_arc_ANY))))

  ;; współczynnik
  (setq taz_s_arc_AK (/ taz_s_arc_R taz_s_arc_ALEN))

  ;; offset +R
  (setq taz_s_arc_A1_plus (list (+ taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_plus (list (+ taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; offset -R
  (setq taz_s_arc_A1_minus (list (- taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_minus (list (- taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ B
  ;; ----------------------------------------------------

  (setq taz_s_arc_BX (- taz_s_arc_X_B2 taz_s_arc_X_B1))
  (setq taz_s_arc_BY (- taz_s_arc_Y_B2 taz_s_arc_Y_B1))

  (setq taz_s_arc_BNX (- taz_s_arc_BY))
  (setq taz_s_arc_BNY taz_s_arc_BX)

  (setq taz_s_arc_BLEN (sqrt (+ (* taz_s_arc_BNX taz_s_arc_BNX) (* taz_s_arc_BNY taz_s_arc_BNY))))
  (setq taz_s_arc_BK (/ taz_s_arc_R taz_s_arc_BLEN))

  (setq taz_s_arc_B1_plus (list (+ taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_plus (list (+ taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  (setq taz_s_arc_B1_minus (list (- taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_minus (list (- taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  ;; PODGLĄD
  (print "Offset A +R:")
  (print taz_s_arc_A1_plus)
  (print taz_s_arc_A2_plus)

  (print "Offset A -R:")
  (print taz_s_arc_A1_minus)
  (print taz_s_arc_A2_minus)

  (print "Offset B +R:")
  (print taz_s_arc_B1_plus)
  (print taz_s_arc_B2_plus)

  (print "Offset B -R:")
  (print taz_s_arc_B1_minus)
  (print taz_s_arc_B2_minus)

  ;; PUNKT PRZECIECIA OFFSETOW +R
  (setq taz_s_arc_x1 (car taz_s_arc_A1_plus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_plus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_plus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_plus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_plus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_plus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_plus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_plus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_plus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_plus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów +R:")
  (print taz_s_arc_P_plus)

  (setq taz_s_arc_x1 (car taz_s_arc_A1_minus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_minus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_minus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_minus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_minus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_minus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_minus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_minus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_minus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_minus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów -R:")
  (print taz_s_arc_P_minus)

  ;; długość łuku A_1 -> P_plus -> B_2
  (if taz_s_arc_P_plus
    (setq taz_s_arc_L_plus (+ (distance taz_s_arc_A_1 taz_s_arc_P_plus) (distance taz_s_arc_P_plus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_plus -> B_2:")
  (print taz_s_arc_L_plus)

  ;; długość łuku A_1 -> P_minus -> B_2
  (if taz_s_arc_P_minus
    (setq taz_s_arc_L_minus (+ (distance taz_s_arc_A_1 taz_s_arc_P_minus) (distance taz_s_arc_P_minus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_minus -> B_2:")
  (print taz_s_arc_L_minus)

  (if (> taz_s_arc_L_plus taz_s_arc_L_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_plus)
  )

  ;; ----------------------------------------------------
  ;; PUNKTY ODDALONE O R WZDŁUŻ NORMALNYCH (Z WYBOREM ZWROTU)
  ;; ----------------------------------------------------

  ;; normalna A
  (setq taz_s_arc_nAx (/ taz_s_arc_ANX taz_s_arc_ALEN))
  (setq taz_s_arc_nAy (/ taz_s_arc_ANY taz_s_arc_ALEN))

  ;; dwie opcje punktu dla A
  (setq taz_s_arc_P1a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (setq taz_s_arc_P1b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P1a taz_s_arc_A_2) (distance taz_s_arc_P1b taz_s_arc_A_2))
    (setq taz_s_arc_P1 taz_s_arc_P1a)
    (setq taz_s_arc_P1 taz_s_arc_P1b)
  )

  ;; normalna B
  (setq taz_s_arc_nBx (/ taz_s_arc_BNX taz_s_arc_BLEN))
  (setq taz_s_arc_nBy (/ taz_s_arc_BNY taz_s_arc_BLEN))

  (setq taz_s_arc_P2a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (setq taz_s_arc_P2b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P2a taz_s_arc_B_1) (distance taz_s_arc_P2b taz_s_arc_B_1))
    (setq taz_s_arc_P2 taz_s_arc_P2a)
    (setq taz_s_arc_P2 taz_s_arc_P2b)
  )

  (print "Punkt od A:")
  (print taz_s_arc_P1)

  (print "Punkt od B:")
  (print taz_s_arc_P2)
    
  ;; ----------------------------------------------------
  ;; PUNKT W KIERUNKU S NA ODLEGŁOŚĆ R
  ;; ----------------------------------------------------

  ;; wektor od P_plusminus do S
  (setq taz_s_arc_vx (- taz_s_arc_S_X (car taz_s_arc_P_plusminus)))
  (setq taz_s_arc_vy (- taz_s_arc_S_Y (cadr taz_s_arc_P_plusminus)))

  ;; długość wektora
  (setq taz_s_arc_vlen (sqrt (+ (* taz_s_arc_vx taz_s_arc_vx)
                                (* taz_s_arc_vy taz_s_arc_vy))))

  ;; współczynnik skalujący
  (setq taz_s_arc_k (/ taz_s_arc_R taz_s_arc_vlen))

  ;; nowy punkt
  (setq taz_s_arc_P3
    (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_vx taz_s_arc_k))
          (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_vy taz_s_arc_k)))
  )

  (print "Punkt w kierunku S o długości R:")
  (print taz_s_arc_P3)
  
  (setq taz_s_plp11_1 taz_s_arc_P1)
  (setq taz_s_plp11_2 taz_s_arc_P3)
  (setq taz_s_plp11_3 taz_s_arc_P2)
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; KONIEC DODATKOWYCH PUNKTOW (PUNKT 11)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; DODATKOWE PUNKTY DLA LUKOW (PUNKT 5)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; 1. DANE WEJSCIOWE
  (setq taz_s_arc_X_A1 (car taz_s_plp11))
  (setq taz_s_arc_Y_A1 (cadr taz_s_plp11))
  (setq taz_s_arc_X_A2 (car taz_s_plp12))
  (setq taz_s_arc_Y_A2 (cadr taz_s_plp12))

  (setq taz_s_arc_X_B1 (car taz_s_plp12))
  (setq taz_s_arc_Y_B1 (cadr taz_s_plp12))
  (setq taz_s_arc_X_B2 (car taz_s_plp1))
  (setq taz_s_arc_Y_B2 (cadr taz_s_plp1))
    
  (setq taz_s_arc_A_1 (list taz_s_arc_X_A1 taz_s_arc_Y_A1)) 
  (setq taz_s_arc_A_2 (list taz_s_arc_X_A2 taz_s_arc_Y_A2)) 
  (setq taz_s_arc_B_1 (list taz_s_arc_X_B1 taz_s_arc_Y_B1)) 
  (setq taz_s_arc_B_2 (list taz_s_arc_X_B2 taz_s_arc_Y_B2)) 

  ;; PROMIEŃ
  (setq taz_s_arc_R taz_s_r2)

  ;; PUNKT PRZECIĘCIA PROSTYCH
  (setq taz_s_arc_S_X taz_s_arc_X_A2)
  (setq taz_s_arc_S_Y taz_s_arc_Y_A2)

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ A
  ;; ----------------------------------------------------

  ;; kierunek
  (setq taz_s_arc_AX (- taz_s_arc_X_A2 taz_s_arc_X_A1))
  (setq taz_s_arc_AY (- taz_s_arc_Y_A2 taz_s_arc_Y_A1))

  ;; normalna
  (setq taz_s_arc_ANX (- taz_s_arc_AY))
  (setq taz_s_arc_ANY taz_s_arc_AX)

  ;; długość normalnej
  (setq taz_s_arc_ALEN (sqrt (+ (* taz_s_arc_ANX taz_s_arc_ANX) (* taz_s_arc_ANY taz_s_arc_ANY))))

  ;; współczynnik
  (setq taz_s_arc_AK (/ taz_s_arc_R taz_s_arc_ALEN))

  ;; offset +R
  (setq taz_s_arc_A1_plus (list (+ taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_plus (list (+ taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (+ taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; offset -R
  (setq taz_s_arc_A1_minus (list (- taz_s_arc_X_A1 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A1 (* taz_s_arc_ANY taz_s_arc_AK))))
  (setq taz_s_arc_A2_minus (list (- taz_s_arc_X_A2 (* taz_s_arc_ANX taz_s_arc_AK)) (- taz_s_arc_Y_A2 (* taz_s_arc_ANY taz_s_arc_AK))))

  ;; ----------------------------------------------------
  ;; OFFSET PROSTEJ B
  ;; ----------------------------------------------------

  (setq taz_s_arc_BX (- taz_s_arc_X_B2 taz_s_arc_X_B1))
  (setq taz_s_arc_BY (- taz_s_arc_Y_B2 taz_s_arc_Y_B1))

  (setq taz_s_arc_BNX (- taz_s_arc_BY))
  (setq taz_s_arc_BNY taz_s_arc_BX)

  (setq taz_s_arc_BLEN (sqrt (+ (* taz_s_arc_BNX taz_s_arc_BNX) (* taz_s_arc_BNY taz_s_arc_BNY))))
  (setq taz_s_arc_BK (/ taz_s_arc_R taz_s_arc_BLEN))

  (setq taz_s_arc_B1_plus (list (+ taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_plus (list (+ taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (+ taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  (setq taz_s_arc_B1_minus (list (- taz_s_arc_X_B1 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B1 (* taz_s_arc_BNY taz_s_arc_BK))))
  (setq taz_s_arc_B2_minus (list (- taz_s_arc_X_B2 (* taz_s_arc_BNX taz_s_arc_BK)) (- taz_s_arc_Y_B2 (* taz_s_arc_BNY taz_s_arc_BK))))

  ;; PODGLĄD
  (print "Offset A +R:")
  (print taz_s_arc_A1_plus)
  (print taz_s_arc_A2_plus)

  (print "Offset A -R:")
  (print taz_s_arc_A1_minus)
  (print taz_s_arc_A2_minus)

  (print "Offset B +R:")
  (print taz_s_arc_B1_plus)
  (print taz_s_arc_B2_plus)

  (print "Offset B -R:")
  (print taz_s_arc_B1_minus)
  (print taz_s_arc_B2_minus)

  ;; PUNKT PRZECIECIA OFFSETOW +R
  (setq taz_s_arc_x1 (car taz_s_arc_A1_plus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_plus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_plus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_plus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_plus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_plus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_plus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_plus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_plus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_plus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów +R:")
  (print taz_s_arc_P_plus)

  (setq taz_s_arc_x1 (car taz_s_arc_A1_minus))  (setq taz_s_arc_y1 (cadr taz_s_arc_A1_minus))
  (setq taz_s_arc_x2 (car taz_s_arc_A2_minus))  (setq taz_s_arc_y2 (cadr taz_s_arc_A2_minus))
  (setq taz_s_arc_x3 (car taz_s_arc_B1_minus))  (setq taz_s_arc_y3 (cadr taz_s_arc_B1_minus))
  (setq taz_s_arc_x4 (car taz_s_arc_B2_minus))  (setq taz_s_arc_y4 (cadr taz_s_arc_B2_minus))

  (setq taz_s_arc_denom (- (* (- taz_s_arc_x1 taz_s_arc_x2) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y2) (- taz_s_arc_x3 taz_s_arc_x4))))

  (if (= taz_s_arc_denom 0)
    (setq taz_s_arc_P_minus nil)
    (progn
      (setq taz_s_arc_t (/ (- (* (- taz_s_arc_x1 taz_s_arc_x3) (- taz_s_arc_y3 taz_s_arc_y4)) (* (- taz_s_arc_y1 taz_s_arc_y3) (- taz_s_arc_x3 taz_s_arc_x4))) taz_s_arc_denom))
      (setq taz_s_arc_Px (+ taz_s_arc_x1 (* taz_s_arc_t (- taz_s_arc_x2 taz_s_arc_x1))))
      (setq taz_s_arc_Py (+ taz_s_arc_y1 (* taz_s_arc_t (- taz_s_arc_y2 taz_s_arc_y1))))
      (setq taz_s_arc_P_minus (list taz_s_arc_Px taz_s_arc_Py))
    )
  )

  (print "Przecięcie offsetów -R:")
  (print taz_s_arc_P_minus)

  ;; długość łuku A_1 -> P_plus -> B_2
  (if taz_s_arc_P_plus
    (setq taz_s_arc_L_plus (+ (distance taz_s_arc_A_1 taz_s_arc_P_plus) (distance taz_s_arc_P_plus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_plus -> B_2:")
  (print taz_s_arc_L_plus)

  ;; długość łuku A_1 -> P_minus -> B_2
  (if taz_s_arc_P_minus
    (setq taz_s_arc_L_minus (+ (distance taz_s_arc_A_1 taz_s_arc_P_minus) (distance taz_s_arc_P_minus taz_s_arc_B_2)))
  )
  (print "Dlugosc luku A_1 -> P_minus -> B_2:")
  (print taz_s_arc_L_minus)

  (if (> taz_s_arc_L_plus taz_s_arc_L_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_minus)
    (setq taz_s_arc_P_plusminus taz_s_arc_P_plus)
  )

  ;; ----------------------------------------------------
  ;; PUNKTY ODDALONE O R WZDŁUŻ NORMALNYCH (Z WYBOREM ZWROTU)
  ;; ----------------------------------------------------

  ;; normalna A
  (setq taz_s_arc_nAx (/ taz_s_arc_ANX taz_s_arc_ALEN))
  (setq taz_s_arc_nAy (/ taz_s_arc_ANY taz_s_arc_ALEN))

  ;; dwie opcje punktu dla A
  (setq taz_s_arc_P1a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (setq taz_s_arc_P1b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nAx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nAy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P1a taz_s_arc_A_2) (distance taz_s_arc_P1b taz_s_arc_A_2))
    (setq taz_s_arc_P1 taz_s_arc_P1a)
    (setq taz_s_arc_P1 taz_s_arc_P1b)
  )

  ;; normalna B
  (setq taz_s_arc_nBx (/ taz_s_arc_BNX taz_s_arc_BLEN))
  (setq taz_s_arc_nBy (/ taz_s_arc_BNY taz_s_arc_BLEN))

  (setq taz_s_arc_P2a (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (setq taz_s_arc_P2b (list (- (car taz_s_arc_P_plusminus) (* taz_s_arc_nBx taz_s_arc_R))
                            (- (cadr taz_s_arc_P_plusminus) (* taz_s_arc_nBy taz_s_arc_R))))

  (if (< (distance taz_s_arc_P2a taz_s_arc_B_1) (distance taz_s_arc_P2b taz_s_arc_B_1))
    (setq taz_s_arc_P2 taz_s_arc_P2a)
    (setq taz_s_arc_P2 taz_s_arc_P2b)
  )

  (print "Punkt od A:")
  (print taz_s_arc_P1)

  (print "Punkt od B:")
  (print taz_s_arc_P2)
    
  ;; ----------------------------------------------------
  ;; PUNKT W KIERUNKU S NA ODLEGŁOŚĆ R
  ;; ----------------------------------------------------

  ;; wektor od P_plusminus do S
  (setq taz_s_arc_vx (- taz_s_arc_S_X (car taz_s_arc_P_plusminus)))
  (setq taz_s_arc_vy (- taz_s_arc_S_Y (cadr taz_s_arc_P_plusminus)))

  ;; długość wektora
  (setq taz_s_arc_vlen (sqrt (+ (* taz_s_arc_vx taz_s_arc_vx)
                                (* taz_s_arc_vy taz_s_arc_vy))))

  ;; współczynnik skalujący
  (setq taz_s_arc_k (/ taz_s_arc_R taz_s_arc_vlen))

  ;; nowy punkt
  (setq taz_s_arc_P3
    (list (+ (car taz_s_arc_P_plusminus) (* taz_s_arc_vx taz_s_arc_k))
          (+ (cadr taz_s_arc_P_plusminus) (* taz_s_arc_vy taz_s_arc_k)))
  )

  (print "Punkt w kierunku S o długości R:")
  (print taz_s_arc_P3)
  
  (setq taz_s_plp12_1 taz_s_arc_P1)
  (setq taz_s_plp12_2 taz_s_arc_P3)
  (setq taz_s_plp12_3 taz_s_arc_P2)
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; KONIEC DODATKOWYCH PUNKTOW (PUNKT 12)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; rysowanie HEA
  (if (= taz_s_family "HEA")
    (progn
      (command "_PLINE"
        taz_s_plp1
        taz_s_plp2
        taz_s_plp3
              
        taz_s_plp4_1
            "A"
            taz_s_plp4_2
            taz_s_plp4_3
            "L"
              
        taz_s_plp5_1
            "A"
            taz_s_plp5_2
            taz_s_plp5_3
            "L"
                  
        taz_s_plp6
        taz_s_plp7
        taz_s_plp8
        taz_s_plp9
                          
        taz_s_plp10_1
            "A"
            taz_s_plp10_2
            taz_s_plp10_3
            "L"
                          
        taz_s_plp11_1
            "A"
            taz_s_plp11_2
            taz_s_plp11_3
            "L"
                          
        taz_s_plp12
        "_C"
        )
    )
    (princ)
  )
  
  ;; rysowanie HEB
  (if (= taz_s_family "HEB")
    (progn
      (command "_PLINE"
        taz_s_plp1
        taz_s_plp2
        taz_s_plp3
              
        taz_s_plp4_1
            "A"
            taz_s_plp4_2
            taz_s_plp4_3
            "L"
              
        taz_s_plp5_1
            "A"
            taz_s_plp5_2
            taz_s_plp5_3
            "L"
                  
        taz_s_plp6
        taz_s_plp7
        taz_s_plp8
        taz_s_plp9
                          
        taz_s_plp10_1
            "A"
            taz_s_plp10_2
            taz_s_plp10_3
            "L"
                          
        taz_s_plp11_1
            "A"
            taz_s_plp11_2
            taz_s_plp11_3
            "L"
                          
        taz_s_plp12
        "_C"
        )
    )
    (princ)
  )
  
  ;; rysowanie IPE
  (if (= taz_s_family "IPE")
    (progn
      (command "_PLINE"
        taz_s_plp1
        taz_s_plp2
        taz_s_plp3
              
        taz_s_plp4_1
            "A"
            taz_s_plp4_2
            taz_s_plp4_3
            "L"
              
        taz_s_plp5_1
            "A"
            taz_s_plp5_2
            taz_s_plp5_3
            "L"
                  
        taz_s_plp6
        taz_s_plp7
        taz_s_plp8
        taz_s_plp9
                          
        taz_s_plp10_1
            "A"
            taz_s_plp10_2
            taz_s_plp10_3
            "L"
                          
        taz_s_plp11_1
            "A"
            taz_s_plp11_2
            taz_s_plp11_3
            "L"
                          
        taz_s_plp12
        "_C"
        )
    )
    (princ)
  )
  
  ;; rysowanie IPN
  (if (= taz_s_family "IPN")
    (progn
      (command "_PLINE"
        taz_s_plp1
        taz_s_plp2
               
        taz_s_plp3_1
            "A"
            taz_s_plp3_2
            taz_s_plp3_3
            "L"
              
        taz_s_plp4_1
            "A"
            taz_s_plp4_2
            taz_s_plp4_3
            "L"
              
        taz_s_plp5_1
            "A"
            taz_s_plp5_2
            taz_s_plp5_3
            "L"
                  
        taz_s_plp6_1
            "A"
            taz_s_plp6_2
            taz_s_plp6_3
            "L"
               
        taz_s_plp7
        taz_s_plp8
               
        taz_s_plp9_1
            "A"
            taz_s_plp9_2
            taz_s_plp9_3
            "L"
                          
        taz_s_plp10_1
            "A"
            taz_s_plp10_2
            taz_s_plp10_3
            "L"
                          
        taz_s_plp11_1
            "A"
            taz_s_plp11_2
            taz_s_plp11_3
            "L"
                          
        taz_s_plp12_1
            "A"
            taz_s_plp12_2
            taz_s_plp12_3
            "L"
        "_C"
        )
    )
    (princ)
  )
  
(command "_CHPROP" (entlast) "" "C" "6" "")

  ;; wybór profilu
  (setq taz_s_create_beam_profile
        (ssname (ssget "_X" '((0 . "LWPOLYLINE") (62 . 6))) 0))

  (command "REGEN")

  ;; SWEEP
  (command "_SWEEP" taz_s_create_beam_profile "" taz_s_create_beam_path)
  (command)
  
  ;; zmiana warstwy
  (command "_CHPROP" (ssadd (entlast)) "" "_LA" "taz_s_beam" "")

  ;; przywrócenie widoku
  (command "-VIEW" "_R" "taz_s_temp_view")
  (command "-VIEW" "_D" "taz_s_temp_view")
  
  (princ)
)
