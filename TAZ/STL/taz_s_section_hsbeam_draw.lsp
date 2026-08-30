(defun taz_s_section_hsbeam_draw ()
  
  (setq taz_s_r1 1)
  (setq taz_s_r2 1)

  ;; pobranie parametrów SHS
  (if (= taz_s_family "SHS")
    (taz_s_section_hsbeam_draw_parametres_rura_kwadratowa)
    (princ)
  )

  ;; pobranie parametrów RHS
  (if (= taz_s_family "RHS")
    (taz_s_section_hsbeam_draw_parametres_rura_prostokatna)
    (princ)
  )
  
  ;; pobranie parametrów CHS
  (if (= taz_s_family "CHS")
    (taz_s_section_hsbeam_draw_parametres_rura_okragla)
    (princ)
  )
  
  ;; zapisz widok
  (command "-VIEW" "_S" "taz_s_temp_view")

  (command "_ZOOM" "_SCALE" "10000X")
  
  ;; Punkt bazowy – zewnętrzny narożnik
  (setq taz_s_p '(0 0 0))
  
  (if (or (= taz_s_family "SHS") (= taz_s_family "RHS"))
    (progn

  ;; ============================
  ;; WYPROWADZENIE WSPÓŁRZĘDNYCH
  ;; ============================

  (setq taz_s_x0 (car taz_s_p))   ;; środek X
  (setq taz_s_y0 (cadr taz_s_p))  ;; środek Y

  ;; pół‑wymiary (od środka do krawędzi)
  (setq taz_s_dx (/ taz_s_b 2.0))   ;; pół‑szerokość
  (setq taz_s_dy (/ taz_s_h 2.0))   ;; pół‑wysokość

  ;; zewnętrzne krawędzie
  (setq taz_s_xld (- taz_s_x0 taz_s_dx))   ;; lewy
  (setq taz_s_xrd (+ taz_s_x0 taz_s_dx))   ;; prawy
  (setq taz_s_ybd (- taz_s_y0 taz_s_dy))   ;; dół
  (setq taz_s_ytd (+ taz_s_y0 taz_s_dy))   ;; góra

  ;; wewnętrzne krawędzie (po grubości t)
  (setq taz_s_t1 (+ taz_s_xld taz_s_t))   ;; lewa wewnętrzna
  (setq taz_s_t2 (+ taz_s_ybd taz_s_t))   ;; dolna wewnętrzna
  (setq taz_s_t3 (- taz_s_xrd taz_s_t))   ;; prawa wewnętrzna
  (setq taz_s_t4 (- taz_s_ytd taz_s_t))   ;; górna wewnętrzna
  
  ;; rysowanie konturu
  ;; ustawienie kamery
  (command "_LINE" '(-50 -50 0) '(50 50 0) "")
  (command "_PLAN" "_C")
  (command "_ZOOM" "_OBJECT" (entlast) "")
  (entdel (entlast))
  (command "_ZOOM" "_SCALE" "1000X")
  (command "REGEN")

  ;; ============================
  ;; PUNKTY KONTURU RHS (bez promieni)
  ;; ============================

  ;; Zewnętrzne narożniki
  (setq taz_s_plp1 (list taz_s_xld taz_s_ybd))   ;; dolny-lewy
  (setq taz_s_plp2 (list taz_s_xrd taz_s_ybd))   ;; dolny-prawy
  (setq taz_s_plp3 (list taz_s_xrd taz_s_ytd))   ;; górny-prawy
  (setq taz_s_plp4 (list taz_s_xld taz_s_ytd))   ;; górny-lewy

  ;; Wewnętrzne narożniki (otwór)
  (setq taz_s_plp5 (list taz_s_t1 taz_s_t2))     ;; dolny-lewy wewnętrzny
  (setq taz_s_plp6 (list taz_s_t3 taz_s_t2))     ;; dolny-prawy wewnętrzny
  (setq taz_s_plp7 (list taz_s_t3 taz_s_t4))     ;; górny-prawy wewnętrzny
  (setq taz_s_plp8 (list taz_s_t1 taz_s_t4))     ;; górny-lewy wewnętrzny
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; DODATKOWE PUNKTY DLA LUKOW (PUNKT 1)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; 1. DANE WEJSCIOWE
  (setq taz_s_arc_X_A1 (car taz_s_plp4))
  (setq taz_s_arc_Y_A1 (cadr taz_s_plp4))
  (setq taz_s_arc_X_A2 (car taz_s_plp1))
  (setq taz_s_arc_Y_A2 (cadr taz_s_plp1))

  (setq taz_s_arc_X_B1 (car taz_s_plp1))
  (setq taz_s_arc_Y_B1 (cadr taz_s_plp1))
  (setq taz_s_arc_X_B2 (car taz_s_plp2))
  (setq taz_s_arc_Y_B2 (cadr taz_s_plp2))
    
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
  
  (setq taz_s_plp1_1 taz_s_arc_P1)
  (setq taz_s_plp1_2 taz_s_arc_P3)
  (setq taz_s_plp1_3 taz_s_arc_P2)
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; KONIEC DODATKOWYCH PUNKTOW (PUNKT 1)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; DODATKOWE PUNKTY DLA LUKOW (PUNKT 2)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; 1. DANE WEJSCIOWE
  (setq taz_s_arc_X_A1 (car taz_s_plp1))
  (setq taz_s_arc_Y_A1 (cadr taz_s_plp1))
  (setq taz_s_arc_X_A2 (car taz_s_plp2))
  (setq taz_s_arc_Y_A2 (cadr taz_s_plp2))

  (setq taz_s_arc_X_B1 (car taz_s_plp2))
  (setq taz_s_arc_Y_B1 (cadr taz_s_plp2))
  (setq taz_s_arc_X_B2 (car taz_s_plp3))
  (setq taz_s_arc_Y_B2 (cadr taz_s_plp3))
    
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
  
  (setq taz_s_plp2_1 taz_s_arc_P1)
  (setq taz_s_plp2_2 taz_s_arc_P3)
  (setq taz_s_plp2_3 taz_s_arc_P2)
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; KONIEC DODATKOWYCH PUNKTOW (PUNKT 2)
  ;; ###########################################################################
  ;; ###########################################################################

  
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
  (setq taz_s_arc_X_B2 (car taz_s_plp1))
  (setq taz_s_arc_Y_B2 (cadr taz_s_plp1))
    
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
  (setq taz_s_arc_X_A1 (car taz_s_plp8))
  (setq taz_s_arc_Y_A1 (cadr taz_s_plp8))
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
  ;; DODATKOWE PUNKTY DLA LUKOW (PUNKT 7)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; 1. DANE WEJSCIOWE
  (setq taz_s_arc_X_A1 (car taz_s_plp6))
  (setq taz_s_arc_Y_A1 (cadr taz_s_plp6))
  (setq taz_s_arc_X_A2 (car taz_s_plp7))
  (setq taz_s_arc_Y_A2 (cadr taz_s_plp7))

  (setq taz_s_arc_X_B1 (car taz_s_plp7))
  (setq taz_s_arc_Y_B1 (cadr taz_s_plp7))
  (setq taz_s_arc_X_B2 (car taz_s_plp8))
  (setq taz_s_arc_Y_B2 (cadr taz_s_plp8))
    
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
  
  (setq taz_s_plp7_1 taz_s_arc_P1)
  (setq taz_s_plp7_2 taz_s_arc_P3)
  (setq taz_s_plp7_3 taz_s_arc_P2)
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; KONIEC DODATKOWYCH PUNKTOW (PUNKT 7)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; DODATKOWE PUNKTY DLA LUKOW (PUNKT 8)
  ;; ###########################################################################
  ;; ###########################################################################
  
  ;; 1. DANE WEJSCIOWE
  (setq taz_s_arc_X_A1 (car taz_s_plp7))
  (setq taz_s_arc_Y_A1 (cadr taz_s_plp7))
  (setq taz_s_arc_X_A2 (car taz_s_plp8))
  (setq taz_s_arc_Y_A2 (cadr taz_s_plp8))

  (setq taz_s_arc_X_B1 (car taz_s_plp8))
  (setq taz_s_arc_Y_B1 (cadr taz_s_plp8))
  (setq taz_s_arc_X_B2 (car taz_s_plp5))
  (setq taz_s_arc_Y_B2 (cadr taz_s_plp5))
    
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
  
  (setq taz_s_plp8_1 taz_s_arc_P1)
  (setq taz_s_plp8_2 taz_s_arc_P3)
  (setq taz_s_plp8_3 taz_s_arc_P2)
  
  ;; ###########################################################################
  ;; ###########################################################################
  ;; KONIEC DODATKOWYCH PUNKTOW (PUNKT 8)
  ;; ###########################################################################
  ;; ###########################################################################
    )  
  )
  
  (if (= taz_s_family "CHS")
    (progn
    ;; ============================
    ;; WYPROWADZENIE WSPÓŁRZĘDNYCH
    ;; ============================
      
    ;; rysowanie konturu
    ;; ustawienie kamery
    (command "_LINE" '(-50 -50 0) '(50 50 0) "")
    (command "_PLAN" "_C")
    (command "_ZOOM" "_OBJECT" (entlast) "")
    (entdel (entlast))
    (command "_ZOOM" "_SCALE" "1000X")
    (command "REGEN")

    ;; Środek przekroju
    (setq taz_s_p_center (list 0.0 0.0))

    ;; Promienie
    (setq taz_s_R_out (/ taz_s_d 2.0))
    (setq taz_s_R_in  (- taz_s_R_out taz_s_t))

    ;; ============================
    ;; PUNKTY ZEWNĘTRZNE (4 punkty)
    ;; ============================

    (setq taz_s_plp1 (list taz_s_R_out 0.0))         ;; prawo
    (setq taz_s_plp2 (list 0.0 taz_s_R_out))         ;; góra
    (setq taz_s_plp3 (list (- taz_s_R_out) 0.0))     ;; lewo
    (setq taz_s_plp4 (list 0.0 (- taz_s_R_out)))     ;; dół

    ;; ============================
    ;; PUNKTY WEWNĘTRZNE (4 punkty)
    ;; ============================

    (setq taz_s_plp5 (list taz_s_R_in 0.0))          ;; prawo
    (setq taz_s_plp6 (list 0.0 taz_s_R_in))          ;; góra
    (setq taz_s_plp7 (list (- taz_s_R_in) 0.0))      ;; lewo
    (setq taz_s_plp8 (list 0.0 (- taz_s_R_in)))      ;; dół

    )
  )
  
  ;; ---------------------------------------------------------
  ;; RYSOWANIE PRZEKROJOW
  ;; ---------------------------------------------------------
      
  ;; rysowanie SHS
  (if (= taz_s_family "SHS")
    (progn
      (command "_PLINE"
        taz_s_plp4_3       
               
        taz_s_plp1_1
        "A"
        taz_s_plp1_2
        taz_s_plp1_3
        "L"
               
        taz_s_plp2_1
        "A"
        taz_s_plp2_2
        taz_s_plp2_3
        "L"
               
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
        
        "C"
        )
      (command "_CHPROP" (entlast) "" "C" "6" "")
      (command "_PLINE"
        taz_s_plp8_3       
        
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
               
        taz_s_plp7_1
        "A"
        taz_s_plp7_2
        taz_s_plp7_3
        "L"
               
        taz_s_plp8_1
        "A"
        taz_s_plp8_2
        taz_s_plp8_3
        "L"
        
        "C"
        )
      (command "_CHPROP" (entlast) "" "C" "210" "")
    )
    (princ)
  )
  
  ;; rysowanie RHS
  (if (= taz_s_family "RHS")
    (progn
      (command "_PLINE"
        taz_s_plp4_3       
      
        taz_s_plp1_1
        "A"
        taz_s_plp1_2
        taz_s_plp1_3
        "L"
               
        taz_s_plp2_1
        "A"
        taz_s_plp2_2
        taz_s_plp2_3
        "L"
               
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
        
        "C"
        )
      (command "_CHPROP" (entlast) "" "C" "6" "")
      (command "_PLINE"
        taz_s_plp8_3
        
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
               
        taz_s_plp7_1
        "A"
        taz_s_plp7_2
        taz_s_plp7_3
        "L"
               
        taz_s_plp8_1
        "A"
        taz_s_plp8_2
        taz_s_plp8_3
        "L"
        
        "C"
        )
      (command "_CHPROP" (entlast) "" "C" "210" "")
    )
    (princ)
  )
  
  ;; rysowanie CHS  
  (if (= taz_s_family "CHS")
    (progn
      ;; --- punkty okregu zewnetrznego (R_out) - 20 punktow co 18 stopni (pi/10 rad) ---
      (setq taz_s_pt_out_01 (polar taz_s_p_center (*  0 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_02 (polar taz_s_p_center (*  1 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_03 (polar taz_s_p_center (*  2 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_04 (polar taz_s_p_center (*  3 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_05 (polar taz_s_p_center (*  4 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_06 (polar taz_s_p_center (*  5 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_07 (polar taz_s_p_center (*  6 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_08 (polar taz_s_p_center (*  7 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_09 (polar taz_s_p_center (*  8 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_10 (polar taz_s_p_center (*  9 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_11 (polar taz_s_p_center (* 10 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_12 (polar taz_s_p_center (* 11 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_13 (polar taz_s_p_center (* 12 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_14 (polar taz_s_p_center (* 13 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_15 (polar taz_s_p_center (* 14 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_16 (polar taz_s_p_center (* 15 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_17 (polar taz_s_p_center (* 16 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_18 (polar taz_s_p_center (* 17 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_19 (polar taz_s_p_center (* 18 (/ pi 10)) taz_s_R_out))
      (setq taz_s_pt_out_20 (polar taz_s_p_center (* 19 (/ pi 10)) taz_s_R_out))

      ;; --- punkty okregu wewnetrznego (R_in) - 20 punktow co 18 stopni (pi/10 rad) ---
      (setq taz_s_pt_in_01 (polar taz_s_p_center (*  0 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_02 (polar taz_s_p_center (*  1 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_03 (polar taz_s_p_center (*  2 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_04 (polar taz_s_p_center (*  3 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_05 (polar taz_s_p_center (*  4 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_06 (polar taz_s_p_center (*  5 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_07 (polar taz_s_p_center (*  6 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_08 (polar taz_s_p_center (*  7 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_09 (polar taz_s_p_center (*  8 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_10 (polar taz_s_p_center (*  9 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_11 (polar taz_s_p_center (* 10 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_12 (polar taz_s_p_center (* 11 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_13 (polar taz_s_p_center (* 12 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_14 (polar taz_s_p_center (* 13 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_15 (polar taz_s_p_center (* 14 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_16 (polar taz_s_p_center (* 15 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_17 (polar taz_s_p_center (* 16 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_18 (polar taz_s_p_center (* 17 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_19 (polar taz_s_p_center (* 18 (/ pi 10)) taz_s_R_in))
      (setq taz_s_pt_in_20 (polar taz_s_p_center (* 19 (/ pi 10)) taz_s_R_in))
    )
  )

  ;; rysowanie CHS
  (if (= taz_s_family "CHS")
    (progn
      (command "_PLINE"
        taz_s_pt_out_01
        taz_s_pt_out_02
        taz_s_pt_out_03
        taz_s_pt_out_04
        taz_s_pt_out_05
        taz_s_pt_out_06
        taz_s_pt_out_07
        taz_s_pt_out_08
        taz_s_pt_out_09
        taz_s_pt_out_10
        taz_s_pt_out_11
        taz_s_pt_out_12
        taz_s_pt_out_13
        taz_s_pt_out_14
        taz_s_pt_out_15
        taz_s_pt_out_16
        taz_s_pt_out_17
        taz_s_pt_out_18
        taz_s_pt_out_19
        taz_s_pt_out_20
        "C"
      )
      (command "_CHPROP" (entlast) "" "C" "6" "")

      (command "_PLINE"
        taz_s_pt_in_01
        taz_s_pt_in_02
        taz_s_pt_in_03
        taz_s_pt_in_04
        taz_s_pt_in_05
        taz_s_pt_in_06
        taz_s_pt_in_07
        taz_s_pt_in_08
        taz_s_pt_in_09
        taz_s_pt_in_10
        taz_s_pt_in_11
        taz_s_pt_in_12
        taz_s_pt_in_13
        taz_s_pt_in_14
        taz_s_pt_in_15
        taz_s_pt_in_16
        taz_s_pt_in_17
        taz_s_pt_in_18
        taz_s_pt_in_19
        taz_s_pt_in_20
        "C"
      )
      (command "_CHPROP" (entlast) "" "C" "210" "")
    )
    (princ)
  )

  (if (or (= taz_s_family "SHS") (= taz_s_family "RHS"))
    (progn
  ;; wybór profilu
  (setq taz_s_create_beam_profile
        (ssname (ssget "_X" '((0 . "LWPOLYLINE") (62 . 6))) 0))
  (command "REGEN")
  
  (setq taz_s_create_beam_profile_cut
        (ssname (ssget "_X" '((0 . "LWPOLYLINE") (62 . 210))) 0))
  (command "REGEN")
    )
  )
  
  (if (= taz_s_family "CHS")
    (progn
  ;; wybór profilu
  (setq taz_s_create_beam_profile
        (ssname (ssget "_X" '((0 . "LWPOLYLINE") (62 . 6))) 0))
  (command "REGEN")
  
  (setq taz_s_create_beam_profile_cut
        (ssname (ssget "_X" '((0 . "LWPOLYLINE") (62 . 210))) 0))
  (command "REGEN")
    )
  )

  ;; SWEEP
  (command "_SWEEP" taz_s_create_beam_profile "" "_B" (trans taz_s_create_beam_p1 0 1) taz_s_create_beam_path)
  (command)
  (setq taz_s_region1 (entlast))
  (command "_SWEEP" taz_s_create_beam_profile_cut "" "_B" (trans taz_s_create_beam_p1 0 1) taz_s_create_beam_profile_cut_path)
  (command)
  (setq taz_s_region2 (entlast))
  (command "SUBTRACT" taz_s_region1 "" taz_s_region2 "")
  (command)
  
  ;; zmiana warstwy
  (command "_CHPROP" (ssadd (entlast)) "" "_LA" "taz_s_beam" "")

  ;; przywrócenie widoku
  (command "-VIEW" "_R" "taz_s_temp_view")
  (command "-VIEW" "_D" "taz_s_temp_view")
  
  (princ)
)
