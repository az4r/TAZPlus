;;; ================================================================
;;; ALL2LINE.LSP
;;;
;;; GstarCAD / AutoLISP
;;;
;;; CEL:
;;;   Zamiana geometrii na obiekty typu LINE.
;;;
;;; OBSLUGIWANE:
;;;   LINE       -> pozostaje LINE
;;;   3DFACE     -> krawedzie jako LINE
;;;   ARC        -> LINE wg zadanej tolerancji
;;;   CIRCLE     -> LINE wg zadanej tolerancji
;;;   LWPOLYLINE -> LINE / ARC -> LINE
;;;   POLYLINE   -> LINE / ARC -> LINE
;;;
;;; TOLERANCJA:
;;;   Dla ARC/CIRCLE maksymalna strzalka aproksymacji.
;;;
;;;   Przykladowo:
;;;       0.10
;;;
;;;   oznacza maksymalny blad aproksymacji ok. 0.10
;;;   jednostki rysunku.
;;;
;;; UWAGA:
;;;   3DFACE przechowuje wierzcholki w WCS.
;;;   ARC/CIRCLE oraz LWPOLYLINE wykorzystuja OCS,
;;;   dlatego wymagaja odpowiedniej transformacji.
;;; ================================================================


(vl-load-com)


;;; ================================================================
;;; FUNKCJA ACOS
;;;
;;; GstarCAD / GstarLISP w niektorych wersjach nie posiada ACOS.
;;; Uzywamy atan(x,y), ktore jest dostepne.
;;; ================================================================

(defun AL:ACOS (x / xx)

  ;; Ograniczenie do [-1,1]
  (setq xx
    (max -1.0
      (min 1.0 x)
    )
  )

  ;; acos(x)
  (atan
    (sqrt
      (max 0.0
        (- 1.0 (* xx xx))
      )
    )
    xx
  )
)


;;; ================================================================
;;; POMOCNICZA FUNKCJA
;;; Pobranie wartosci DXF
;;; ================================================================

(defun AL:DXF (code data default / a)

  (setq a (assoc code data))

  (if a
    (cdr a)
    default
  )
)


;;; ================================================================
;;; UTWORZENIE LINE
;;;
;;; P1/P2 musza byc w WCS.
;;;
;;; Zachowujemy:
;;;   layer
;;;   color
;;;   true color
;;;   linetype
;;;   linetype scale
;;;   lineweight
;;; ================================================================

(defun AL:MakeLine
  (p1 p2 source
   / data layer
     c62 c420 c6 c48 c370
     lineData
  )

  ;; Nie tworzymy zerowej linii
  (if (> (distance p1 p2) 1e-12)

    (progn

      (setq data (entget source))

      ;; Wlasciwosci
      (setq layer (AL:DXF 8 data "0"))

      (setq c62  (assoc 62  data))
      (setq c420 (assoc 420 data))
      (setq c6   (assoc 6   data))
      (setq c48  (assoc 48  data))
      (setq c370 (assoc 370 data))

      ;; Minimalna definicja LINE
      (setq lineData
        (list
          '(0 . "LINE")
          '(100 . "AcDbEntity")
          (cons 8 layer)

          '(100 . "AcDbLine")

          (cons 10 p1)
          (cons 11 p2)
        )
      )

      ;; Kolor ACI
      (if c62
        (setq lineData
          (append lineData (list c62))
        )
      )

      ;; True Color
      (if c420
        (setq lineData
          (append lineData (list c420))
        )
      )

      ;; Linetype
      (if c6
        (setq lineData
          (append lineData (list c6))
        )
      )

      ;; Linetype scale
      (if c48
        (setq lineData
          (append lineData (list c48))
        )
      )

      ;; Lineweight
      (if c370
        (setq lineData
          (append lineData (list c370))
        )
      )

      ;; Tworzenie
      (entmakex lineData)
    )
  )
)


;;; ================================================================
;;; OBLICZENIE MAKSYMALNEGO KATA SEGMENTU
;;;
;;; Dla:
;;;
;;;     sagitta = R * (1 - cos(theta/2))
;;;
;;; obliczamy theta dla zadanej tolerancji.
;;; ================================================================

(defun AL:MaxSegmentAngle (radius tol / x)

  (if
    (or
      (<= radius 0.0)
      (<= tol 0.0)
    )

    ;; Awaryjnie 10 stopni
    (/ pi 18.0)

    (if (>= tol (* 2.0 radius))

      ;; Tolerancja ogromna
      (* 2.0 pi)

      (progn

        (setq x
          (- 1.0
            (/ tol radius)
          )
        )

        ;; Zabezpieczenie numeryczne
        (setq x
          (max -1.0
            (min 1.0 x)
          )
        )

        (* 2.0
          (AL:ACOS x)
        )
      )
    )
  )
)


;;; ================================================================
;;; ARC -> LINE
;;;
;;; Punkty ARC pobierane sa z OCS i transformowane do WCS.
;;; ================================================================

(defun AL:ArcToLines
  (e tol
   / data
     cen rad a1 a2 extent
     maxAng n i
     ang1 ang2
     p1 p2
     p1ocs p2ocs
  )

  (setq data (entget e))

  ;; ARC:
  ;; 10 = center
  ;; 40 = radius
  ;; 50 = start angle
  ;; 51 = end angle

  (setq cen (cdr (assoc 10 data)))
  (setq rad (cdr (assoc 40 data)))
  (setq a1  (cdr (assoc 50 data)))
  (setq a2  (cdr (assoc 51 data)))

  ;; Zakres CCW
  (setq extent (- a2 a1))

  (if (<= extent 0.0)
    (setq extent
      (+ extent (* 2.0 pi))
    )
  )

  ;; Maksymalny kat pojedynczego segmentu
  (setq maxAng
    (AL:MaxSegmentAngle rad tol)
  )

  ;; Liczba segmentow
  (setq n
    (max 1
      (fix
        (1+ (/ extent maxAng))
      )
    )
  )

  (setq i 0)

  (while (< i n)

    ;; ----------------------------------------
    ;; Pierwszy kat
    ;; ----------------------------------------

    (setq ang1
      (+ a1
        (* extent
          (/ (float i) n)
        )
      )
    )

    ;; ----------------------------------------
    ;; Drugi kat
    ;; ----------------------------------------

    (setq ang2
      (+ a1
        (* extent
          (/ (float (1+ i)) n)
        )
      )
    )

    ;; ----------------------------------------
    ;; Punkty w OCS
    ;; ----------------------------------------

    (setq p1ocs
      (list
        (+ (car cen)
          (* rad (cos ang1))
        )

        (+ (cadr cen)
          (* rad (sin ang1))
        )

        0.0
      )
    )

    (setq p2ocs
      (list
        (+ (car cen)
          (* rad (cos ang2))
        )

        (+ (cadr cen)
          (* rad (sin ang2))
        )

        0.0
      )
    )

    ;; ----------------------------------------
    ;; OCS -> WCS
    ;; ----------------------------------------

    (setq p1
      (trans p1ocs e 0)
    )

    (setq p2
      (trans p2ocs e 0)
    )

    ;; ----------------------------------------
    ;; LINE
    ;; ----------------------------------------

    (AL:MakeLine p1 p2 e)

    (setq i
      (1+ i)
    )
  )

  ;; Usuwamy ARC
  (entdel e)
)


;;; ================================================================
;;; CIRCLE -> LINE
;;; ================================================================

(defun AL:CircleToLines
  (e tol
   / data
     cen rad
     maxAng n i
     ang1 ang2
     p1ocs p2ocs
     p1 p2
  )

  (setq data (entget e))

  ;; CIRCLE
  (setq cen (cdr (assoc 10 data)))
  (setq rad (cdr (assoc 40 data)))

  ;; Maksymalny kat
  (setq maxAng
    (AL:MaxSegmentAngle rad tol)
  )

  ;; Pelny okrag
  (setq n
    (max 3
      (fix
        (1+ (/ (* 2.0 pi) maxAng))
      )
    )
  )

  (setq i 0)

  (while (< i n)

    ;; ----------------------------------------
    ;; Katy
    ;; ----------------------------------------

    (setq ang1
      (* 2.0 pi
        (/ (float i) n)
      )
    )

    (setq ang2
      (* 2.0 pi
        (/ (float (1+ i)) n)
      )
    )

    ;; ----------------------------------------
    ;; Punkty OCS
    ;; ----------------------------------------

    (setq p1ocs
      (list
        (+ (car cen)
          (* rad (cos ang1))
        )

        (+ (cadr cen)
          (* rad (sin ang1))
        )

        0.0
      )
    )

    (setq p2ocs
      (list
        (+ (car cen)
          (* rad (cos ang2))
        )

        (+ (cadr cen)
          (* rad (sin ang2))
        )

        0.0
      )
    )

    ;; ----------------------------------------
    ;; OCS -> WCS
    ;; ----------------------------------------

    (setq p1
      (trans p1ocs e 0)
    )

    (setq p2
      (trans p2ocs e 0)
    )

    ;; ----------------------------------------
    ;; LINE
    ;; ----------------------------------------

    (AL:MakeLine p1 p2 e)

    (setq i
      (1+ i)
    )
  )

  ;; Usuwamy CIRCLE
  (entdel e)
)


;;; ================================================================
;;; 3DFACE -> LINE
;;;
;;; UWAGA:
;;; Wierzcholki 3DFACE sa w WCS.
;;; NIE wykonujemy tutaj TRANS.
;;;
;;; Kod 70:
;;;   1 = niewidoczna krawedz 1
;;;   2 = niewidoczna krawedz 2
;;;   4 = niewidoczna krawedz 3
;;;   8 = niewidoczna krawedz 4
;;;
;;; Respektujemy te flagi.
;;; ================================================================

(defun AL:3DFaceToLines
  (e
   / data
     p1 p2 p3 p4
     flags
  )

  (setq data (entget e))

  ;; Wierzcholki sa bezposrednio w WCS
  (setq p1 (cdr (assoc 10 data)))
  (setq p2 (cdr (assoc 11 data)))
  (setq p3 (cdr (assoc 12 data)))
  (setq p4 (cdr (assoc 13 data)))

  ;; Flagi niewidocznych krawedzi
  (setq flags
    (AL:DXF 70 data 0)
  )

  ;; ----------------------------------------
  ;; Krawedz 1: P1 -> P2
  ;; ----------------------------------------

  (if (= 0 (logand flags 1))
    (AL:MakeLine p1 p2 e)
  )

  ;; ----------------------------------------
  ;; Krawedz 2: P2 -> P3
  ;; ----------------------------------------

  (if (= 0 (logand flags 2))
    (AL:MakeLine p2 p3 e)
  )

  ;; ----------------------------------------
  ;; Krawedz 3: P3 -> P4
  ;; ----------------------------------------

  (if (= 0 (logand flags 4))
    (AL:MakeLine p3 p4 e)
  )

  ;; ----------------------------------------
  ;; Krawedz 4: P4 -> P1
  ;; ----------------------------------------

  (if (= 0 (logand flags 8))
    (AL:MakeLine p4 p1 e)
  )

  ;; Usuwamy 3DFACE
  (entdel e)
)


;;; ================================================================
;;; POLYLINE / LWPOLYLINE
;;;
;;; Wykorzystujemy EXPLODE poprzez ActiveX.
;;;
;;; W wyniku:
;;;   prosty segment -> LINE
;;;   luk -> ARC
;;;
;;; ARC zostaje pozniej zamieniony na LINE.
;;; ================================================================

(defun AL:PolylineToLines
  (e tol
   / obj result arr lst x en typ
  )

  (setq obj
    (vlax-ename->vla-object e)
  )

  ;; EXPLODE
  (setq result
    (vl-catch-all-apply
      'vlax-invoke
      (list obj 'Explode)
    )
  )

  (if
    (not
      (vl-catch-all-error-p result)
    )

    (progn

      ;; SAFEARRAY -> LIST
      (setq arr
        (vl-catch-all-apply
          'vlax-variant-value
          (list result)
        )
      )

      (if
        (not
          (vl-catch-all-error-p arr)
        )

        (progn

          (setq lst
            (vl-catch-all-apply
              'vlax-safearray->list
              (list arr)
            )
          )

          (if
            (not
              (vl-catch-all-error-p lst)
            )

            (progn

              (foreach x lst

                (setq en
                  (vl-catch-all-apply
                    'vlax-vla-object->ename
                    (list x)
                  )
                )

                (if
                  (not
                    (vl-catch-all-error-p en)
                  )

                  (progn

                    (setq typ
                      (cdr
                        (assoc 0
                          (entget en)
                        )
                      )
                    )

                    (cond

                      ;; ----------------------------------
                      ;; LINE
                      ;; ----------------------------------

                      ((= typ "LINE")

                       ;; Nic nie robimy.
                       ;; LINE juz jest odpowiednim obiektem.

                      )


                      ;; ----------------------------------
                      ;; ARC
                      ;; ----------------------------------

                      ((= typ "ARC")

                       (AL:ArcToLines
                         en
                         tol
                       )

                      )


                      ;; ----------------------------------
                      ;; CIRCLE
                      ;; ----------------------------------

                      ((= typ "CIRCLE")

                       (AL:CircleToLines
                         en
                         tol
                       )

                      )

                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  ;; Usuwamy oryginalna POLYLINE
  (if (entget e)
    (entdel e)
  )
)


;;; ================================================================
;;; RAPORT KOŃCOWY
;;;
;;; Sprawdza, czy po operacji pozostaly obiekty inne niz LINE.
;;; ================================================================

(defun AL:FinalCheck
  (/ ss i e typ
     cntLine
     cntOther
     otherTypes
  )

  (setq cntLine 0)
  (setq cntOther 0)
  (setq otherTypes '())

  (setq ss
    (ssget "_X")
  )

  (if ss

    (progn

      (setq i 0)

      (while
        (< i (sslength ss))

        (setq e
          (ssname ss i)
        )

        (if (entget e)

          (progn

            (setq typ
              (cdr
                (assoc 0
                  (entget e)
                )
              )
            )

            (if (= typ "LINE")

              (setq cntLine
                (1+ cntLine)
              )

              (progn

                (setq cntOther
                  (1+ cntOther)
                )

                (if
                  (not
                    (member typ otherTypes)
                  )

                  (setq otherTypes
                    (cons typ otherTypes)
                  )
                )
              )
            )
          )
        )

        (setq i
          (1+ i)
        )
      )
    )
  )

  ;; Raport
  (princ "\n")
  (princ "\n==============================================")
  (princ "\n KONTROLA KONCOWA")
  (princ "\n==============================================")

  (princ
    (strcat
      "\nLINE       : "
      (itoa cntLine)
    )
  )

  (princ
    (strcat
      "\nINNE OBIEKTY: "
      (itoa cntOther)
    )
  )

  (if (> cntOther 0)

    (progn

      (princ "\nTypy pozostalych obiektow:")

      (foreach typ
        (reverse otherTypes)

        (princ
          (strcat
            "\n  - "
            typ
          )
        )
      )

      (princ
        "\n\nUWAGA: pozostaly obiekty nie zostaly automatycznie usuniete."
      )

    )

    (princ
      "\n\nOK - w rysunku pozostaly tylko obiekty LINE."
    )
  )

  (princ
    "\n=============================================="
  )
)


;;; ================================================================
;;; GLOWNE POLECENIE
;;;
;;; ALL2LINE
;;; ================================================================

(defun c:ALL2LINE
  (/ *error*
     ss
     tol
     i
     e
     typ
     cntLine
     cntFace
     cntArc
     cntCircle
     cntPline
     cntOther
  )

  ;; --------------------------------------------
  ;; Lokalna obsluga bledu
  ;; --------------------------------------------

  (defun *error* (msg)

    (if
      (and msg
           (/= msg "Function cancelled")
           (/= msg "quit / exit abort")
      )

      (princ
        (strcat
          "\nBLAD: "
          msg
        )
      )
    )

    (princ)
  )


  (vl-load-com)

  (princ "\n")
  (princ "==============================================")
  (princ "\n ALL2LINE")
  (princ "\n GstarCAD - konwersja geometrii do LINE")
  (princ "\n==============================================")


  ;; --------------------------------------------
  ;; Tolerancja
  ;; --------------------------------------------

  (setq tol
    (getreal
      "\nPodaj maksymalna dokladnosc dla lukow/okregow <0.10>: "
    )
  )

  (if (null tol)
    (setq tol 0.10)
  )


  ;; Kontrola tolerancji
  (if (<= tol 0.0)

    (progn

      (princ
        "\nBLAD: tolerancja musi byc wieksza od zera."
      )

    )

    (progn

      ;; ----------------------------------------
      ;; Wybor
      ;; ----------------------------------------

      (princ "\n")
      (princ
        "\nWybierz obiekty do konwersji."
      )

      (princ
        "\nENTER = caly rysunek."
      )

      (setq ss
        (ssget)
      )

      ;; ENTER = caly rysunek
      (if (null ss)
        (setq ss
          (ssget "_X")
        )
      )


      ;; ----------------------------------------
      ;; Czy cos znaleziono?
      ;; ----------------------------------------

      (if (null ss)

        (princ
          "\nBrak obiektow do przetworzenia."
        )

        (progn

          ;; Liczniki
          (setq cntLine   0)
          (setq cntFace   0)
          (setq cntArc    0)
          (setq cntCircle 0)
          (setq cntPline  0)
          (setq cntOther  0)

          ;; ------------------------------------
          ;; Przetwarzanie
          ;; ------------------------------------

          (setq i 0)

          (while
            (< i (sslength ss))

            (setq e
              (ssname ss i)
            )

            ;; Obiekt mogl zostac usuniety
            (if (entget e)

              (progn

                (setq typ
                  (cdr
                    (assoc 0
                      (entget e)
                    )
                  )
                )

                (cond

                  ;; ==============================
                  ;; LINE
                  ;; ==============================

                  ((= typ "LINE")

                   (setq cntLine
                     (1+ cntLine)
                   )
                  )


                  ;; ==============================
                  ;; 3DFACE
                  ;; ==============================

                  ((= typ "3DFACE")

                   (AL:3DFaceToLines e)

                   (setq cntFace
                     (1+ cntFace)
                   )
                  )


                  ;; ==============================
                  ;; ARC
                  ;; ==============================

                  ((= typ "ARC")

                   (AL:ArcToLines
                     e
                     tol
                   )

                   (setq cntArc
                     (1+ cntArc)
                   )
                  )


                  ;; ==============================
                  ;; CIRCLE
                  ;; ==============================

                  ((= typ "CIRCLE")

                   (AL:CircleToLines
                     e
                     tol
                   )

                   (setq cntCircle
                     (1+ cntCircle)
                   )
                  )


                  ;; ==============================
                  ;; POLYLINE
                  ;; ==============================

                  ((or
                     (= typ "LWPOLYLINE")
                     (= typ "POLYLINE")
                   )

                   (AL:PolylineToLines
                     e
                     tol
                   )

                   (setq cntPline
                     (1+ cntPline)
                   )
                  )


                  ;; ==============================
                  ;; INNY TYP
                  ;; ==============================

                  (T

                   ;; Niczego nie usuwamy.
                   ;; Zostanie pokazane w raporcie.

                   (setq cntOther
                     (1+ cntOther)
                   )
                  )
                )
              )
            )

            ;; Co 500 obiektow informacja
            (if (= (rem (1+ i) 500) 0)

              (princ
                (strcat
                  "\nPrzetworzono: "
                  (itoa (1+ i))
                  " / "
                  (itoa (sslength ss))
                )
              )
            )

            (setq i
              (1+ i)
            )
          )


          ;; ----------------------------------------
          ;; Raport przetwarzania
          ;; ----------------------------------------

          (princ "\n")
          (princ "\n==============================================")
          (princ "\n KONIEC PRZETWARZANIA")
          (princ "\n==============================================")

          (princ
            (strcat
              "\nTolerancja: "
              (rtos tol 2 6)
            )
          )

          (princ
            (strcat
              "\nLINE       : "
              (itoa cntLine)
            )
          )

          (princ
            (strcat
              "\n3DFACE     : "
              (itoa cntFace)
            )
          )

          (princ
            (strcat
              "\nARC        : "
              (itoa cntArc)
            )
          )

          (princ
            (strcat
              "\nCIRCLE     : "
              (itoa cntCircle)
            )
          )

          (princ
            (strcat
              "\nPOLYLINE   : "
              (itoa cntPline)
            )
          )

          (princ
            (strcat
              "\nINNE       : "
              (itoa cntOther)
            )
          )

          ;; ----------------------------------------
          ;; Kontrola calego rysunku
          ;; ----------------------------------------

          (AL:FinalCheck)
        )
      )
    )
  )

  (princ)
)


;;; ================================================================
;;; INFORMACJA
;;; ================================================================

(princ
  "\nALL2LINE zaladowany. Uruchom polecenie: ALL2LINE"
)

(princ)