(defun taz_s_annotation_scale_apply ()

  ;; Wartosc taz_s_annotation_scale jest przekazywana z zewnatrz.
  ;; Nazwa funkcji jest celowo inna niz nazwa zmiennej skali,
  ;; aby wartosc np. 50 nie nadpisywala funkcji w GstarCAD AutoLISP.
  ;; Funkcja pozostaje ogolna i odpowiada tylko za przeliczenie
  ;; wartosci zależnych od wybranej skali.
  ;;
  ;; Jesli wywolano ja bez wczesniejszego ustawienia skali,
  ;; zachowujemy dotychczasowa wartosc domyslna 1:1.
  (if (not taz_s_annotation_scale)
    (setq taz_s_annotation_scale 1)
  )

  ;; debug
  (princ (strcat "\nWybrana skala opisu = " (itoa taz_s_annotation_scale)))
  
  ;; 1:1
  (if (= taz_s_annotation_scale 1)
    (progn
      (setq taz_s_annotation_scale_label 2.5)
      (setq taz_s_annotation_scale_axis 3.5)
    )
  )

  ;; 1:2
  (if (= taz_s_annotation_scale 2)
    (progn
      (setq taz_s_annotation_scale_label 5)
      (setq taz_s_annotation_scale_axis 7)
    )
  )

  ;; 1:5
  (if (= taz_s_annotation_scale 5)
    (progn
      (setq taz_s_annotation_scale_label 12.5)
      (setq taz_s_annotation_scale_axis 17.5)
    )
  )

  ;; 1:10
  (if (= taz_s_annotation_scale 10)
    (progn
      (setq taz_s_annotation_scale_label 25)
      (setq taz_s_annotation_scale_axis 35)
    )
  )

  ;; 1:20
  (if (= taz_s_annotation_scale 20)
    (progn
      (setq taz_s_annotation_scale_label 50)
      (setq taz_s_annotation_scale_axis 70)
    )
  )

  ;; 1:25
  (if (= taz_s_annotation_scale 25)
    (progn
      (setq taz_s_annotation_scale_label 62.5)
      (setq taz_s_annotation_scale_axis 87.5)
    )
  )

  ;; 1:50
  (if (= taz_s_annotation_scale 50)
    (progn
      (setq taz_s_annotation_scale_label 125)
      (setq taz_s_annotation_scale_axis 175)
    )
  )

  ;; 1:100
  (if (= taz_s_annotation_scale 100)
    (progn
      (setq taz_s_annotation_scale_label 250)
      (setq taz_s_annotation_scale_axis 350)
    )
  )

  ;; 1:200
  (if (= taz_s_annotation_scale 200)
    (progn
      (setq taz_s_annotation_scale_label 500)
      (setq taz_s_annotation_scale_axis 700)
    )
  )

  (princ)
)
