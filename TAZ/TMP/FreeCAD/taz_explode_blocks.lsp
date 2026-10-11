(defun c:TAZ_EXPLODE_BLOCKS ( / )

  (setq taz_g_bloki_istnieja T)

  (while (= taz_g_bloki_istnieja T)

    (setq taz_g_zbior_blokow nil)

    (setq taz_g_zbior_blokow
      (ssget "_X" '((0 . "INSERT")))
    )

    (if (= taz_g_zbior_blokow nil)

      (setq taz_g_bloki_istnieja nil)

      (progn

        (command "_.EXPLODE" taz_g_zbior_blokow)

        (setq taz_g_zbior_blokow nil)

      )

    )

  )

  (princ)
)