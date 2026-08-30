taz_s_select_section : dialog {
    label = "Wybór profilu";

    : boxed_column {

        : row {
            : text { label = "Kategoria:"; width = 20; fixed_width = true; }
            : popup_list { key = "taz_s_cat"; width = 30; fixed_width = true; }
        }

        : row {
            : text { label = "Rodzina:"; width = 20; fixed_width = true; }
            : popup_list { key = "taz_s_fam"; width = 30; fixed_width = true; }
        }

        : row {
            : text { label = "Typ:"; width = 20; fixed_width = true; }
            : popup_list { key = "taz_s_typ"; width = 30; fixed_width = true; }
        }
    }

    : row {
        : button { key = "ok"; label = "OK"; is_default = true; }
        : button { key = "anuluj"; label = "Anuluj"; is_cancel = true; }
    }
}
