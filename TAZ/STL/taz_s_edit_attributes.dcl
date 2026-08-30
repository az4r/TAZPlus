taz_s_edit_attributes_dialog : dialog {
  label = "TAZ - Edycja Atrybutów";

  : column {

    : row {
      : text { label = "NUMER ELEMENTU EP:"; width = 40; fixed_width = true; }
      : edit_box { key = "taz_s_attr1"; edit_width = 25; }
    }

    : row {
      : text { label = "NUMER ELEMENTU EW:"; width = 40; fixed_width = true; }
      : edit_box { key = "taz_s_attr2"; edit_width = 25; }
    }

    : row {
      : text { label = "PREFIKS ELEMENTU EP:"; width = 40; fixed_width = true; }
      : edit_box { key = "taz_s_attr3"; edit_width = 25; }
    }

    : row {
      : text { label = "PREFIKS ELEMENTU EW:"; width = 40; fixed_width = true; }
      : edit_box { key = "taz_s_attr4"; edit_width = 25; }
    }

    : row {
      : text { label = "ETAP:"; width = 40; fixed_width = true; }
      : edit_box { key = "taz_s_attr5"; edit_width = 25; }
    }

    : row {
      : text { label = "RODZINA PROFILU:"; width = 40; fixed_width = true; }
      : edit_box { key = "taz_s_attr6"; edit_width = 25; is_enabled = false; }
    }

    : row {
      : text { label = "TYP PROFILU:"; width = 40; fixed_width = true; }
      : edit_box { key = "taz_s_attr7"; edit_width = 25; is_enabled = false; }
    }

    : row {
      : text { label = "MATERIAŁ ELEMENTU:"; width = 40; fixed_width = true; }
      : edit_box { key = "taz_s_attr8"; edit_width = 25; }
    }

    : row {
      : text { label = "FUNKCJA:"; width = 40; fixed_width = true; }
      : edit_box { key = "taz_s_attr9"; edit_width = 25; }
    }

    : row {
      : text { label = "ATRYBUT DODATKOWY:"; width = 40; fixed_width = true; }
      : edit_box { key = "taz_s_attr10"; edit_width = 25; }
    }

  }

  ok_cancel;
}
