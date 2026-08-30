taz_s_move_copy_dialog : dialog {
  label = "Move / Copy";

  : column {

    : radio_row {
      : radio_button { key="taz_s_mode_move"; label="Move"; value="1"; }
      : radio_button { key="taz_s_mode_copy"; label="Copy"; value="0"; }
    }

    spacer;

    : row {
      : text { label="X:"; width=5; }
      : edit_box { key="taz_s_x"; edit_width=12; }
    }

    : row {
      : text { label="Y:"; width=5; }
      : edit_box { key="taz_s_y"; edit_width=12; }
    }

    : row {
      : text { label="Z:"; width=5; }
      : edit_box { key="taz_s_z"; edit_width=12; }
    }

    spacer;

    : row {
      : button { key="taz_s_point"; label="Point"; }
    }

    spacer;

    ok_cancel;

  }

}
