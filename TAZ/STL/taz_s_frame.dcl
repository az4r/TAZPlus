taz_s_frame_dialog : dialog {
  label = "TAZ - Ramka rysunkowa";

  : column {

    : row {
      alignment = centered;

      : text {
        label = "Format arkusza:";
        width = 20;
        fixed_width = true;
      }

      : popup_list {
        key = "taz_s_frame_format_popup";
        width = 20;
        fixed_width = true;
      }
    }

    : row {
      alignment = centered;

      : text {
        label = "Skala:";
        width = 20;
        fixed_width = true;
      }

      : popup_list {
        key = "taz_s_frame_scale_popup";
        width = 20;
        fixed_width = true;
      }
    }
  }

  ok_cancel;
}
