taz_s_create_drawings_execution_design_organize_dialog : dialog {
  label = "TAZ - Rysunki wykonawcze";

  : column {

    : text {
      key = "taz_s_execution_design_case_label";
      label = "Przypadek";
      alignment = centered;
      width = 42;
      fixed_width = true;
    }

    spacer;

    : row {
      alignment = centered;

      : text {
        label = "Skala opisu:";
        width = 20;
        fixed_width = true;
      }

      : popup_list {
        key = "taz_s_annotation_scale_popup";
        width = 20;
        fixed_width = true;
      }
    }

    : row {
      alignment = centered;

      : text {
        label = "Format arkusza:";
        width = 20;
        fixed_width = true;
      }

      : popup_list {
        key = "taz_s_organize_frame_format_popup";
        width = 20;
        fixed_width = true;
      }
    }

    : toggle {
      key = "taz_s_execution_design_apply_all_toggle";
      label = "Zastosuj do wszystkich";
      value = "1";
      alignment = centered;
    }
  }

  ok_cancel;
}
