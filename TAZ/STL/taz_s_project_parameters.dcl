taz_s_project_parameters_dialog : dialog {
  label = "TAZ - Parametry projektu";

  : column {

    : row {
      alignment = centered;

      : text {
        label = "Ciezar objetosciowy stali [kg/m3]:";
        width = 38;
        fixed_width = true;
      }

      : edit_box {
        key = "taz_s_unit_weight_steel_edit";
        width = 15;
        fixed_width = true;
      }
    }

    : row {
      alignment = centered;

      : text {
        label = "Ciezar objetosciowy betonu [kg/m3]:";
        width = 38;
        fixed_width = true;
      }

      : edit_box {
        key = "taz_s_unit_weight_concrete_edit";
        width = 15;
        fixed_width = true;
      }
    }

    : row {
      alignment = centered;

      : text {
        label = "Naddatek na polaczenia [%]:";
        width = 38;
        fixed_width = true;
      }

      : edit_box {
        key = "taz_s_additional_mass_edit";
        width = 15;
        fixed_width = true;
      }
    }
  }

  ok_cancel;
}
