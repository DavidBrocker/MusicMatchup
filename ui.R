# UI ------------------------------------------------------------------------------------------
ui <- 
  page_fillable(
    tags$style(
      HTML(".sweet-alert.modal_al.alert-size-s.showSweetAlert.visible {
            display: block;
            margin-top: -238px;
            border-radius: 40px;
            } .sa-icon.sa-custom {
            display: block;
            border-radius: 50%;
           }")
    ),
    window_title = "MusicMatchup",
    fillable_mobile = FALSE,
    title = "MusicMatchup",
    tags$style(
      HTML("
          .dropdown.bootstrap-select.form-control.fit-width{
            border-radius: 50px;
            position: absolute;
            top: 3rem;
            left: 12rem;
           } img {
           border-radius: 50%;
           align-self: center;
           }
           .sir{
           float: left;
           width: 50%
           }.card-header{
           color:grey;
           } iframe{
           border-radius:12px;
           }h2 {
           font-size: 34px;
           background: -webkit-linear-gradient(#f44336, #333);
           -webkit-background-clip: text;
           -webkit-text-fill-color: transparent;
           }.rfsh {
           margin-left: 30%;
           } .main_card{
            height: 1rem;
           } .main_card_body {

           }.main_card:hover {
            box-shadow: 0px 0px 4px 0px darkgreen;
           } .net_card:hover {
            box-shadow: 0px 0px 4px 0px darkgreen;
           } .pre_card:hover {
            box-shadow: 0px 0px 4px 0px darkgreen;
           } .gen_card:hover {
           box-shadow: 0px 0px 4px 0px darkgreen;
           } .name_card:hover {
           box-shadow: 0px 0px 4px 0px darkgreen;
           } .fol_card:hover {
           box-shadow: 0px 0px 4px 0px darkgreen;
           } .bi.bi-diagram-3,
             .bi.bi-music-note {
           color: blue;
           position: absolute;
           right: 20px;
           } .bi.bi-info-circle {
           position: absolute;
           bottom: 1rem;
           left: 18rem;
           } #search {
            width: 100px;
            position: absolute;
            left: 26rem;
            border-radius: 3px;
           } #artist {
            position: absolute;
            max-width: 10rem;
            top: 48px;
            border: 1rem;
            border-radius: 3px;
            background-color: #f8f8f8;
            height: 47px;
            font-weight: 600;
           } #artist:hover {
            background-color: #d3d3d3;
            }
           "
      )),
    layout_column_wrap(
      layout_column_wrap(
        width = 1,
        heights_equal = "row",
        card_srch,
        layout_column_wrap(
          card_name,
          card_fol
        ),
        layout_column_wrap(
          card_pre,
          card_gen
        ),
      ),
      layout_columns(
        width = 1/2,
        card_vis
      )
    )
  )
