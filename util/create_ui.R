create_ui <- function(index, question, control = NULL, controlWidth = NULL, plot) {
  ui <- tags$section(
    id = paste("q", index, sep=""),
    tags$h1(paste("Question", index)),
    tags$h2(question),
    fluidPage(fluidRow(
      {
        if (!is.null(control)) {
          column(
            width = 2,
            style = paste("margin-top: 47px; max-width: ", controlWidth, "px;", sep=""),
            control
          )
        }
      },
      column(
        width = {
          if (!is.null(control)) {
            10
          } else {
            12
          }
        },
        plot
      )
    ))
  )
               
  return(ui)
}
