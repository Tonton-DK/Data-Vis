create_ui <- function(index, question, conclusion, plot, control = NULL, controlWidth = NULL) {
  ui <- tags$section(
    id = paste("q", index, sep=""),
    tags$div(
      class = "question",
      tags$h1(paste("Question", index)),
      tags$h2(question),
      fluidPage(class = "chart", fluidRow(
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
      )),
      tags$p(conclusion)
    )
  )
  
  return(ui)
}
