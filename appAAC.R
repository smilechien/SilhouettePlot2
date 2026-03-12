# appAAC.R  (Shiny module)
library(shiny)
library(ggplot2)

aac_ui <- function(id, TOTAL_AB = 3 - 0.01, C_FIXED = 0.01) {
  ns <- NS(id)

  tagList(
    tags$head(
      tags$style(HTML("
        .vslider { height: 320px; margin: 0 auto; }
        .vslider .ui-slider-handle { width: 1.2em; height: 1.2em; }
        .aacBox{
          font-size: 40px; font-weight: 800; text-align: center;
          padding: 10px 0; border-radius: 14px;
          border: 1px solid #ddd; margin-bottom: 12px;
        }
        .subtxt{ text-align:center; color:#666; margin-top:-6px; margin-bottom:10px;}
      ")),
      # IMPORTANT: target the namespaced slider id (e.g., aac-B)
      tags$script(HTML(sprintf("
        $(document).on('shiny:connected', function(){
          function makeVertical(sel){
            var el = $(sel);
            if (!el.length) return;
            try{
              el.slider('option', 'orientation', 'vertical');
              el.addClass('vslider');
            } catch(e) {}
          }
          makeVertical('#%s');
        });
      ", ns("B"))))
    ),

    fluidRow(
      column(
        4,
        div(class = "aacBox", textOutput(ns("aac_txt"))),
        div(class = "subtxt", textOutput(ns("abc_txt"))),

        sliderInput(
          inputId = ns("B"),
          label   = sprintf("Adjust B (vertical). Constraints: A = %.2f - B, and C ≤ B ≤ A", TOTAL_AB),
          min     = C_FIXED,         # B >= C
          max     = TOTAL_AB / 2,    # ensures B <= A
          value   = max(0.8, C_FIXED),
          step    = 0.001
        )
      ),

      column(
        8,
        plotOutput(ns("abc_plot"), height = 420),
        tags$div(
          style="margin-top:10px;color:#666;",
          tags$ul(
            tags$li(sprintf("Constraint enforced: A + B = %.2f and B ≤ A.", TOTAL_AB)),
            tags$li("r = (A/B)/(B/C) = A*C/B^2 ; AAC = r/(1+r)."),
            tags$li(sprintf("C is fixed at %.2f.", C_FIXED)),
            tags$li("AAC reference: https://www.rasch.org/rmt/rmt263c.htm")
          )
        )
      )
    )
  )
}

aac_server <- function(id, TOTAL_AB = 3 - 0.01, C_FIXED = 0.01) {
  moduleServer(id, function(input, output, session) {

    vals <- reactive({
      B <- input$B
      C <- C_FIXED
      B <- max(B, C)          # guard
      A <- TOTAL_AB - B

      r <- (A * C) / (B^2)
      aac <- r / (1 + r)

      list(A = A, B = B, C = C, r = r, AAC = aac)
    })

    output$aac_txt <- renderText({
      v <- vals()
      sprintf("AAC = %.2f", v$AAC)
    })

    output$abc_txt <- renderText({
      v <- vals()
      sprintf("A = %.3f,  B = %.3f,  C = %.2f", v$A, v$B, v$C)
    })

    output$abc_plot <- renderPlot({
      v <- vals()
      df <- data.frame(
        var = factor(c("A","B","C"), levels = c("A","B","C")),
        val = c(v$A, v$B, v$C)
      )

      ggplot(df, aes(x = var, y = val, group = 1)) +
        geom_line(linewidth = 1) +
        geom_point(size = 4) +
        scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5)) +
        labs(x = NULL, y = "Value", title = "A, B, C on x-axis; value on y-axis") +
        theme_minimal(base_size = 14)
    })

  })
}