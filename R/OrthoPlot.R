



create_overlay <- function(...) {
  
  vlist <- list(...)
  
  axial_overlay <- do.call(Overlay, lapply(vlist, Layer, view="LPI"))
  coronal_overlay <- do.call(Overlay, lapply(vlist, Layer, view="LIP"))
  sagittal_overlay <- do.call(Overlay, lapply(vlist, Layer, view="AIL"))
  
  gen_el <- function(overlay) {
    vspace <- overlay@layers[[1]]@view_space
    range <- bounds(vspace)[3,]
    list(
      overlay=overlay,
      zrange=range,
      start_slice= median(seq(range[1], range[2])),
      vrange=c(1, dim_of(overlay@uspace, vspace))
    )
      
  }
  
  list(
    axial=gen_el(axial_overlay),
    coronal=gen_el(coronal_overlay),
    sagittal=gen_el(sagittal_overlay)
  )
}

ortho_plot <- function(...) {
  overlay_set <- create_overlay(...)
  
  height <- 256
  
  gen_slice_box <- function(title, id, view, sid) {
    box(title, plotOutput(id, height = height, click = "plot_click"), 
        sliderInput(sid, "Slice:", 
                    view$vrange[1],
                    view$vrange[2],
                    median(round(c(view$vrange[1], 
                                   view$vrange[2])))), width=4, 
        solidHeader=TRUE, status="primary")
  }
   
  body <- dashboardBody(
    fluidRow(
      gen_slice_box("Axial", "axial_plot", overlay_set$axial, "ax_slider"),
      gen_slice_box("Coronal", "coronal_plot", overlay_set$coronal, "cor_slider"),
      gen_slice_box("Sagittal", "sagittal_plot", overlay_set$sagittal, "sag_slider")
    )
  )

  
  # We'll save it in a variable `ui` so that we can preview it in the console
  ui <- dashboardPage(
    dashboardHeader(title = "Column layout"),
    dashboardSidebar(),
    body
  )
  
  
  server <- function(input, output) {
    
    axial_slice <- reactiveVal()
    coronal_slice <- reactiveVal()
    sagittal_slice <- reactiveVal()
    
    observeEvent(input$plot_click, {
      message("click x", input$plot_click$x)
      message("click y", input$plot_click$y)
      print(mean(axial_slice()@slices[[1]]))
    })
    
    
    gen_render_plot <- function(view, slider_id, rval) {
      vspace=view$overlay@layers[[1]]@view_space
      
      renderPlot({
        ind <- input[[slider_id]]
        zpos <- indexToAxis(vspace, ind, 3)
        slice <- renderSlice(view$overlay, zpos, 1,1, units="npc")
        rval(slice)
        grid.draw(slice@grob)
      })
    }
      
    output$axial_plot <- gen_render_plot(overlay_set$axial, "ax_slider", axial_slice)
    output$coronal_plot <- gen_render_plot(overlay_set$coronal, "cor_slider", coronal_slice)
    output$sagittal_plot <- gen_render_plot(overlay_set$sagittal, "sag_slider", sagittal_slice)
  }
  
  # Preview the UI in the console
  shinyApp(ui = ui, server = server)
}

# Function to plot color bar
# color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
#   scale = (length(lut)-1)/(max-min)
#   
#   dev.new(width=1.75, height=5)
#   plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
#   axis(2, ticks, las=1)
#   for (i in 1:(length(lut)-1)) {
#     y = (i-1)/scale + min
#     rect(0,y,10,y+1/scale, col=lut[i], border=NA)
#   }
# }
