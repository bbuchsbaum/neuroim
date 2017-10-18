



create_overlay <- function(...) {
  
  vlist <- list(...)
  
  axial_overlay <- do.call(Overlay$new, lapply(vlist, Layer$new, view="LPI"))
  coronal_overlay <- do.call(Overlay$new, lapply(vlist, Layer$new, view="LIP"))
  sagittal_overlay <- do.call(Overlay$new, lapply(vlist, Layer$new, view="AIL"))
  
  gen_el <- function(overlay) {
    vspace <- overlay$view_space
    range <- bounds(vspace)[3,]
    list(
      overlay=overlay,
      zrange=range,
      start_slice= median(seq(range[1], range[2])),
      vrange=c(1, overlay$zdim())
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
  
  height <- 300
  #width <- "100%"
  gen_slice_box <- function(title, id, view, sid) {
    box(title, plotOutput(id, height = height, click = "plot_click"), 
        sliderInput(sid, "Slice:", 
                    view$vrange[1],
                    view$vrange[2],
                    median(round(c(view$vrange[1], 
                                   view$vrange[2])))), width=4, 
        solidHeader=TRUE, status="primary", background="black", align="center")

  }
  
  gen_background_panel <- function() {
    menuItem("Background",icon=icon("adjust"),
             sliderInput("background_range", "Intensity Range", min=0, max=200, value=c(-100,100)),
             sliderInput("background_threshold", "Threshold", min=0, max=200, value=c(-100,100)))
                 
  }
  
  gen_foreground_panel <- function() {
    menuItem("Foreground",icon=icon("adjust"),
             sliderInput("foreground_range", "Intensity Range", min=0, max=200, value=c(-100,100)),
             sliderInput("foreground_threshold", "Threshold", min=0, max=200, value=c(-100,100)),
             plotOutput("foreground_colorbar", width="100%", height=50))
  }
   
  body <- dashboardBody(
    fluidRow(
      gen_slice_box("Axial", "axial_plot", overlay_set$axial, "ax_slider"),
      gen_slice_box("Coronal", "coronal_plot", overlay_set$coronal, "cor_slider"),
      gen_slice_box("Sagittal", "sagittal_plot", overlay_set$sagittal, "sag_slider")
    )
  )


  # We'll save it in a variable `ui` so that we can preview it in the console
  ui <- dashboardPage(title = "Orthogonal Slice View",
    dashboardHeader(),
    dashboardSidebar(
      sidebarMenu(
        selectInput("layer_selection", "Overlay Image: ", overlay_set$axial$overlay$names(), selected=overlay_set$axial$overlay$names()[1])
      )),
    body
  )
  
  
  server <- function(input, output, session) {

    axial_slice <- reactiveVal()
    coronal_slice <- reactiveVal()
    sagittal_slice <- reactiveVal()
    
    observeEvent(input$plot_click, {
      message("click x", input$plot_click$x)
      message("click y", input$plot_click$y)
      #print(mean(axial_slice()@slices[[1]]))
    })
    
    
    gen_render_plot <- function(view, slider_id, rval, plot_id) {
      vspace=view$overlay$view_space
      
      renderPlot({
        width <- session$clientData[[paste0("output_", plot_id, "_width")]]
        height <- session$clientData[[paste0("output_", plot_id, "_height")]]
        
        print(width)
        print(height)
        ind <- input[[slider_id]]
        print(paste("index: ", ind))
        zpos <- view$overlay$layers[[1]]$get_zpos(ind)
        print(paste("zpos: ", zpos))
        slice <- view$overlay$render_slice(zpos, width, height)
        rval(slice)

        grid.draw(slice@grob)
      }, height = function() {
          if (plot_id == "coronal_plot") {
            .6 * session$clientData[[paste0("output_", plot_id, "_width")]]
          } else {
            session$clientData[[paste0("output_", plot_id, "_width")]]
          }
          
  
        slice$draw()
      })
    }
      
    output$axial_plot <- gen_render_plot(overlay_set$axial, "ax_slider", axial_slice, "axial_plot")
    output$coronal_plot <- gen_render_plot(overlay_set$coronal, "cor_slider", coronal_slice, "coronal_plot")
    output$sagittal_plot <- gen_render_plot(overlay_set$sagittal, "sag_slider", sagittal_slice, "sagittal_plot")

    
    output$foreground_colorbar <- renderPlot({
      width <- session$clientData[[paste0("output_foreground_colorbar_width")]]
      height <- session$clientData[[paste0("output_foreground_colorbar_height")]]
      color_bar(rainbow(25), width, height)
    })

  }
  
  # Preview the UI in the console
  shinyApp(ui = ui, server = server)
}

# Function to plot color bar
color_bar <- function(lut, width=100, height=20) {
  print("color bar")
  print(width)
  print(height)
  
  #outfile=tempfile(fileext='.png')
  #png(outfile, width=width, height=height)
  grid.newpage()
  grid.rect(x=0, y=0, width=unit(width, "points"), height=unit(height, "points"), gp=gpar(fill="black"))
  
  strip_w <- width/(length(lut)-1)
  
  for (i in 1:length(lut)) {
     grid.rect(x= unit((i-1)*strip_w, "points"), y=unit(.5,"npc"), width=unit(strip_w, "points"), height=unit(1, "npc"), gp=gpar(fill=lut[i]))
  }
  
  #dev.off()
  
  #list(src=outfile, alt="color bar")
  
}

