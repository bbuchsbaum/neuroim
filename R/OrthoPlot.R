
## TODO
## proper selection of foreground layer
# proper color bar (looks ugly)
## cross hair
# layout options ('triangle', '1row')
## reusable components
## break out into neuroviz package

wrap_slider <- function(...) {
  div(style = "height: 85px; padding: 0px 0px; font-size: 12px;",  
      sliderInput(...))
}

background_panel <- function(vol) {
  maxval <- signif(max(vol),3)
  minval <- signif(min(vol),3)
  
  menuItem("Background",icon=icon("adjust"), startExpanded=TRUE,
           wrap_slider("background_range", "Intensity Range", ticks=FALSE, 
                       min=minval, max=maxval, value=c(minval, maxval)),
           div(style="display: inline-block;vertical-align:top; width: 130px;",
               selectInput("background_col", "Color Map:",color_map$get_map_names(), "grayscale")),
           div(style="display: inline-block;vertical-align:top; width: 100px;",
               numericInput(inputId="background_col_size", label="Size: ", value = 256, min=2, max=256))
         #plotOutput("background_colorbar", width="100%", height=50)
           
  )
}

foreground_panel <- function(vol) {
    maxval <- signif(max(vol),3)
    minval <- signif(min(vol),3)
    
    menuItem("Foreground",icon=icon("adjust"),
             
             wrap_slider(inputId="foreground_range", label="Intensity Range", ticks=FALSE, 
                         min=minval, max=maxval, value=c(minval, maxval)),
             wrap_slider(inputId="foreground_threshold", label="Threshold", ticks=FALSE, 
                         min=minval, max=maxval, value=c((minval+maxval)/2, (minval+maxval)/2)),
             wrap_slider(inputId="foreground_opacity", label="Opacity", ticks=FALSE, 
                         min=0, max=1, value=1),
             div(style="display: inline-block;vertical-align:top; width: 130px;",
                 selectInput("foreground_col", "Color Map:",color_map$get_map_names(), "rainbow")),
             div(style="display: inline-block;vertical-align:top; width: 100px;",
                 numericInput(inputId="foreground_col_size", label="Size: ", value = 20, min=2, max=256)))
}



slice_box <- function(title, id, slice_range, sid) {
  box(title, plotOutput(id, height=height, click = "plot_click"), 
      sliderInput(sid, "Slice:", 
                  slice_range[1],
                  slice_range[2],
                  ticks=FALSE,
                  median(round(c(slice_range[1], 
                                 slice_range[2])))), width=4, 
      
      solidHeader=TRUE, status="primary", background="black", align="center")
  
}


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

ortho_plot <- function(..., height=300) {
  
  overlay_set <- create_overlay(...)
  axial_overlay <- overlay_set$axial$overlay
  
  color_map <- ColorMaps$new()

  gen_layer_selection <- function() {
    if (axial_overlay$length() > 2) {
      selectInput("layer_selection", "Overlay Image: ", axial_overlay$names(), 
                  selected=overlay_set$axial$overlay$names()[1])
    } 
  }
  
  body <- dashboardBody(
    fluidRow(
      slice_box("Axial", "axial_plot", overlay_set$axial, view$vrange, "ax_slider"),
      slice_box("Coronal", "coronal_plot", overlay_set$coronal, view$vrange, "cor_slider"),
      slice_box("Sagittal", "sagittal_plot", overlay_set$sagittal, view$vrange, "sag_slider")
    )
  )
  
  ui <- dashboardPage(
    dashboardHeader(title = "Ortho Plot"),
    dashboardSidebar(
      sidebarMenu(
        background_panel(axial_overlay$layers[[1]]$vol),
        if (length(axial_overlay$layers) > 1) gen_foreground_panel()  else NULL,
        gen_layer_selection()
      )
    ),
    body
  )
  
  
  server <- function(input, output, session) {

    axial_slice <- reactiveVal()
    coronal_slice <- reactiveVal()
    sagittal_slice <- reactiveVal()
    
    observeEvent(input$plot_click, {
      message("click x", input$plot_click$x)
      message("click y", input$plot_click$y)
     
    })
    
    gen_render_plot <- function(view, slider_id, rval, plot_id) {
      vspace=view$overlay$view_space
      
      renderPlot({
      
        width <- session$clientData[[paste0("output_", plot_id, "_width")]]
        height <- session$clientData[[paste0("output_", plot_id, "_height")]]
        
        ind <- input[[slider_id]]
       
        zpos <- view$overlay$get_zpos(ind)
        
        view$overlay$set_irange(1, input[["background_range"]])
        csize1 <- input[["background_col_size"]]
        cols1 <- color_map$get_colors(input[["background_col"]], as.numeric(csize1))
        view$overlay$set_color_map(1, cols1)
        
        if (view$overlay$length() > 1) {
        
          view$overlay$set_irange(2, input[["foreground_range"]])
          view$overlay$set_threshold(2, input[["foreground_threshold"]])
        
          csize2 <- input[["foreground_col_size"]]
          cols2 <- color_map$get_colors(input[["foreground_col"]], as.numeric(csize2))
  
          view$overlay$set_color_map(2, cols2)
          view$overlay$set_alpha(2, input[["foreground_opacity"]])
        }
        
        slice <- view$overlay$render_slice(zpos, 1:view$overlay$length(), width, height)
        rval(slice)
        slice$draw()
      })
    }
      
    output$axial_plot <- gen_render_plot(overlay_set$axial, "ax_slider", axial_slice, "axial_plot")
    output$coronal_plot <- gen_render_plot(overlay_set$coronal, "cor_slider", coronal_slice, "coronal_plot")
    output$sagittal_plot <- gen_render_plot(overlay_set$sagittal, "sag_slider", sagittal_slice, "sagittal_plot")

  }
  
  # Preview the UI in the console
  shinyApp(ui = ui, server = server)
}

# Function to plot color bar
color_bar <- function(lut, xrange=c(0,100)) {
  
  par(bg="black")
  par(mar = c(0,0,0,0))
  plot.new()
  plot.window(xlim=xrange, ylim=c(0,1))
  #axis(1, seq(xrange[1],xrange[2], by=.5), col="white", col.axis="white")
  
  strip_w <- (xrange[2] - xrange[1])/length(lut) 
  
  for (i in 1:length(lut)) {
    xmin <- xrange[1] + ((i-1) * strip_w)
    rect(xleft=xmin, ybottom=0, ytop=1, xright=xmin+strip_w, col=lut[i], border=lut[i])
     #grid.rect(x= unit((i-1)*strip_w, "points"), y=unit(.5,"npc"), width=unit(strip_w, "points"), height=unit(1, "npc"), gp=gpar(fill=lut[i]))
  }
  
  #dev.off()
  #list(src=outfile, alt="color bar")
  
}

# output$foreground_colorbar <- renderPlot({
#   width <- session$clientData[[paste0("output_foreground_colorbar_width")]]
#   height <- session$clientData[[paste0("output_foreground_colorbar_height")]]
#   color_bar(rainbow(25), range(axial_overlay$layers[[2]]$vol))
# })
# 
# output$background_colorbar <- renderPlot({
#   width <- session$clientData[[paste0("output_background_colorbar_width")]]
#   height <- session$clientData[[paste0("output_background_colorbar_height")]]
#   color_bar(axial_overlay$layers[[1]]$get_color_map(), range(axial_overlay$layers[[1]]$vol))
# })

