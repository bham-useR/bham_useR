# libharu hpdf example with V8
(function(){

# run these hpdf.js examples
#   http://manuels.github.io/hpdf.js/
# in R with V8

library(V8)
library(pipeR)
library(htmltools)

ct = new_context("window")
ct$source("https://raw.githubusercontent.com/manuels/hpdf.js/master/hpdf.min.js")

# hpdf needs a base64 encode like windows.btoa
#   we'll use this implementation
# https://code.google.com/p/stringencoders/source/browse/trunk/javascript/base64.js?r=230
ct$source(
  "https://stringencoders.googlecode.com/svn-history/r230/trunk/javascript/base64.js"
)
# use the base64 encode and make it windows.btoa
ct$eval(
  "window.btoa = base64.encode"
)
ct$eval("
        function print_grid(pdf, page) {
        var height = page.height();
        var width = page.width();
        var font = pdf.font('Helvetica');
        
        page.setFontAndSize(font, 5);
        page.setGrayFill(0.5);
        page.setGrayStroke(0.8);
        
        /* Draw horizontal lines */
        var y = 0;
        while (y < height) {
        if (y % 10 == 0)
        page.setLineWidth(0.5);
        else {
        if (page.getLineWidth() != 0.25)
        page.setLineWidth(0.25);
        }
        
        page.moveTo(0, y);
        page.lineTo(width, y);
        page.stroke();
        
        if (y % 10 == 0 && y > 0) {
        page.setGrayStroke(0.5);
        
        page.moveTo(0, y);
        page.lineTo(5, y);
        page.stroke();
        
        page.setGrayStroke(0.8);
        }
        
        y += 5;
        }
        
        
        /* Draw virtical lines */
        var x = 0;
        while (x < width) {
        if (x % 10 == 0)
        page.setLineWidth(0.5);
        else {
        if (page.setLineWidth() != 0.25)
        page.setLineWidth(0.25);
        }
        
        page.moveTo(x, 0);
        page.lineTo(x, height);
        page.stroke()
        
        if (x % 50 == 0 && x > 0) {
        page.setGrayStroke(0.5);
        
        page.moveTo(x, 0);
        page.lineTo(x, 5);
        page.stroke();
        
        page.moveTo(x, height);
        page.lineTo(x, height - 5);
        page.stroke();
        
        page.setGrayStroke(0.8);
        }
        
        x += 5;
        }
        
        /* Draw horizontal text */
        y = 0;
        while (y < height) {
        if (y % 10 == 0 && y > 0) {
        page.beginText();
        page.moveTextPos(5, y - 2);
        page.showText(y.toString());
        page.endText();
        }
        
        y += 5;
        }
        
        
        /* Draw virtical text */
        x = 0;
        while (x < width) {
        if (x % 50 == 0 && x > 0) {
        page.beginText();
        page.moveTextPos(x, 5);
        page.showText(x.toString());
        page.endText();
        
        page.beginText();
        page.moveTextPos(x, height - 10);
        page.showText(x.toString());
        page.endText();
        }
        
        x += 5;
        }
        
        page.setGrayFill(0);
        page.setGrayStroke(0);
        }
        
        var pdf = new HPDF();
        
        /* add a new page object. */
        var page = pdf.addPage();
        
        page.setHeight(220);
        page.setWidth(200);
        
        /* draw grid to the page */
        print_grid(pdf, page);
        
        /* draw pie chart
        *
        *   A: 45% Red
        *   B: 25% Blue
        *   C: 15% green
        *   D: other yellow
        */
        
        /* A */
        page.setRGBFill(1.0, 0, 0);
        page.moveTo(100, 100);
        page.lineTo(100, 180);
        page.arc(100, 100, 80, 0, 360 * 0.45);
        pos = page.currentPos();
        page.lineTo(100, 100);
        page.fill();
        
        /* B */
        page.setRGBFill(0, 0, 1.0);
        page.moveTo(100, 100);
        page.lineTo(pos.x, pos.y);
        page.arc(100, 100, 80, 360 * 0.45, 360 * 0.7);
        pos = page.currentPos();
        page.lineTo(100, 100);
        page.fill();
        
        /* C */
        page.setRGBFill(0, 1.0, 0);
        page.moveTo(100, 100);
        page.lineTo(pos.x, pos.y);
        page.arc(100, 100, 80, 360 * 0.7, 360 * 0.85);
        pos = page.currentPos();
        page.lineTo(100, 100);
        page.fill();
        
        /* D */
        page.setRGBFill(1.0, 1.0, 0);
        page.moveTo(100, 100);
        page.lineTo(pos.x, pos.y);
        page.arc(100, 100, 80, 360 * 0.85, 360);
        pos = page.currentPos();
        page.lineTo(100, 100);
        page.fill();
        
        /* draw center circle */
        page.setGrayStroke(0);
        page.setGrayFill(1);
        page.circle(100, 100, 30);
        page.fill();
        
        
        pdf.toDataUri()
        ")  %>>%
  # can't figure out how to pass base64 to R browseURL
  # so use iframe instead
  (html_print(
    tags$iframe(src = ., style = "border: 0; position:fixed; top:0; left:0; right:0; bottom:0; width:100%; height:100%")
    # RStudio Viewer doesn't show the pdf
    ,viewer = utils::browseURL
  ) )
})()

# epiwidgets with dendrogram
(function(){
  library(epiwidgets)
  treewidget(
    ape::as.phylo(hclust(dist(USArrests), "ave"))
  )
})()

# parallel coordinates example
(function(){
  library(parcoords)
  library(dplyr)
  data(diamonds,package = "ggplot2")
  diamonds[sample(1:nrow(diamonds),5000),] %>%
    mutate( carat = cut(carat, breaks=c(0,1,2,3,4,5), right = T)) %>%
    select( carat, color, cut, clarity, depth, table, price,  x, y, z) %>%
    parcoords(
      rownames = F # turn off rownames from the data.frame
      , brushMode = "2D-strums"
      , reorderable = T
      , queue = T
      , color = list(
        colorBy = "cut"
        ,colorScale = htmlwidgets::JS("d3.scale.category10()")
      )    
    )
})()