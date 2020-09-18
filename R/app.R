#' plot mobility traces over time for a single nation
#' @importFrom magrittr %>%
#' @importFrom dplyr filter summarize pull
#' @importFrom glue glue
#' @import ggplot2
#' @importFrom vroom vroom
#' @param cntry character(1) 
#' @param datasource either character(1) in which case content of file named should be ingestible to
#' data.frame via vroom, or a data.frame-like object
#' @examples
#' limdat = get_mob_sel()
#' plot_one(cntry = "India", datasource=limdat)
#' @export
plot_one = function(cntry = "Italy", datasource = system.file("csv/mobsel_aug15.csv.gz", package="mobpack")) {
 #lks2p = requireNamespace("sars2pack") # only if need google_mobility_data
 #if (!lks2p) stop("install seandavi/sars2pack to use this function")
 if (is(datasource, "character")) mob_data = vroom(datasource)
 else if (inherits(datasource, "data.frame")) mob_data = datasource
 else stop("datasource must be either string or data.frame")  # not great!
 stopifnot (cntry %in% mob_data$country_region)
 mobility_data_one <- mob_data %>%
             filter(country_region == cntry, admin_level == 0)

 maxdate <- mobility_data_one %>% summarize(max(date) ) %>% pull()

 ggplot(data=mobility_data_one) +
            geom_line(mapping=aes(x=date,y=percent_change_from_baseline, color=places_category, linetype=places_category) ) +
            labs(title=paste0("Google mobility metric for ", cntry),
                subtitle=glue("Data as of {mdate}.", mdate = as.Date(maxdate), .sep =" " ) )
 }

#' get pre-packaged data on mobility
#' @export
get_mob_sel = function() 
  vroom(system.file("csv/mob_sel_aug15_ad0.csv.gz", package="mobpack"))

#' shiny app for mobility visualization
#' @import shiny
#' @param mobdata a data.frame-like object with country_region and mobility measure data as used by plot_one
#' @export
mobility_app = function(mobdata) {
 nats = sort(unique(mobdata$country_region))
 ui = fluidPage(
  sidebarLayout(
   sidebarPanel(
    helpText("Mobility display"),
    selectInput("nat", "Country", choices=nats),
    width=2),
   mainPanel(
    plotOutput("one")
   )
  )
 ) # end fluidPage
 server = function(input, output) {
  output$one = renderPlot(
   plot_one(input$nat, mobdata)
  )
 }
 runApp(list(ui=ui, server=server))
}
