library(shiny)
load("LIBD_Data.rda")
gene.list <- unique(libd_data$Symbol)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("LIBD Neural Induction Time-course"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectizeInput("Gene",
                           "Select Gene:",
                           choices= gene.list)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("expressionPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$expressionPlot <- renderPlot({
        gene_df    <- subset(libd_data, Symbol == input$Gene)
        gene_id    <- subset(gene_info, Symbol == input$Gene)
        
        ggplot(gene_df, aes(x=Day, y=Expression, colour=Donor, shape=Condition))+
            geom_point(aes(fill=Donor), colour="black", size=2)+
            stat_summary(data = gene_df[grepl("TIMECOURSE",gene_df$Experiment),],
                         mapping = aes(x = Day, y = Expression, group=Line),size=0.7,
                         fun=mean, geom="line")+
            labs(y="Expression (log2 RPKM+1)", x="Days in vitro")+
            scale_shape_manual(values=c(21,22,22,23,24,25))+
            theme_black+scale_fill_manual(values= cols_donor)+scale_color_manual(values= cols_donor)+
            ggtitle(paste("Gene:",input$Gene,"| Feature ID",gene_id[1,4],"| Entrez",gene_id[1,3]))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
