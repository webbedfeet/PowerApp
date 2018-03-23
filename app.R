library(shiny)
suppressPackageStartupMessages(library(dplyr))
library(purrr)
library(pwr)
library(TrialSize)
library(cowplot)
load('data/rda/finaldat.rda')

sd_das <- sd(dat$dasesrdiff, na.rm=T)
sd_sdai <- sd(dat$sdaidiff, na.rm=T)
prop_test <- function(n, p0, pwr) power.prop.test(n=n, p1=p0, power=pwr, 
                                                  alternative = 'two.sided')$p2

find_margin_mean <- function(x, N, stdev, alpha=0.05, pwr=0.8, delta=0 ){
  n <- TwoSampleMean.NIS(alpha=alpha, beta = 1 - pwr, sigma = stdev, k = 1, 
                         delta = delta, margin = x)
  return(N - n)
}

find_margin_prop <- function(x, N, p1, alpha = 0.05, pwr = 0.8, delta=0 ){
  n <- TwoSampleProportion.NIS(alpha = alpha, beta = 1 - pwr,
                               p1 = p1, p2 = p1, k = 1, delta = delta, 
                               margin = x)
  return(N - n)
}


server <- function(input, output, session){
  output$ssize <- renderTable({
    n_vec <- seq(input$n[1], input$n[2], by = 10)
     pwr <- as.numeric(input$power)
     tab1 <- data.frame(N = n_vec, power = pwr)
     tab1 <- tab1 %>% 
       mutate(eff_size = map2_dbl(N, power, ~pwr.t.test(n=.x, power=.y, 
                                                        type='two.sample',
                                                        alternative='two.sided')$d),
              delta_das = eff_size * as.numeric(input$sd_das),
              delta_sdai = eff_size * as.numeric(input$sd_sdai),
              p0_acr20 = input$p0_20, 
              p0_acr50 = input$p0_50) %>% 
       mutate(p1_acr20 = pmap_dbl(list(N, p0_acr20,power), prop_test),
              p1_acr50 = pmap_dbl(list(N, p0_acr50, power), prop_test)) %>% 
       mutate(delta_acr20 = p1_acr20 - p0_acr20, delta_acr50 = p1_acr50 - p0_acr50) %>%
       select(N,  delta_das:p0_acr20, p1_acr20,   p0_acr50, p1_acr50) %>% 
       round(digits = 2) %>% 
       setNames(c('N', 'DAS','SDAI','ACR20 p0', 'ACR20 p1', 'ACR50 p0', 'ACR50 p1')) %>% 
       mutate(N = as.integer(N))
     tab1
    })
  
  output$noninf <- renderTable({
    tab2 <- tibble(N = seq(input$n[1], input$n[2], by = 10),
                   pwr = as.numeric(input$power)) %>% 
      mutate(margin_ACR20 = map2_dbl(N, pwr, 
                                     ~uniroot(find_margin_prop, c(-0.5,-0.005), N=.x, 
                                              p1 = 0.5, pwr=.y)$root),
             margin_ACR50 = map2_dbl(N, pwr,
                                     ~uniroot(find_margin_prop, c(-0.5, -0.005),
                                              N = .x, p1 = 0.2, pwr=.y)$root),
             margin_DAS = map2_dbl(N, pwr,
                                   ~uniroot(find_margin_mean, c(-2, -0.005),
                                            N = .x, stdev=as.numeric(input$sd_das), pwr=.y)$root),
             margin_SDAI = map2_dbl(N, pwr,
                                    ~uniroot(find_margin_mean, c(-20,-0.005),
                                             N = .x, stdev=as.numeric(input$sd_sdai), pwr=.y)$root),
             N = as.integer(N)) %>% 
      select(-pwr) %>% 
      setNames(c('N','ACR20','ACR50','DAS','SDAI'))
    tab2
  })
  
  output$fig1 <- renderPlot({
    Nseq <- seq(input$n[1], input$n[2], by = 10)
    d_plot <- tibble(N = rep(Nseq, 4),
                     p0 = rep(c(0.4,0.5, 0.6, 0.7), rep(length(Nseq), 4)),
                     pwr = as.numeric(input$power)) %>% 
      mutate(eff_size = map2_dbl(N, pwr, ~pwr.t.test(n=.x, power=.y,
                                                     type='two.sample',
                                                     alternative='two.sided')$d),
             p1 = map2_dbl(N, p0,
                           ~power.prop.test(n=.x, p1=.y, power=0.8,
                                            alternative='two.sided')$p2),
             Delta = p1-p0,
             Delta_DAS = eff_size*as.numeric(input$sd_das),
             Delta_SDAI = eff_size*as.numeric(input$sd_sdai))
    
    plt_das <- ggplot(d_plot, aes(x=Delta_DAS, y = Delta, group=factor(p0), color=factor(p0)))+
      geom_line()+
      scale_x_continuous(expression(Delta[DAS]))+
      scale_y_continuous(expression(Delta[ACR20]))+
      labs(color=expression(p[0]))
    
    legend_b <- get_legend(plt_das+theme(legend.position='bottom', 
                                         legend.text= element_text(size=10),
                                         legend.title = element_text(size=11)))
    
    plt_sdai <- ggplot(d_plot, aes(x=Delta_SDAI, y = Delta, group=factor(p0), color=factor(p0)))+
      geom_line()+
      scale_x_continuous(expression(Delta[SDAI]))+
      scale_y_continuous(expression(Delta[ACR20]))+
      labs(color=expression(p[0]))
    
    p <- plot_grid(plt_das+theme(legend.position='none'),
                   plt_sdai+theme(legend.position='none'),
                   rel_widths = c(1,1),
                   align='h', 
                   labels = c('A','B'), 
                   hjust = -1,
                   nrow=1)
    p1 <- plot_grid(p, legend_b, ncol=1, rel_heights = c(1,.2))
    print(p1)
    
  })
  output$about <- renderText({"The data used to generate this app is from Dasgupta & Ward (2017). In particular, the app is pre-filled
  with empirical values from the dataset reported in the paper. This app allows the user to vary quantities based on their 
  own datasets to understand the correspondences between the four RA response measures reported here."})
 
}

ui <- pageWithSidebar(
  headerPanel("Equivalence of different RA responses"),
  sidebarPanel(
    sliderInput("n", "N", min = 10, max = 200, 
                value = c(50,100), step = 10),
    radioButtons('power', "Power", choiceNames = paste0(c(80,90), '%'), choiceValues = c(0.8, 0.9)),
    HTML('<hr>'),
    sliderInput('p0_20', 'Baseline ACR20 %', value=0.5, min = 0.05, max = 0.95, step=0.05),
    sliderInput('p0_50', 'Baseline ACR50 %', value = 0.2, min = 0.05, max = 0.95, step = 0.05),
    HTML('<hr>'),
    textInput('sd_das', 'DAS Std Dev', value = sprintf('%.2f', sd_das)),
    textInput('sd_sdai', 'SDAI Std Dev', value = sprintf('%.2f', sd_sdai)),
    HTML('<p style="position: fixed; bottom: 0; width:30%; text-align: justify">
This work was supported by the Intramural Research Program of the National Institute of Arthritis,
         Musculoskeletal and Skin Diseases, National Institutes of Health, Bethesda, Maryland. This work is in the public domain.
         </p>')
    
  ),
  mainPanel(
    tabsetPanel(id = 'test',
                tabPanel('Superiority', h4("Detectable differences"),
                         # textOutput('test')#,
                         tableOutput('ssize'),
                         h4('Equivalent differences'),
                         plotOutput('fig1')),
                
                tabPanel('Non-inferiority',
                         h4("Required non-inferiority margins when there is truly no difference"),
                         tableOutput('noninf')),
                
                tabPanel('About', h4('Basis for this app'),
                         textOutput('about')
                         )
    )
)
)
shinyApp(ui = ui, server = server)