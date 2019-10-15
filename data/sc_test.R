# loop through all seasons of steph curry data

test <- Map(bayes_beta, sc$X3P_cumsum, sc$X3PA_cumsum - sc$X3P_cumsum, sc$X3P, sc$X3PA, sc$Season)

test <- do.call(rbind, test)

test_plot <- plot_bayes_animate(test)
