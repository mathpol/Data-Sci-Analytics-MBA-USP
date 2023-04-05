inter <- 4995.16
beta1 <- 947.23
idade <- 40
lambda <- 2.659051

cm <- inter + beta1*40
cm

glue::glue('A criança com 40 semanas possui, em média, {round((((cm*lambda)+1)^(1/lambda)),0)} centíumetros.')
