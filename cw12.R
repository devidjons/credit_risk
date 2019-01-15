library(dplyr)
r = data.frame(
    t = c(0.0833, 0.25, 1, 2, 3, 5, 10),
    R = 0.01 * c(5.56, 5.64, 5.68, 5.64, 5.62, 5.66, 5.76)
)
r %>% mutate(B_0_T = 1 * exp(-R * t)) %>%mutate(phi_t=log(B_0_T)/-t) %>% select(t, B_0_T,phi_t) -> data_for_modelling
opt=function(x)
{
    a=x[1]
    b=x[2]
    c=x[3]
    rho=x[4]
    sum(
        (
            a +
            (b + c) * (1 - exp(-rho * data_for_modelling$t)) / (rho * data_for_modelling$t) -
            c * exp(-rho * data_for_modelling$t) -
            data_for_modelling$B_0_T
        )
        ^ 2)
}
answer=nlm(opt, c(1,1,1,0.5))$estimate
