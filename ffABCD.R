##############################################################
# My forcing equations
##############################################################

#parameteres herd A
tp.A <- 70 #day of parturition
Tg <- 150
tc.A <- 365 + tp - Tg #day conception
Tp <- Tc <- 8 #sd of day of parturition and conception

#ff.A
HF <- function(x) {
exp(dglogis(x, location = tp.A, scale = 4.987, shape = 1.125, log = TRUE) - pglogis(x, location = tp.A, scale = 4.987, shape = 1.125, log = TRUE, lower=FALSE))
}
curve(HF,from=0,to=100)

HF2 <- function(x) {
  exp(dglogis(x, location = tc.A, scale = 4.987, shape = 1.125, log = TRUE) - pglogis(x, location = tc.A, scale = 4.987, shape = 1.125, log = TRUE, lower=FALSE))
}
curve(HF2,from=0,to=365)

Qp.A <- function(t) {ifelse(mod(t,365,1) > (tp.A+40) | mod(t,365,1) < (tp.A-30) ,0,HF(mod(t,365,1)))}
# t <- 1:(365*2)
# plot(Qp.A(t))

Qc.A <- function(t) {ifelse(mod(t,365,1) > (tc.A+40) | mod(t,365,1) < (tc.A-30),0,HF2(mod(t,365,1)))}
# points(Qc(1:(365*2)))
##############################################################
#parameteres herd B
tp.B <- 170 #day of parturition
tc.B <- 365 + tp - Tg #day conception

#ff.B
HF <- function(x) {
  exp(dglogis(x, location = tp.B, scale = 4.987, shape = 1.125, log = TRUE) - pglogis(x, location = tp.B, scale = 4.987, shape = 1.125, log = TRUE, lower=FALSE))
}
curve(HF,from=0,to=100)

HF2 <- function(x) {
  exp(dglogis(x, location = tc.B, scale = 4.987, shape = 1.125, log = TRUE) - pglogis(x, location = tc.B, scale = 4.987, shape = 1.125, log = TRUE, lower=FALSE))
}
curve(HF2,from=0,to=365)

Qp.B <- function(t) {ifelse(mod(t,365,1) > (tp.B+40) | mod(t,365,1) < (tp.B-30) ,0,HF(mod(t,365,1)))}
# t <- 1:(365*2)
# plot(Qp.B(t))

Qc.B <- function(t) {ifelse(mod(t,365,1) > (tc.B+40) | mod(t,365,1) < (tc.B-30),0,HF2(mod(t,365,1)))}
# points(Qc(1:(365*2)))

##############################################################
#parameteres herd C
tp.C <- 243 #day of parturition
tc.C <- 365 + tp - Tg #day conception

#ff.C
HF <- function(x) {
  exp(dglogis(x, location = tp.C, scale = 4.987, shape = 1.125, log = TRUE) - pglogis(x, location = tp.C, scale = 4.987, shape = 1.125, log = TRUE, lower=FALSE))
}
curve(HF,from=0,to=100)

HF2 <- function(x) {
  exp(dglogis(x, location = tc.C, scale = 4.987, shape = 1.125, log = TRUE) - pglogis(x, location = tc.C, scale = 4.987, shape = 1.125, log = TRUE, lower=FALSE))
}
curve(HF2,from=0,to=365)

Qp.C <- function(t) {ifelse(mod(t,365,1) > (tp.C+40) | mod(t,365,1) < (tp.C-30) ,0,HF(mod(t,365,1)))}
# t <- 1:(365*2)
# plot(Qp.C(t))

Qc.C <- function(t) {ifelse(mod(t,365,1) > (tc.C+40) | mod(t,365,1) < (tc.C-30),0,HF2(mod(t,365,1)))}
# points(Qc(1:(365*2)))

##############################################################
#parameteres herd D
tp.D <- 323 #day of parturition
tc.D <- 365 + tp - Tg #day conception

#ff.D
HF <- function(x) {
  exp(dglogis(x, location = tp.D, scale = 4.987, shape = 1.125, log = TRUE) - pglogis(x, location = tp.D, scale = 4.987, shape = 1.125, log = TRUE, lower=FALSE))
}
curve(HF,from=0,to=100)

HF2 <- function(x) {
  exp(dglogis(x, location = tc.D, scale = 4.987, shape = 1.125, log = TRUE) - pglogis(x, location = tc.D, scale = 4.987, shape = 1.125, log = TRUE, lower=FALSE))
}
curve(HF2,from=0,to=365)

Qp.D <- function(t) {ifelse(mod(t,365,1) > (tp.D+40) | mod(t,365,1) < (tp.D-30) ,0,HF(mod(t,365,1)))}
# t <- 1:(365*2)
# plot(Qp.D(t))

Qc.D <- function(t) {ifelse(mod(t,365,1) > (tc.D+40) | mod(t,365,1) < (tc.D-30),0,HF2(mod(t,365,1)))}
# points(Qc(1:(365*2)))