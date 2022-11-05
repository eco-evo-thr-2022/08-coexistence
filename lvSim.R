# birth
lambda <- 4

# intrinsic death rates
mu10 <- 0.2
mu20 <- 0.4

# intra-specific competition
mu11 <- 0.04
mu22 <- 0.06

# inter-specific competition
mu12 <- 0.002
mu21 <- 0.003


# initial population sizes
n <- c(10, 10)

# a matrix to hold populations sizes through time (rows are time points)
ntstep <- 5000 # number of time steps
nmat <- matrix(0, nrow = ntstep, ncol = 2)
nmat[1, ] <- n # record the initial conditions

# here's the for loop, so called cause we use the word "for"
for(i in 2:ntstep) {
    # everything could going extinct, so this is a safety precaution
    # it will stop the loop if everything is dead
    if(all(n <= 0)) break

    # birth or death?
    # death probabilities
    dprob1 <- n[1] * (mu10 + mu11 * n[1] + mu12 * n[2])
    dprob2 <- n[2] * (mu20 + mu22 * n[2] + mu21 * n[1])

    # birth probabilities
    bprob1 <- lambda * n[1]
    bprob2 <- lambda * n[2]

    # `event = 0` is death and `event = 1` is birth
    event <- sample(0:1, size = 1, prob = c(dprob1 + dprob2, bprob1 + bprob2))

    if(event == 0) { # someone dies
        # who dies?
        thisOneDead <- sample(1:2, size = 1, prob = c(dprob1, dprob2))

        # update populations
        n[thisOneDead] <- n[thisOneDead] - 1
    } else { # someone is born
        thisOneBorn <- sample(1:2, size = 1, prob = c(bprob1, bprob2))

        # update populations
        n[thisOneBorn] <- n[thisOneBorn] + 1
    }

    nmat[i, ] <- n
}


plot(1:ntstep, nmat[, 1], type = 'l', ylim = c(0, max(nmat)), col = 'blue',
     xlab = 'Time steps', ylab = 'n')
lines(1:ntstep, nmat[, 2], col = 'orange')
