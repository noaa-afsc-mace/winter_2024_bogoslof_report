# generate a simulated fish population
lengths <- round(rnorm(mean = 30, sd =3, n = 100000), digits = 0)
weights <- lengths^3 * 0.01
numbers <- sample(c(0, 3e3), size = 100000, replace = TRUE)

# plot it
plot_biomass_nums_by_length(length_vector = lengths,
                            biomass_vector = weights,
                            numbers_vector = numbers)

# plot it with totals added as text
plot_biomass_nums_by_length(length_vector = lengths,
                            biomass_vector = weights,
                            numbers_vector = numbers,
                            add_totals = TRUE)
