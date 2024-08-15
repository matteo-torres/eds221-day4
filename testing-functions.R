# nested for-loop
file_prefix <- c("temp", "ph", "salinity")
file_suffix <- c(1, 2, 3, 4)

for (i in 1:length(file_prefix)) {
  for (j in 1:length(file_suffix)) {
    print(paste0(file_prefix[i],"_", file_suffix[j]))
  }
}

birddog_sum <-  function(bird, dog) {
  pets <- bird + dog
  return(pets)
}

x <- birddog_sum(bird = 2, dog = 5)
x

double_it <- function(x) {
  print(2 * x)
}

double_it(4)
double_it(1:4)

exclaim_age <- function(age) {
  print(paste("I am", age, "years old!"))
}
exclaim_age(age = 10)

find_max <- function(val1, val2) {
  if (val1 > val2) {
    return(val1)
  } else if (val2 > val1) {
    return(val2)
  }
}

find_max(7, 3)

# code warm-up
quarter_splits <- c(1.0, 1.1, 1.2, 1.1, 1.4, 1.5, 1.6, 1.4)

for (i in 1:length(quarter_splits)) {
  print(quarter_splits[i] + quarter_splits[i+1])
}

animal_age <- function(animal, age) {
  if (!animal %in% c("dog", "goat")) {
    stop("Oopz! Animal must be a dog or goat")
  }
  if (is.numeric(age) == FALSE) {
    stop("The age must be a number greater than 0")
  }
  if (age > 0) {
    stop("Age must be greater than 0")
  }
  if (animal == "dog") {
  print(age * 7)
} else if (animal == "goat") {
  print (age *4.7)
  }
}

animal_age(animal = "dog", age = 2)
animal_age(animal = "cow", age = 2)
animal_age(animal = "dog", age = "yellow")


dog_choice <- data.frame(dog_name = c("Khora", "Teddy", "Waffle", "Banjo"), 
                         food = c("everything", "salmon", "pancakes", "chicken"))

library(tidyverse)

dog_menu <- function(name) {
  my_sub <- dog_choice %>%
  filter(dog_name == name)
  print(paste("My name is", my_sub$dog_name, "and I like to eat", my_sub$food))
}

dog_menu("Khora")
dog_menu("Waffle")

calc_windpower <- function(rho, radius, windspeed) {
  if (windspeed > 130) {
    warning("wow, that's fasyt! are you sure?")
  }
  if (rho > 1.225) {
    warning("that air density is suspicious. are you sure?")
  }
  if (radius < 0) {
    stop("rotor radius must be a positive value (meters)")
  }
  print(0.3 * rho * pi * (radius^2) * (windspeed^3))
}

calc_windpower(roh = 2, radius = -1, windspeed = 50)
