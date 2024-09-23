## add two numbers together and print the output to the console
2 + 2

## add two numbers together and store the result in a variable called "result"
result <- 2 + 2

## case sensitivity: add a different two numbers together and 
## store the result in a variable called "Result"
Result <- 10 - 5

## use the variables to do mathematics
result * Result

## put multiple different 'string' variables into a vector
presenter_names <- c("Huw", "Daniel", "Trevor")

## access the second entry of the vector and print it to the console
presenter_names[2]

## use some built-in functions (mean and round)
student_grades <- c(80, 39, 63, 93, 66, 51)     ## specify the grades for 6 imaginary students
mean(student_grades)                            ## calculate the mean student grade
round(3.1415)                                   ## round pi to the nearest integer
round(3.1415, digits = 2)                       ## round pi to two decimal places
round(mean(student_grades), digits = 1)         ## calculate the mean student grade to one decimal place 

## write our own function
add_two_numbers <- function(first_number, second_number){
    sum_of_inputs <- first_number + second_number
    return(sum_of_inputs)
}
add_two_numbers(100,26)

## write an if-loop
number_to_test <- 0
if (number_to_test > 0){
    print("Positive")
} else if (number_to_test > 0){
    print("Negative")
} else{
    print("Zero")
}

## the colon operator
print(1:5)                                      ## the colon operator prints all the integers between a start- and end-point (inclusive)

## write a first for-loop
for (index_variable in 1:10){
    print(index_variable)
}

## write a first for-loop
for (index_variable in 200:190){
    print(index_variable)
}

## write a first for-loop
for (index_variable in 200:190){
    print(index_variable)
}

## use a for-loop to index along a vector
for (i in length(presenter_names)){
    print(presenter_names[i])
}

## use a for-loop to index along a vector (with an if-loop inside the for-loop)
for (i in 1:length(presenter_names)){
    if (presenter_names[i] == "Daniel"){
        print(presenter_names[i])
        print("Boo")
    } else{
        print(presenter_names[i])
        print("Hooray")
    }
}
    