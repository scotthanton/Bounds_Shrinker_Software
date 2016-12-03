using NLPModels
using AmplNLReader
using ForwardDiff

c = 0           #Number of constraints
alpha = 0.5     #Feasibility distance tolerance
beta = 0.1      #Vector length tolerance

function shrink(filename)
  global c, alpha, beta

  #Load AMPL model based on filename
  m = AmplModel(filename)

  #Get number of constraints from model
  c = m.meta.ncon

  #Starting point
  x = [10.0, 10.0]

  #Set counter to 0
  i = 0

  #Loop until close enough (should be infinity, this is here for development)
  while i < 10
    i = i + 1

    #Number of violated constraints for a given variable
    n = Int64[m.meta.nvar]

    #Our component vector for single constraint
    f = Floa64[m.meta.nvar]

    #Sum of component vectors for the variable over the feasibility vectors for all violated constraints
    s = Float64[m.meta.nvar]

    #Number of constraints violated
    ninf = 0

    #Consensus vectors
    t = Float64[m.meta.nvar]

    #Constraint counter for looping all constraints
    constraint_counter = 1

    #Loop each constraint
    for c in cons(m, x)

      constraint_violated = false #Upper or lower bound violated?
      d = 1; #Used for direction of feasibility vector
      violation = 0; #Amount the constraint was violated by

      #If the constraint upper bound is violated
      if c > m.meta.ucon[constraint_counter]

        constraint_violated = true
        d = -1
        violation = c - m.meta.ucon[constraint_counter]
        println("Constraint $constraint_counter upper bound was violated ($c)")

      else

        println("Constraint $constraint_counter uper bound  not violated ($c)")

      end

      #If the constraint lower bound is violated
      if c < m.meta.lcon[constraint_counter]

        violation = m.meta.lcon[constraint_counter] - c
        constraint_violated = true
        println("Constraint $constraint_counter lower bound was violated ($c)")

      else

        println("Constraint $constraint_counter lower bound not violated ($c)")

      end

      #If upper or lower bound was violated
      if constraint_violated

          println("Constraint violation is $violation")

          #Calculate gradient
          #==
          THIS DOES NOT WORK, WE NEED THE GRADIENT OF CONSTRAINT NOT OF THE
          ENTIRE MODEL, NOT SURE HOW TO DO THIS CURRENTLY
          ==#
          gradient = grad(m, x)

          #Feasibility vector calculation bottom value
          gradient_vector_squared_sum = 0

          #Get sum of each variables gradient vector squared
          for a in gradient
            gradient_vector_squared_sum = gradient_vector_squared_sum + a ^ 2
          end

          #Calculate feasibility vector
          x_counter = 1

          #Set each component of feasibility vector
          for x_value in x

            #Feasibility vector calculation based on gradient, violation and direction
            f[x_counter] = (violation * d * gradient[x_counter]) / gradient_vector_squared_sum

            #Continue to next variable
            x_counter = x_counter + 1

          end

          #Calculate distance of feasibility vector
          distance_sum = 0
          for f in f_vector
            distance_sum = distance_sum + f ^ 2
          end

          #Distance is root of sum of each length squared
          distance = sqrt(distance_sum)

          println("The distance of the component vector is $distance")

          #If feasibility distance greater than alpha
          if distance > alpha

            println("Feasibility distance is greater than alpha")

            #Increment the number of constraints violated
            ninf = ninf + 1


          end

      #End if constraint violated
      end

      #Increment constraint counter
      constraint_counter = constraint_counter + 1

    #End constraint loop
    end

  #End while loop
  end

end
