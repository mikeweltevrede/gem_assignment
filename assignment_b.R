### From pseudocode

## Initialising steps


## Algorithm
# Start;

# Reassign preferences for non-assigned patients to the most preferred
# available option;

# Find all cycles C;

# Find the patient t with the highest priority in C with currently best
# preference p_t^∗

# Set p_t := p_t^∗

# What if p_t^∗ is an available kidney?

# while cycles exist do

  # Step X;

  # while cycles of length less than or equal to q exist do
    # Assign patients in the cycle accordingly;

    # Remove assigned patients from the patient list;

    # Return to Start;

  # Step Y: Set pt to the next preferred option pt' of patient t (“break” their
  # current preference);

  # if pt is w then
    # Reset pt to p_t^∗

    # Find the patient t' != t with the highest priority in C;

    # Set t to t'

    # Return to Step Y;

  # else if pt is an available kidney then
    # Assign pt to patient t;
    # Remove pt from and add kt to the list of available kidneys;
    # Remove t from patient list;
    # Return to Start;
  # else
    #   Find all cycles;
    # Go to Step X

# w-chains
