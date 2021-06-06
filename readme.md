
## Problem
    - Every instruction has a label specified by number followed by a colon e.g. "4:"
    - There are 3 types of instruction:

      1) Value x         - Returns the value x
      2) Add <labels>    - Evaluates the instruction at each label in <labels> and adds the result together
      3) Mult <labels>   - Evaluates the instruction at each label in <labels> and multiplies the results together

      <labels> is a list of one or more numbers that refer to the label of other instructions in the input.

    - The overall result for a set of instruction is the result of evaluating the first instruction.

For example, given the input:

    0: Add 4 4 1
    1: Mult 6 2
    2: Value -3
    3: Add 6 1 2
    4: Value 5
    6: Value 2

The first instruction is "Add 4 4 1", and the overall result is 4, which is produced as follows:

            Add 4 4 1
      ->    Add (5) (5) (Mult 6 2)
      ->    Add (5) (5) (Mult (2) (-3))
      ->    Add (5) (5) (-6)
      ->    Add (10) (-6)
      ->    (4)

Result:
    What is the overall result of evaluating the instructions in the input.txt file?


## My Notes
I first attempted this in C# - using a stack based approach which worked well for a small input size. In a larger input though with many cycles I discovered that the stack grew huge and eventually the program ran out of memory.

a) How else can I do this that doesn't involve storing the intermediate steps?
b) Would F# be more efficient here? Perhaps match would be good to help tokenise the string?