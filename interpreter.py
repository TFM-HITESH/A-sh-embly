import sys

# Reads the arguement given in the command line
# Command line input = python interpreter.py prog1.ash 
# sys.argv[0] = interpreter.py
# sys.argv[1] = prog1.ash
# So now the program file is loaded for interpreting
programFilepath = sys.argv[1]


##############################################################################################################################
#                                               Tokenizing the program                                                       #
##############################################################################################################################

# To read the lines of the file one by one

programLines = []

# Open file in read mode
programFile = open(programFilepath, "r")

programLines = programFile.readlines()
programLines = [line.strip() for line in programLines]

# We now have the individual lines of the program
# Now to tokenize it into meaningful instructions

programTokens = [] 
tokenCounter = 0
# To stop the program when tokenCounter == programCounter
labelTracker = {}
# To keep track of the location of all the labels in our program. Is a python dict of the form { labelName : tokenIndex }

for line in programLines:

    # All lines are of form OPCODE or OPCODE param
    lineParts = line.split(" ")
    # First word is always opcode
    opcode = lineParts[0]

    if (opcode == ""):
        # Skip empty lines
        continue
    
    if (opcode.endswith(":")):
        # Opcodes ending with : must be a LABEL 
        labelTracker[opcode[:-1]] = tokenCounter
        # Putting the opcode without the : (opcode[:-1]) as a key value pair with the current token number into dict
        continue
    
    # All opcodes possibilities are dealt with. We can push it into the programTokens.
    programTokens.append(opcode)
    tokenCounter += 1

    # Now to deal with the possible arguement that exists past the opcode. (lineParts[1])
    
    if (opcode == "PUSH"):
        # Expecting a number now. Append it to the programTokens
        inputNum = lineParts[1]
        programTokens.append(inputNum)
        tokenCounter += 1
    elif (opcode == "PRINT"):
        # Expecting a string literal now. Append it to the programTokens
        # The string may be separated by spaces. Need to join all remaining lineParts together
        inputStr = ''
        for stringLine in lineParts[1:]:
            inputStr += stringLine
            inputStr += " "
        inputStr = inputStr[1:-2]  # Slicing away the " " at the end of strings
        programTokens.append(inputStr)
        tokenCounter += 1
    elif (opcode == "JUMP.EQ.0"):
        # Expecting a label to go to now. Append it to programTokens
        jumpEqLabel = lineParts[1]
        programTokens.append(jumpEqLabel)
        tokenCounter += 1
    elif (opcode == "JUMP.GT.0"):
        # Expecting a label to go to now. Append it to programTokens
        jumpGtLabel = lineParts[1]
        programTokens.append(jumpGtLabel)
        tokenCounter += 1

# We have successfully tokenized the program along with handling labels 
# print(programTokens)
# print(labelTracker)


##############################################################################################################################
#                                       Creating the Stack for program execution                                             #
##############################################################################################################################

