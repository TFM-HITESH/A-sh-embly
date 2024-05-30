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
    elif (opcode == "JUMP.LT.0"):
        # Expecting a label to go to now. Append it to programTokens
        jumpLtLabel = lineParts[1]
        programTokens.append(jumpLtLabel)
        tokenCounter += 1

# We have successfully tokenized the program along with handling labels 
# print(programTokens)
# print(labelTracker)


##############################################################################################################################
#                                       Creating the Stack for program execution                                             #
##############################################################################################################################

# Regular DSA stack implementation
class Stack:
    def __init__(self, size):
        self.stackPointer = -1
        self.stackArr = [0 for ele in range(size)]

    def push(self, num):
        self.stackPointer += 1
        self.stackArr[self.stackPointer] = num

    def pop(self):
        num = self.stackArr[self.stackPointer]
        self.stackPointer -= 1
        return num
    
    def top(self):
        return self.stackArr[self.stackPointer]
        

##############################################################################################################################
#                                    Creating the Interpreter for program execution                                          #
##############################################################################################################################

# Stack of size 256 for our program
programStack = Stack(256)
# Program counter keeps track of next instruction
programCounter = 0

# Keep running the program unless and until HALT is encountered
while(programTokens[programCounter] != "HALT"):
    # Getting current instruction
    currentOpcode = programTokens[programCounter]
    programCounter += 1

    # The next token would be the number. To push that into our stack
    if(currentOpcode == "PUSH"):
        number = programTokens[programCounter]
        programStack.push(num=number)
        # Skip the next token (number)
        programCounter += 1
    # To pop from the top of Stack
    elif(currentOpcode == "POP"):
        number = programStack.pop()
    # For adding, pop the top 2 numbers and then add and push them into stack
    elif(currentOpcode == "ADD"):
        num1 = programStack.pop()
        num2 = programStack.pop()
        programStack.push(int(num2) + int(num1))
    # For subtracting, pop the top 2 numbers and then subtract and push them into stack
    elif(currentOpcode == "SUB"):
        num1 = programStack.pop()
        num2 = programStack.pop()
        programStack.push(int(num2) - int(num1))
    # For multiplying, pop the top 2 numbers and then multiply and push them into stack
    elif(currentOpcode == "MUL"):
        num1 = programStack.pop()
        num2 = programStack.pop()
        programStack.push(int(num2) * int(num1))
    # For diividing, pop the top 2 numbers and then divide and push them into stack
    elif(currentOpcode == "DIV"):
        num1 = programStack.pop()
        num2 = programStack.pop()
        programStack.push(int(int(num2) / int(num1)))
    # Display the string literal on screen
    elif(currentOpcode == "PRINT"):
        stringLiteral = programTokens[programCounter]
        print(stringLiteral)
        # Skip the next token (string literal)
        programCounter += 1
    # Read a number from the user and put it on top of stack
    elif(currentOpcode == "READ"):
        number = int(input())
        programStack.push(number)
    # Check if equal to 0 and jump if needed
    elif(currentOpcode == "JUMP.EQ.0"):
        number = programStack.top() # Check if 0 or not
        if (number == 0):
            programCounter = labelTracker[programTokens[programCounter]]
            # The program counter shifts to the location pointed to by the label in the dictionary
            # programTokens[programCounter] gives us the label name
            # labelTracker[labelName] in the dict gives us the new label program counter location
        else:
            # Otherwise, just skip the next token (label to jump to)
            programCounter += 1
    elif(currentOpcode == "JUMP.GT.0"):
        number = programStack.top() # Check if >0 or not
        if (number > 0):
            programCounter = labelTracker[programTokens[programCounter]]
            # The program counter shifts to the location pointed to by the label in the dictionary
            # programTokens[programCounter] gives us the label name
            # labelTracker[labelName] in the dict gives us the new label program counter location
        else:
            # Otherwise, just skip the next token (label to jump to)
            programCounter += 1
    elif(currentOpcode == "JUMP.LT.0"):
        number = programStack.top() # Check if >0 or not
        if (number < 0):
            programCounter = labelTracker[programTokens[programCounter]]
            # The program counter shifts to the location pointed to by the label in the dictionary
            # programTokens[programCounter] gives us the label name
            # labelTracker[labelName] in the dict gives us the new label program counter location
        else:
            # Otherwise, just skip the next token (label to jump to)
            programCounter += 1



