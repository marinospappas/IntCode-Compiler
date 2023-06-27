package mpdev.compiler.intcode

import java.io.File
import java.io.PrintStream
import java.lang.System.err
import java.lang.System.out
import java.util.Date
import mpdev.compiler.intcode.IntcodeInstructions.OpCode.*
import mpdev.compiler.intcode.IntcodeInstructions.ParamMode.*

/** this class implements all the instructions for the target machine */
class IntcodeInstructions(outFile: String = ""): CodeModule {

    private val CODE_ID = "IntCode Program"
    override val COMMENT = "#"
    override var outputSize = 0
    override var outStream: PrintStream = out
    override var PC: Int = 0

    override var code = mutableListOf<Any>()
    override val labelsMap = mutableMapOf<String,Int>()

    private val mainEntryPoint = "__main__start"

    // IntCode specific variables
    var SP = 10_000        // Stack Pointer
    var heap = 1_000_000   // Start of Heap
    // registers
    var A = SP - 1          // Accumulator
    var B = SP - 2          // Reg B
    var C = SP - 3          // Reg C
    var D = SP - 4          // Reg C
    var E = SP - 5          // Reg C
    var Z = SP - 6          // Zero flag
    var S = SP - 7          // Sign flag

    private val globalVars = mutableMapOf<String,Int>()
    private var globalVarsIndx = 0

    // the offset from base pointer for the next local variable (in the stack)
    override var stackVarOffset = 0

    // flag to include the string buffer in the assembly code
    var includeStringBuffer = false

    // architecture word size.
    val WORD_SIZE = 1  // each variable is a Long

    // sizes of various types
    override val INT_SIZE = WORD_SIZE    // 64-bit integers
    override val BYTE_SIZE = 1
    override val PTR_SIZE = WORD_SIZE    // pointer 64 bit

    override val DEF_INT_FMT = "def_int_fmt"
    override val INT_FMT = "int_fmt"

    /** initialisation code - class InputProgramScanner */
    init {
        if (outFile != "") {
            try {
                outStream = PrintStream(File(outFile))
            } catch (e: Exception) {
                err.println("could not create output file - $e")
                err.println("output code will be sent to stdout")
            }
        }
    }

    // intcode specific functions

    /*
    Opcode 3 takes a single integer as input and saves it to the position given by its only parameter.
            For example, the instruction 3,50 would take an input value and store it at address 50.
    Opcode 4 outputs the value of its only parameter.
            For example, the instruction 4,50 would output the value at address 50.
    Opcode 5 is jump-if-true: if the first parameter is non-zero, it sets the instruction pointer to the value from the second parameter.
            Otherwise, it does nothing.
    Opcode 6 is jump-if-false: if the first parameter is zero, it sets the instruction pointer to the value from the second parameter.
            Otherwise, it does nothing.
    Opcode 7 is less than: if the first parameter is less than the second parameter, it stores 1 in the position given by the third parameter.
            Otherwise, it stores 0.
    Opcode 8 is equals: if the first parameter is equal to the second parameter, it stores 1 in the position given by the third parameter.
            Otherwise, it stores 0.
    */
    enum class OpCode(val intValue: Int) {
        ADD(1), MULT(2), IN(3), OUT(4), JIT(5), JIF(6), LT(7), EQ(8),
        REL(9), DIV(10), EXIT(99)
    }
    enum class ParamMode(val intValue: Int) {
        POS(0), IMMED(1), IND(2)
    }

    fun copyMemory(srcAddr: Int, destAddr: Int) {
        val paramMode = POS.intValue * 100 + IMMED.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + OpCode.ADD.intValue
        outputCode(listOf(opCode,srcAddr,0,destAddr))
    }

    fun pushToStack(address: Int) {
        var paramMode = IMMED.intValue
        var opCode = paramMode * 100 + REL.intValue
        outputCode(listOf(opCode,1))
        paramMode = IND.intValue * 100 + IMMED.intValue * 10 + POS.intValue
        opCode = paramMode * 100 + OpCode.ADD.intValue
        outputCode(listOf(opCode,address,0,0))
    }

    fun pushNumberToStack(value: Any) {
        var paramMode = IMMED.intValue
        var opCode = paramMode * 100 + REL.intValue
        outputCode(listOf(opCode,1))
        paramMode = IND.intValue * 100 + IMMED.intValue * 10 + IMMED.intValue
        opCode = paramMode * 100 + OpCode.ADD.intValue
        outputCode(listOf(opCode,value,0,0))
    }

    fun popFromStack(address: Int) {
        var paramMode = POS.intValue * 100 + IMMED.intValue * 10 + IND.intValue
        var opCode = paramMode * 100 + OpCode.ADD.intValue
        outputCode(listOf(opCode,0,0,address))
        paramMode = IMMED.intValue
        opCode = paramMode * 100 + REL.intValue
        outputCode(listOf(opCode,-1))
    }

    /** register names for the function params - in order 1-6 (x86-64 architecture specific) */
    // these registers hold the fun params at the time of the call
    override val funInpParamsCpuRegisters = arrayOf("%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9")
    // during the assignment of the parameters, their values are saved temporarily here,
    // so that they are not corrupted by function calls executed during the assignment of the parameters
    override val funTempParamsCpuRegisters = arrayOf("%rbx", "%r12", "%r13", "%r14", "%r15", "%rax")
    // 6 params maximum allowed
    override val MAX_FUN_PARAMS = funInpParamsCpuRegisters.size

    /** initialisation code for assembler */
    override fun progInit(progOrLib: String, progName: String) {
        outputComment(CODE_ID)
        outputComment("$progOrLib $progName")
        outputComment("compiled on ${Date()}")
        jump(mainEntryPoint)
    }

    /** declare int variable (64bit) */
    override fun declareInt(varName: String, initValue: String) {
        globalVars[varName] = heap + globalVarsIndx++
    }

    /** declare byte variable (8bit) */
    override fun declareByte(varName: String, initValue: String) {
    }

    /** declare int array variable (64bit) */
    override fun declareIntArray(varName: String, length: String) {
    }

    /** declare byte array variable */
    override fun declareByteArray(varName: String, length: String) {
    }

    /** initial code for functions */
    override fun funInit() {
    }

    override fun outputLabel(s: String) {
        labelsMap[s] = PC
    }

    /** declare function */
    override fun declareAsmFun(name: String) {
        outputLabel(name)
        //outputCodeTab("pushq\t%rbx\t\t")
        //outputCommentNl("save \"callee\"-save registers")
        //newStackFrame()
    }

    /** transfer a function parameter to stack variable */
    override fun storeFunParamToStack(paramIndx: Int, stackOffset: Int) {
        //outputCodeTabNl("movq\t${funInpParamsCpuRegisters[paramIndx]}, $stackOffset(%rbp)")
    }

    /** end of function - tidy up stack */
    private fun funEnd() {
        //restoreStackFrame()
    }

    /** set a temporary function param register to the value of %rax (the result of the last expression) */
    override fun setIntTempFunParam(paramIndx: Int) {
        //outputCodeTab("pushq\t${funTempParamsCpuRegisters[paramIndx]}\t")
        //outputCodeTabNl("movq\t%rax, ${funTempParamsCpuRegisters[paramIndx]}")
    }

    /** set a function input param register from the temporary register */
    override fun setFunParamRegFromTempReg(paramIndx: Int) {
        //outputCodeTabNl("movq\t${funTempParamsCpuRegisters[paramIndx]}, ${funInpParamsCpuRegisters[paramIndx]}")
    }

    /** set a function input param register from accumulator */
    override fun setFunParamRegFromAcc(paramIndx: Int) {
        //outputCodeTabNl("movq\t%rax, ${funInpParamsCpuRegisters[paramIndx]}")
    }

    /** restore a function input param register */
    override fun restoreFunTempParamReg(paramIndx: Int) {
        if (funTempParamsCpuRegisters[paramIndx] == "%rax")
            return
        //outputCodeTab("popq\t${funTempParamsCpuRegisters[paramIndx]}\t")
    }

    /** initial code for main */
    override fun mainInit() {
        outputLabel(mainEntryPoint)
        val opCode = IMMED.intValue*100 + REL.intValue
        outputCode(listOf(opCode,SP))
    }

    /** termination code for assembler */
    override fun mainEnd() {
        outputCode(listOf(EXIT.intValue))
    }

    /** create relative addresses for global vars */
    override fun createRelativeAddresses() {}

    /*
    /** set new stack frame */
    private fun newStackFrame() {
        outputCodeTab("pushq\t%rbp\t\t")
        outputCommentNl("new stack frame")
        outputCodeTabNl("movq\t%rsp, %rbp")
        stackVarOffset = 0  // reset the offset for stack vars in this new frame
    }

    /** restore stack frame */
    private fun restoreStackFrame() {
        outputCodeTab("movq\t%rbp, %rsp\t\t")
        outputCommentNl("restore stack frame")
        outputCodeTabNl("popq\t%rbp")
    }
     */

    /*
    /**
     * allocate variable space in the stack
     * returns the new stack offset for this new variable
     */
    override fun allocateStackVar(size: Int): Int {
        outputCodeTabNl("subq\t$${size}, %rsp")
        stackVarOffset -= size
        return stackVarOffset
    }

    /** release variable space in the stack */
    override fun releaseStackVar(size: Int) {
        outputCodeTabNl("addq\t$${size}, %rsp")
        stackVarOffset += size
    }

    /** initiliase an int stack var */
    override fun initStackVarInt(stackOffset : Int, initValue: String) {
        outputCodeTab("movq\t$$initValue, ")
        if (stackOffset != 0)
            outputCode("$stackOffset")
        outputCodeNl("(%rbp)")
    }
     */

    /** exit the program */
    override fun exitProgram() {
    }
    //////////////////////////////////////////////////////////////

    /** set accumulator to a value */
    override fun setAccumulator(value: String) {
        clearAccumulator()
        val paramMode = POS.intValue * 100 + IMMED.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + OpCode.ADD.intValue
        outputCode(listOf(opCode,A,value.toInt(),A))
    }

    /** clear accumulator */
    override fun clearAccumulator() {
        val paramMode = POS.intValue * 100 + IMMED.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + MULT.intValue
        outputCode(listOf(opCode,A,0,A))
    }

    /** increment accumulator */
    override fun incAccumulator() {
        val paramMode = POS.intValue * 100 + IMMED.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + OpCode.ADD.intValue
        outputCode(listOf(opCode,A,1,A))
    }

    /** decrement accumulator */
    override fun decAccumulator() {
        val paramMode = POS.intValue * 100 + IMMED.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + OpCode.ADD.intValue
        outputCode(listOf(opCode,A,-1,A))
    }

    /** push accumulator to the stack */
    override fun saveAccumulator() = pushToStack(A)

    /** add top of stack to accumulator */
    override fun addToAccumulator() {
        popFromStack(B)
        val paramMode = POS.intValue * 100 + POS.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + OpCode.ADD.intValue
        outputCode(listOf(opCode,A,B,A))
    }

    /** subtract top of stack from accumulator */
    override fun subFromAccumulator() {
        popFromStack(B)
        var paramMode = POS.intValue * 100 + IMMED.intValue * 10 + POS.intValue
        var opCode = paramMode * 100 + MULT.intValue
        outputCode(listOf(opCode,A,-1,A))
        paramMode = POS.intValue * 100 + POS.intValue * 10 + POS.intValue
        opCode = paramMode * 100 + OpCode.ADD.intValue
        outputCode(listOf(opCode,A,B,A))
    }

    /** negate accumulator */
    override fun negateAccumulator() {
        val paramMode = POS.intValue * 100 + IMMED.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + MULT.intValue
        outputCode(listOf(opCode,A,-1,A))
    }

    /** multiply accumulator by top of stack */
    override fun multiplyAccumulator() {
        popFromStack(B)
        val paramMode = POS.intValue * 100 + POS.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + MULT.intValue
        outputCode(listOf(opCode,A,B,A))
    }

    /** divide accumulator by top of stack */
    override fun divideAccumulator() {
        popFromStack(B)
        val paramMode = POS.intValue * 100 + POS.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + DIV.intValue
        outputCode(listOf(opCode,A,B,A))
    }

    /** modulo after divide accumulator by top of stack */
    override fun moduloAccumulator() {
    }

    /** bitwise not accumulator */
    override fun notAccumulator() {
    }

    /** or top of stack with accumulator */
    override fun orAccumulator() {
    }

    /** exclusive or top of stack with accumulator */
    override fun xorAccumulator() {
    }

    /** and top of stack with accumulator */
    override fun andAccumulator() {
    }

    /** shift accumulator left */
    override fun shiftAccumulatorLeft() {
    }

    /** shift accumulator right */
    override fun shiftAccumulatorRight() {
    }

    /** set accumulator to static int variable value */
    override fun setAccumulatorToVar(identifier: String) {
        val paramMode = POS.intValue * 100 + IMMED.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + OpCode.ADD.intValue
        val varAddress = globalVars[identifier] ?: 0
        outputCode(listOf(opCode,varAddress,0,A))
        // also set flags - Z flag set = FALSE
    }

    /** set int variable to accumulator */
    override fun assignment(identifier: String) {
        val paramMode = POS.intValue * 100 + IMMED.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + OpCode.ADD.intValue
        val varAddress = globalVars[identifier] ?: 0
        outputCode(listOf(opCode,A,0,varAddress))
    }

    /** read int into global variable */
    override fun readInt(identifier: String) {
        val paramMode = POS.intValue
        val opCode = paramMode * 100 + IN.intValue
        val varAddress = globalVars[identifier] ?: 0
        outputCode(listOf(opCode,varAddress))
    }

    /** print accumulator as integer */
    override fun printInt() {
        val paramMode = POS.intValue
        val opCode = paramMode * 100 + OUT.intValue
        outputCode(listOf(opCode,A))
    }

    /** compare and set accumulator and flags - is equal to */
    override fun compareEquals() {
        popFromStack(B)
        val paramMode = POS.intValue * 100 + POS.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + EQ.intValue
        outputCode(listOf(opCode,A,B,Z))    // Z flag will be 1 if false (i.e. if A<>B)
    }

    /** compare and set accumulator and flags - is not equal to */
    override fun compareNotEquals() {
        popFromStack(B)
        var paramMode = POS.intValue * 100 + POS.intValue * 10 + POS.intValue
        var opCode = paramMode * 100 + EQ.intValue
        outputCode(listOf(opCode,A,B,Z))
        paramMode = POS.intValue * 100 + IMMED.intValue * 10 + POS.intValue
        opCode = paramMode * 100 + EQ.intValue
        outputCode(listOf(opCode,Z,0,Z))    // Z flag will be 1 if false (i.e. if A==B)
    }

    /** compare and set accumulator and flags - is less than */
    override fun compareLess() {
        popFromStack(B)
        val paramMode = POS.intValue * 100 + POS.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + LT.intValue
        outputCode(listOf(opCode,B,A,Z))    // Z flag will be 1 if false (i.e. if A>=B)
    }

    /** compare and set accumulator and flags - is less than or equal to */
    override fun compareLessEqual() {
        popFromStack(B)
        var paramMode = POS.intValue * 100 + POS.intValue * 10 + POS.intValue
        var opCode = paramMode * 100 + LT.intValue
        outputCode(listOf(opCode,A,B,S))
        paramMode = POS.intValue * 100 + IMMED.intValue * 10 + POS.intValue
        opCode = paramMode * 100 + EQ.intValue
        outputCode(listOf(opCode,S,0,Z))    // Z flag will be 1 if false (i.e. if A>B)
    }

    /** compare and set accumulator and flags - is greater than */
    override fun compareGreater() {
        popFromStack(B)
        val paramMode = POS.intValue * 100 + POS.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + LT.intValue
        outputCode(listOf(opCode,A,B,Z))    // Z flag will be 1 if false (i.e. if A<=B)
    }

    /** compare and set accumulator and flags - is greater than or equal to */
    override fun compareGreaterEqual() {
        popFromStack(B)
        var paramMode = POS.intValue * 100 + POS.intValue * 10 + POS.intValue
        var opCode = paramMode * 100 + LT.intValue
        outputCode(listOf(opCode,B,A,S))
        paramMode = POS.intValue * 100 + IMMED.intValue * 10 + POS.intValue
        opCode = paramMode * 100 + EQ.intValue
        outputCode(listOf(opCode,S,0,Z))    // Z flag will be 1 if false (i.e. if A>B)
    }

    /** branch if false */
    override fun jumpIfFalse(label: String) {
        val paramMode = IMMED.intValue * 10 + POS.intValue
        val opCode = paramMode * 100 + JIF.intValue
        outputCode(listOf(opCode,Z,label))
    }

    /** branch */
    override fun jump(label: String) {
        val paramMode = IMMED.intValue * 10 + IMMED.intValue
        val opCode = paramMode * 100 + JIT.intValue
        outputCode(listOf(opCode,1,label))
    }

    /** jump indirect */
    override fun jumpInd(label: String) {
        val paramMode = POS.intValue * 10 + IMMED.intValue
        val opCode = paramMode * 100 + JIT.intValue
        outputCode(listOf(opCode,1,label))
    }

    /** call a function */
    override fun callFunction(subroutine: String, returnLabel: String) {
        pushNumberToStack(returnLabel)
        jump(subroutine)
    }

    /** return from function */
    override fun returnFromCall() {
        funEnd()
        // pop the return address from stack and jump to it
        popFromStack(E)
        val label = "return_to_$E"
        jumpInd(label)
        labelsMap[label] = E
    }

    /** end of program */
    override fun progEnd(libOrProg: String) {
        outputComment("size: $PC")
    }

    /*
    /** set accumulator to static int array variable value */
    override fun setAccumulatorToArrayVar(identifier: String) {
        // index already in %rcx
        outputCodeTabNl("lea\t${identifier}(%rip), %rax")  // array address in %rax
        outputCodeTabNl("movq\t(%rax, %rcx, $INT_SIZE), %rax")  // get array element
        outputCodeTabNl("testq\t%rax, %rax")    // also set flags - Z flag set = FALSE
    }

    /** set accumulator to static byte variable value */
    override fun setAccumulatorToByteVar(identifier: String) {
        outputCodeTabNl("movb\t${identifier}(%rip), %al")   // fetch 1 byte
        outputCodeTabNl("andq\t$0xFF, %rax")     // zero rest of %rax and set flags - Z flag set = FALSE
    }

    /** set accumulator to static byte array variable value */
    override fun setAccumulatorToByteArrayVar(identifier: String) {
        // index already in %rcx
        outputCodeTabNl("lea\t${identifier}(%rip), %rax")  // array address in %rax
        outputCodeTabNl("movb\t(%rax, %rcx, 1), %al")  // get array element
        outputCodeTabNl("andq\t$0xFF, %rax")     // zero rest of %rax and set flags - Z flag set = FALSE
    }

    /** set accumulator to global variable address */
    override fun setAccumulatorToVarAddress(identifier: String) {
        outputCodeTabNl("lea\t${identifier}(%rip), %rax")
    }

    /** save accumulator to a temp register for later use */
    override fun saveAccToTempReg() {
        outputCodeTabNl("movq\t%rax, %rcx")
    }

    /** save accumulator to the previously saved address the pointer is pointing to */
    override fun pointerAssignment() {
        outputCodeTabNl("movq\t%rax, (%rcx)")
    }

    /** set accumulator to the contents of the address already in accumulator */
    override fun setAccumulatorToPointerVar() {
        outputCodeTabNl("movq\t(%rcx), %rax")
    }

    /** set accumulator to local int variable */
    override fun setAccumulatorToLocalVar(offset: Int) {
        outputCodeTab("movq\t")
        if (offset != 0)
            outputCode("$offset")
        outputCodeNl("(%rbp), %rax")
        outputCodeTabNl("testq\t%rax, %rax")    // also set flags - Z flag set = FALSE
    }

    override fun setAccumulatorToLocalArrayVar(offset: Int) {
        // index already in %rcx
        outputCodeTab("movq\t")
        if (offset != 0)
            outputCode("$offset")
        outputCodeNl("(%rbp), %rax")            // array address in %rax
        outputCodeTabNl("movq\t(%rax, %rcx, $INT_SIZE), %rax")  // get array element
        outputCodeTabNl("testq\t%rax, %rax")    // also set flags - Z flag set = FALSE
    }

    /** set accumulator to local byte variable */
    override fun setAccumulatorToLocalByteVar(offset: Int) {
        outputCodeTab("movb\t")
        if (offset != 0)
            outputCode("$offset")
        outputCodeNl("(%rbp), %al")
        outputCodeTabNl("andq\t$0xFF, %rax")     // zero rest of %rax and set flags - Z flag set = FALSE
    }

    /** set accumulator to local byte array variable */
    override fun setAccumulatorToLocalByteArrayVar(offset: Int) {
        // index already in %rcx
        outputCodeTab("movq\t")
        if (offset != 0)
            outputCode("$offset")
        outputCodeNl("(%rbp), %rax")            // array address in %rax
        outputCodeTabNl("movb\t(%rax, %rcx, 1), %al")  // get array element
        outputCodeTabNl("andq\t$0xFF, %rax")     // zero rest of %rax and set flags - Z flag set = FALSE
    }

    /** set accumulator to local variable address */
    override fun setAccumulatorToLocalVarAddress(offset: Int) {
        outputCodeTab("lea\t")
        if (offset != 0)
            outputCode("$offset")
        outputCodeNl("(%rbp), %rax")
    }

    /** set int variable to accumulator */
    override fun assignment(identifier: String) = outputCodeTabNl("movq\t%rax, ${identifier}(%rip)")

    /** set int stack variable to accumulator */
    override fun assignmentLocalVar(offset: Int) {
        outputCodeTab("movq\t%rax, ")
        if (offset != 0)
            outputCode("$offset")
        outputCodeNl("(%rbp)")
    }

    /** set int array element to accumulator */
    override fun arrayAssignment(identifier: String) {
        // index already in %rcx
        outputCodeTabNl("movq\t%rax, %rbx")     // save value in %rbx
        outputCodeTabNl("lea\t${identifier}(%rip), %rax")  // array start address in %rax
        outputCodeTabNl("movq\t%rbx, (%rax, %rcx, $INT_SIZE)")  // save array element
    }

    /** set int stack array element to accumulator */
    override fun assignmentLocalArrayVar(offset: Int) {
        // index already in %rcx
        outputCodeTabNl("movq\t%rax, %rbx")     // save value in %rbx
        outputCodeTab("movq\t")
        if (offset != 0)
            outputCode("$offset")
        outputCodeNl("(%rbp), %rax")            // array start address in %rax
        outputCodeTabNl("movq\t%rbx, (%rax, %rcx, $INT_SIZE)")  // save array element
    }

    /** set byte variable to accumulator */
    override fun assignmentByte(identifier: String) = outputCodeTabNl("movb\t%al, ${identifier}(%rip)")

    /** set int stack variable to accumulator */
    override fun assignmentLocalByteVar(offset: Int) {
        outputCodeTab("movb\t%al, ")
        if (offset != 0)
            outputCode("$offset")
        outputCodeNl("(%rbp)")
    }

    /** set byte array element to accumulator */
    override fun arrayByteAssignment(identifier: String) {
        // index already in %rcx
        outputCodeTabNl("movb\t%al, %bl")     // save value in %rbx
        outputCodeTabNl("lea\t${identifier}(%rip), %rax")  // array start address in %rax
        outputCodeTabNl("movb\t%bl, (%rax, %rcx, 1)")  // save array element
    }

    /** set byte stack array element to accumulator */
    override fun assignmentLocalByteArrayVar(offset: Int) {
        // index already in %rcx
        outputCodeTabNl("movb\t%al, %bl")     // save value in %rbx
        outputCodeTab("movq\t")
        if (offset != 0)
            outputCode("$offset")
        outputCodeNl("(%rbp), %rax")            // array start address in %rax
        outputCodeTabNl("movb\t%bl, (%rax, %rcx, 1)")  // save array element
    }

    /** convert accumulator to byte */
    override fun convertToByte() {
        outputCodeTabNl("andq\t\$0xFF, %rax")
    }

    /** boolean not accumulator */
    override fun booleanNotAccumulator() {
        outputCodeTabNl("testq\t%rax, %rax")
        outputCodeTabNl("sete\t%al")        // set AL to 1 if rax is 0
        outputCodeTabNl("andq\t$1, %rax")   // zero the rest of rax and set flags - Z flag set = FALSE

    }

    override fun booleanOrAccumulator() {
        outputCodeTabNl("popq\t%rbx")
        outputCodeTabNl("testq\t%rbx, %rbx") // convert op1 to 0-1
        outputCodeTabNl("setne\t%bl")        // set BL to 1 if rbx is not 0
        outputCodeTabNl("andq\t$1, %rbx")   // zero the rest of rbx and set flags

        outputCodeTabNl("testq\t%rax, %rax")  // convert op2 to 0-1
        outputCodeTabNl("setne\t%al")        // set AL to 1 if rax is 0
        outputCodeTabNl("andq\t$1, %rax")   // zero the rest of rax and set flags - Z flag set = FALSE

        outputCodeTabNl("orq\t%rbx, %rax")
    }

    override fun booleanAndAccumulator() {
        outputCodeTabNl("popq\t%rbx")
        outputCodeTabNl("testq\t%rbx, %rbx") // convert op1 to 0-1
        outputCodeTabNl("setne\t%bl")        // set BL to 1 if rbx is not 0
        outputCodeTabNl("andq\t$1, %rbx")   // zero the rest of rbx and set flags

        outputCodeTabNl("testq\t%rax, %rax")  // convert op2 to 0-1
        outputCodeTabNl("setne\t%al")        // set AL to 1 if rax is 0
        outputCodeTabNl("andq\t$1, %rax")   // zero the rest of rax and set flags - Z flag set = FALSE

        outputCodeTabNl("andq\t%rbx, %rax")
    }

    /** print a newline */
    override fun printNewline() {
        outputCodeTabNl("lea\tnewline_(%rip), %rdi")
        outputCodeTabNl("call\twrite_s_")
    }

    /** print accumulator as integer */
    override fun printInt(fmt: String) {
        //TODO: support various dec formats - call printf instead
        outputCodeTabNl("movq\t%rax, %rdi\t\t# value to be printed in rdi")
        outputCodeTabNl("call\twrite_i_")
    }

    /** read global int var into variable */
    override fun readInt(identifier: String) {
        outputCodeTabNl("lea\t$identifier(%rip), %rdi\t\t# address of the variable to be read")
        outputCodeTabNl("call\tread_i_")
    }

    /** read local int var into variable */
    override fun readIntLocal(stackOffset: Int) {
        outputCodeTab("movq\t")
        if (stackOffset != 0)
            outputCode("$stackOffset")
        outputCodeNl("(%rbp), %rdi\t\t# address of the variable to be read")
        outputCodeTabNl("call\tread_i_")
    }

    ////////// string operations ///////////////////////

    /** declare string global variable */
    override fun declareString(varName: String, initValue: String, length: Int) {
        if (length == 0 || initValue != "")
            outputCodeTabNl("$varName:\t.string \"$initValue\"")
        else
            outputCodeTabNl("$varName:\t.space $length") // uninitialised string vars must have length
        outputCodeNl(".align 8")
    }

    /** initialise a str stack var */
    override fun initStackVarString(stackOffset: Int, stringDataOffset: Int, constStrAddress: String) {
        outputCodeTabNl("lea\t$stringDataOffset(%rbp), %rax")
        outputCodeTab("movq\t%rax, $stackOffset(%rbp)\t\t")
        outputCommentNl("initialise local var string address")
        if (constStrAddress.isNotEmpty()) {
            outputCodeTabNl("lea\t$constStrAddress(%rip), %rsi")
            outputCodeTabNl("movq\t$stackOffset(%rbp), %rdi")
            outputCodeTab("call\tstrcpy_\t\t")
            outputCommentNl("initialise local var string")
        }
    }

    /** get address of string variable in accumulator */
    override fun getStringVarAddress(identifier: String) = outputCodeTabNl("lea\t${identifier}(%rip), %rax")

    /** save acc string to buffer and address in stack - acc is pointer */
    override fun saveString() {
        outputCodeTab("movq\t%rax, %rsi\t\t")
        outputCommentNl("save string - strcpy_(string_buffer, %rax)")
        outputCodeTabNl("lea\t$STRING_BUFFER(%rip), %rdi")
        outputCodeTabNl("call\tstrcpy_")
        outputCodeTabNl("pushq\t%rax")
        includeStringBuffer = true
    }

    /** add acc string to buf string - both are pointers*/
    override fun addString() {
        outputCodeTab("popq\t%rdi\t\t")
        outputCommentNl("add string - strcat_(top-of-stack, %rax)")
        outputCodeTabNl("movq\t%rax, %rsi")
        outputCodeTabNl("call\tstrcat_")
    }

    /** set string variable from accumulator (var and acc are pointers */
    override fun assignmentString(identifier: String) {
        outputCodeTab("movq\t%rax, %rsi\t\t")
        outputCommentNl("assign string - strcpy_(identifier, %rax)")
        outputCodeTabNl("lea\t${identifier}(%rip), %rdi")
        outputCodeTabNl("call\tstrcpy_")
    }

    /** set string variable from accumulator (var and acc are pointers */
    override fun assignmentStringLocalVar(stackOffset: Int) {
        outputCodeTab("movq\t%rax, %rsi\t\t")
        outputCommentNl("assign string - strcpy_(offset(%rbp), %rax)")
        outputCodeTab("movq\t")
        if (stackOffset != 0)
            outputCode("$stackOffset")
        outputCodeNl("(%rbp), %rdi")
        outputCodeTabNl("call\tstrcpy_")
    }

    /** print string - address in accumulator */
    override fun printStr() {
        outputCodeTabNl("movq\t%rax, %rdi\t\t# string pointer to be printed in rdi")
        outputCodeTabNl("call\twrite_s_")
    }

    /** read string into global variable - address in accumulator*/
    override fun readString(identifier: String, length: Int) {
        outputCodeTabNl("lea\t$identifier(%rip), %rdi\t\t# address of the string to be read")
        outputCodeTabNl("movq\t$${length}, %rsi\t\t# max number of bytes to read")
        outputCodeTabNl("call\tread_s_")
    }

    /** read string into local variable - address in accumulator*/
    override fun readStringLocal(stackOffset: Int, length: Int) {
        outputCodeTab("movq\t")
        if (stackOffset != 0)
            outputCode("$stackOffset")
        outputCodeNl("(%rbp), %rdi\t\t# address of the string to be read")
        outputCodeTabNl("movq\t$${length}, %rsi\t\t# max number of bytes to read")
        outputCodeTabNl("call\tread_s_")
    }

    /** compare 2 strings for equality */
    override fun compareStringEquals() {
        outputCodeTab("popq\t%rdi\t\t")
        outputCommentNl("compare strings - streq_(top-of-stack, %rax)")
        outputCodeTabNl("movq\t%rax, %rsi")
        outputCodeTabNl("call\tstreq_")
        outputCodeTabNl("andq\t$1, %rax")   // set flags - Z flag set = FALSE
    }

    /** compare 2 strings for non-equality */
    override fun compareStringNotEquals() {
        outputCodeTab("popq\t%rdi\t\t")
        outputCommentNl("compare strings - streq_(top-of-stack, %rax)")
        outputCodeTabNl("movq\t%rax, %rsi")
        outputCodeTabNl("call\tstreq_")
        outputCodeTabNl("xorq\t$1, %rax")   // boolean not rax and set flags - Z flag set = FALSE
    }

    /** string constants */
    override fun stringConstantsDataSpace() {
        code.outputCodeNl()
        code.outputCodeNl(".data")
        code.outputCodeTabNl(".align 8")
        if (includeStringBuffer) {
            code.outputCommentNl("buffer for string operations - max str length limit")
            code.outputCodeTabNl("$STRING_BUFFER:\t.space $STR_BUF_SIZE")
        }
    }

    //////////////////////////////////////////////////////////
    /** dummy instruction */
    override fun dummyInstr(cmd: String) = outputCodeTabNl(cmd)
*/
}
