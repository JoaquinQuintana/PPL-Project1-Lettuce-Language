package edu.colorado.csci3155.project1

object StackMachineCompiler {
    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stack machine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        //TODO: Your code here
        e match {
            case Const(dub) => List(PushI(dub))
            case Ident(str) => List(StoreI(str))
            //will use ::: for concatenation in this section as in part 1 I use ++. ::: is said to be more efficient for pattern matching
            case Plus(e1,e2) => compileToStackMachineCode(e1) ::: compileToStackMachineCode(e2) ::: List(AddI)
            case Minus(e1,e2) => compileToStackMachineCode(e1) ::: compileToStackMachineCode(e2) ::: List(SubI)
            case Mult(e1,e2) => compileToStackMachineCode(e1) ::: compileToStackMachineCode(e2) ::: List(MultI)
            case Let(str,e1,e2) => compileToStackMachineCode(e1) ::: List(LoadI(str)) ::: compileToStackMachineCode(e2)
            case Log(e) => compileToStackMachineCode(e) ::: List(LogI)
            case Exp(e) => compileToStackMachineCode(e) ::: List(ExpI)
            //need testing but should be correct. None of the provided tests evaluate these!!!!
            case Div(e1,e2) => compileToStackMachineCode(e1) ::: compileToStackMachineCode(e2) ::: List(DivI)
            case Cosine(e) => compileToStackMachineCode(e) ::: List(CosI)
            case Sine(e) => compileToStackMachineCode(e) ::: List(SinI)

        }
    }
}
