package edu.colorado.csci3155.project1


/* -- Here are all the instructions to be supported --*/
sealed trait StackMachineInstruction
case class LoadI(s: String) extends StackMachineInstruction
case class  StoreI(s: String) extends StackMachineInstruction
case class PushI(f: Double) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object PopI extends StackMachineInstruction


object StackMachineEmulator {



    /* Function emulateSingleInstruction
        Given a list of doubles to represent a stack
              a map from string to double precision numbers for the environment
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified environment that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: List[Double],
                                 env: Map[String, Double],
                                 ins: StackMachineInstruction): (List[Double], Map[String, Double]) = {
        ins match {
            //use head to get first element. Use
            case LoadI(str) => {
                //I needed a way to deal with shadowing.
                //use scope and new environment to temporaily set value to a str already taken in the immutable map
                if (env.contains(str)){

                    val newEnv  = env + (str->stack.head)
                    (stack.tail,  Map(str->stack.head)++newEnv)
                } else { (stack.tail,  Map(str->stack.head)++env)} }

            case StoreI(str) => {
                //if the string exist append the values to the from of the list.
                if (env.contains(str)) {(List(env(str))++stack,env)}
                else {throw new IllegalArgumentException(s"Environment does not contain mapping for $str")} }
            //simply append to front of list
            case PushI(d) => (List(d) ++ stack, env)
            //I believe they want just the first element removed and the list returned
            case PopI => if (stack.size == 0 ){ throw new IllegalArgumentException(s"List is empty.") } else {(stack.tail,env)}

            case AddI => {
                if (stack.size >= 2) (List(stack(0)+stack(1))++stack.tail.tail,env)
                //if large than two add and return the list
                else { throw new IllegalArgumentException(s"List has < 2 elements. AddI requires 2 elements") } }

            case SubI => {
                if (stack.size >= 2) (List(stack(1)-stack(0))++stack.tail.tail,env)
                //if large than two add and return the list
                else { throw new IllegalArgumentException(s"List has < 2 elements. SubI requires 2 elements") } }

            case MultI => {
                if (stack.size >= 2) (List(stack(0)*stack(1))++stack.tail.tail,env)
                //if large than two add and return the list
                else { throw new IllegalArgumentException(s"List has < 2 elements. AddI requires 2 elements") } }

            case DivI => {
                if (stack(0) != 0.0 && stack.size >= 2 ) (List(stack(1)/stack(0))++stack.tail.tail,env)
                else { throw new IllegalArgumentException(s"Division by 0.0 or List has < 2 elements.") } }

            case LogI => {
                if (stack(0) > 0 && stack.size >= 1) (List(math.log(stack(0))) ++ stack.tail,env)
                else { throw new IllegalArgumentException(s"Error Log(0.0) or List has < 1 element.") } }

            case ExpI => {
                if (stack.size > 0) (List(math.exp(stack(0))) ++ stack.tail,env)
                else { throw new IllegalArgumentException(s"List is empty or input is <= 0.0") } }

            case CosI => {
                if (stack.size != 0) (List(math.cos(stack(0))) ++ stack.tail,env)
                else { throw new IllegalArgumentException(s"List is empty or input is <= 0.0") } }

            case SinI => {
                if (stack.size != 0) (List(math.sin(stack(0))) ++ stack.tail,env)
                else { throw new IllegalArgumentException(s"List is empty or input is <= 0.0") } }

        }
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Use foldLeft over list of instruction rather than a for loop if you can.
       Return value must be the final environment.

       Hint: accumulator for foldLeft must be a tuple (List[Double], Map[String,Double])
             initial value of this accumulator must be (Nil, Map.empty)
             You should use emulateSingleInstruction to update the accmulator.
             It will all fit nicely once you figure it out.
     */
    def emulateStackMachine(instructionList: List[StackMachineInstruction]): Map[String, Double] ={
        instructionList.foldLeft( Nil : List[Double], Map.empty[String,Double] )((acc, ins) => {
            emulateSingleInstruction(acc._1, acc._2, ins)
        })._2 // we only want the second option from the foldLeft as if we attempt to return the tuple we violate the
        // output type for emulatStackMachine.
    }
}