package dfa // leave this line in the file

case class State(label: String)
case class Transition(from: State, to: State, symbol: Char)

class DFA(val states: Set[State], val transitions: Set[Transition],
    val start: State, val accept: Set[State]):
    
    def accepts(line: String): Boolean = {
        var currState = start
        // char by char -> for loop c
        for (c <- line) {
            val currTransition = transitions
                .filter(_.from == currState)
                .filter(_.symbol == c)

            // set new state
            currState = currTransition.head.to
        }

        if accept.contains(currState) then true
        else false
    }