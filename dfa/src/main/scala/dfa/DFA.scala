package dfa // leave this line in the file

// TODO: replace this comment with your implementation
case class State(label: String)
case class Transition(from: State, symbol: Char, to: State)

class DFA(val state: Set[State], val transitions: Set[Transition],
    val startState: State, val acceptingStates: Set[State]):

    def accept(line: String): Boolean = {
        var currState = startState
        // char by char -> for loop c
        for (c <- line) {
            // TODO: find transition(from: currState, symbol: currChar)
            val currTransition = transitions.head

            // set new state
            currState = currTransition.to
        }

        if acceptingStates.contains(currState) then true
        else false
    }