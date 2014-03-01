package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal
  
  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () => afterDelay(0) {
        println(
          "  " + currentTime + ": " + name + " -> " +  wire.getSignal)
      }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(a1Sig | a2Sig) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }
  
  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val b1,c1, b2, c2, d1 = new Wire
    andGate(a1, a1, b1)
    inverter(b1,c1)
    andGate(a2, a2, b2)
    inverter(b2, c2)
    andGate(c1, c2, d1)
    inverter(d1, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {

    def deMux1(in: Wire, c1: Wire) : List[Wire] = {
      val c1Invert, o1, o2 = new Wire
      inverter(c1, c1Invert)
      andGate(in, c1Invert, o1)
      andGate(in, c1, o2)
      List(o1,o2)
    }

    def deMuxLayer(in: Wire, cList: List[Wire], outputList: List[Wire]) = {
      c match {
        case List() => ??? // collapse list
        case c1 :: ctail => for( i <- outputList) yield deMux1(i, c1) ::: outputList
      }
    }

    if( c.isEmpty) andGate(in, in, out.head)
    else deMuxLayer(in, c, out)
  }

}

/*def demux(in: Wire, c: List[Wire], out: List[Wire]) {

def deMux1(in: Wire, c1: Wire, o2 : List[Wire]) = {
val c1Invert = new Wire
inverter(c1, c1Invert)
andGate(in, c1Invert, o2.head)
andGate(in, c1, o2.last)
}

def deMuxLayer(in: Wire, cList: List[Wire], outputList: List[Wire]) = {
c match {
case List() => andGate(in, in, outputList.head)
case c1 :: ctail => deMux1(in, c1, outputList.takeRight(2))
}
}

if( c.isEmpty) andGate(in, in, out.head)
else deMuxLayer(in, c, out)
}

}*/


object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
