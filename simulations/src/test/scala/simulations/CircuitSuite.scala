package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import java.util

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")

    in1.setSignal(false)
    run

    assert(out.getSignal === false, "and 4")
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "and 4")
  }


  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate2(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "and 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "and 3")

    in1.setSignal(false)
    run

    assert(out.getSignal === true, "and 4")
  }

  test("demux with 0 control") {
    val in, out1 = new Wire
    demux(in, List(), List(out1))

    in.setSignal(false)
    run

    assert(out1.getSignal === false, "demux 1 test out 1")

    in.setSignal(true)
    run

    assert(out1.getSignal === true, "demux 2 test out 1")
  }


  test("demux with 1 control") {
    val c, in, out1, out2 = new Wire
    demux(in, List(c), List(out1, out2))

    in.setSignal(false)
    c.setSignal(false)
    run

    assert(out1.getSignal === false, "demux 1 test out 1")
    assert(out2.getSignal === false, "demux 2 test out 2")

    in.setSignal(false)
    c.setSignal(true)
    run

    assert(out1.getSignal === false, "demux 3 test out 1")
    assert(out2.getSignal === false, "demux 4 test out 2")

    in.setSignal(true)
    c.setSignal(false)
    run

    assert(out1.getSignal === true, "demux 5 test out 1")
    assert(out2.getSignal === false, "demux 6 test out 2")

    in.setSignal(true)
    c.setSignal(true)
    run

    assert(out1.getSignal === false, "demux 7 test out 1")
    assert(out2.getSignal === true, "demux 8 test out 2")

  }

  test("demux medium") {
    val in, c0, c1, o0, o1, o2, o3 = new Wire
    val c = c1 :: c0 :: Nil
    val o = o0 :: o1 :: o2 :: o3 :: Nil
    demux(in, c, o)

    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === false, 3)

    in.setSignal(true)
    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === false, 2)
    assert(o3.getSignal === true, 3)

    in.setSignal(true)
    c0.setSignal(true)
    run
    assert(o0.getSignal === false, 0)
    assert(o1.getSignal === false, 1)
    assert(o2.getSignal === true, 2)
    assert(o3.getSignal === false, 3)
  }
}
