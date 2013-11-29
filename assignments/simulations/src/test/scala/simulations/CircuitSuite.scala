package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

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
  }

  //
  // to complete with tests for orGate, demux, ...
  //
  test("inverter example") {
    val in1, out = new Wire
    inverter(in1, out)
    in1.setSignal(false)
    run

    assert(out.getSignal === true, "not 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === false, "not 2")

  }

  test("orGate example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  // EMD
  test("orGate2 example") {
    val in1, in2, out = new Wire
    orGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run

    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run

    assert(out.getSignal === true, "or 3")
  }

  // EMD
  // Truth table for a 1-4 mux
  // -------------------------
  //
  // in | c1  | c0  | o0  | o1  | o2 | o3
  // D  |  0  |  0  | D   | 0   | 0  | 0
  // D  |  0  |  1  | 0   | D   | 0  | 0
  // D  |  1  |  0  | 0   | 0   | D  | 0
  // D  |  1  |  1  | 0   | 0   | 0  | D
  //
  // http://www.allaboutcircuits.com/vol_4/chpt_9/6.html
  // http://www.wisc-online.com/objects/ViewObject.aspx?ID=DIG5704
  // http://upload.wikimedia.org/wikipedia/commons/thumb/1/15/Demultiplexer_Example01.svg/450px-Demultiplexer_Example01.svg.png
  //
  test("demux") {
    val in = new Wire
    val c0 = new Wire
    val c1 = new Wire
    val c = List(c0, c1)
    val o0 = new Wire
    val o1 = new Wire
    val o2 = new Wire
    val o3 = new Wire
    val out = List(o0, o1, o2, o3)
    demux(in, c, out)

    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(false)
    run
    assert(o0.getSignal === false)
    assert(o1.getSignal === false)
    assert(o2.getSignal === false)
    assert(o3.getSignal === true)

    in.setSignal(true)
    c0.setSignal(true)
    c1.setSignal(false)
    run
    assert(o0.getSignal === false)
    assert(o1.getSignal === true)
    assert(o2.getSignal === false)
    assert(o3.getSignal === false)

    in.setSignal(true)
    c0.setSignal(false)
    c1.setSignal(true)
    run
    assert(o0.getSignal === false)
    assert(o1.getSignal === false)
    assert(o2.getSignal === true)
    assert(o3.getSignal === false)

    in.setSignal(true)
    c0.setSignal(true)
    c1.setSignal(true)
    run
    assert(o0.getSignal === true)
    assert(o1.getSignal === false)
    assert(o2.getSignal === false)
    assert(o3.getSignal === false)

    in.setSignal(false)
    c0.setSignal(false)
    c1.setSignal(false)
    checkAllFalse(out)

    in.setSignal(false)
    c0.setSignal(true)
    c1.setSignal(false)
    checkAllFalse(out)

    in.setSignal(false)
    c0.setSignal(false)
    c1.setSignal(true)
    checkAllFalse(out)

    in.setSignal(false)
    c0.setSignal(true)
    c1.setSignal(true)
    checkAllFalse(out)
  }

  // EMD
  def checkAllFalse(out: List[Wire]) {
    run
    out.foreach(a => assert(a.getSignal === false))
  }

  // EMD
  // result checks output wire signal values (demux tests)
  def checkOutputSignalsGen(o: List[Wire]): (Int => Boolean) => Unit = {
    val olen = o.length
    val testName = "demux 1-to-" + olen
    def inner(expected: Int => Boolean): Unit = {
      def checkSignals(o: List[Wire], index: Int, expected: Int => Boolean): Unit = o match {
        case Nil => ()
        case head :: tail => {
          assert(head.getSignal === expected(index), testName + ", " + index)
          checkSignals(tail, index-1, expected)
        }
      }
      checkSignals(o, o.length-1, expected)
    }
    inner
  }

  // EMD
  // result updates control wire signals to match the index bit pattern (demux tests)
  def updateControlSignalsGen(c: List[Wire]): (Int => Unit) = {
    val outputCount = (1 << c.length)
    val highBit = outputCount >>> 1 // most significant binary digit, since we're working backwards
    val mask = outputCount-1 // masks out the bits higher than the high bit

    // gets the boolean value of the most significant bit
    def bitValue(bits: Int): Boolean = {
      (bits & highBit) != 0
    }
    def inner(index: Int): Unit = {
      // updates the control signals to match the bit configuration for the current index
      def updateSignals(c: List[Wire], delta: Int, bits: Int): Unit = {
        if ((delta & mask) != 0) {
          val head :: tail = c
          if (bitValue(delta)) {
            head.setSignal(bitValue(bits))
          }
          updateSignals(tail, delta << 1, bits << 1) // left-shift because we're working high-to-low
        }
      }
      val delta = index ^ (index - 1) // changed bits since last index
      updateSignals(c, delta, index)
    }
    inner
  }

  // EMD
  def demuxTest(controlCount: Int): Unit = {
    val outputCount = (1 << controlCount)
    val in = new Wire
    val c = (0 until controlCount).map(_ => new Wire).toList
    val o = (0 until outputCount).map(_ => new Wire).toList
    val checkOutput = checkOutputSignalsGen(o)
    val updateControls = updateControlSignalsGen(c)
    demux(in, c, o)

    // check initial state
    run
    checkOutput(Set())

    // check all other states (input on)
    in.setSignal(true)
    for (i <- 0 until outputCount) {
      // update the control signals
      updateControls(i)

      // run the simulation
      run

      // check the output signals
      checkOutput(Set(i))
    }

    // check all other states (input off)
    in.setSignal(false)
    for (i <- 0 until outputCount) {
      // update the control signals
      updateControls(i)

      // run the simulation
      run

      // check the output signals
      checkOutput(Set())
    }

    // check all other states (input on)
    in.setSignal(true)
    for (i <- 0 until outputCount) {
      // update the control signals
      updateControls(i)

      // run the simulation
      run

      // check the output signals
      checkOutput(Set(i))
    }
  }

  test("demux 1-to-256") {
    demuxTest(4)
  }

  // EMD
  test("demux1"){
    val nrC=5                        // nr of control wires
    val nrOW=math.pow(2,nrC).toInt   // nr of output wires

    val in=new Wire()
    val controlWires=listOfWires(nrC)    // control wires
    val outWires=listOfWires(nrOW)       // output wires

    demux(in,controlWires,outWires)     // setting up demux circuit

    for(outWire <- 0 to (nrOW-1)){      //check for all output Wires
      setControlWires(controlWires,outWire)   //set the control wires so that we get signal at given outWire
      in.setSignal(true)
      run
      val outShouldBe=getOutSignals(nrC,outWire)   //expected outcomes
      assert(outWires.zip(outShouldBe).forall{case(wire,b)=>(wire.getSignal==b)&in.getSignal})
    }
  }

  // EMD
  def setControlWires(cws:List[Wire],cSig:Int)=
    for(cw <- cws) cw.setSignal((cSig/(math.pow(2,cws.size-1-cws.indexOf(cw)).toInt)%2)==1)

  // EMD
  def getOutSignals(nrC:Int,cSig:Int):List[Boolean]=
    (for(i<-1 to math.pow(2,nrC).toInt) yield(i==cSig+1)).toList.reverse

  // EMD
  def listOfWires(nr:Int):List[Wire]=(for(i<-1 to nr) yield new Wire).toList

  // EMD
  test("1:16 demux") {
    val i, s3, s2, s1, s0 = new Wire
    val out = Range(0, 16).map((_) => new Wire)

    demux(i, List(s3, s2, s1, s0), out.reverse.toList)

    s3.setSignal(false)
    s2.setSignal(true)
    s1.setSignal(true)
    s0.setSignal(false)
    i.setSignal(true)
    run

    assert(out(6).getSignal === true)
    assert(out.filterNot(_ == out(6)).forall(!_.getSignal) === true)

    s3.setSignal(false)
    s2.setSignal(true)
    s1.setSignal(false)
    s0.setSignal(true)
    i.setSignal(true)
    run

    assert(out(5).getSignal === true)
    assert(out.filterNot(_ == out(5)).forall(!_.getSignal) === true)

    s3.setSignal(true)
    s2.setSignal(true)
    s1.setSignal(true)
    s0.setSignal(false)
    i.setSignal(true)
    run

    assert(out(14).getSignal === true)
    assert(out.filterNot(_ == out(14)).forall(!_.getSignal) === true)
  }
}
