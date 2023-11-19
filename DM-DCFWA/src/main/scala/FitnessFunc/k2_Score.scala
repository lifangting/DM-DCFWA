package FitnessFunc

class k2_Score() {

  def fitnessFunction(selectSNPSet: Array[Int], data: Array[Array[Int]], label: Array[Int]): Double = {

    val D = selectSNPSet.length
    val m = math.pow(3.0, D).toInt
    val sampleNumber = data.length

    var caseobserved =new Array[Int](m)
    var caseexpected =new Array[Double](m)
    var controlobserved =new Array[Int](m)
    var controlexpected =new Array[Double](m)
    var k2:Double=0

    var y: Double = 0
    var r: Double = 0

    var casecount = 0
    var controlcount = 0

    for (i <- 0 to (sampleNumber-1)) {
      var sum=0
      for (j <- 0 to (D-1) ) {
        sum = sum*3 + data(i)(selectSNPSet(j))
      }
      if (label(i) == 1) {
        casecount = casecount + 1
        caseobserved(sum) = caseobserved(sum) + 1
      } else {
        controlcount = controlcount + 1
        controlobserved(sum) = controlobserved(sum) + 1
      }
    }

    for(i <- 0 to m - 1) {
      y = myFactorial(caseobserved(i) + controlobserved(i) + 1)
      r = myFactorial(caseobserved(i)) + myFactorial(controlobserved(i))
      k2 = k2 + (r - y)
    }

    Math.abs(k2)
  }

  def myFactorial(e: Int): Double = {
    var f: Double = 0

    if(e > 0) {
      for(i <- 1 to e) {
        f = f + Math.log(i)
      }
    }
    f
  }

}

