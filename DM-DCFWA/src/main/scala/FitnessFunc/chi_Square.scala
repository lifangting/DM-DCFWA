package FitnessFunc

class chi_Square {

  def fitnessFunction(selectSNPSet: Array[Int], data: Array[Array[Int]], label: Array[Int]): Double = {

    val D = selectSNPSet.length
    val m = math.pow(3.0, D).toInt
    val sampleNumber = data.length

    var caseobserved =new Array[Double](m)
    var caseexpected =new Array[Double](m)
    var controlobserved =new Array[Double](m)
    var controlexpected =new Array[Double](m)
    var x2:Double=0

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

    for (k <- 0 to (m-1) ) {
      caseexpected(k) = (caseobserved(k) + controlobserved(k)) * casecount /sampleNumber
      controlexpected(k) = (caseobserved(k) + controlobserved(k)) * controlcount / sampleNumber
    }



    //1.计算卡方值
    for (i <- 0 to (m-1)) {
      if (caseexpected(i) != 0)
        x2 = x2 + (((caseexpected(i) - caseobserved(i)) * (caseexpected(i) - caseobserved(i))) / caseexpected(i))
    }

    for (j <- 0 until m) {
      if (controlexpected(j) != 0)
        x2 = x2 + (((controlexpected(j) - controlobserved(j)) * (controlexpected(j) - controlobserved(j))) / controlexpected(j))
    }



    -x2
  }
}

