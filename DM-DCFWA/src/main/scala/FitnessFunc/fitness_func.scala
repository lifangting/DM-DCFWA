package main.scala.FitnessFunc


class fitness_func {


  def myFactorial(e: Int): Double = {
    var f: Double = 0

    if (e > 0) {
      for (i <- 1 to e) {
        f = f + Math.log(i)
      }
    }
    f
  }

  def log2(x:Double):Double ={
    return  Math.log(x) / Math.log(2)
  }


  def fitnessFunction(selectSNPSet: Array[Int], data: Array[Array[Int]], label: Array[Int]) = {

    val D = selectSNPSet.length
    val m = math.pow(3.0, D).toInt
    val sampleNumber = data.length

    var caseobserved = new Array[Int](m)
    var caseexpected = new Array[Double](m)
    var controlobserved = new Array[Int](m)
    var controlexpected = new Array[Double](m)
    var x2: Double = 0
    var k2: Double = 0


    var casecount = 0
    var controlcount = 0

    for (i <- 0 to (sampleNumber - 1)) {
      var sum = 0
      for (j <- 0 to (D - 1)) {
        sum = sum * 3 + data(i)(selectSNPSet(j))
      }
      if (label(i) == 1) {
        casecount = casecount + 1
        caseobserved(sum) = caseobserved(sum) + 1
      } else {
        controlcount = controlcount + 1
        controlobserved(sum) = controlobserved(sum) + 1
      }
    }

    for (k <- 0 to (m - 1)) {
      caseexpected(k) = (caseobserved(k) + controlobserved(k)) * casecount / sampleNumber
      controlexpected(k) = (caseobserved(k) + controlobserved(k)) * controlcount / sampleNumber
    }



    //1.计算卡方值
    //    for (i <- 0 to (m-1)) {
    //      if (caseexpected(i) != 0)
    //        x2 = x2 + (((caseexpected(i) - caseobserved(i)) * (caseexpected(i) - caseobserved(i))) / caseexpected(i))
    //    }
    //
    //    for (j <- 0 until m) {
    //      if (controlexpected(j) != 0)
    //        x2 = x2 + (((controlexpected(j) - controlobserved(j)) * (controlexpected(j) - controlobserved(j))) / controlexpected(j))
    //    }


    //2.计算BN k2值
    var y: Double = 0
    var r: Double = 0
    for (i <- 0 to m - 1) {
      y = myFactorial(caseobserved(i) + controlobserved(i) + 1)
      r = myFactorial(caseobserved(i)) + myFactorial(controlobserved(i))
      k2 = k2 + (r - y)
    }


    //3.计算互信息  I(X;Y)=H(Y)-H(Y|X)     H(Y|X)= sum_x sum_y  -p(x)p(y|x)logp(y|x)
    var Entroy_label = 0.0
    var p_case_label = casecount.toDouble / sampleNumber
    var p_control_label = controlcount.toDouble / sampleNumber
    var p_case = new Array[Double](m)
    var p_control = new Array[Double](m)

    Entroy_label = -(p_case_label * Math.log(p_case_label) + p_control_label * Math.log(p_control_label))

    var tmp = 0.0
    var MI = 0.0
    for (l <- 0 to m - 1) {
      if ((caseobserved(l) + controlobserved(l)) != 0) {
        p_case(l) = caseobserved(l).toDouble / (caseobserved(l) + controlobserved(l))
        p_control(l) = controlobserved(l).toDouble / (caseobserved(l) + controlobserved(l))
      } else {
        p_case(l) = 0
        p_control(l) = 0
      }
      if (p_case(l) != 0)
        tmp = tmp + p_case(l) * Math.log(p_case(l)) * caseobserved(l) / sampleNumber
      if (p_control(l) != 0)
        tmp = tmp + p_control(l) * Math.log(p_control(l)) * controlobserved(l) / sampleNumber

    }

    tmp = -tmp
    MI = Entroy_label - tmp



    (Math.abs(k2), -MI)
  }


}
