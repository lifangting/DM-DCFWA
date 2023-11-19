package main.scala.FitnessFunc

class js_Score {
  def fitnessFunction(selectSNPSet: Array[Int], data: Array[Array[Int]], label: Array[Int]): Double = {

    val D = selectSNPSet.length
    val m = math.pow(3.0, D).toInt
    val sampleNumber = data.length

    var casee =new Array[Int](m)
    var control =new Array[Int](m)
    var js:Double=0

    var casecount = 0
    var controlcount = 0
    var pcase = new Array[Double](m)
    var pcontrol = new Array[Double](m)

    for (i <- 0 to (sampleNumber-1)) {
      var sum=0
      for (j <- 0 to (D-1) ) {
        sum = sum*3 + data(i)(selectSNPSet(j))
      }
      if (label(i) == 1) {
        casecount = casecount + 1
        casee(sum) = casee(sum) + 1
      } else {
        controlcount = controlcount + 1
        control(sum) = control(sum) + 1
      }
    }

    for(i <- 0 to m - 1) {
      //      println("case = " + casee(i) + " control = " + control(i))
      if(casee(i) + control(i) != 0) {
        if(casee(i) == 0) {
          js = js + (control(i).toDouble / (casee(i) + control(i)).toDouble) * Math.log(2.0 * control(i).toDouble / (casee(i) + control(i)).toDouble)
        } else if(control(i) == 0) {
          js = js + (casee(i).toDouble / (casee(i) + control(i)).toDouble) * Math.log(2.0 * casee(i).toDouble / (casee(i) + control(i)).toDouble)
        } else {
          js = js + (casee(i).toDouble / (casee(i) + control(i)).toDouble) * Math.log(2.0 * casee(i).toDouble / (casee(i) + control(i)).toDouble) + (control(i).toDouble / (casee(i) + control(i)).toDouble) * Math.log(2.0 * control(i).toDouble / (casee(i) + control(i)).toDouble)
        }
        //        println(js)
      }
    }
    -js*0.5
  }
}
