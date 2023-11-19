package FWA

//import FitnessFunc.{chi_Square, k2_Score}
import breeze.linalg.sum
import breeze.numerics.sqrt
import main.scala.FitnessFunc.fitness_func

import scala.collection.mutable
import scala.collection.mutable.Set
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class FWA_operator(fireworks: Array[Individual], fireworks_Number: Int, partdata: Array[Array[Int]], label: Array[Int], dim1: Int, dim2: Int, count: Array[Int]) {


  //1.计算每个烟花生成的火星数
  def sparksnum_cal(): Array[Int] = {
    val Max_Sparks_Num = 20

    //求火星数的数组
    val s = new Array[Int](fireworks_Number)
    for (i <- 0 to (fireworks_Number - 1)) {

      s(i) = Max_Sparks_Num * scala.math.pow(1.1, count(i)).toInt
    }
    s
  }


  //2.计算每个烟花生成的爆炸半径
  def sparkscope_cal(): Array[Int] = {

    //求烟花的爆炸半径
    val A = new Array[Int](fireworks_Number)
    for (r <- 0 to (fireworks_Number - 1)) {
      A(r) = 20 * scala.math.pow(1.1, count(r)).toInt

    }
    A
  }


  //3.火花的生成
  def spark_generateAndselect(sparknum_Array: Array[Int], sparksscope_Array: Array[Int], z1: Double, z2: Double, fwLambda: Array[Array[Double]], evaTime: Int,index:Int) = {

    //求生成的火花总数
    var evaTime_return = evaTime
    var Ep = new ArrayBuffer[Individual]()
    var sparks=new Array[ArrayBuffer[Individual]](fireworks_Number)


    for (i <- 0 to (fireworks_Number - 1)) { //对于每一个烟花
      var flag = new Array[Int](sparknum_Array(i))
      sparks(i)=new ArrayBuffer[Individual]()

      for (j <- 0 to (sparknum_Array(i) - 1)) { //对每一个火花
        var firework_position = new Array[Int](2)
        flag(j) = 1
        firework_position(0) = fireworks(i).position(0) //!!!!想复制数组，要一位一位复制，不能直接所有位一起
        firework_position(1) = fireworks(i).position(1)

        //选择变异的维度数
        val dimens_select = Random.nextInt(2) + 1
        if (dimens_select == 1) {
          val select = Random.nextInt(2)
          val offset = ((Random.nextDouble() * 2 - 1) * sparksscope_Array(i)).toInt
          firework_position(select) = firework_position(select) + offset
        }
        if (dimens_select == 2) {
          val offset0 = ((Random.nextDouble() * 2 - 1) * sparksscope_Array(i)).toInt
          val offset1 = ((Random.nextDouble() * 2 - 1) * sparksscope_Array(i)).toInt
          firework_position(0) = firework_position(0) + offset0
          firework_position(1) = firework_position(1) + offset1
        }

        //控制变异范围在定义域内
        if (firework_position(0) > dim1 || firework_position(0) < 0) {
          //          firework_position(0) = Random.nextInt(dim1 + 1)
          if (firework_position(0) > dim1) {
            firework_position(0) = dim1 - firework_position(0) % dim1
          } else {
            firework_position(0) = (-1) * firework_position(0) % dim1
          }
        }
        if (firework_position(1) > dim2 || firework_position(1) < 0) {
          //          firework_position(1) = Random.nextInt(dim2 + 1)
          if (firework_position(1) > dim2) {
            firework_position(1) = dim2 - firework_position(1) % dim2
          } else {
            firework_position(1) = (-1) * firework_position(1) % dim2
          }
        }

        //该烟花的所有火花存在sparks中
        var f1=0.0
        var f2=0.0
        val f=new fitness_func().fitnessFunction(firework_position, partdata, label)
        f1=f._1
        if(firework_position(0)==firework_position(1)){
          f2=0
        }else{
          f2=f._2
        }
        sparks(i) += Individual(firework_position, f1, f2)
        evaTime_return = evaTime_return + 1



      } //一个火花结束


      //      //更新best_f1,best_f2
      //      sparks(i)=sparks(i).sortBy(a=>a.fitnessScore1)
      //      var mm=0
      //      var nn=9
      //      var flag3=0
      //      while(flag3==0){
      //        if(sparks(i)(mm).fitnessScore1<best_f1(nn).fitnessScore1) {
      //          best_f1(nn).position(0) = sparks(i)(mm).position(0)
      //          best_f1(nn).position(1) = sparks(i)(mm).position(1)
      //          best_f1(nn).fitnessScore1 = sparks(i)(mm).fitnessScore1
      //          best_f1(nn).fitnessScore2 = sparks(i)(mm).fitnessScore2
      //        }else
      //          flag3=1
      //        nn=nn-1
      //        mm=mm+1
      //        if(nn==0)
      //          flag3=1
      //      }
      //
      //
      //      sparks(i)=sparks(i).sortBy(a=>a.fitnessScore2)
      //      var aa=0
      //      var bb=9
      //      var flag4=0
      //      while(flag4==0){
      //        if(sparks(i)(aa).fitnessScore2<best_f2(bb).fitnessScore2) {
      //          best_f2(bb).position(0) = sparks(i)(aa).position(0)
      //          best_f2(bb).position(1) = sparks(i)(aa).position(1)
      //          best_f2(bb).fitnessScore1 = sparks(i)(aa).fitnessScore1
      //          best_f2(bb).fitnessScore2 = sparks(i)(aa).fitnessScore2
      //        }else
      //          flag4=1
      //        bb=bb-1
      //        aa=aa+1
      //        if(bb==0)
      //          flag4=1
      //      }






      //对一个烟花的所有火花进行非支配排序
      for (p <- 0 to (sparks(i).length - 2)) {
        for (q <- (p + 1) to (sparks(i).length - 1)) {
          //          if(flag2(p)!= 0 && flag2(q)!= 0 ) {
          if (isDominate(sparks(i)(p), sparks(i)(q))) { //p支配q
            flag(q) = 0;
          }
          if (isDominate(sparks(i)(q), sparks(i)(p))) { //p支配q
            flag(p) = 0;
          }
          //          }
        }
      }


      for (p <- (0 to sparks(i).length - 1).reverse) {
        if(flag(p)==0)
          sparks(i).remove(p)
        else
        {
          Ep += sparks(i)(p)
        }
      }
      sparks(i) += fireworks(i)

    } //一个烟花结束


    //对Ep非支配排序
    var flag2=new Array[Int](Ep.length)
    for(m <- 0 to Ep.length-1)
      flag2(m)=1

    if (Ep.length > 1) {
      for (p <- 0 to (Ep.length - 2)) {
        for (q <- (p + 1) to (Ep.length - 1)) {
          //          if(flag2(p)!= 0 && flag2(q)!= 0 ) {

          if (isDominate(Ep(p), Ep(q))) { //p支配q
            flag2(q) = 0;
          } else if (Ep(p).position(0) == Ep(q).position(0) && Ep(p).position(1) == Ep(q).position(1)) {
            flag2(q) = 0;
          }
          if (isDominate(Ep(q), Ep(p))) { //p支配q
            flag2(p) = 0;
          }
          //          }

        }
      }
    }

    for (p <- (0 to Ep.length - 1).reverse) {
      if(flag2(p)==0)
        Ep.remove(p)
    }

    //计算z
    var minz1 = z1
    var minz2 = z2
    for (elem <- Ep) {
      if (elem.fitnessScore1 < minz1) {
        minz1 = elem.fitnessScore1
      }

      if (elem.fitnessScore2 < minz2) {
        minz2 = elem.fitnessScore2
      }
    }


    //更新每个烟花
    var flag = 0
    for (i <- 0 to fireworks_Number - 1) {

      var spMax1 = Math.abs(sparks(i)(0).fitnessScore1 - minz1) * fwLambda(i)(0)
      var spMax2 = Math.abs(sparks(i)(0).fitnessScore2 - minz2) * fwLambda(i)(1)
      var spMax = Math.max(spMax1, spMax2)
      var b=sparks(i)(0)

      for (j <- 0 to sparks(i).length - 1) {

        spMax1 = Math.abs(sparks(i)(j).fitnessScore1 - minz1) * fwLambda(i)(0)
        spMax2 = Math.abs(sparks(i)(j).fitnessScore2 - minz2) * fwLambda(i)(1)
        if(Math.max(spMax1, spMax2)<spMax) {
          b = sparks(i)(j)
          spMax=Math.max(spMax1, spMax2)
        }

      }
      //      b=sparks(i)(Random.nextInt(sparks(i).length))


      //      var index1=0
      //      var min1=sparks(i)(0).fitnessScore1
      //      for(l <-(1 to sparks(i).length-1)){
      //        if(sparks(i)(l).fitnessScore1<min1) {
      //          min1 = sparks(i)(l).fitnessScore1
      //          index1 = l
      //        }
      //      }
      //
      //      var index2=0
      //      var min2=sparks(i)(0).fitnessScore2
      //      for(l <-(1 to sparks(i).length-1)){
      //        if(sparks(i)(l).fitnessScore2<min2) {
      //          min2 = sparks(i)(l).fitnessScore2
      //          index2 = l
      //        }
      //      }
      //      var u=Random.nextInt(2)
      //      if(u==0)
      //        b=sparks(i)(index1)
      //      else
      //        b=sparks(i)(index2)


      if (b.position(0) == fireworks(i).position(0) && b.position(1) == fireworks(i).position(1)) {
        count(i) = count(i) + 1
        if (count(i) == 4) {
          val a = Array(Random.nextInt(dim1), Random.nextInt(dim2))
          val f=new fitness_func().fitnessFunction(a, partdata, label)
          b = Individual(a, f._1, f._2)
          count(i) = 0
        }
      } else {
        count(i) = 0
      }

      fireworks(i) = b


    }


    (minz1, minz2, evaTime_return, fireworks, count, Ep)
  }




  def isDominate(x: Individual, y: Individual) = {
    //x支配y
    var res = false
    if (x.fitnessScore1 <= y.fitnessScore1 && x.fitnessScore2 <= y.fitnessScore2 && (x.fitnessScore1 < y.fitnessScore1 || x.fitnessScore2 < y.fitnessScore2)) {
      res = true
    }
    res
  }


}



