package FWA

import java.util

//import FitnessFunc.{chi_Square, k2_Score}
import breeze.numerics.sqrt
//import main.scala.FitnessFunc.js_Score
import  main.scala.FitnessFunc.fitness_func
import scala.collection.mutable.{ArrayBuffer, Set}
import scala.util.Random

class FWA {
  def performFWA(partData: Array[Array[Int]], label: Array[Int], dim1: Int, dim2: Int,index:Int)={

    val fireworks_Number = 50
    val maxEva = 150000
    var z1=9999.0;
    var z2=9999.0;
    var evaTime = 0

    //均匀初始化切比雪夫方法的lambda
    var fwLambda = Array.ofDim[Double](fireworks_Number, 2)
    for (i <- 0 to fireworks_Number - 1) {
      fwLambda(i)(0) = i.toDouble / (fireworks_Number - 1).toDouble
      fwLambda(i)(1) = (fireworks_Number - 1 - i).toDouble / (fireworks_Number - 1).toDouble
    }


    //region 初始化烟花
    var fireworks = new Array[Individual](fireworks_Number)
    val S_len: Int = dim2 / dim1 //Snum
    val h: Int = sqrt(fireworks_Number / S_len).toInt
    val dim1_piece = new Array[Int](h + 1)
    val dim2_piece = new Array[Int](h * S_len + 1)
    val piece: Int = dim1 / h //derta
    var fit1=0.0
    var fit2=0.0
    dim1_piece(0) = 0
    dim2_piece(0) = 0
    dim1_piece(h) = dim1
    dim2_piece(h * S_len) = dim2
    for (g <- 1 to h - 1) {
      dim1_piece(g) = dim1_piece(g - 1) + piece
    }
    for (w <- 1 to h * S_len - 1) {
      dim2_piece(w) = dim2_piece(w - 1) + piece
    }
    for (z <- 0 to h - 1) {
      for (x <- 0 to h * S_len - 1) {
        val r1 = dim1_piece(z) + Random.nextInt(dim1_piece(z + 1) - dim1_piece(z) + 1)
        val r2 = dim2_piece(x) + Random.nextInt(dim2_piece(x + 1) - dim2_piece(x) + 1)
        val position = Array(r1, r2)
        val f=new fitness_func().fitnessFunction  (position, partData, label)
        fit1 = f._1
        fit2 = f._2
        if(r1==r2)
          fit2=0
        fireworks(z * h * S_len + x) = new Individual(position, fit1, fit2)
        if (fit1 < z1) {
          z1=fit1
        } else
        if (fit2 < z2) {
          z2=fit2
        }
        evaTime = evaTime + 1
      }
    }
    for (t <- h * h * S_len to fireworks_Number - 1) {
      val rand1 = Random.nextInt(dim1 + 1)
      val rand2 = Random.nextInt(dim2 + 1)
      val position = Array(rand1, rand2)
      val f=new fitness_func().fitnessFunction  (position, partData, label)
      var fitness1 = f._1
      var fitness2 = f._2
      if(rand1==rand2)
        fitness2=0
      fireworks(t) = new Individual(position, fitness1, fitness2)
      if (fit1 < z1) {
        z1=fit1
      } else
      if (fit2 < z2) {
        z2=fit2
      }
      evaTime = evaTime + 1
    }
    //endregion 初始化烟花





    //region 大循环
    var iter = 0
    var count = new Array[Int](fireworks_Number)
    var dominates: ArrayBuffer[Individual] = new ArrayBuffer[Individual]()

    while (evaTime < maxEva) {
      val fwa_op = new FWA_operator(fireworks, fireworks_Number, partData, label, dim1, dim2, count)
      val s = fwa_op.sparksnum_cal()
      val A = fwa_op.sparkscope_cal()

      val tup1 = fwa_op.spark_generateAndselect(s, A, z1, z2, fwLambda, evaTime,index)

      z1 = tup1._1
      z2 = tup1._2
      evaTime = tup1._3
      fireworks = tup1._4
      count = tup1._5

      for(ele <-tup1._6)
        dominates+=ele

      var flag2=new Array[Int](dominates.length);
      for( m<- 0 to dominates.length-2)
        for( n<- m+1 to dominates.length-1){
          if(isDominate(dominates(m),dominates(n))){
            flag2(n)= 2
          }else if(dominates(m).position(0)==dominates(n).position(0)&&dominates(m).position(1)==dominates(n).position(1))
            flag2(n)= 2
          if(isDominate(dominates(n),dominates(m))){
            flag2(m)= 2
          }
        }

      for( u <- (0 to flag2.length-1).reverse)
        if(flag2(u)==2)
          dominates.remove(u)


      //      val dominates_size = dominates.size
      //      var flag=true
      //      var flag2=new Array[Int](dominates_size);
      //      var v=0
      //
      //      if(dominates_size==0)
      //        dominates=tup1._6
      //      else {
      //        for (i <- 0 to tup1._6.length - 1) {
      //          v=0
      //          while (flag && v < dominates_size) {
      //            if ((tup1._6(i).position(0) == dominates(v).position(0)) && (tup1._6(i).position(1) == dominates(v).position(1))) {
      //              flag = false
      //            }
      //            if(isDominate(dominates(v),tup1._6(i)))
      //              flag = false
      //            if(isDominate(tup1._6(i),dominates(v)))
      //              flag2(v)= -1
      //            v = v + 1
      //            if (v == dominates_size)
      //              dominates += tup1._6(i)
      //          }
      //        }
      //      }

      //
      //      for(u <- (0 to dominates_size-1).reverse)
      //        if(flag2(u)== -1)
      //          dominates.remove(u)


      //      //去重
      //      for (p <- 0 to dominates.length - 2) {
      //        for (q <- ((p + 1) to (dominates.length - 1)).reverse) {
      //          if(dominates(p).position(0) == dominates(q).position(0) && dominates(p).position(1) == dominates(q).position(1)) {
      //            dominates.remove(q)
      //          }
      //        }
      //      }



      iter = iter + 1

    }


    dominates


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

