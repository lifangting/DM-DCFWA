import java.io.{File, FileWriter, PrintWriter}

import APP.{BroadcastV, ReadData}
import FWA.Individual
import RDD.{PartFunc, RangeControl}
import breeze.numerics.sqrt
import org.apache.spark.rdd.RDD
import org.apache.spark.{SparkConf, SparkContext}

import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ArrayBuffer

object DMDCFWA {

  def main(args: Array[String]): Unit = {

    val sparkConf = new SparkConf().setAppName("DMDCFWA")
    sparkConf.setMaster("spark://master:7077")
//    sparkConf.setMaster("local")
    val sc = new SparkContext(sparkConf)


    //参数设置
    val model_Number = 6
    val dataset_Number = 50
    val part_Num = 8
    var right = 0
    var timeave = 0.0
    var timestore = new Array[Double](dataset_Number)
    var file = new File("/home/lft/code/rightNumber.txt")
//    var file = new File("D:\\SNP_datasets\\rightNumber.txt")
    var filewriter = new FileWriter(file, true)
    val printwriter = new PrintWriter(filewriter)



    for (model_index <- 1 to model_Number) {
      var rightNum = 0
      val time1 = System.currentTimeMillis()
      for (dataset_index <- 1 to dataset_Number) {
        val time3 = System.currentTimeMillis()
        //处理数据集名称
        var dataset_name: String = ""
        if (dataset_index < 10) {
          dataset_name = "model" + model_index + "_EDM-1_0" + dataset_index + ".txt"
        } else if (dataset_index < 50) {
          dataset_name = "model" + model_index + "_EDM-1_" + dataset_index + ".txt"
        }
        val path = "/home/code/lft/NMF1000SNP4000SAM/model" + model_index + "_EDM-1/" + dataset_name
//        val path = "D:\\SNP_datasets\\NMF1000SNP4000SAM\\model" + model_index + "_EDM-1/" + dataset_name
        println(path)

        //读取数据集，进行预处理
        val tup = new ReadData(path).readdata
        val snp_number = tup._1
        val sample_Number = tup._2
        val data: Array[Array[Int]] = tup._3.toArray
        val label: Array[Int] = tup._4.toArray
        val snp_Names: Array[String] = tup._5

        //分区
        val arr: Array[Int] = (1 to part_Num).toArray
        val rdd: RDD[Int] = sc.parallelize(arr, part_Num)

        //广播变量
        val bv = BroadcastV(part_Num, data, label, snp_number, sample_Number)
        val broadcastv = sc.broadcast(bv)

        //开始并行计算
        val PartBest = rdd.mapPartitionsWithIndex(new PartFunc(broadcastv).partfunc)

        //结果收集
        val bestCollect: Array[(Individual, Int)] = PartBest.collect()
        var bestCollectBuffer: ArrayBuffer[(Individual, Int)] = bestCollect.toBuffer.asInstanceOf[ArrayBuffer[(Individual, Int)]]


        //结果处理,对Ep非支配排序
        var flag2=new Array[Int](bestCollectBuffer.length)
        for(m <- 0 to bestCollectBuffer.length-1)
          flag2(m)=1


        for (p <- 0 to (bestCollectBuffer.length - 2)) {
          for (q <- (p + 1) to (bestCollectBuffer.length - 1)) {

            if (isDominate(bestCollectBuffer(p)._1, bestCollectBuffer(q)._1)) { //p支配q
              flag2(q) = 0;
            } else if (bestCollectBuffer(p)._1.position(0) == bestCollectBuffer(q)._1.position(0) && bestCollectBuffer(p)._1.position(1) == bestCollectBuffer(q)._1.position(1)) {
              flag2(q) = 0;
            }
            if (isDominate(bestCollectBuffer(q)._1, bestCollectBuffer(p)._1)) { //p支配q
              flag2(p) = 0;
            }

          }
        }
        for (p <- (0 to bestCollectBuffer.length - 1).reverse) {
          if(flag2(p)==0)
            bestCollectBuffer.remove(p)
        }


        for (elem <- bestCollectBuffer) {
          println("res://" + elem._1.position.toList + "k2 = " + elem._1.fitnessScore1 + ", mi = " + elem._1.fitnessScore2)
        }


        var flag=0
        for (elem <- bestCollectBuffer) {
          if(flag==0&&((elem._1.position(0) == snp_number - 1 && elem._1.position(1) == snp_number - 2) || (elem._1.position(0) == snp_number - 2 && elem._1.position(1) == snp_number - 1))) {
            rightNum += 1
            flag=1
          }
        }




        printwriter.println("model" + model_index + " dataset" + dataset_index +  " rightNum = " + rightNum)
        printwriter.flush()
        println("rightNum = " + rightNum)


        val time4 = System.currentTimeMillis()
        println("一个数据集时间为：" + (time4 - time3))
        printwriter.println("一个数据集时间为：" + (time4 - time3))

      }

      val time2 = System.currentTimeMillis()
      println("一个模型时间为：" + (time2 - time1))
      printwriter.println("一个模型的总时间为：" + (time2 - time1))

      printwriter.flush()
    }
    printwriter.close()
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
