package RDD
//
//import java.lang.reflect.Array
//import java.util
import scala.collection.mutable.{ArrayBuffer, Set}
import scala.collection
import APP.BroadcastV
import FWA.{FWA, Individual}
import org.apache.spark.broadcast.Broadcast

import scala.collection.mutable
class PartFunc(broadcastv: Broadcast[BroadcastV]) extends Serializable {

  //获取广播变量
  val bv = broadcastv.value
  val part_Num = bv.part_Num
  val data = bv.data
  val label = bv.label
  val sample_Number = bv.sample_Number
  val snp_Number = bv.snp_Number

  def partfunc(index: Int, itera: Iterator[Int]) = {

    //提取part_Data
    val rc = new RangeControl().rangecontrol(snp_Number, part_Num, index)
    val split = rc._1//断点
    val partData_Index = rc._2//一个子空间数据
    val partData: Array[Array[Int]] = Array.ofDim[Int](sample_Number, partData_Index.length)

    for (i <- 0 to (sample_Number - 1)) {
      for (j <- 0 to (partData_Index.length - 1)) {
        partData(i)(j) = data(i)(partData_Index(j))
      }
    }

    //分别计算第一维，第二维的长度
    var dim1 = snp_Number / part_Num - 1
    if (index == (part_Num - 1)) {
      dim1 = split(part_Num - 1) - split(part_Num - 2) - 1
    }
    val dim2 = partData_Index.length - 1



    //开始进行烟花算法
    val time6=System.currentTimeMillis()
    var fitnessbest_array:ArrayBuffer[Individual] = new FWA().performFWA(partData, label, dim1, dim2,index)   //非支配解


    for(elem <- fitnessbest_array){
      elem.position(0)=partData_Index(elem.position(0))
      elem.position(1)=partData_Index(elem.position(1))
    }



    def fitnessandkey(individual: Individual)={
      (individual,index)
    }

    val tmp: ArrayBuffer[(Individual, Int)] = fitnessbest_array.map(fitnessandkey)
    tmp.toIterator
  }


}


