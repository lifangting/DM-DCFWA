package APP

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class ReadData (path:String){
  def readdata() = {
    //读入文件
    val lines = Source.fromFile(path).getLines()

    //读取文件第一行,统计snp数量
    val firstline = lines.next()
    val snp_Names: Array[String] = firstline.split("\t")
    val snp_Number = snp_Names.length - 1
    println("SNP数量="+snp_Number)

    //统计样本数量,将数据存入二维数组
    val data = new ArrayBuffer[Array[Int]]()
    val label = new ArrayBuffer[Int]()

    while (lines.hasNext) {
      val line: Array[Int] = lines.next().split("\t").map(_.toInt)
      val line2: Array[Int] = new Array[Int](snp_Number)
      for (i <- 0 to (snp_Number - 1)) {
        line2(i) = line(i)
      }
      data.append(line2)
      label.append(line(snp_Number))
    }
    val sample_Number = data.length
    println("sample数量="+sample_Number)

    (snp_Number, sample_Number, data, label, snp_Names)

  } //方法结束

}

