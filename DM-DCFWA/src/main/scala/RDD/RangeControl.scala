package RDD

class RangeControl {
  def rangecontrol(snp_Number: Int, part_Num: Int, part_Index: Int) = {
    //                 100           6            第0个分区

    //计算snp块的切点
    val tmp = snp_Number / part_Num //16
    val split = new Array[Int](part_Num) // [15,31,47,63,79,99]
    for (i <- 0 to (part_Num - 1)) {
      if (i != part_Num - 1) {
        split(i) = tmp * (i + 1) - 1
      } else {
        split(i) = snp_Number - 1
      }
    }

    var set1 = List[Int]()
    //**********（1）set1，奇数偶数都一样***********************
    if (part_Index == 0) {
      set1 = (0 to split(0)).toList
    } else {
      set1 = ((split(part_Index - 1) + 1) to split(part_Index)).toList
    }

    //**********（2)set2,奇数偶数不一样***********************
    var set2 = List[Int]()
    var nowPIndex = part_Index

    //1.当节点为奇数时
    if (part_Num % 2 == 1) {
      val otherPnum = (part_Num - 1) / 2
      for (j <- 1 to otherPnum) {
        var u = (nowPIndex + 1) % part_Num
        nowPIndex = u

        if (u == 0) {
          set2 = set2 ::: (0 to split(0)).toList///:::表示两个List的连接
        } else {
          set2 = set2 ::: (((split(u - 1) + 1) to split(u)).toList)
        }
      }
    }

    //当节点为偶数时
    else {
      var nowPIndex2 = part_Index
      var otherPnum0 = 0
      if (nowPIndex2 <= (part_Num / 2 - 1)) {
        otherPnum0 = part_Num / 2
      } else {
        otherPnum0 = part_Num / 2 - 1
      }

      for (j <- 1 to otherPnum0) {
        var d = (nowPIndex2 + 1) % part_Num
        nowPIndex2 = d
        //        otherPIndex=otherPIndex :+ d

        if (d == 0) {
          set2 = set2 ::: (0 to split(0)).toList
        } else {
          set2 = set2 ::: ((split(d - 1) + 1 to split(d)).toList)
        }
      }
    }

    val allset = (set1 ::: set2).toArray
    (split,allset)

  }
}


