//package dsentric.performance
//
//import dsentricTests.J._
//import dsentric.ObjectOps
//import org.scalatest.{FunSuite, Matchers}
//
//
//class StdTests  extends FunSuite with Matchers with ObjectOps {
//
//  test("Deserializing") {
//    val innerObject = ((1 to 100).map(_.toString) zip (1 to 100)).toMap[String, Any]
//    val secondInnerObject = ((1 to 32).map(i => (i * 3).toString) zip (1 to 16) ++ (17 to 32).map(_ => null)).toMap[String, Any]
//
//
//    val json= StdTestObj.$create {s =>
//      import dsentric.std.Dsentric._
//      s.triple.$set(("string", 1, false)) ~
//      s.nested.$set(innerObject)
//    }
//
//    object run {
//      val once = true
//      val thousand = true
//    }
//
//
//    if(run.once) {
//      Timer.print("match once") {
//        json match {
//          case StdTestObj.triple(s, i, b) =>
//            (s, i, b)
//        }
//        json match {
//          case StdTestObj.nested(m) =>
//            m
//        }
//      }
//    }
//
//    if(run.thousand) {
//      Timer.print("match 1000 times") {
//        (1 to 1000).foreach { _ =>
//          json match {
//            case StdTestObj.triple(s, i, b) =>
//              (s, i, b)
//          }
//          json match {
//            case StdTestObj.nested(m) =>
//              m
//          }
//        }
//      }
//    }
//
//    if(run.once) {
//      Timer.print("match raw once") {
//        json.asInstanceOf[Map[String, Any]]("string") match {
//          case s: String =>
//            s
//        }
//
//
//        json.asInstanceOf[Map[String, Any]]("bool") match {
//          case b: Boolean =>
//            b
//        }
//
//        json.asInstanceOf[Map[String, Any]]("int") match {
//          case i: Int =>
//            i
//        }
//        json.asInstanceOf[Map[String, Any]]("nested").asInstanceOf[Map[String, Any]] match {
//          case m: Map[String, Any] =>
//            m
//        }
//      }
//    }
//
//    if(run.thousand) {
//      Timer.print("match raw 1000 times") {
//        (1 to 1000).foreach { _ =>
//          json.asInstanceOf[Map[String, Any]]("string") match {
//            case s: String =>
//              s
//          }
//
//          json.asInstanceOf[Map[String, Any]]("bool") match {
//            case b: Boolean =>
//              b
//          }
//
//          json.asInstanceOf[Map[String, Any]]("int") match {
//            case i: Int =>
//              i
//          }
//          json.asInstanceOf[Map[String, Any]]("nested").asInstanceOf[Map[String, Any]] match {
//            case m: Map[String, Any] =>
//              m
//          }
//        }
//      }
//    }
//
//    if(run.once) {
//      Timer.print("nested map once") {
//        import dsentric.std.Dsentric._
//        nestedMap[Int, Int, Any, Map[String, Any]](json) {
//          case i => i + 1
//        }
//      }
//    }
//
//    if(run.thousand) {
//      Timer.print("nested map 1000 times") {
//        import dsentric.std.Dsentric._
//        (1 to 1000).foreach { _ =>
//          nestedMap[Int, Int, Any, Map[String, Any]](json) {
//            case i => i + 1
//          }
//        }
//      }
//    }
//
//    if(run.once) {
//      Timer.print("nested map raw once") {
//        json.asInstanceOf[Map[String, Any]] + ("nested" ->
//          json.asInstanceOf[Map[String, Any]]("nested").asInstanceOf[Map[String, Any]].map {
//            case (s, i: Int) => s -> (i + 1)
//            case kv => kv
//          })
//      }
//    }
//
//    if(run.thousand) {
//      Timer.print("nested map raw 1000 times") {
//        (1 to 1000).foreach { _ =>
//          json.asInstanceOf[Map[String, Any]] + ("nested" ->
//            json.asInstanceOf[Map[String, Any]]("nested").asInstanceOf[Map[String, Any]].map {
//              case (s, i: Int) => s -> (i + 1)
//              case kv => kv
//            })
//        }
//      }
//    }
//
//    if(run.once) {
//      Timer.print("apply delta once") {
//        import dsentric.std.Dsentric._
//        applyDelta[Any, Map[String, Any]](json, Map("nested" -> secondInnerObject), Some(null))
//      }
//    }
//
//    if(run.thousand) {
//      Timer.print("apply delta 1000 times") {
//        import dsentric.std.Dsentric._
//        (1 to 1000).foreach { _ =>
//          applyDelta[Any, Map[String, Any]](json, Map("nested" -> secondInnerObject), Some(null))
//        }
//      }
//    }
//
//    def rawApplyDelta(current:Map[String, Any], delta: Map[String, Any], deleteDelta:Option[Any]): Map[String,Any] = {
//      def deltaRecurse(target: Map[String, Any], delta: Map[String, Any]): Map[String, Any] = {
//        delta.foldLeft(target){
//          case (acc, (k, v)) if deleteDelta.contains(v) =>
//            acc - k
//          case (acc, (k, v)) =>
//            v match {
//              case deltaInnerMap:Map[String @unchecked, Any @unchecked] =>
//                current.get(k) match {
//                  case maybeCurrentInnerMap:Option[Map[String @unchecked, Any @unchecked] @unchecked] =>
//                    maybeCurrentInnerMap match {
//                      case Some(currentInnerMap) =>
//                        acc + (k -> deltaRecurse(currentInnerMap, deltaInnerMap))
//                      case None =>
//                        acc + (k -> deltaInnerMap)
//                    }
//                  case other =>
//                    acc + (k -> deltaInnerMap)
//                }
//              case other =>
//                acc + (k -> other)
//            }
//        }
//      }
//      deltaRecurse(current, delta)
//    }
//
//    if(run.once) {
//      Timer.print("apply delta raw once") {
//        rawApplyDelta(json.asInstanceOf[Map[String, Any]], Map("nested" -> secondInnerObject), Some(null))
//      }
//    }
//
//    if(run.thousand) {
//      Timer.print("apply delta raw 1000 times") {
//        (1 to 1000).foreach { _ =>
//          rawApplyDelta(json.asInstanceOf[Map[String, Any]], Map("nested" -> secondInnerObject), Some(null))
//        }
//      }
//    }
//  }
//
//}