package dsentric.amf

import monocle.Prism
import scodec.bits.ByteVector

/**
  * Created by Jamie Pullar on 19/12/2015.
  */
object Dsentric extends
  dsentric.AndMatcher {

  type Data = ByteVector

//  implicit val jsString =
//    Prism[Data, String]{
//      b =>
//        b.headOption.collect {
//          case 0x01 =>
//            b.get()
//        }
//    }{b =>
//      b.
//    }
}
