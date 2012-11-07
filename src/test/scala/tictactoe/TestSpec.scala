package tictactoe

import org.specs2.mutable.Specification

case class Irs()
case class Fra()
class ModelsTest extends Specification {

  val irsSpec = new XmlSerializationSpecification[Irs] with Specification {

    "IRS xml serialization specification".title
    def klazz = classOf[Irs]
    def input = Irs()
    def expectedXml = """<irs maturity="18" rate="2.5" lastUpdate="0"/>"""
    addFragments()
  }

  val fraSpec = new XmlSerializationSpecification[Fra] with Specification {
    "FRA xml serialization specification".title
    def klazz = classOf[Fra]
    def input = Fra()
    def expectedXml = """<fra start="6" maturity="18" imm="false" rate="2.5" lastUpdate="0"/>"""
    addFragments()
  }

  "The models specifications " ^
    irsSpec ^
    fraSpec
}

trait XmlSerializationSpecification[T] {  self: Specification =>

  def klazz: Class[T]
  def input: T
  def expectedXml: String

  final lazy val xmlSerializationTest = ok

  def addFragments() = {
    "The xml serialization of " + klazz.getSimpleName should{
      "work bidirectionally" in {
        xmlSerializationTest
      }
      "work with the expected data format" in {
        xmlSerializationTest
      }
    }
  }
}

