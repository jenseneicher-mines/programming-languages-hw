import org.scalatest.FlatSpec
import Parsing._

// unit tests for the Tree functions
class ParsingTest extends FlatSpec {
  "Lexer" should "properly evaluate simple strings" in {
    assert(Parsing.lexer("1.234truefalse cool \"one two\"123".toList) === List(NumToken("1.234"), KeywordToken("true"), KeywordToken("false"), StrToken("one two"), NumToken("123")))
    assert(Parsing.lexer("1.234 + 2 + 6 +0+1".toList) === List(NumToken("1.234"), OpToken("+"), NumToken("2"), OpToken("+"), NumToken("6"), OpToken("+"), NumToken("0"), OpToken("+"), NumToken("1")))
    assert(Parsing.lexer("1".toList) === List(NumToken("1")))
    assert(Parsing.lexer("\"string\"".toList) === List(StrToken("string")))
    assert(Parsing.lexer("\"string\"\"two\"".toList) === List(StrToken("string"), StrToken("two")))
    assert(Parsing.lexer("".toList) === List())

    assert(Parsing.lexer("false".toList) === List(KeywordToken("false")))
    assert(Parsing.lexer("2 / 4".toList) === List(NumToken("2"), OpToken("/"), NumToken("4")))
    assert(Parsing.lexer("garbage".toList) === List())
    assert(Parsing.lexer("true || false".toList) == List(KeywordToken("true"), OpToken("||"), KeywordToken("false")))
    assert(Parsing.lexer("\"String\" + 2".toList) === List(StrToken("String"), OpToken("+"), NumToken("2")))
    assert(Parsing.lexer("true && false".toList) == List(KeywordToken("true"), OpToken("&&"), KeywordToken("false")))
    assert(Parsing.lexer("true ! false".toList) == List(KeywordToken("true"), OpToken("!"), KeywordToken("false")))
  }

  "Parser" should "properly evaluate simple strings" in {
    assert(Parsing.parser(Parsing.lexer("1 + 2 + 3 * 5 + 6".toList)) === (Some(BopExpr(ConstIntExpr(1),PlusBop,BopExpr(ConstIntExpr(2),PlusBop,BopExpr(BopExpr(ConstIntExpr(3),TimesBop,ConstIntExpr(5)),PlusBop,ConstIntExpr(6))))),List()))
  }
  "Parser" should "properly evaluate simple strings with a double" in {
    assert(Parsing.parser(Parsing.lexer("1 + 4.16 + 3 * 5".toList)) === (Some(BopExpr(ConstIntExpr(1),PlusBop,BopExpr(ConstFloatExpr(4.16f),PlusBop,BopExpr(ConstIntExpr(3),TimesBop,ConstIntExpr(5))))),List()))
  }
  "Parser" should "properly evaluate simple strings with a division" in {
    assert(Parsing.parser(Parsing.lexer("4 / 2".toList)) === (Some(BopExpr(ConstIntExpr(4),DivBop,ConstIntExpr(2))),List()))
  }
  "Parser" should "properly evaluate simple strings with a true AND false" in {
    assert(Parsing.parser(Parsing.lexer("true && false".toList)) === (Some(BopExpr(ConstBoolExpr(true),AndBop,ConstBoolExpr(false))),List()))
  }
  "Parser" should "properly evaluate simple strings with a true OR false" in {
    assert(Parsing.parser(Parsing.lexer("true || false".toList)) === (Some(BopExpr(ConstBoolExpr(true),OrBop,ConstBoolExpr(false))),List()))
  }
  "Parser" should "properly evaluate simple strings with a NOT" in {
    assert(Parsing.parser(Parsing.lexer("true || !false".toList)) === (Some(BopExpr(ConstBoolExpr(true),OrBop,UopExpr(NotUop,ConstBoolExpr(false)))),List()))
  }
  "Parser" should "properly evaluate simple strings with a Negitive" in {
    assert(Parsing.parser(Parsing.lexer("1 + -6".toList)) === (Some(BopExpr(ConstIntExpr(1),PlusBop,UopExpr(NegUop,ConstIntExpr(6)))),List()))
  }
  "Parser" should "properly evaluate simple strings with a String" in {
    assert(Parsing.parser(Parsing.lexer("\"String\" + 2".toList)) === (Some(BopExpr(ConstStringExpr("String"),PlusBop,ConstIntExpr(2))),List()))
  }
}
