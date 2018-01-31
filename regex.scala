
object Regex {

abstract class Rexp
case object ZERO extends Rexp
case object ONE extends Rexp
case class CHAR(c: Char) extends Rexp
case class ALT(r1: Rexp, r2: Rexp) extends Rexp   // alternative 
case class SEQ(r1: Rexp, r2: Rexp) extends Rexp   // sequence
case class STAR(r: Rexp) extends Rexp             // star


// some convenience for typing in regular expressions

import scala.language.implicitConversions    
import scala.language.reflectiveCalls 

def charlist2rexp(s: List[Char]): Rexp = s match {
  case Nil => ONE
  case c::Nil => CHAR(c)
  case c::s => SEQ(CHAR(c), charlist2rexp(s))
}
implicit def string2rexp(s: String): Rexp = charlist2rexp(s.toList)

implicit def RexpOps (r: Rexp) = new {
  def | (s: Rexp) = ALT(r, s)
  def % = STAR(r)
  def ~ (s: Rexp) = SEQ(r, s)
}

implicit def stringOps (s: String) = new {
  def | (r: Rexp) = ALT(s, r)
  def | (r: String) = ALT(s, r)
  def % = STAR(s)
  def ~ (r: Rexp) = SEQ(s, r)
  def ~ (r: String) = SEQ(s, r)
}


//testing 

def main( args: Array[String]){
	val EVIL = (a*)*b
	// val EVIL = SEQ(STAR(STAR(CHAR('a') )), CHAR('b'))
	//println(   ders(List.fill(5)('a'),EVIL))


	val EVIL = SEQ(STAR(STAR(CHAR('a'))), CHAR('b'))

println(matcher(EVIL, "a" * 1000 ++ "b")   == true)
println(matcher(EVIL, "a" * 1000)          == false)


	println(matcher(("a" ~ "b") ~ "c", "abc"))
	println(matcher(("a" ~ "b") ~ "c", "ab"))
 	
	//println( simp(ONE ~ (ONE ~ (ONE ~ CHAR('a')))))
 	//println(   simp(CHAR('a') | CHAR('a')) )
 	//println(der('b', STAR(CHAR('a'))) )

} 


//  checks whether a regular expression can match the 
// empty string and Returns a boolean accordingly

def nullable (r: Rexp) : Boolean = (r) match {
	case CHAR(_) => false
	case ALT(r1, r2) => (nullable(r1) || nullable(r2))
	case SEQ(r1, r2) => (nullable(r1) && nullable(r2))
	case STAR(_) => true
	case ZERO => false
	case ONE => true
	case _ => false
}


// calculates the derivative of a regular expression

def der (c: Char, r: Rexp) : Rexp = ( r) match {
	case ZERO => ZERO
	case ONE => ZERO
	case CHAR(d) => {
			if(c == d){
				ONE
			}
			else{
				ZERO
			}
		}
	case ALT(r1, r2) =>  der(c, r1) | der(c, r2)
	case SEQ(r1, r2) => {
			if( nullable(r1)){
				(der(c, r1) ~ r2) | (der(c, r2)) 
			}
			else{
				(der(c,r1) ~ r2)
			}
		}
	case STAR(rs) => (der(c, rs)~  r) 
}


//  simplifies a regular expression from  the inside out

	def simp(r: Rexp) : Rexp = {
		val fin = simploop(r)
		val simpf = simploop(fin) 
		if(fin != simpf){
			( simp(fin))
		}
		else{
		(fin)
		}

	}

	def simploop(r: Rexp) : Rexp = r match {
		case SEQ(_, ZERO) => ZERO
		case SEQ(ZERO, _) => ZERO
		case SEQ(r1, ONE) => simploop(r1)
		case SEQ(ONE, r2) => simploop(r2)
		case ALT(r1, ZERO) => simploop(r1)
		case ALT(ZERO, r2) => simploop(r2)
		case ALT(r1, r2) => {
			if(r1 == r2){
				simploop(r1)
			}else{
				ALT(simploop(r1), simploop(r2))
			}	
		}	
		case SEQ(r1, r2) => SEQ(simploop(r1), simploop(r2))
		case _ => r
	}


//  taking a regular expression and a string and 
//checks whether the string matches the regular expression

//def ders (s: List[Char], r: Rexp) : Rexp = ... 

	def ders (s: List[Char], r: Rexp) : Rexp = (s, r) match {
		case (Nil, _) => r
		case (c::rest, r1) => (ders(rest, simp(der(c, r1) )))

	}


	def matcher(r: Rexp, s: String): Boolean = {
		val list = List[String](s)
		nullable(ders(list.flatten, r))

	}	


	def size(r: Rexp): Int = r match {
		case ZERO => (1)
		case ONE => (1)
		case CHAR(_) => (1)
		case ALT(r1, r2) => (1 + size(r1) + size(r2))
		case SEQ(r1, r2) => (1 + size(r1) + size(r2))
		case STAR(rs) => (1 + size(rs))
	}


}