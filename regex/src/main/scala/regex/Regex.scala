package regex

/** *****************************************************************************
  * Regular Languages
  *
  * data structure definitions for regular languages
  */

// Add your definitions here
trait RegularLanguage

case object Empty extends RegularLanguage
case object Epsilon extends RegularLanguage

case class Character(val c: Char) extends RegularLanguage
case class Star(val pattern: RegularLanguage) extends RegularLanguage
case class Union(val firstLang: RegularLanguage, val secondLang: RegularLanguage) extends RegularLanguage
case class Concat(val firstLang: RegularLanguage, val secondLang: RegularLanguage) extends RegularLanguage

/** *****************************************************************************
  * Derivatives
  *
  * Fill in the function definitions below
  */

/** Simplifies a regular language */
def simplify(lang: RegularLanguage): RegularLanguage = lang match
  // base case
  case (Star(Epsilon)) => Epsilon
  case (Star(Empty)) => Empty

  // recursive cases
  case Concat(Empty, _) | Concat(_, Empty) => simplify(Empty)

  case Concat(Epsilon, lang) => simplify(lang)
  case Concat(lang, Epsilon) => simplify(lang)
  case Union(Empty, lang) => simplify(lang)
  case Union(lang, Empty) => simplify(lang)

  case Concat(firstLang, secondLang) => Concat(simplify(firstLang), simplify(secondLang))
  case Union(firstLang, secondLang) => Union(simplify(firstLang), simplify(secondLang))
  case Star(lang) => Star(simplify(lang))

  case _ => lang

/** A language is nullable if it contains ε */
def nullable(lang: RegularLanguage): Boolean = lang match
  case Empty | Character(_) => false
  case Epsilon | Star(_) => true

  case Concat(firstLang, secondLang) => nullable(firstLang) & nullable(secondLang)
  case Union(firstLang, secondLang) => nullable(firstLang) | nullable(secondLang)
  case _ => nullable(simplify(lang))



/** Computes the derivative of a language, with respect to a character */
def derivative(l: RegularLanguage)(c: Char): RegularLanguage = l match
  case Character(d) if d == c => Epsilon
  case Empty | Epsilon | Character(_) => Empty

  case Union(firstLang, secondLang) => Union(derivative(firstLang)(c), derivative(secondLang)(c))
  case Concat(firstLang, secondLang) if !nullable(firstLang) => Concat(derivative(firstLang)(c), secondLang)
  case Concat(firstLang, secondLang) => Union(Concat(derivative(firstLang)(c), secondLang), derivative(secondLang)(c))
  case Star(lang) => Concat(derivative(lang)(c), Star(lang))


/** *****************************************************************************
  * String-matching with regular expressions
  */

/** Given a string s and a language l, returns true if the string matches the
  * pattern described by l
  */
def matches(s: String, l: RegularLanguage): Boolean =
  if (s.isEmpty) then nullable(l)
  else matches(s.tail, derivative(l)(s.head))
