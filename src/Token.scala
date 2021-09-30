/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student(s):
 * Description: Prg 01 - Token
 */

/*
mouse       = { statement } ´$$´
statement   = ´?´ | ´!´ | string | identifier | ´=´ | literal | ´+´ | ´-´ | ´*´ | ´/´ | ´\´ | ´^´ | ´.´ | if | while
string      = ´"´ { character } ´"´
identifier  = letter
literal     = ´0´ | nonzero { digit }
nonzero     = ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
digit       = ´0´ | ´1´ | ´2´ | ´3´ | ´4´ | ´5´ | ´6´ | ´7´ | ´8´ | ´9´
if          = ´[´ { statement } ´]´
while       = ´(´ { statement } ´)´
letter      = ´a´ | ´b´ | ´c´ | ´d´ | ´e´ | ´f´ | ´g´ | ´h´ | ´i´ | ´j´ | ´k´ | ´l´ | ´m´ | ´n´ | ´o´ | ´p´ | ´q´ | ´r´ | ´s´ | ´t´ | ´u´ | ´v´ | ´x´ | ´y´ | ´w´ | ´z´ | ´A´ | ´B´ | ´C´ | ´D´ | ´E´ | ´F´ | ´G´ | ´H´ | ´I´ | ´J´ | ´K´ | ´L´ | ´M´ | ´N´ | ´O´ | ´P´ | ´Q´ | ´R´ | ´S´ | ´T´ | ´U´ | ´V´ | ´X´ | ´Y´ | ´W´ | ´Z´
punctuation = ´.´ | ´,´ | ´;´ | ´:´ | ´?´ | ´!´
special     = ´<´ | ´_´ | ´@´ | ´#´ | ´$´ | ´%´ | ´^´ | ´&´ | ´(´ | ´)´ | ´-´ | ´+´ | ´=´ | ´'´ | ´/´ | ´\´ | ´[´ | ´]´ | ´{´ | ´}´ | ´|´
blank       = ´ ´
character   = letter | digit | punctuation | special | blank
 */

object Token extends Enumeration {
  val COMMENT       = Value // '  0
  val IDENTIFIER    = Value //    1
  val STRING        = Value //    2
  val LITERAL       = Value //    3
  val QUESTION_MARK = Value // ?  4
  val BANG          = Value // !  5
  val EQUAL         = Value // =  6
  val PLUS          = Value // +  7
  val DASH          = Value // -  8
  val ASTERISK      = Value // *  9
  val CIRCUMFLEX    = Value // ^  10
  val PERIOD        = Value // .  11
  val SLASH         = Value // /  12
  val BACKSLASH     = Value // \  13
  val WHILE    = Value // (  14
  val END_LOOP      = Value // )  15
  val EOF           = Value //    16
  val EO_PRG        = Value // $$ 17
  val BEGIN_IF      = Value // [  18
  val END_IF        = Value // ]  19
  val NEW_LINE      = Value // \n 20
  val BEGIN_MACRO   = Value // $ 
  val END_MACRO     = Value // @
  val MACRO_DEF     = Value // #
}
