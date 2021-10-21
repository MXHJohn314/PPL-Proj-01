/*
 * CS3210 - Principles of Programming Languages - Fall 2021
 * Instructor: Thyago Mota
 * Student(s) Name(s): Malcolm Johnson, Harrison Jones
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
  val MULTIPLY      = Value // *
  val MODULO     = Value // \
  val BREAK    = Value // ^
  val COMMENT       = Value // '
  val STD_OUT          = Value // !
  val BEGIN_IF      = Value // [
  val MACRO_CALL    = Value // $
  val MACRO_DEF     = Value // #
  val END_CALL      = Value // ;
  val END_DEF       = Value // @
  val END_IF        = Value // ]
  val END_WHILE      = Value // )
  val EOF           = Value //  
  val EO_PRG        = Value // $$
  val EQUAL         = Value // = 
  val SUBTRACT        = Value // - 
  val IDENTIFIER    = Value //   
  val LITERAL       = Value //   
  val NEW_LINE      = Value // \n
  val PUSH_VAL        = Value // .
  val ADD          = Value // + 
  val STD_IN = Value // ? 
  val DIVIDE         = Value // / 
  val STRING        = Value //   
  val BEGIN_WHILE         = Value // (
  val ARG           = Value // %
  val PARAMETER     = Value // <number>%
}
