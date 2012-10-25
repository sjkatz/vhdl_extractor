# Known Bugs:
#   * breaks for 'operator' followed by 'sign'
#       eg. (9 + (-2)) * 3 works but (9 + -2) * 3 doesn't
#	* "( others => '0')" others is not recognized as a choice keyword

from pyparsing import *
import re
import string

def found(s,l,t):
    print "FOUND", t

AND        = CaselessKeyword("and")
OR        = CaselessKeyword("or")
XOR        = CaselessKeyword("xor")
NAND        = CaselessKeyword("nand")
NOR        = CaselessKeyword("nor")
XNOR        = CaselessKeyword("xnor")

SLL        = CaselessKeyword("sll")
SRL        = CaselessKeyword("srl")
SLA        = CaselessKeyword("sla")
SRA        = CaselessKeyword("sra")
ROL        = CaselessKeyword("rol")
ROR        = CaselessKeyword("ror")

ABS        = CaselessKeyword("abs")
NOT        = CaselessKeyword("not")

REM        = CaselessKeyword("rem")
MOD        = CaselessKeyword("mod")

RETURN        = CaselessKeyword("return")
TO			= CaselessKeyword('to')
DOWNTO		= CaselessKeyword('downto')

OTHERS = CaselessKeyword('others')
ALL = CaselessKeyword('all')
NEW = CaselessKeyword('new')
OPEN = CaselessKeyword('open')
#NEW.setParseAction(found)

E        = CaselessKeyword("E")
B        = CaselessKeyword("b")
O        = CaselessKeyword("o")
X        = CaselessKeyword("x")

NULL        = CaselessKeyword("null")

DOT       = Literal(".")
HASH       = Literal("#")


POW  = Literal("**")
LPAREN = Literal("(")
RPAREN = Literal(")")
UNDERLINE = Literal("_")
APOS = Literal("'")
ICOMMA = Literal("\"")
BACKSLASH = Literal("\\")
VBAR = Literal("|")
COMMA = Literal(",")

#digit = Word(nums)
digit = Regex("[0-9]")
#digit = oneOf(nums)
#letter = Word(alphas)
letter = Regex("[a-zA-Z]")
#letter = oneOf(alphas)
letter_or_digit = letter | digit
lower_case_letter = Regex("[a-z]")
#LOWER_CASE = string.lowercase
#lower_case_letter = Word(LOWER_CASE, max=1)
upper_case_letter = Regex("[A-Z]")
#UPPER_CASE = string.uppercase
#upper_case_letter = Word(UPPER_CASE, max=1)

#new_printables = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ!"#$%&\\\'*+,-./:;<=>?@\\^_`{|}~'
new_printables = '!"$%&\\\'*+,-./:;<=>?@\\^_`{|}~'
#print new_printables
#other_special_character = Regex('[!"$%&\\\'*+,-./:;<=>?@\\^_`{|}~]')
#basic_graphic_character =  Regex('[!"$%?@\\^`{|}~]')
SPECIAL = '!"$%?@\\^`{|}~'
space_character = Regex("[\n\r\t ]")
special_character =  Regex('[!$%?@\\^`{|}~]')
#basic_graphic_character = upper_case_letter | digit | special_character #| space_character
basic_graphic_character = (
        upper_case_letter
        | digit
        | special_character
        | space_character
        )
#other_special_character = Word(SPECIAL, max=1)
#other_special_character = oneOf(new_printables)
#basic_graphic_character = oneOf(new_printables)

#ident = Word(alphanums+"_")
graphic_character = (
        basic_graphic_character
        | lower_case_letter
        #| other_special_character
        )

basic_identifier = letter + ZeroOrMore( Optional( UNDERLINE ) + letter_or_digit )
extended_identifier = BACKSLASH + graphic_character + ZeroOrMore( graphic_character ) + BACKSLASH

identifier = Combine(basic_identifier | extended_identifier)

character_literal = Combine(APOS + graphic_character + APOS)
enumeration_literal = identifier | character_literal

simple_name = identifier

string_literal = Forward()
operator_symbol = string_literal

prefix = Forward()

selected_name = Forward()
attribute_name = Forward()
expression = Forward()
discrete_range = Forward()

indexed_name = prefix + LPAREN + expression + ZeroOrMore( COMMA + expression ) + RPAREN

slice_name = prefix + LPAREN + discrete_range + RPAREN
name = (
	simple_name
	^ operator_symbol
	^ selected_name
	^ indexed_name
	^ slice_name
	^ attribute_name
	)

integer = digit + ZeroOrMore( Optional( UNDERLINE ) + digit )

exponent = E + Optional( "+" ) + integer | E + "-" + integer

decimal_literal = integer + Optional( DOT + integer ) + Optional( exponent )

base = integer
#extended_digit = digit | letter
extended_digit = Regex('[a-zA-Z0-9]')
based_integer = extended_digit + ZeroOrMore( Optional( UNDERLINE ) + extended_digit )
based_literal = Combine( base + HASH + based_integer + Optional( DOT + based_integer ) + HASH + Optional( exponent ) )
#based_literal = base + HASH + based_integer + Optional( DOT + based_integer ) + HASH + Optional( exponent )

#abstract_literal = decimal_literal | based_literal
abstract_literal =  based_literal | decimal_literal
physical_literal = Optional( abstract_literal ) + name
numeric_literal = abstract_literal | physical_literal

string_literal << ( Combine( ICOMMA + ZeroOrMore( graphic_character ) + ICOMMA ) )

base_specifier = B | O | X
bit_value = extended_digit + ZeroOrMore( Optional( UNDERLINE ) + extended_digit )
bit_string_literal = Combine(base_specifier + ICOMMA + bit_value + ICOMMA)

#NOTE  ^ is the same as | but muches to longest instead of first

literal = (
	numeric_literal
	^ enumeration_literal
	^ string_literal
	^ bit_string_literal
	^ NULL
    )

simple_expression = Forward()
subtype_indication = Forward()
type_mark = Forward()

signature = Optional( Optional( type_mark + ZeroOrMore( COMMA + type_mark ) ) + Optional( RETURN + type_mark ))

function_call = Forward()
prefix << (
	simple_name
	#name				# BUG: RuntimeError: maximum recursion depth exceeded
						# temp fix with simple_name instead
						# maybe try again after function_call is defined
	#| function_call    # function_call causes Max Recursion as well
	)

suffix = (
	simple_name
	| character_literal
	| operator_symbol
	| ALL
)
selected_name << ( Combine( prefix + DOT  + suffix ) )

attribute_simple_name = simple_name
attribute_designator = attribute_simple_name
attribute_name << ( prefix + Optional( signature ) + APOS + attribute_designator + Optional( LPAREN + expression + RPAREN ) )

range_attribute_name =	attribute_name
direction = TO|DOWNTO
range_ = (
	range_attribute_name
	| ( simple_expression + direction + simple_expression )
	)
range_constraint = range_ + range_

index_constraint = LPAREN + discrete_range + ZeroOrMore( COMMA + discrete_range ) + RPAREN

constraint = (
	range_constraint
	| index_constraint
	)

type_name = name
subtype_name = name
type_mark = type_name | subtype_name
resolution_function_name = name

subtype_indication << ( Optional( resolution_function_name ) + type_mark + Optional(constraint) )

discrete_subtype_indication = subtype_indication
discrete_range << ( discrete_subtype_indication | range_ )

choice = (
	simple_expression
	^ discrete_range
	^ simple_name
	^ OTHERS		#	BUG:  "( others => '0')" others is not recognized as a choice keyword
	)
choices = choice + ZeroOrMore( VBAR + choice )
element_association = Optional( choices + "=>" ) + expression
aggregate = LPAREN + element_association + ZeroOrMore( COMMA + element_association ) + RPAREN

qualified_expression = (
        type_mark + APOS + LPAREN + expression + RPAREN
        | type_mark + APOS + aggregate
        )

type_conversion = type_mark + LPAREN + expression + RPAREN

allocator = (
	NEW + subtype_indication
	| NEW + qualified_expression
    )


generic_name = name
port_name = name
parameter_name = name
function_name = name
signal_name = name
variable_name = name
file_name = name
actual_designator = (
	expression
	| signal_name
	| variable_name
	| file_name
	| OPEN
    )
actual_part = (
	actual_designator
	| function_name + LPAREN + actual_designator + RPAREN
	| type_mark + LPAREN + actual_designator + RPAREN
    )
formal_designator = (
	generic_name
	| port_name
	| parameter_name
    )
formal_part = (
	formal_designator
	| function_name + LPAREN + formal_designator + RPAREN
	| type_mark + LPAREN + formal_designator + RPAREN
    )
association_element = Optional( formal_part + "=>" ) + actual_part
association_list = association_element + ZeroOrMore( COMMA + association_element )
parameter_association_list = association_list
actual_parameter_part = parameter_association_list
function_call << ( function_name + Optional( LPAREN + actual_parameter_part + RPAREN  ) )

#NOT COMPLETE
primary = (
        Combine(
            name
            ^ literal
            ^ aggregate
            ^ function_call
            ^ qualified_expression
            ^ type_conversion           #BUG: 'BUS_VAL(X_INT)', Recognised as indexed_name
            ^ allocator                 #BUG: 'new BIT_VECTOR(1 to 3)', Not Recognised
            ,' ')
        | LPAREN + expression + RPAREN
        )

factor = primary + Optional( POW + primary )      \
	| ABS + primary                             \
	| NOT + primary

multiplying_operator = oneOf( "* /") | REM | MOD
term = factor + ZeroOrMore( multiplying_operator + factor )

sign = oneOf( "+ -" )
adding_operator = oneOf( "+ - &" )

simple_expression << ( Optional( sign ) + term + ZeroOrMore( adding_operator + term ) )
shift_operator = SLL | SRL | SLA | SRA | ROL | ROR

shift_expression = simple_expression + Optional( shift_operator + simple_expression )

relational_operator = oneOf( "= /= < <= > >=")
relation = shift_expression + Optional( relational_operator + shift_expression )

expression << (
        relation + ZeroOrMore( AND + relation )
        | relation + ZeroOrMore( OR + relation )
        | relation + ZeroOrMore( XOR + relation )
        | relation + Optional( NAND + relation )
        | relation + Optional( NOR + relation )
        | relation + ZeroOrMore( XNOR + relation )
        )

expr = expression

test = [
		'2 ns',
		'B"1100_1001"', 'b"1001011"',
		'X"C9"', 'X"4b"',
		'O"311"', 'o"113"',
		"\"BOB\" + 1",
		"\\BOB CAT\\ + 1",
        "'Z' + 'X'",
        "9 and 2",
        "(9 and 2) and 3",
        "9 and 2 nand 3",
        "16#1D# mod 2",
        "ab + 2#1111_1111# ",
        "a = b and c /= d",
        "a sll b = c ror d and c /= d",
        "2 sll 3 = c ror d and c /= d",
        "96 + 2 + 3",
        "9 + 2 * 3",
		"9 + 2 REM 3",
        "(9 + 2) * 3",
        #"(9 + -2) * 3",
        "(9 + (-2)) * 3**2**2",
        #"(9! + -2) * 3**2**2",
        "M*X + B",
        "M!*(X + B)",
        "(1+2)*(-3**(4*5))",
        "1+2*-3^4*5+-+-6",
		"(7 => '1', 5 downto 1 => '1', 6 => B_BIT, others => '0')",
		"( others => '0')",
		"BLK_NAME.SIG_NAME",
		"signal(512)",
		"signal(J,g)",
        "signal(K*I)",
		"A_BUS(0 to 11)",
		"string'(\"0010\")",
        "bit_vector'(\"0010\")",
        "std_logic_vector'( others => '0')",
        'BUS_VAL(X_INT)',
        "new BIT_VECTOR(1 to 3)",
        "new",
		]

for t in test:
    print t
    print expr.parseString(t)
    print

