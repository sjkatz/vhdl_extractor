#**************************************************
# AUTHOR        :   Shaun Katz
# DATE          :   16/11/2012
# DESCRIPTION   :
#   A Python-based parser for extracting
#   port and generic definitions from a vhdl source
#
#**************************************************

import re;
from pyparsing import *
import sys;
#ENTITY\s+(.*)\s+IS\s+(?:GENERIC\s*\((.*)\);)?\s+(?:PORT\s*\((.*)\);)?\s+END(?:\s+ENTITY)?

portList = []
genericList = []

def found(s,l,t):
    print "FOUND", t

class port:
	def __init__(self, name,mode,type,lrange, direction,rrange,val):
		self.name = name
		self.mode = mode
		self.type = type
		self.lrange = lrange
		self.direction = direction
		self.rrange = rrange
		self.val = val

class generic:
	def __init__(self, name,mode,type,lrange, direction,rrange,val):
		self.name = name
		self.mode = mode
		self.type = type
		self.lrange = lrange
		self.direction = direction
		self.rrange = rrange
		self.val = val

def buildGenericList(s,l,t):
    print 'buildGenericList'
    global genericList
    #print 't:', t.asList(), '\n'
    for elmt in t:
        print elmt
        print
        num_elmt = len(elmt)
        lrange = ''
        rrange = ''
        direction = ''
        val = ''
        name = elmt[0]
        mode = elmt[1]
        type = elmt[2]
        if (num_elmt == 4 ):
            val = elmt[3]
        elif (num_elmt == 8 ):
            lrange = elmt[4]
            direction = elmt[5]
            rrange = elmt[6]
        elif (num_elmt == 9 ):
            lrange = elmt[4]
            direction = elmt[5]
            rrange = elmt[6]
            val = elmt[8]
        g = generic(name,mode,type,lrange,direction,rrange,val)
        genericList = genericList + [g]

def buildPortList(s,l,t):
    print 'buildPortList'
    global portList
    #print 't:', t.asList(), '\n'
    for elmt in t:
        print elmt
        print
        num_elmt = len(elmt)
        lrange = ''
        rrange = ''
        direction = ''
        val = ''
        name = elmt[0]
        mode = elmt[1]
        type = elmt[2]
        if (num_elmt == 4 ):
            val = elmt[3]
        elif (num_elmt == 8 ):
            lrange = elmt[4]
            direction = elmt[5]
            rrange = elmt[6]
        elif (num_elmt == 9 ):
            lrange = elmt[4]
            direction = elmt[5]
            rrange = elmt[6]
            val = elmt[8]
        p = port(name,mode,type,lrange,direction,rrange,val)
        portList = portList + [p]


##########################
#	PYPARSING IMPLIMENTATION
##########################

# bnf Keyword
LIBRARY = CaselessKeyword('library')
USE = CaselessKeyword('use')
ENTITY = CaselessKeyword('entity')
IS = CaselessKeyword('is')
PORT = CaselessKeyword('port')
GENERIC = CaselessKeyword('generic')
IN = CaselessKeyword('in')
OUT = CaselessKeyword('out')
INOUT = CaselessKeyword('inout')
BUFFER = CaselessKeyword('buffer')
LINKAGE = CaselessKeyword('linkage')
TO = CaselessKeyword('to')
DOWNTO = CaselessKeyword('downto')
END = CaselessKeyword('end')
BEGIN = CaselessKeyword('begin')

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
RANGE = CaselessKeyword('range')
CONSTANT = CaselessKeyword('constant')
SIGNAL = CaselessKeyword('signal')
VARIABLE = CaselessKeyword('variable')
FILE = CaselessKeyword('file')
BUS = CaselessKeyword('bus')

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
LBRACK = Literal("[")
RBRACK = Literal("]")
UNDERLINE = Literal("_")
APOS = Literal("'")
ICOMMA = Literal("\"")
BACKSLASH = Literal("\\")
VBAR = Literal("|")
COMMA = Literal(",")

# bnf punctuation
SEMI  = Suppress(";")
#LPAREN = Suppress("(")
#RPAREN = Suppress(")")
COLON = Suppress(":")
EQ = Suppress("=")
COMMA = Suppress(",")

# bnf grammar
bnfComment = "--" + restOfLine
digit = Regex("[0-9]")
letter = Regex("[a-zA-Z]")
letter_or_digit = letter | digit
lower_case_letter = Regex("[a-z]")
upper_case_letter = Regex("[A-Z]")

SPECIAL = '" # & \' ( ) * + , - . / ; : < = > _ | ! $ % @ ? [ \ ] ^ ` { } ~'
#space_character = Regex("[\n\r\t ]")
space_character = Regex("[\t ]")
#special_character =  Regex('[!$%?@\\^`{|}~]')
special_character =  oneOf(SPECIAL)

basic_graphic_character = (
        upper_case_letter
        | digit
        | special_character
        | space_character
        )
#other_special_character = oneOf(new_printables)

graphic_character = (
        basic_graphic_character
        | lower_case_letter
        #| other_special_character
        )

basic_identifier = letter + ZeroOrMore( Optional( UNDERLINE ) + letter_or_digit )
extended_identifier = BACKSLASH + graphic_character + ZeroOrMore( graphic_character ) + BACKSLASH

identifier = Combine(basic_identifier | extended_identifier)

identifier_list = identifier + ZeroOrMore( COMMA + identifier )

bnfExpr = Word(alphanums+"+"+"-"+"<"+">"+"="+"*"+"\\")

a = '''
direction = TO|DOWNTO

name = identifier
range = bnfExpr + direction + bnfExpr
type_mark = name

discrete_range = range#|subtype_indication
index_constraint = LPAREN + discrete_range + ZeroOrMore( COMMA + discrete_range ) + RPAREN
constraint = index_constraint
subtype_indication = type_mark + Optional(constraint)
#'''

#a = '''
###################################################
#   EXPRESSION
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
	#^ slice_name           # Probably not neccessary for this type of parser
	^ attribute_name
	)

integer = Combine(digit + ZeroOrMore( Optional( UNDERLINE ) + digit ) )

exponent = E + Optional( "+" ) + integer | E + "-" + integer

decimal_literal = integer + Optional( DOT + integer ) + Optional( exponent )

base = integer
extended_digit = digit | letter
#extended_digit = Regex('[a-zA-Z0-9]')
based_integer = extended_digit + ZeroOrMore( Optional( UNDERLINE ) + extended_digit )
based_literal = Combine( base + HASH + based_integer + Optional( DOT + based_integer ) + HASH + Optional( exponent ) )
#based_literal = base + HASH + based_integer + Optional( DOT + based_integer ) + HASH + Optional( exponent )

abstract_literal = decimal_literal | based_literal
#abstract_literal =  based_literal | decimal_literal
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

signature = LBRACK + Optional( type_mark + ZeroOrMore( COMMA + type_mark ) ) + Optional( RETURN + type_mark ) + RBRACK

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


#range_ = bnfExpr + direction + bnfExpr
#a='''
range_ = (
	range_attribute_name
    #| ( LPAREN + Combine(simple_expression, ' ') + direction + Combine(simple_expression , ' ') + RPAREN)
	| ( Combine(simple_expression, ' ') + direction + Combine(simple_expression , ' ') )
	)
#'''

range_constraint =  Suppress( RANGE ) + range_
#range_constraint = ( range_ + Optional(range_ ))
#range_constraint = range_

index_constraint = Suppress(LPAREN) + discrete_range + ZeroOrMore( COMMA + discrete_range ) + Suppress(RPAREN)
#index_constraint = LPAREN + discrete_range + ZeroOrMore( COMMA + discrete_range ) + RPAREN

constraint = (
	range_constraint
	| index_constraint
	)


type_name = name
subtype_name = name

a='''
##############################################
subtype_name = (
#IEEE's STD_LOGIC_1164 Predefined subtypes
         CaselessKeyword("std_logic")
         | CaselessKeyword("X01")
         | CaselessKeyword("X01Z")
         | CaselessKeyword("UX01")
         | CaselessKeyword("UX01Z")
        )

type_name = (
        CaselessKeyword("boolean")
        | CaselessKeyword("integer")
        | CaselessKeyword("natural")
        | CaselessKeyword("positive")
        | CaselessKeyword("real")
        | CaselessKeyword("bit")
        | CaselessKeyword("bit_vector")
        | CaselessKeyword("character")
        | CaselessKeyword("string")
        | CaselessKeyword("time")
        | CaselessKeyword("delay_length")
#IEEE's STD_LOGIC_1164 Predefined types
        | CaselessKeyword("std_ulogic")
        | CaselessKeyword("std_ulogic_vector")
        | CaselessKeyword("std_logic_vector")
#IEEE's NUMERIC_STD Predefined types
        | CaselessKeyword("unsigned")
        | CaselessKeyword("signed")
        )
##############################################
#'''
type_mark = type_name | subtype_name
resolution_function_name = name

#subtype_indication << (
        #Optional( resolution_function_name ) + type_mark + Optional(constraint)
        #^ type_mark + Optional(constraint)
        #)
subtype_indication << ( type_mark + Optional(Group(constraint)) )  # HACK

discrete_subtype_indication = subtype_indication
discrete_range << (
        #discrete_subtype_indication    # BUG: causes 'std_logic_vector(WIDTH - 1 downto 0)' bug
        range_
        )

choice = (
	simple_expression
	^ discrete_range
	^ simple_name
	^ OTHERS		#	BUG:  "( others => '0')" others is not recognized as a choice keyword
	)
choices = choice + ZeroOrMore( VBAR + choice )
element_association = Optional( choices + "=>" ) + expression
aggregate = Combine( LPAREN + element_association + ZeroOrMore( COMMA + element_association ) + RPAREN , ' ')

qualified_expression = (
        ( type_mark + APOS + LPAREN + expression + RPAREN )
        | ( type_mark + APOS + aggregate )
        )

type_conversion = type_mark + LPAREN + expression + RPAREN

allocator = (
	( NEW + subtype_indication )
	| ( NEW + qualified_expression )
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
	| ( function_name + LPAREN + actual_designator + RPAREN )
	| ( type_mark + LPAREN + actual_designator + RPAREN )
    )
formal_designator = (
	generic_name
	| port_name
	| parameter_name
    )
formal_part = (
	formal_designator
	| ( function_name + LPAREN + formal_designator + RPAREN )
	| ( type_mark + LPAREN + formal_designator + RPAREN )
    )
association_element = Optional( formal_part + "=>" ) + actual_part
association_list = association_element + ZeroOrMore( COMMA + association_element )
parameter_association_list = association_list
actual_parameter_part = parameter_association_list
function_call << ( function_name + Optional( LPAREN + actual_parameter_part + RPAREN  ) )

#NOT COMPLETE
primary = (
        #Combine(
            name
            ^ literal
            ^ aggregate
            ^ function_call
            ^ qualified_expression
            ^ type_conversion           #BUG: 'BUS_VAL(X_INT)', Recognised as indexed_name
            ^ allocator                 #BUG: 'new BIT_VECTOR(1 to 3)', Not Recognised
           # ,' ')
        | ( LPAREN + expression + RPAREN )
        )

factor = (
    ( primary + Optional( POW + primary ) )
	| ( ABS + primary )
	| ( NOT + primary )
    )

multiplying_operator = oneOf( "* /") | REM | MOD
term = factor + ZeroOrMore( multiplying_operator + factor )

sign = oneOf( "+ -" )
adding_operator = oneOf( "+ - &" )

simple_expression << ( Optional( sign ) + term + ZeroOrMore( adding_operator + term ) )

#simple_expression << operatorPrecedence( primary,
    #[(sign, 1, opAssoc.RIGHT),
     #(multiplying_operator, 2, opAssoc.LEFT),
     #(adding_operator, 2, opAssoc.LEFT),]
    #)
#simple_expression.setParseAction(found)

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

expr = Combine( expression, ' ' )
#expression.setParseAction(found)
###################################################
#'''

mode = IN|OUT|INOUT|BUFFER|LINKAGE

interface_constant_declaration = Optional( CONSTANT ) + Group(identifier_list) + COLON + Group(Optional(IN)) + Group(subtype_indication) + Group(Optional(COLON + EQ + expr))
interface_signal_declaration = Optional( SIGNAL ) + Group(identifier_list) + COLON + Group(Optional(mode)) + Group(subtype_indication) + Optional( BUS ) + Group(Optional(COLON + EQ + expr))
interface_variable_declaration = Optional( VARIABLE ) + Group(identifier_list) + COLON + Group(Optional(mode)) + Group(subtype_indication) + Group(Optional(COLON + EQ + expr))
interface_file_declaration = FILE + identifier_list + COLON + subtype_indication

interface_declaration = (
	interface_constant_declaration
	^ interface_signal_declaration
	^ interface_variable_declaration
	^ interface_file_declaration
    )

interface_element = interface_declaration

#interface_element = identifier_list + COLON + Optional(mode) + subtype_indication + Optional(COLON + EQ + bnfExpr)
#interface_element = Group(identifier_list) + COLON + Group(Optional(mode)) + Group(subtype_indication) + Group(Optional(COLON + EQ + expr))
interface_list = Group(interface_element) + ZeroOrMore(SEMI + Group(interface_element))

generic_list = interface_list
generic_clause = Suppress(GENERIC) + Suppress(LPAREN) + generic_list + Suppress(RPAREN) + SEMI
generic_clause.setParseAction( buildGenericList )

port_list = interface_list
#port_list.setParseAction( buildPortList )
port_clause = Suppress(PORT) + Suppress(LPAREN) + port_list + Suppress(RPAREN) + SEMI
port_clause.setParseAction( buildPortList )

entity_header = Optional( generic_clause ) + Optional( port_clause )

libraryDef  = Suppress( LIBRARY + identifier + SEMI )
useDef = Suppress( USE + Word(alphanums+"_"+".") + SEMI )
#entityDef = Suppress(ENTITY) + identifier + Suppress(IS) + entity_header
entityDef = Suppress(SkipTo(ENTITY)) + Suppress(ENTITY) + identifier + Suppress(IS) + entity_header + Suppress(Optional(SkipTo(BEGIN) + BEGIN + SkipTo(END))) + Suppress(END + Optional(ENTITY) + Optional(identifier)) + SEMI
entityDef.setParseAction(found)

#bnf = ZeroOrMore(libraryDef|useDef|entityDef)# <-- grammar defined here
bnf = ZeroOrMore(entityDef)# <-- grammar defined here

bnf.ignore(bnfComment)



#f = open('example.vhd', 'r')
f = open(sys.argv[1], 'r')
content = f.read()

parsed = bnf.parseString( content )

a='''
for p in parsed:
    print p
    print

print "GENERICS:"
for generic in genericList:
    print '\nname:', generic.name, '\n\tmode:', generic.mode, '\n\ttype:', generic.type, '\n\tlrange:', generic.lrange, '\n\tdirection:', generic.direction, '\n\trrange:', generic.rrange, '\n\tval:', generic.val

print "PORTS:"
for port in portList:
    print '\nname:', port.name, '\n\tmode:', port.mode, '\n\ttype:', port.type, '\n\tlrange:', port.lrange, '\n\tdirection:', port.direction, '\n\trrange:', port.rrange, '\n\tval:', port.val
#'''

#print parsed

#print "ENTITY:"
#print parsed[0]
#print "\n"
#print "GENERIC:"
#print parsed[1]
#print "\n"
#print "PORT:"
#print parsed[2]
#print "\n"

test = [
		'WIDTH-1',
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
a = '''
for t in test:
    print t
    print expr.parseString(t)
    print

testport = [
        """RST   : in std_logic := (others=> '0');
    CLK   : in std_logic;
    LOAD  : in std_logic;
    DATA  : in std_logic_vector(WIDTH-1 downto 0);
    Q     : out std_logic_vector(WIDTH-1 downto 0)"""
        ]
for t in testport:
    print t
    print port_list.parseString(t)
    print
#'''
