# Samuel van der Sloot
# 111459704

import sys
import ply.lex as lex
import ply.yacc as yacc

# list of token names
tokens = (
    'INTEGER',
    'REAL',
    'BOOLEAN',
    'STRING',
    'LPAREN',
    'RPAREN',
    'LBRACKET',
    'RBRACKET',
    'EXPONENT',
    'MULT',
    'DIV',
    'INTDIV',
    'MOD',
    'ADD',
    'MINUS',
    'IN',
    'APPEND',
    'NOT',
    'CONJUNCTION',
    'OR',
    'LESSTHAN',
    'LESSTHANEQ',
    'EQUALS',
    'NOTEQUALS',
    'GREATERTHAN',
    'GREATERTHANEQ',
    'COMMA',
    'POUND',
    'SEMICOLON',
)

# constant definitions
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_EXPONENT = r'\*\*'
t_MULT = r'\*'
t_DIV = r'\/'
t_INTDIV = r'div'
t_MOD = r'mod'
t_ADD = r'\+'
t_MINUS = r'\-'
t_IN = r'in'
t_APPEND = r'\:\:'
t_NOT = r'not'
t_CONJUNCTION = r'andalso'
t_OR = r'orelse'
t_LESSTHAN = r'<'
t_LESSTHANEQ = r'<='
t_EQUALS = r'=='
t_NOTEQUALS = r'<>'
t_GREATERTHAN = r'>'
t_GREATERTHANEQ = r'>='
t_COMMA = r','
t_POUND = r'\#'
t_SEMICOLON = r';'

t_ignore = ' \t'


# operand definition
def t_REAL(t):
    r'((\d*\.\d+)|(\d+\.\d*))([e][-]?\d+)?'
    t.value = float(t.value)
    return t


def t_INTEGER(t):
    r'0*[0-9]+'
    t.value = int(t.value)
    return t


def t_BOOLEAN(t):
    r'(False)|(True)'
    t.value = eval(t.value)
    return t


def t_STRING(t):
    r'(\"[^\"]*\")|(\'[^\']*\')'
    return t


# newline character
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


# error handling
def t_error(t):
    print("SEMANTIC ERROR")


def p_error(p):
    print("SYNTAX ERROR")


lexer = lex.lex()


# grammar for parsing

# CLOSING BRACKET
def p_statement(p):
    'statement : expr SEMICOLON'
    p[0] = p[1]

# OPERAND GRAMMAR
def p_int(p):
    'expr : INTEGER'
    p[0] = p[1]


def p_real(p):
    'expr : REAL'
    p[0] = p[1]


def p_boolean(p):
    'expr : BOOLEAN'
    p[0] = p[1]


def p_string(p):
    'expr : STRING'
    if p[1][0] is '"':
        p[0] = p[1].strip('"')
    elif p[1][0] is "'":
        p[0] = p[1].strip("'")
    else:
        p_error(p)



# LIST GRAMMAR
def p_expr_listdefault(p):
    'expr : LBRACKET expr listtail RBRACKET'
    p[0] = [p[2]] + p[3]


def p_expr_listempy(p):
    'expr : LBRACKET RBRACKET'
    p[0] = []


def p_listtail_default(p):
    'listtail : COMMA expr listtail'
    p[0] = [p[2]] + p[3]


def p_listtail_empty(p):
    'listtail : '
    p[0] = []


def p_expr_listindex(p):
    'expr : expr LBRACKET expr RBRACKET %prec LISTINDEX'
    if type(p[1]) == list and type(p[3]) == int:
        p[0] = p[1][p[3]]
    else:
        p_error(p)
        print("@listindex")


# TUPLE GRAMMAR
def p_expr_tupledefault(p):
    'expr : LPAREN expr tupletail RPAREN %prec TUPLE'
    p[0] = (p[2],) + p[3]


def p_expr_tupleempty(p):
    'expr : LPAREN RPAREN'
    p[0] = ()


def p_expr_tupleindex(p):
    'expr : POUND expr expr %prec TUPLEINDEX'
    if type(p[2]) == int and type(p[3]) == tuple:
        p[0] = p[3][p[2] - 1]
    else:
        p_error(p)
        print("@tupleindex")


def p_tupletail_default(p):
    'tupletail : COMMA expr tupletail'
    p[0] = (p[2],) + p[3]


def p_tupletail_empty(p):
    'tupletail : '
    p[0] = ()


# ADDITION
def p_addition(p):
    'expr : expr ADD expr'
    if type(p[1]) == type(p[3]):
        p[0] = p[1] + p[3]
    elif type(p[1]) != type(p[3]):
        if type(p[1]) == str:
            if type(p[3]) == list:
                p[0] = [p[1]] + p[3]
        elif type(p[3]) == str:
            if type(p[1]) == list:
                p[0] = p[1] + [p[3]]
        elif type(p[1]) == int or type(p[1]) == float:
            p[0] = [p[1]] + p[3]
        else:
            p[0] = p[1] + [p[3]]
    else:
        p_error(p)
        print("@addition")


# SUBTRACTION

def p_negativenum(p):
    'expr : MINUS expr %prec NEG'
    p[0] = -(p[2])


def p_subtraction(p):
    'expr : expr MINUS expr %prec SUB'
    if (type(p[1]) == int or type(p[1]) == float) and (type(p[3]) == int or type(p[3]) == float):
        p[0] = p[1] - p[3]
    else:
        p_error(p)
        print("@subtraction")


# EXPRESSION WITH PARENTHESES


def p_parenthetical(p):
    'expr : LPAREN expr RPAREN'
    p[0] = (p[2])


# EXPRESSION WITH EXPONENT
def p_exponentiate(p):
    'expr : expr EXPONENT expr'
    if (type(p[1]) == int or type(p[1]) == float) and (type(p[3]) == int or type(p[3]) == float):
        p[0] = p[1] ** p[3]
    else:
        p_error(p)
        print("@exponentiate")


# DIVISION
def p_divide(p):
    'expr : expr DIV expr'
    if ((type(p[1]) == int and type(p[3]) == int) or (type(p[1]) == float and type(p[3]) == float)) and p[3] != 0:
        p[0] = p[1] / p[3]
    else:
        p_error(p)
        print("@divide")


# MULTIPLICATION
def p_multiply(p):
    'expr : expr MULT expr'
    if type(p[1]) == int or type(p[1]) == float and type(p[3]) == int or type(p[3]) == float:
        p[0] = p[1] * p[3]
    else:
        p_error(p)
        print("@multiply")


# DIVISION
def p_intDivide(p):
    'expr : expr INTDIV expr'
    if type(p[1]) == int and type(p[3]) == int and p[3] != 0:
        p[0] = p[1] // p[3]
    else:
        p_error(p)
        print("@intdivide")


# MODULO
def p_modulo(p):
    'expr : expr MOD expr'
    if type(p[1]) == int and type(p[3]) == int:
        p[0] = p[1] % p[3]
    else:
        p_error(p)
        print("@modulo")


# MEMBER OF SET
def p_in(p):
    'expr : expr IN expr %prec MEMBER'
    if type(p[3]) == list:
        p[0] = p[1] in p[3]
    else:
        p_error(p)
        print("@in")


# APPEND ITEM TO SET
def p_append(p):
    'expr : expr APPEND expr'
    if type(p[3]) == list:
        p[3].append(p[1])
        p[0] = p[3]
    else:
        p_error(p)
        print("@append")


# NEGATION
def p_not(p):
    'expr : NOT expr'
    if type(p[2]) == bool:
        p[0] = not p[2]
    else:
        p_error(p)
        print("@not")


# CONJUNCTION
def p_conjunction(p):
    'expr : expr CONJUNCTION expr'
    if type(p[1]) == bool and type(p[3]) == bool:
        p[0] = p[1] and p[3]
    else:
        p_error(p)
        print("@conjunction")


# OR ELSE
def p_or(p):
    'expr : expr OR expr'
    if type(p[1]) == bool and type(p[3]) == bool:
        p[0] = p[1] or p[3]
    else:
        p_error(p)
        print("@or")


# LESS THAN
def p_lessThan(p):
    'expr : expr LESSTHAN expr'
    if (type(p[1]) == int and type(p[3]) == int) or (type(p[1]) == str and type(p[3]) == str):
        p[0] = p[1] < p[3]
    else:
        p_error(p)
        print("@lessthan")


# LESS THAN OR EQUAL TO
def p_lessThanEq(p):
    'expr : expr LESSTHANEQ expr'
    if (type(p[1]) == int and type(p[3]) == int) or (type(p[1]) == str and type(p[3]) == str):
        p[0] = p[1] <= p[3]
    else:
        p_error(p)
        print("@lessthaneq")


# EQUALS (BOOL)
def p_equals(p):
    'expr : expr EQUALS expr'
    if (type(p[1]) == int and type(p[3]) == int) or (type(p[1]) == str and type(p[3]) == str):
        p[0] = p[1] == p[3]
    else:
        p_error(p)
        print("@equals")


# NOT EQUAL
def p_notEquals(p):
    'expr : expr NOTEQUALS expr'
    if (type(p[1]) == int and type(p[3]) == int) or (type(p[1]) == str and type(p[3]) == str):
        p[0] = p[1] != p[3]
    else:
        p_error(p)
        print("@notequals")


# GREATER THAN
def p_greaterThan(p):
    'expr : expr GREATERTHAN expr'
    if (type(p[1]) == int and type(p[3]) == int) or (type(p[1]) == str and type(p[3]) == str):
        p[0] = p[1] > p[3]
    else:
        p_error(p)
        print("@greaterthan")


# GREATER THAN OR EQUAL TO
def p_greaterThanEq(p):
    'expr : expr GREATERTHANEQ expr'
    if (type(p[1]) == int and type(p[3]) == int) or (type(p[1]) == str and type(p[3]) == str):
        p[0] = p[1] >= p[3]
    else:
        p_error(p)
        print("@greaterthaneq")


# setting precedence to resolve ambiguity
precedence = (
    ('left', 'GREATERTHAN'),
    ('left', 'GREATERTHANEQ'),
    ('left', 'NOTEQUALS'),
    ('left', 'EQUALS'),
    ('left', 'LESSTHAN'),
    ('left', 'LESSTHANEQ'),
    ('left', 'OR'),
    ('left', 'CONJUNCTION'),
    ('left', 'NOT'),
    ('right', 'APPEND'),
    ('left', 'MEMBER'),
    ('left', 'SUB', 'ADD'),
    ('right', 'NEG'),
    ('left', 'INTDIV', 'MOD', 'DIV', 'MULT'),
    ('right', 'EXPONENT'),
    ('left', 'LISTINDEX'),
    ('left', 'TUPLEINDEX'),
    ('left', 'TUPLE'),
    ('left', 'LPAREN', 'RPAREN'),
)

parser = yacc.yacc()

try:
    f = open(sys.argv[-1])
    s = f.read()
except EOFError:
    print("invalid file")
result = parser.parse(s)  # , debug=True
print("RESULT:", result)
