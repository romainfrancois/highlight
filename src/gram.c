/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton implementation for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.3"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Using locations.  */
#define YYLSP_NEEDED 1



/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     END_OF_INPUT = 258,
     ERROR = 259,
     STR_CONST = 260,
     NUM_CONST = 261,
     NULL_CONST = 262,
     SYMBOL = 263,
     FUNCTION = 264,
     LEFT_ASSIGN = 265,
     EQ_ASSIGN = 266,
     RIGHT_ASSIGN = 267,
     LBB = 268,
     FOR = 269,
     IN = 270,
     IF = 271,
     ELSE = 272,
     WHILE = 273,
     NEXT = 274,
     BREAK = 275,
     REPEAT = 276,
     GT = 277,
     GE = 278,
     LT = 279,
     LE = 280,
     EQ = 281,
     NE = 282,
     AND = 283,
     OR = 284,
     AND2 = 285,
     OR2 = 286,
     NS_GET = 287,
     NS_GET_INT = 288,
     COMMENT = 289,
     SPACES = 290,
     ROXYGEN_COMMENT = 291,
     SYMBOL_FORMALS = 292,
     EQ_FORMALS = 293,
     EQ_SUB = 294,
     SYMBOL_SUB = 295,
     SYMBOL_FUNCTION_CALL = 296,
     SYMBOL_PACKAGE = 297,
     COLON_ASSIGN = 298,
     SLOT = 299,
     LOW = 300,
     TILDE = 301,
     NOT = 302,
     UNOT = 303,
     SPECIAL = 304,
     UPLUS = 305,
     UMINUS = 306
   };
#endif
/* Tokens.  */
#define END_OF_INPUT 258
#define ERROR 259
#define STR_CONST 260
#define NUM_CONST 261
#define NULL_CONST 262
#define SYMBOL 263
#define FUNCTION 264
#define LEFT_ASSIGN 265
#define EQ_ASSIGN 266
#define RIGHT_ASSIGN 267
#define LBB 268
#define FOR 269
#define IN 270
#define IF 271
#define ELSE 272
#define WHILE 273
#define NEXT 274
#define BREAK 275
#define REPEAT 276
#define GT 277
#define GE 278
#define LT 279
#define LE 280
#define EQ 281
#define NE 282
#define AND 283
#define OR 284
#define AND2 285
#define OR2 286
#define NS_GET 287
#define NS_GET_INT 288
#define COMMENT 289
#define SPACES 290
#define ROXYGEN_COMMENT 291
#define SYMBOL_FORMALS 292
#define EQ_FORMALS 293
#define EQ_SUB 294
#define SYMBOL_SUB 295
#define SYMBOL_FUNCTION_CALL 296
#define SYMBOL_PACKAGE 297
#define COLON_ASSIGN 298
#define SLOT 299
#define LOW 300
#define TILDE 301
#define NOT 302
#define UNOT 303
#define SPECIAL 304
#define UPLUS 305
#define UMINUS 306




/* Copy the first part of user declarations.  */
#line 1 "highlight/src/gram.y"

/*{{{ Prologue */
/* :tabSize=4:indentSize=4:noTabs=false:folding=explicit:collapseFolds=1: */


#include "highlight.h"

#define YYERROR_VERBOSE 1
#define PARSE_ERROR_SIZE 256	    /* Parse error messages saved here */
#define PARSE_CONTEXT_SIZE 256	    /* Recent parse context kept in a circular buffer */

static int identifier ;
static void incrementId(void);
static void initId(void);
static void record_( int, int, int, int, int, int, int, int ) ;

static Rboolean known_to_be_utf8 = FALSE ;
static Rboolean known_to_be_latin1 = FALSE ;

static void yyerror(char *);
static int yylex();
int yyparse(void);

static PROTECT_INDEX DATA_INDEX ;
static PROTECT_INDEX ID_INDEX ;

static int	H_ParseError = 0; /* Line where parse error occurred */
static int	H_ParseErrorCol;    /* Column of start of token where parse error occurred */
static char	H_ParseErrorMsg[PARSE_ERROR_SIZE]=  "";
static char	H_ParseContext[PARSE_CONTEXT_SIZE] = "";
static int	H_ParseContextLast = 0 ; /* last character in context buffer */
static int	H_ParseContextLine; /* Line in file of the above */
static Rboolean R_WarnEscapes = TRUE ;   /* Warn on unrecognized escapes */
static SEXP	R_CurrentExpr;	    /* Currently evaluating expression */
// static int	R_PPStackTop;	    /* The top of the stack */

static int	EatLines = 0;
static int	EndOfFile = 0;
static int	xxcharcount, xxcharsave;
static int	xxlineno, xxbyteno, xxcolno,  xxlinesave, xxbytesave, xxcolsave;

static int pushback[PUSHBACK_BUFSIZE];
static unsigned int npush = 0;

static int prevpos = 0;
static int prevlines[PUSHBACK_BUFSIZE];
static int prevcols[PUSHBACK_BUFSIZE];
static int prevbytes[PUSHBACK_BUFSIZE];

static FILE *fp_parse;
static int (*ptr_getc)(void);

static int	SavedToken;
static SEXP	SavedLval;
static char	contextstack[CONTEXTSTACK_SIZE], *contextp;

/*{{{ Parsing entry points functions */
static void HighlightParseContextInit(void);
static void HighlightParseInit(void);
static SEXP Highlight_Parse1(ParseStatus *) ;
static SEXP Highlight_Parse(int, ParseStatus *, SEXP) ;
attribute_hidden SEXP Highlight_ParseFile(FILE *, int , ParseStatus *, SEXP, int) ;    
/*}}}*/

#define yyconst const
typedef struct yyltype{
  int first_line;
  int first_column;
  int first_byte;

  int last_line;
  int last_column;
  int last_byte;
  
  int id ;
} yyltype;

static void setfirstloc( int, int, int ) ;

static int NLINES ;       /* number of lines in the file */
static int data_count ;
static int data_size ;
static int nterminals ;

static int id_size ;

static SEXP data ;
static SEXP ids ; 
static void finalizeData( ) ;
static void growData( ) ;
static void growID( int ) ;

#define _FIRST_LINE( i )   INTEGER( data )[ 9*(i)     ]
#define _FIRST_COLUMN( i ) INTEGER( data )[ 9*(i) + 1 ]
#define _FIRST_BYTE( i )   INTEGER( data )[ 9*(i) + 2 ]
#define _LAST_LINE( i )    INTEGER( data )[ 9*(i) + 3 ]
#define _LAST_COLUMN( i )  INTEGER( data )[ 9*(i) + 4 ]
#define _LAST_BYTE( i )    INTEGER( data )[ 9*(i) + 5 ]
#define _TOKEN( i )        INTEGER( data )[ 9*(i) + 6 ]
#define _ID( i )           INTEGER( data )[ 9*(i) + 7 ]
#define _PARENT(i)         INTEGER( data )[ 9*(i) + 8 ]

#define ID_ID( i )      INTEGER(ids)[ 2*(i) ]
#define ID_PARENT( i )  INTEGER(ids)[ 2*(i) + 1 ]

static void modif_token( yyltype*, int ) ;
static void recordParents( int, yyltype*, int) ;

/* This is used as the buffer for NumericValue, SpecialValue and
   SymbolValue.  None of these could conceivably need 8192 bytes.

   It has not been used as the buffer for input character strings
   since Oct 2007 (released as 2.7.0), and for comments since 2.8.0
 */
static char yytext_[MAXELTSIZE];

static int _current_token ;

/**
 * Records an expression (non terminal symbol 'expr') and gives it an id
 *
 * @param expr expression we want to record and flag with the next id
 * @param loc the location of the expression
 */   
static void setId( SEXP expr, yyltype loc){
	                                                                                                     
	if( expr != R_NilValue ){
		record_( 
			(loc).first_line, (loc).first_column, (loc).first_byte, 
			(loc).last_line, (loc).last_column, (loc).last_byte, 
			_current_token, (loc).id ) ;
	
		SEXP id_ ;
		PROTECT( id_ = allocVector( INTSXP, 1) ) ;
		INTEGER( id_)[0] = (loc).id ;
		setAttrib( expr , mkString("id") , id_ );
		UNPROTECT( 1 ) ;
	}
	
}

# define YYLTYPE yyltype
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
	do	{ 								\
		if (YYID (N)){								\
		  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
		  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
		  (Current).first_byte   = YYRHSLOC (Rhs, 1).first_byte;	\
		  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
		  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
		  (Current).last_byte    = YYRHSLOC (Rhs, N).last_byte;		\
		  incrementId( ) ; \
		  (Current).id = identifier ; \
		  _current_token = yyr1[yyn] ; \
		  yyltype childs[N] ; \
		  int ii = 0; \
		  for( ii=0; ii<N; ii++){ \
			  childs[ii] = YYRHSLOC (Rhs, (ii+1) ) ; \
		  } \
		  recordParents( identifier, childs, N) ; \
		} else	{								\
		  (Current).first_line   = (Current).last_line   =		\
		    YYRHSLOC (Rhs, 0).last_line;				\
		  (Current).first_column = (Current).last_column =		\
		    YYRHSLOC (Rhs, 0).last_column;				\
		  (Current).first_byte   = (Current).last_byte =		\
		    YYRHSLOC (Rhs, 0).last_byte;				\
		} \
	} while (YYID (0))

		
# define YY_LOCATION_PRINT(File, Loc)			\
 fprintf ( stderr, "%d.%d.%d-%d.%d.%d (%d)",			\
      (Loc).first_line, (Loc).first_column,	(Loc).first_byte, \
      (Loc).last_line,  (Loc).last_column, (Loc).last_byte, \
	  (Loc).id ) \

		
#define LBRACE	'{'
#define RBRACE	'}'

#define YYDEBUG 1

static int colon ;

/*{{{ Routines used to build the parse tree */
static SEXP	xxnullformal(void);
static SEXP	xxfirstformal0(SEXP);
static SEXP	xxfirstformal1(SEXP, SEXP);
static SEXP	xxaddformal0(SEXP, SEXP, YYLTYPE *);
static SEXP	xxaddformal1(SEXP, SEXP, SEXP, YYLTYPE *);
static SEXP	xxexprlist0();
static SEXP	xxexprlist1(SEXP);
static SEXP	xxexprlist2(SEXP, SEXP);
static SEXP	xxsub0(void);
static SEXP	xxsub1(SEXP, YYLTYPE *);
static SEXP	xxsymsub0(SEXP, YYLTYPE *);
static SEXP	xxsymsub1(SEXP, SEXP, YYLTYPE *);
static SEXP	xxnullsub0(YYLTYPE *);
static SEXP	xxnullsub1(SEXP, YYLTYPE *);
static SEXP	xxsublist1(SEXP);
static SEXP	xxsublist2(SEXP, SEXP);
static SEXP	xxcond(SEXP);
static SEXP	xxifcond(SEXP);
static SEXP	xxif(SEXP, SEXP, SEXP);
static SEXP	xxifelse(SEXP, SEXP, SEXP, SEXP);
static SEXP	xxforcond(SEXP, SEXP);
static SEXP	xxfor(SEXP, SEXP, SEXP);
static SEXP	xxwhile(SEXP, SEXP, SEXP);
static SEXP	xxrepeat(SEXP, SEXP);
static SEXP	xxnxtbrk(SEXP);
static SEXP	xxfuncall(SEXP, SEXP);
static SEXP	xxdefun(SEXP, SEXP, SEXP);
static SEXP	xxunary(SEXP, SEXP);
static SEXP	xxbinary(SEXP, SEXP, SEXP);
static SEXP	xxparen(SEXP, SEXP);
static SEXP	xxsubscript(SEXP, SEXP, SEXP);
static SEXP	xxexprlist(SEXP, SEXP);
static int	xxvalue(SEXP, int);
/*}}}*/ 

/*{{{ Functions used in the parsing process */

static void	CheckFormalArgs(SEXP, SEXP, YYLTYPE* );
static SEXP	TagArg(SEXP, SEXP, YYLTYPE* );
static void IfPush(void);
static void ifpop(void); 
static SEXP	NextArg(SEXP, SEXP, SEXP);
static SEXP	FirstArg(SEXP, SEXP);
static int KeywordLookup(const char*);

static SEXP mkComplex(const char *);
static SEXP mkFloat(const char *);
SEXP mkInt(const char *); 
static SEXP mkNA(void);
SEXP mkTrue(void);
SEXP mkFalse(void);

static int	xxgetc();
static int	xxungetc(int);
/*}}}*/

/* The idea here is that if a string contains \u escapes that are not
   valid in the current locale, we should switch to UTF-8 for that
   string.  Needs wide-char support.
*/
#if defined(SUPPORT_MBCS)
# include <R_ext/rlocale.h>
#ifdef HAVE_LANGINFO_CODESET
# include <langinfo.h>
#endif
/**
 * Gets the next character (one MBCS worth character)
 * 
 * @param c 
 * @param wc
 * @return 
 */ 
static int mbcs_get_next(int c, wchar_t *wc){
	
    int i, res, clen = 1; char s[9];
    mbstate_t mb_st;

    s[0] = c;
    /* This assumes (probably OK) that all MBCS embed ASCII as single-byte
       lead bytes, including control chars */
    if((unsigned int) c < 0x80) {
		*wc = (wchar_t) c;
		return 1;
    }
    if(utf8locale) {
		clen = utf8clen(c);
		for(i = 1; i < clen; i++) {
		    s[i] = xxgetc();  
		    if(s[i] == R_EOF) {
				error(_("EOF whilst reading MBCS char at line %d"), xxlineno);
			}
		}
		res = mbrtowc(wc, s, clen, NULL);
		if(res == -1) {
			error(_("invalid multibyte character in parser at line %d"), xxlineno);
		}
    } else {
		/* This is not necessarily correct for stateful MBCS */
		while(clen <= MB_CUR_MAX) {
		    mbs_init(&mb_st);
		    res = mbrtowc(wc, s, clen, &mb_st);
		    if(res >= 0) break;
		    if(res == -1){
				error(_("invalid multibyte character in parser at line %d"), xxlineno);
			}
		    /* so res == -2 */
		    c = xxgetc();
		    if(c == R_EOF){
				error(_("EOF whilst reading MBCS char at line %d"), xxlineno);
			}
		    s[clen++] = c;
		} /* we've tried enough, so must be complete or invalid by now */
    }
    for(i = clen - 1; i > 0; i--){
		xxungetc(s[i]);
	}
    return clen;
}
#endif 

#define YYSTYPE		SEXP


/*}}} Prologue */


/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif

#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
} YYLTYPE;
# define yyltype YYLTYPE /* obsolescent; will be withdrawn */
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif


/* Copy the second part of user declarations.  */


/* Line 216 of yacc.c.  */
#line 532 "highlight/src/gram.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int i)
#else
static int
YYID (i)
    int i;
#endif
{
  return i;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef _STDLIB_H
#      define _STDLIB_H 1
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined _STDLIB_H \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef _STDLIB_H
#    define _STDLIB_H 1
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined _STDLIB_H && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
	     && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss;
  YYSTYPE yyvs;
    YYLTYPE yyls;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE) + sizeof (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack)					\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack, Stack, yysize);				\
	Stack = &yyptr->Stack;						\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  46
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   751

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  73
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  13
/* YYNRULES -- Number of rules.  */
#define YYNRULES  90
/* YYNRULES -- Number of states.  */
#define YYNSTATES  163

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   306

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      64,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    69,     2,     2,    60,    70,     2,     2,
      62,    68,    53,    51,    72,    52,     2,    54,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    56,    65,
       2,     2,     2,    45,    61,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,    63,     2,    71,    59,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    66,     2,    67,    47,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      46,    48,    49,    50,    55,    57,    58
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     5,     7,    10,    13,    15,    17,    19,
      23,    25,    27,    29,    31,    35,    39,    42,    45,    48,
      51,    54,    58,    62,    66,    70,    74,    78,    82,    86,
      90,    94,    98,   102,   106,   110,   114,   118,   122,   126,
     130,   134,   138,   142,   149,   154,   158,   164,   168,   172,
     175,   181,   186,   190,   194,   198,   202,   206,   210,   214,
     218,   222,   226,   230,   234,   236,   238,   242,   246,   252,
     253,   255,   259,   262,   266,   269,   271,   276,   277,   279,
     282,   286,   289,   293,   296,   300,   301,   303,   307,   311,
     317
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int8 yyrhs[] =
{
      74,     0,    -1,     3,    -1,    64,    -1,    75,    64,    -1,
      75,    65,    -1,     1,    -1,    77,    -1,    76,    -1,    77,
      11,    75,    -1,     6,    -1,     5,    -1,     7,    -1,     8,
      -1,    66,    81,    67,    -1,    62,    75,    68,    -1,    52,
      77,    -1,    51,    77,    -1,    69,    77,    -1,    47,    77,
      -1,    45,    77,    -1,    77,    56,    77,    -1,    77,    51,
      77,    -1,    77,    52,    77,    -1,    77,    53,    77,    -1,
      77,    54,    77,    -1,    77,    59,    77,    -1,    77,    55,
      77,    -1,    77,    70,    77,    -1,    77,    47,    77,    -1,
      77,    45,    77,    -1,    77,    24,    77,    -1,    77,    25,
      77,    -1,    77,    26,    77,    -1,    77,    27,    77,    -1,
      77,    23,    77,    -1,    77,    22,    77,    -1,    77,    28,
      77,    -1,    77,    29,    77,    -1,    77,    30,    77,    -1,
      77,    31,    77,    -1,    77,    10,    77,    -1,    77,    12,
      77,    -1,     9,    62,    84,    68,    85,    75,    -1,    77,
      62,    82,    68,    -1,    16,    79,    75,    -1,    16,    79,
      75,    17,    75,    -1,    14,    80,    75,    -1,    18,    78,
      75,    -1,    21,    75,    -1,    77,    13,    82,    71,    71,
      -1,    77,    63,    82,    71,    -1,     8,    32,     8,    -1,
       8,    32,     5,    -1,     5,    32,     8,    -1,     5,    32,
       5,    -1,     8,    33,     8,    -1,     8,    33,     5,    -1,
       5,    33,     8,    -1,     5,    33,     5,    -1,    77,    60,
       8,    -1,    77,    60,     5,    -1,    77,    61,     8,    -1,
      77,    61,     5,    -1,    19,    -1,    20,    -1,    62,    77,
      68,    -1,    62,    77,    68,    -1,    62,     8,    15,    77,
      68,    -1,    -1,    75,    -1,    81,    65,    75,    -1,    81,
      65,    -1,    81,    64,    75,    -1,    81,    64,    -1,    83,
      -1,    82,    85,    72,    83,    -1,    -1,    77,    -1,     8,
      11,    -1,     8,    11,    77,    -1,     5,    11,    -1,     5,
      11,    77,    -1,     7,    11,    -1,     7,    11,    77,    -1,
      -1,     8,    -1,     8,    11,    77,    -1,    84,    72,     8,
      -1,    84,    72,     8,    11,    77,    -1,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   357,   357,   358,   359,   360,   361,   364,   365,   368,
     371,   372,   373,   374,   376,   377,   379,   380,   381,   382,
     383,   385,   386,   387,   388,   389,   390,   391,   392,   393,
     394,   395,   396,   397,   398,   399,   400,   401,   402,   403,
     404,   406,   407,   408,   411,   412,   413,   416,   418,   419,
     420,   421,   422,   423,   424,   425,   426,   427,   428,   429,
     430,   431,   432,   433,   434,   435,   439,   442,   445,   449,
     450,   451,   452,   453,   454,   457,   458,   461,   462,   463,
     464,   465,   466,   467,   468,   471,   472,   473,   474,   475,
     479
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "END_OF_INPUT", "ERROR", "STR_CONST",
  "NUM_CONST", "NULL_CONST", "SYMBOL", "FUNCTION", "LEFT_ASSIGN",
  "EQ_ASSIGN", "RIGHT_ASSIGN", "LBB", "FOR", "IN", "IF", "ELSE", "WHILE",
  "NEXT", "BREAK", "REPEAT", "GT", "GE", "LT", "LE", "EQ", "NE", "AND",
  "OR", "AND2", "OR2", "NS_GET", "NS_GET_INT", "COMMENT", "SPACES",
  "ROXYGEN_COMMENT", "SYMBOL_FORMALS", "EQ_FORMALS", "EQ_SUB",
  "SYMBOL_SUB", "SYMBOL_FUNCTION_CALL", "SYMBOL_PACKAGE", "COLON_ASSIGN",
  "SLOT", "'?'", "LOW", "'~'", "TILDE", "NOT", "UNOT", "'+'", "'-'", "'*'",
  "'/'", "SPECIAL", "':'", "UPLUS", "UMINUS", "'^'", "'$'", "'@'", "'('",
  "'['", "'\\n'", "';'", "'{'", "'}'", "')'", "'!'", "'%'", "']'", "','",
  "$accept", "prog", "expr_or_assign", "equal_assign", "expr", "cond",
  "ifcond", "forcond", "exprlist", "sublist", "sub", "formlist", "cr", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,    63,   300,   126,   301,   302,
     303,    43,    45,    42,    47,   304,    58,   305,   306,    94,
      36,    64,    40,    91,    10,    59,   123,   125,    41,    33,
      37,    93,    44
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    73,    74,    74,    74,    74,    74,    75,    75,    76,
      77,    77,    77,    77,    77,    77,    77,    77,    77,    77,
      77,    77,    77,    77,    77,    77,    77,    77,    77,    77,
      77,    77,    77,    77,    77,    77,    77,    77,    77,    77,
      77,    77,    77,    77,    77,    77,    77,    77,    77,    77,
      77,    77,    77,    77,    77,    77,    77,    77,    77,    77,
      77,    77,    77,    77,    77,    77,    78,    79,    80,    81,
      81,    81,    81,    81,    81,    82,    82,    83,    83,    83,
      83,    83,    83,    83,    83,    84,    84,    84,    84,    84,
      85
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     1,     1,     2,     2,     1,     1,     1,     3,
       1,     1,     1,     1,     3,     3,     2,     2,     2,     2,
       2,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     6,     4,     3,     5,     3,     3,     2,
       5,     4,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     1,     1,     3,     3,     5,     0,
       1,     3,     2,     3,     2,     1,     4,     0,     1,     2,
       3,     2,     3,     2,     3,     0,     1,     3,     3,     5,
       0
};

/* YYDEFACT[STATE-NAME] -- Default rule to reduce with in state
   STATE-NUM when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     6,     2,    11,    10,    12,    13,     0,     0,     0,
       0,    64,    65,     0,     0,     0,     0,     0,     0,     3,
      69,     0,     0,     0,     8,     7,     0,     0,     0,     0,
      85,     0,     0,     0,     0,     0,     0,    49,    20,    19,
      17,    16,     0,    70,     0,    18,     1,     4,     5,     0,
       0,     0,    77,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    77,    77,     0,    55,    54,    59,
      58,    53,    52,    57,    56,    86,     0,     0,    47,     0,
      45,     0,    48,    15,    74,    72,    14,    41,     9,    42,
      11,    12,    13,    78,    90,    75,    36,    35,    31,    32,
      33,    34,    37,    38,    39,    40,    30,    29,    22,    23,
      24,    25,    27,    21,    26,    61,    60,    63,    62,    90,
      90,    28,     0,    90,     0,     0,    67,     0,    66,    73,
      71,    81,    83,    79,     0,     0,    44,    51,    87,     0,
      88,     0,    46,    82,    84,    80,    50,    77,    43,     0,
      68,    76,    89
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,    22,    23,    24,    25,    36,    34,    32,    44,   104,
     105,    86,   145
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -64
static const yytype_int16 yypact[] =
{
      85,   -64,   -64,    38,   -64,   -64,    52,   -58,   -46,   -44,
     -38,   -64,   -64,   150,   150,   150,   150,   150,   150,   -64,
     150,   150,    59,    32,   -64,   218,     7,     9,    15,    17,
      24,    56,   150,   150,   150,   150,   150,   -64,   488,   590,
     192,   192,    10,   -64,   -54,   661,   -64,   -64,   -64,   150,
     150,   150,   170,   150,   150,   150,   150,   150,   150,   150,
     150,   150,   150,   150,   150,   150,   150,   150,   150,   150,
     150,   150,    26,    28,   170,   170,   150,   -64,   -64,   -64,
     -64,   -64,   -64,   -64,   -64,    69,   -63,    67,   -64,   272,
      70,   326,   -64,   -64,   150,   150,   -64,   488,   -64,   539,
      -5,    78,    -3,   434,    27,   -64,   681,   681,   681,   681,
     681,   681,   661,   610,   661,   610,   488,   590,    13,    13,
     148,   148,   248,   192,   192,   -64,   -64,   -64,   -64,    34,
      29,   434,   150,   -64,   101,   150,   -64,   150,   -64,   -64,
     -64,   150,   150,   150,    39,    40,   -64,   -64,   434,   150,
     100,   380,   -64,   434,   434,   434,   -64,   170,   -64,   150,
     -64,   -64,   434
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -64,   -64,    45,   -64,   -14,   -64,   -64,   -64,   -64,    33,
     -43,   -64,   -20
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If zero, do what YYDEFACT says.
   If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -1
static const yytype_int16 yytable[] =
{
      38,    39,    40,    41,    30,   133,   141,    45,   143,   134,
      94,    95,    77,    96,    79,    78,    31,    80,    33,    89,
      81,    91,    83,    82,    35,    84,    52,    26,    27,    28,
      29,   125,    85,   127,   126,    97,   128,    99,   103,   106,
     107,   108,   109,   110,   111,   112,   113,   114,   115,   116,
     117,   118,   119,   120,   121,   122,   123,   124,    37,    46,
     103,   103,   131,    42,    87,    43,    67,    68,    69,    70,
      26,    27,    71,    72,    73,    74,    75,    88,    93,    90,
     132,    92,   135,    76,    28,    29,     1,   137,     2,   142,
       3,     4,     5,     6,     7,    98,    47,    48,   144,     8,
     147,     9,   146,    10,    11,    12,    13,   129,   130,   150,
     156,   159,   157,   149,   161,     0,     0,     0,   148,     0,
       0,   151,     0,     0,     0,     0,     0,   153,   154,   155,
      14,     0,    15,     0,     0,     0,    16,    17,     0,   139,
     140,     0,     0,   103,     0,   162,     0,    18,     0,    19,
       0,    20,     0,     0,    21,     3,     4,     5,     6,     7,
       0,    52,     0,     0,     8,     0,     9,     0,    10,    11,
      12,    13,     0,     0,     0,   100,     4,   101,   102,     7,
       0,     0,   152,     0,     8,     0,     9,     0,    10,    11,
      12,    13,     0,     0,   158,    14,     0,    15,     0,     0,
       0,    16,    17,    69,    70,    52,     0,    71,    72,    73,
      74,    75,    18,     0,     0,    14,    20,    15,    76,    21,
       0,    16,    17,     0,     0,     0,     0,     0,    49,    50,
      51,    52,    18,     0,     0,     0,    20,     0,     0,    21,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
       0,    71,    72,    73,    74,    75,     0,     0,     0,     0,
       0,    52,    76,    63,     0,    64,     0,     0,     0,    65,
      66,    67,    68,    69,    70,     0,     0,    71,    72,    73,
      74,    75,    49,     0,    51,    52,     0,     0,    76,     0,
       0,     0,     0,     0,    53,    54,    55,    56,    57,    58,
      59,    60,    61,    62,    70,     0,     0,    71,    72,    73,
      74,    75,     0,     0,     0,     0,     0,    63,    76,    64,
       0,     0,     0,    65,    66,    67,    68,    69,    70,     0,
       0,    71,    72,    73,    74,    75,    49,     0,    51,    52,
     136,     0,    76,     0,     0,     0,     0,     0,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    63,     0,    64,     0,     0,     0,    65,    66,    67,
      68,    69,    70,     0,     0,    71,    72,    73,    74,    75,
      49,     0,    51,    52,   138,     0,    76,     0,     0,     0,
       0,     0,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,     0,    64,     0,     0,
       0,    65,    66,    67,    68,    69,    70,     0,     0,    71,
      72,    73,    74,    75,    49,     0,    51,    52,   160,     0,
      76,     0,     0,     0,     0,     0,    53,    54,    55,    56,
      57,    58,    59,    60,    61,    62,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
       0,    64,     0,     0,     0,    65,    66,    67,    68,    69,
      70,     0,     0,    71,    72,    73,    74,    75,    49,     0,
      51,    52,     0,     0,    76,     0,     0,     0,     0,     0,
      53,    54,    55,    56,    57,    58,    59,    60,    61,    62,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    64,     0,     0,     0,    65,
      66,    67,    68,    69,    70,     0,     0,    71,    72,    73,
      74,    75,    52,     0,     0,     0,     0,     0,    76,     0,
       0,    53,    54,    55,    56,    57,    58,    59,    60,    61,
      62,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    64,     0,     0,     0,
      65,    66,    67,    68,    69,    70,     0,     0,    71,    72,
      73,    74,    75,    52,     0,     0,     0,     0,     0,    76,
       0,     0,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,     0,    52,     0,     0,     0,     0,     0,     0,
       0,     0,    53,    54,    55,    56,    57,    58,    59,     0,
      61,    65,    66,    67,    68,    69,    70,     0,     0,    71,
      72,    73,    74,    75,     0,     0,     0,     0,     0,     0,
      76,    65,    66,    67,    68,    69,    70,     0,     0,    71,
      72,    73,    74,    75,    52,     0,     0,     0,     0,     0,
      76,     0,     0,    53,    54,    55,    56,    57,    58,     0,
       0,     0,     0,     0,    52,     0,     0,     0,     0,     0,
       0,     0,     0,    -1,    -1,    -1,    -1,    -1,    -1,     0,
       0,     0,    65,    66,    67,    68,    69,    70,     0,     0,
      71,    72,    73,    74,    75,     0,     0,     0,     0,     0,
       0,    76,    65,    66,    67,    68,    69,    70,     0,     0,
      71,    72,    73,    74,    75,     0,     0,     0,     0,     0,
       0,    76
};

static const yytype_int16 yycheck[] =
{
      14,    15,    16,    17,    62,    68,    11,    21,    11,    72,
      64,    65,     5,    67,     5,     8,    62,     8,    62,    33,
       5,    35,     5,     8,    62,     8,    13,    32,    33,    32,
      33,     5,     8,     5,     8,    49,     8,    51,    52,    53,
      54,    55,    56,    57,    58,    59,    60,    61,    62,    63,
      64,    65,    66,    67,    68,    69,    70,    71,    13,     0,
      74,    75,    76,    18,     8,    20,    53,    54,    55,    56,
      32,    33,    59,    60,    61,    62,    63,    32,    68,    34,
      11,    36,    15,    70,    32,    33,     1,    17,     3,    11,
       5,     6,     7,     8,     9,    50,    64,    65,    71,    14,
      71,    16,    68,    18,    19,    20,    21,    74,    75,     8,
      71,    11,    72,   133,   157,    -1,    -1,    -1,   132,    -1,
      -1,   135,    -1,    -1,    -1,    -1,    -1,   141,   142,   143,
      45,    -1,    47,    -1,    -1,    -1,    51,    52,    -1,    94,
      95,    -1,    -1,   157,    -1,   159,    -1,    62,    -1,    64,
      -1,    66,    -1,    -1,    69,     5,     6,     7,     8,     9,
      -1,    13,    -1,    -1,    14,    -1,    16,    -1,    18,    19,
      20,    21,    -1,    -1,    -1,     5,     6,     7,     8,     9,
      -1,    -1,   137,    -1,    14,    -1,    16,    -1,    18,    19,
      20,    21,    -1,    -1,   149,    45,    -1,    47,    -1,    -1,
      -1,    51,    52,    55,    56,    13,    -1,    59,    60,    61,
      62,    63,    62,    -1,    -1,    45,    66,    47,    70,    69,
      -1,    51,    52,    -1,    -1,    -1,    -1,    -1,    10,    11,
      12,    13,    62,    -1,    -1,    -1,    66,    -1,    -1,    69,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,
      -1,    13,    70,    45,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    59,    60,    61,
      62,    63,    10,    -1,    12,    13,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    -1,    22,    23,    24,    25,    26,    27,
      28,    29,    30,    31,    56,    -1,    -1,    59,    60,    61,
      62,    63,    -1,    -1,    -1,    -1,    -1,    45,    70,    47,
      -1,    -1,    -1,    51,    52,    53,    54,    55,    56,    -1,
      -1,    59,    60,    61,    62,    63,    10,    -1,    12,    13,
      68,    -1,    70,    -1,    -1,    -1,    -1,    -1,    22,    23,
      24,    25,    26,    27,    28,    29,    30,    31,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    47,    -1,    -1,    -1,    51,    52,    53,
      54,    55,    56,    -1,    -1,    59,    60,    61,    62,    63,
      10,    -1,    12,    13,    68,    -1,    70,    -1,    -1,    -1,
      -1,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    -1,    -1,
      -1,    51,    52,    53,    54,    55,    56,    -1,    -1,    59,
      60,    61,    62,    63,    10,    -1,    12,    13,    68,    -1,
      70,    -1,    -1,    -1,    -1,    -1,    22,    23,    24,    25,
      26,    27,    28,    29,    30,    31,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    47,    -1,    -1,    -1,    51,    52,    53,    54,    55,
      56,    -1,    -1,    59,    60,    61,    62,    63,    10,    -1,
      12,    13,    -1,    -1,    70,    -1,    -1,    -1,    -1,    -1,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    47,    -1,    -1,    -1,    51,
      52,    53,    54,    55,    56,    -1,    -1,    59,    60,    61,
      62,    63,    13,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    47,    -1,    -1,    -1,
      51,    52,    53,    54,    55,    56,    -1,    -1,    59,    60,
      61,    62,    63,    13,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    22,    23,    24,    25,    26,    27,    28,    29,
      30,    31,    -1,    13,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    22,    23,    24,    25,    26,    27,    28,    -1,
      30,    51,    52,    53,    54,    55,    56,    -1,    -1,    59,
      60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    51,    52,    53,    54,    55,    56,    -1,    -1,    59,
      60,    61,    62,    63,    13,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    22,    23,    24,    25,    26,    27,    -1,
      -1,    -1,    -1,    -1,    13,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    22,    23,    24,    25,    26,    27,    -1,
      -1,    -1,    51,    52,    53,    54,    55,    56,    -1,    -1,
      59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    51,    52,    53,    54,    55,    56,    -1,    -1,
      59,    60,    61,    62,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    70
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,     1,     3,     5,     6,     7,     8,     9,    14,    16,
      18,    19,    20,    21,    45,    47,    51,    52,    62,    64,
      66,    69,    74,    75,    76,    77,    32,    33,    32,    33,
      62,    62,    80,    62,    79,    62,    78,    75,    77,    77,
      77,    77,    75,    75,    81,    77,     0,    64,    65,    10,
      11,    12,    13,    22,    23,    24,    25,    26,    27,    28,
      29,    30,    31,    45,    47,    51,    52,    53,    54,    55,
      56,    59,    60,    61,    62,    63,    70,     5,     8,     5,
       8,     5,     8,     5,     8,     8,    84,     8,    75,    77,
      75,    77,    75,    68,    64,    65,    67,    77,    75,    77,
       5,     7,     8,    77,    82,    83,    77,    77,    77,    77,
      77,    77,    77,    77,    77,    77,    77,    77,    77,    77,
      77,    77,    77,    77,    77,     5,     8,     5,     8,    82,
      82,    77,    11,    68,    72,    15,    68,    17,    68,    75,
      75,    11,    11,    11,    71,    85,    68,    71,    77,    85,
       8,    77,    75,    77,    77,    77,    71,    72,    75,    11,
      68,    83,    77
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  */

#define YYFAIL		goto yyerrlab

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      yytoken = YYTRANSLATE (yychar);				\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* YY_LOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

#ifndef YY_LOCATION_PRINT
# if YYLTYPE_IS_TRIVIAL
#  define YY_LOCATION_PRINT(File, Loc)			\
     fprintf (File, "%d.%d-%d.%d",			\
	      (Loc).first_line, (Loc).first_column,	\
	      (Loc).last_line,  (Loc).last_column)
# else
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value, Location); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
#endif
{
  if (!yyvaluep)
    return;
  YYUSE (yylocationp);
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep, yylocationp)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
    YYLTYPE const * const yylocationp;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  YY_LOCATION_PRINT (yyoutput, *yylocationp);
  YYFPRINTF (yyoutput, ": ");
  yy_symbol_value_print (yyoutput, yytype, yyvaluep, yylocationp);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *bottom, yytype_int16 *top)
#else
static void
yy_stack_print (bottom, top)
    yytype_int16 *bottom;
    yytype_int16 *top;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; bottom <= top; ++bottom)
    YYFPRINTF (stderr, " %d", *bottom);
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, YYLTYPE *yylsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yylsp, yyrule)
    YYSTYPE *yyvsp;
    YYLTYPE *yylsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      fprintf (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       , &(yylsp[(yyi + 1) - (yynrhs)])		       );
      fprintf (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, yylsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif



#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into YYRESULT an error message about the unexpected token
   YYCHAR while in state YYSTATE.  Return the number of bytes copied,
   including the terminating null byte.  If YYRESULT is null, do not
   copy anything; just return the number of bytes that would be
   copied.  As a special case, return 0 if an ordinary "syntax error"
   message will do.  Return YYSIZE_MAXIMUM if overflow occurs during
   size calculation.  */
static YYSIZE_T
yysyntax_error (char *yyresult, int yystate, int yychar)
{
  int yyn = yypact[yystate];

  if (! (YYPACT_NINF < yyn && yyn <= YYLAST))
    return 0;
  else
    {
      int yytype = YYTRANSLATE (yychar);
      YYSIZE_T yysize0 = yytnamerr (0, yytname[yytype]);
      YYSIZE_T yysize = yysize0;
      YYSIZE_T yysize1;
      int yysize_overflow = 0;
      enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
      char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
      int yyx;

# if 0
      /* This is so xgettext sees the translatable formats that are
	 constructed on the fly.  */
      YY_("syntax error, unexpected %s");
      YY_("syntax error, unexpected %s, expecting %s");
      YY_("syntax error, unexpected %s, expecting %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s");
      YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s");
# endif
      char *yyfmt;
      char const *yyf;
      static char const yyunexpected[] = "syntax error, unexpected %s";
      static char const yyexpecting[] = ", expecting %s";
      static char const yyor[] = " or %s";
      char yyformat[sizeof yyunexpected
		    + sizeof yyexpecting - 1
		    + ((YYERROR_VERBOSE_ARGS_MAXIMUM - 2)
		       * (sizeof yyor - 1))];
      char const *yyprefix = yyexpecting;

      /* Start YYX at -YYN if negative to avoid negative indexes in
	 YYCHECK.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;

      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yycount = 1;

      yyarg[0] = yytname[yytype];
      yyfmt = yystpcpy (yyformat, yyunexpected);

      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
	if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR)
	  {
	    if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
	      {
		yycount = 1;
		yysize = yysize0;
		yyformat[sizeof yyunexpected - 1] = '\0';
		break;
	      }
	    yyarg[yycount++] = yytname[yyx];
	    yysize1 = yysize + yytnamerr (0, yytname[yyx]);
	    yysize_overflow |= (yysize1 < yysize);
	    yysize = yysize1;
	    yyfmt = yystpcpy (yyfmt, yyprefix);
	    yyprefix = yyor;
	  }

      yyf = YY_(yyformat);
      yysize1 = yysize + yystrlen (yyf);
      yysize_overflow |= (yysize1 < yysize);
      yysize = yysize1;

      if (yysize_overflow)
	return YYSIZE_MAXIMUM;

      if (yyresult)
	{
	  /* Avoid sprintf, as that infringes on the user's name space.
	     Don't have undefined behavior even if the translation
	     produced a string with the wrong number of "%s"s.  */
	  char *yyp = yyresult;
	  int yyi = 0;
	  while ((*yyp = *yyf) != '\0')
	    {
	      if (*yyp == '%' && yyf[1] == 's' && yyi < yycount)
		{
		  yyp += yytnamerr (yyp, yyarg[yyi++]);
		  yyf += 2;
		}
	      else
		{
		  yyp++;
		  yyf++;
		}
	    }
	}
      return yysize;
    }
}
#endif /* YYERROR_VERBOSE */


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, YYLTYPE *yylocationp)
#else
static void
yydestruct (yymsg, yytype, yyvaluep, yylocationp)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
    YYLTYPE *yylocationp;
#endif
{
  YYUSE (yyvaluep);
  YYUSE (yylocationp);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */

#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */



/* The look-ahead symbol.  */
int yychar;

/* The semantic value of the look-ahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;
/* Location data for the look-ahead symbol.  */
YYLTYPE yylloc;



/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
  
  int yystate;
  int yyn;
  int yyresult;
  /* Number of tokens to shift before error messages enabled.  */
  int yyerrstatus;
  /* Look-ahead token as an internal (translated) token number.  */
  int yytoken = 0;
#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

  /* Three stacks and their tools:
     `yyss': related to states,
     `yyvs': related to semantic values,
     `yyls': related to locations.

     Refer to the stacks thru separate pointers, to allow yyoverflow
     to reallocate them elsewhere.  */

  /* The state stack.  */
  yytype_int16 yyssa[YYINITDEPTH];
  yytype_int16 *yyss = yyssa;
  yytype_int16 *yyssp;

  /* The semantic value stack.  */
  YYSTYPE yyvsa[YYINITDEPTH];
  YYSTYPE *yyvs = yyvsa;
  YYSTYPE *yyvsp;

  /* The location stack.  */
  YYLTYPE yylsa[YYINITDEPTH];
  YYLTYPE *yyls = yylsa;
  YYLTYPE *yylsp;
  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[2];

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  YYSIZE_T yystacksize = YYINITDEPTH;

  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY;		/* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */

  yyssp = yyss;
  yyvsp = yyvs;
  yylsp = yyls;
#if YYLTYPE_IS_TRIVIAL
  /* Initialize the default location before parsing starts.  */
  yylloc.first_line   = yylloc.last_line   = 1;
  yylloc.first_column = yylloc.last_column = 0;
#endif

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;
	YYLTYPE *yyls1 = yyls;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yyls1, yysize * sizeof (*yylsp),
		    &yystacksize);
	yyls = yyls1;
	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss);
	YYSTACK_RELOCATE (yyvs);
	YYSTACK_RELOCATE (yyls);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     look-ahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to look-ahead token.  */
  yyn = yypact[yystate];
  if (yyn == YYPACT_NINF)
    goto yydefault;

  /* Not known => get a look-ahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid look-ahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yyn == 0 || yyn == YYTABLE_NINF)
	goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the look-ahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token unless it is eof.  */
  if (yychar != YYEOF)
    yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;
  *++yylsp = yylloc;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];

  /* Default location.  */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 357 "highlight/src/gram.y"
    { return 0; ;}
    break;

  case 3:
#line 358 "highlight/src/gram.y"
    { return xxvalue(NULL,2); ;}
    break;

  case 4:
#line 359 "highlight/src/gram.y"
    { return xxvalue((yyvsp[(1) - (2)]),3); ;}
    break;

  case 5:
#line 360 "highlight/src/gram.y"
    { return xxvalue((yyvsp[(1) - (2)]),4); ;}
    break;

  case 6:
#line 361 "highlight/src/gram.y"
    { YYABORT; ;}
    break;

  case 7:
#line 364 "highlight/src/gram.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 8:
#line 365 "highlight/src/gram.y"
    { (yyval) = (yyvsp[(1) - (1)]); ;}
    break;

  case 9:
#line 368 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); setId( (yyval), (yyloc) ) ; ;}
    break;

  case 10:
#line 371 "highlight/src/gram.y"
    { (yyval) = (yyvsp[(1) - (1)]);  setId( (yyval), (yyloc)); ;}
    break;

  case 11:
#line 372 "highlight/src/gram.y"
    { (yyval) = (yyvsp[(1) - (1)]);  setId( (yyval), (yyloc)); ;}
    break;

  case 12:
#line 373 "highlight/src/gram.y"
    { (yyval) = (yyvsp[(1) - (1)]);  setId( (yyval), (yyloc)); ;}
    break;

  case 13:
#line 374 "highlight/src/gram.y"
    { (yyval) = (yyvsp[(1) - (1)]);  setId( (yyval), (yyloc)); ;}
    break;

  case 14:
#line 376 "highlight/src/gram.y"
    { (yyval) = xxexprlist((yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]));  setId( (yyval), (yyloc)); ;}
    break;

  case 15:
#line 377 "highlight/src/gram.y"
    { (yyval) = xxparen((yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]));		setId( (yyval), (yyloc)); ;}
    break;

  case 16:
#line 379 "highlight/src/gram.y"
    { (yyval) = xxunary((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 17:
#line 380 "highlight/src/gram.y"
    { (yyval) = xxunary((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 18:
#line 381 "highlight/src/gram.y"
    { (yyval) = xxunary((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 19:
#line 382 "highlight/src/gram.y"
    { (yyval) = xxunary((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 20:
#line 383 "highlight/src/gram.y"
    { (yyval) = xxunary((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 21:
#line 385 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 22:
#line 386 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 23:
#line 387 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 24:
#line 388 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 25:
#line 389 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 26:
#line 390 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 27:
#line 391 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 28:
#line 392 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 29:
#line 393 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 30:
#line 394 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 31:
#line 395 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 32:
#line 396 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 33:
#line 397 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 34:
#line 398 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 35:
#line 399 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 36:
#line 400 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 37:
#line 401 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 38:
#line 402 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 39:
#line 403 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 40:
#line 404 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));     setId( (yyval), (yyloc)); ;}
    break;

  case 41:
#line 406 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); setId( (yyval), (yyloc)); ;}
    break;

  case 42:
#line 407 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]),(yyvsp[(1) - (3)])); setId( (yyval), (yyloc)); ;}
    break;

  case 43:
#line 409 "highlight/src/gram.y"
    { (yyval) = xxdefun((yyvsp[(1) - (6)]),(yyvsp[(3) - (6)]),(yyvsp[(6) - (6)]));             setId( (yyval), (yyloc)); ;}
    break;

  case 44:
#line 411 "highlight/src/gram.y"
    { (yyval) = xxfuncall((yyvsp[(1) - (4)]),(yyvsp[(3) - (4)]));  setId( (yyval), (yyloc)); modif_token( &(yylsp[(1) - (4)]), SYMBOL_FUNCTION_CALL ) ; ;}
    break;

  case 45:
#line 412 "highlight/src/gram.y"
    { (yyval) = xxif((yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]));    setId( (yyval), (yyloc)); ;}
    break;

  case 46:
#line 414 "highlight/src/gram.y"
    { (yyval) = xxifelse((yyvsp[(1) - (5)]),(yyvsp[(2) - (5)]),(yyvsp[(3) - (5)]),(yyvsp[(5) - (5)])); setId( (yyval), (yyloc)); ;}
    break;

  case 47:
#line 417 "highlight/src/gram.y"
    { (yyval) = xxfor((yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)])); setId( (yyval), (yyloc)); ;}
    break;

  case 48:
#line 418 "highlight/src/gram.y"
    { (yyval) = xxwhile((yyvsp[(1) - (3)]),(yyvsp[(2) - (3)]),(yyvsp[(3) - (3)]));   setId( (yyval), (yyloc)); ;}
    break;

  case 49:
#line 419 "highlight/src/gram.y"
    { (yyval) = xxrepeat((yyvsp[(1) - (2)]),(yyvsp[(2) - (2)]));         setId( (yyval), (yyloc));;}
    break;

  case 50:
#line 420 "highlight/src/gram.y"
    { (yyval) = xxsubscript((yyvsp[(1) - (5)]),(yyvsp[(2) - (5)]),(yyvsp[(3) - (5)]));       setId( (yyval), (yyloc)); ;}
    break;

  case 51:
#line 421 "highlight/src/gram.y"
    { (yyval) = xxsubscript((yyvsp[(1) - (4)]),(yyvsp[(2) - (4)]),(yyvsp[(3) - (4)]));       setId( (yyval), (yyloc)); ;}
    break;

  case 52:
#line 422 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));      	 setId( (yyval), (yyloc)); modif_token( &(yylsp[(1) - (3)]), SYMBOL_PACKAGE ) ; ;}
    break;

  case 53:
#line 423 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));      setId( (yyval), (yyloc));modif_token( &(yylsp[(1) - (3)]), SYMBOL_PACKAGE ) ; ;}
    break;

  case 54:
#line 424 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));      setId( (yyval), (yyloc)); ;}
    break;

  case 55:
#line 425 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));          setId( (yyval), (yyloc)); ;}
    break;

  case 56:
#line 426 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));          setId( (yyval), (yyloc)); modif_token( &(yylsp[(1) - (3)]), SYMBOL_PACKAGE ) ;;}
    break;

  case 57:
#line 427 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));      setId( (yyval), (yyloc)); modif_token( &(yylsp[(1) - (3)]), SYMBOL_PACKAGE ) ;;}
    break;

  case 58:
#line 428 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));      setId( (yyval), (yyloc)); ;}
    break;

  case 59:
#line 429 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]) );     setId( (yyval), (yyloc));;}
    break;

  case 60:
#line 430 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));              setId( (yyval), (yyloc)); ;}
    break;

  case 61:
#line 431 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));              setId( (yyval), (yyloc)); ;}
    break;

  case 62:
#line 432 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));              setId( (yyval), (yyloc)); modif_token( &(yylsp[(3) - (3)]), SLOT ) ; ;}
    break;

  case 63:
#line 433 "highlight/src/gram.y"
    { (yyval) = xxbinary((yyvsp[(2) - (3)]),(yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]));              setId( (yyval), (yyloc)); ;}
    break;

  case 64:
#line 434 "highlight/src/gram.y"
    { (yyval) = xxnxtbrk((yyvsp[(1) - (1)]));                       setId( (yyval), (yyloc)); ;}
    break;

  case 65:
#line 435 "highlight/src/gram.y"
    { (yyval) = xxnxtbrk((yyvsp[(1) - (1)]));                       setId( (yyval), (yyloc)); ;}
    break;

  case 66:
#line 439 "highlight/src/gram.y"
    { (yyval) = xxcond((yyvsp[(2) - (3)])); ;}
    break;

  case 67:
#line 442 "highlight/src/gram.y"
    { (yyval) = xxifcond((yyvsp[(2) - (3)])); ;}
    break;

  case 68:
#line 445 "highlight/src/gram.y"
    { (yyval) = xxforcond((yyvsp[(2) - (5)]),(yyvsp[(4) - (5)])); ;}
    break;

  case 69:
#line 449 "highlight/src/gram.y"
    { (yyval) = xxexprlist0(); ;}
    break;

  case 70:
#line 450 "highlight/src/gram.y"
    { (yyval) = xxexprlist1((yyvsp[(1) - (1)])); ;}
    break;

  case 71:
#line 451 "highlight/src/gram.y"
    { (yyval) = xxexprlist2((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 72:
#line 452 "highlight/src/gram.y"
    { (yyval) = (yyvsp[(1) - (2)]); ;}
    break;

  case 73:
#line 453 "highlight/src/gram.y"
    { (yyval) = xxexprlist2((yyvsp[(1) - (3)]), (yyvsp[(3) - (3)])); ;}
    break;

  case 74:
#line 454 "highlight/src/gram.y"
    { (yyval) = (yyvsp[(1) - (2)]);;}
    break;

  case 75:
#line 457 "highlight/src/gram.y"
    { (yyval) = xxsublist1((yyvsp[(1) - (1)])); ;}
    break;

  case 76:
#line 458 "highlight/src/gram.y"
    { (yyval) = xxsublist2((yyvsp[(1) - (4)]),(yyvsp[(4) - (4)])); ;}
    break;

  case 77:
#line 461 "highlight/src/gram.y"
    { (yyval) = xxsub0(); 				;}
    break;

  case 78:
#line 462 "highlight/src/gram.y"
    { (yyval) = xxsub1((yyvsp[(1) - (1)]), &(yylsp[(1) - (1)])); 		;}
    break;

  case 79:
#line 463 "highlight/src/gram.y"
    { (yyval) = xxsymsub0((yyvsp[(1) - (2)]), &(yylsp[(1) - (2)])); 	modif_token( &(yylsp[(2) - (2)]), EQ_SUB ) ; modif_token( &(yylsp[(1) - (2)]), SYMBOL_SUB ) ; ;}
    break;

  case 80:
#line 464 "highlight/src/gram.y"
    { (yyval) = xxsymsub1((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]), &(yylsp[(1) - (3)])); 	modif_token( &(yylsp[(2) - (3)]), EQ_SUB ) ; modif_token( &(yylsp[(1) - (3)]), SYMBOL_SUB ) ; ;}
    break;

  case 81:
#line 465 "highlight/src/gram.y"
    { (yyval) = xxsymsub0((yyvsp[(1) - (2)]), &(yylsp[(1) - (2)])); 	modif_token( &(yylsp[(2) - (2)]), EQ_SUB ) ; ;}
    break;

  case 82:
#line 466 "highlight/src/gram.y"
    { (yyval) = xxsymsub1((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]), &(yylsp[(1) - (3)])); 	modif_token( &(yylsp[(2) - (3)]), EQ_SUB ) ; ;}
    break;

  case 83:
#line 467 "highlight/src/gram.y"
    { (yyval) = xxnullsub0(&(yylsp[(1) - (2)])); 		modif_token( &(yylsp[(2) - (2)]), EQ_SUB ) ; ;}
    break;

  case 84:
#line 468 "highlight/src/gram.y"
    { (yyval) = xxnullsub1((yyvsp[(3) - (3)]), &(yylsp[(1) - (3)])); 	modif_token( &(yylsp[(2) - (3)]), EQ_SUB ) ; ;}
    break;

  case 85:
#line 471 "highlight/src/gram.y"
    { (yyval) = xxnullformal(); ;}
    break;

  case 86:
#line 472 "highlight/src/gram.y"
    { (yyval) = xxfirstformal0((yyvsp[(1) - (1)])); 			modif_token( &(yylsp[(1) - (1)]), SYMBOL_FORMALS ) ; ;}
    break;

  case 87:
#line 473 "highlight/src/gram.y"
    { (yyval) = xxfirstformal1((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)])); 			modif_token( &(yylsp[(1) - (3)]), SYMBOL_FORMALS ) ; modif_token( &(yylsp[(2) - (3)]), EQ_FORMALS ) ; ;}
    break;

  case 88:
#line 474 "highlight/src/gram.y"
    { (yyval) = xxaddformal0((yyvsp[(1) - (3)]),(yyvsp[(3) - (3)]), &(yylsp[(3) - (3)])); 		modif_token( &(yylsp[(3) - (3)]), SYMBOL_FORMALS ) ; ;}
    break;

  case 89:
#line 476 "highlight/src/gram.y"
    { (yyval) = xxaddformal1((yyvsp[(1) - (5)]),(yyvsp[(3) - (5)]),(yyvsp[(5) - (5)]),&(yylsp[(3) - (5)]));		modif_token( &(yylsp[(3) - (5)]), SYMBOL_FORMALS ) ; modif_token( &(yylsp[(4) - (5)]), EQ_FORMALS ) ;;}
    break;

  case 90:
#line 479 "highlight/src/gram.y"
    { EatLines = 1; ;}
    break;


/* Line 1267 of yacc.c.  */
#line 2469 "highlight/src/gram.c"
      default: break;
    }
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
      {
	YYSIZE_T yysize = yysyntax_error (0, yystate, yychar);
	if (yymsg_alloc < yysize && yymsg_alloc < YYSTACK_ALLOC_MAXIMUM)
	  {
	    YYSIZE_T yyalloc = 2 * yysize;
	    if (! (yysize <= yyalloc && yyalloc <= YYSTACK_ALLOC_MAXIMUM))
	      yyalloc = YYSTACK_ALLOC_MAXIMUM;
	    if (yymsg != yymsgbuf)
	      YYSTACK_FREE (yymsg);
	    yymsg = (char *) YYSTACK_ALLOC (yyalloc);
	    if (yymsg)
	      yymsg_alloc = yyalloc;
	    else
	      {
		yymsg = yymsgbuf;
		yymsg_alloc = sizeof yymsgbuf;
	      }
	  }

	if (0 < yysize && yysize <= yymsg_alloc)
	  {
	    (void) yysyntax_error (yymsg, yystate, yychar);
	    yyerror (yymsg);
	  }
	else
	  {
	    yyerror (YY_("syntax error"));
	    if (yysize != 0)
	      goto yyexhaustedlab;
	  }
      }
#endif
    }

  yyerror_range[0] = yylloc;

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse look-ahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval, &yylloc);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse look-ahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  yyerror_range[0] = yylsp[1-yylen];
  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (yyn != YYPACT_NINF)
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;

      yyerror_range[0] = *yylsp;
      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp, yylsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  if (yyn == YYFINAL)
    YYACCEPT;

  *++yyvsp = yylval;

  yyerror_range[1] = yylloc;
  /* Using YYLLOC is tempting, but would change the location of
     the look-ahead.  YYLOC is available though.  */
  YYLLOC_DEFAULT (yyloc, (yyerror_range - 1), 2);
  *++yylsp = yyloc;

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#ifndef yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEOF && yychar != YYEMPTY)
     yydestruct ("Cleanup: discarding lookahead",
		 yytoken, &yylval, &yylloc);
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp, yylsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}


#line 481 "highlight/src/gram.y"

/*}}}*/
/*}}}*/

/*{{{ Private pushback, since file ungetc only guarantees one byte.
   We need up to one MBCS-worth */
#define DECLARE_YYTEXT_BUFP(bp) char *bp = yytext_ ;
#define YYTEXT_PUSH(c, bp) do { \
    if ((bp) - yytext_ >= sizeof(yytext_) - 1){ \
		error(_("input buffer overflow at line %d"), xxlineno); \
	} \
	*(bp)++ = (c); \
} while(0) ;
/*}}}*/
	
/*{{{ set of functions used in the parsing process */

/*{{{ Get the next or the previous character from the stream */

/** 
 * Gets the next character and update the trackers xxlineno, 
 * xxcolno, xxbyteno, ...
 * 
 * @return the next character
 */
static int xxgetc(void) {
    int c;

    if(npush) {
		c = pushback[--npush]; 
	} else  {
		c = ptr_getc();
	}

    prevpos = (prevpos + 1) % PUSHBACK_BUFSIZE;
    prevcols[prevpos] = xxcolno;
    prevbytes[prevpos] = xxbyteno;
    prevlines[prevpos] = xxlineno;    
    
    if (c == EOF) {
		EndOfFile = 1;
		return R_EOF;
    }
    H_ParseContextLast = (H_ParseContextLast + 1) % PARSE_CONTEXT_SIZE;
    H_ParseContext[H_ParseContextLast] = c;

    if (c == '\n') {
		xxlineno += 1;
		xxcolno = 0;
    	xxbyteno = 0;
    } else {
        xxcolno++;
    	xxbyteno++;
    }
    /* only advance column for 1st byte in UTF-8 */
    if (0x80 <= (unsigned char)c && (unsigned char)c <= 0xBF && known_to_be_utf8){ 
    	xxcolno--;
	}

    if (c == '\t'){
		xxcolno = ((xxcolno + 7) & ~7);
	}
    
    H_ParseContextLine = xxlineno;    

    xxcharcount++;
    return c;
}

/**
 * Returns to the state previous to the call to xxgetc
 *
 * @return the character before
 */ 
static int xxungetc(int c) {
	
    /* this assumes that c was the result of xxgetc; if not, some edits will be needed */
    xxlineno = prevlines[prevpos];
    xxbyteno = prevbytes[prevpos];
    xxcolno  = prevcols[prevpos];
    prevpos = (prevpos + PUSHBACK_BUFSIZE - 1) % PUSHBACK_BUFSIZE;

    H_ParseContextLine = xxlineno;
    xxcharcount--;
    H_ParseContext[H_ParseContextLast] = '\0';
    
	/* precaution as to how % is implemented for < 0 numbers */
    H_ParseContextLast = (H_ParseContextLast + PARSE_CONTEXT_SIZE -1) % PARSE_CONTEXT_SIZE;
    if(npush >= PUSHBACK_BUFSIZE) return EOF;
    pushback[npush++] = c;
    return c;
}

/*}}}*/

/*{{{ Deal with formal arguments of functions*/
               
/**
 * Returns a NULL (R_NilValue) used for functions without arguments
 * f <- function(){}
 * 
 * @return a NULL
 */
static SEXP xxnullformal(){
    SEXP ans;
    PROTECT(ans = R_NilValue);
	return ans;
}

/**
 * first formal argument, not being associated to a value
 * f <- function( x ){}
 *
 * @param sym name of the symbol
 * @return
 */ 
static SEXP xxfirstformal0(SEXP sym){
    SEXP ans;
    UNPROTECT_PTR(sym);
    PROTECT(ans = FirstArg(R_MissingArg, sym));
    return ans;
}

/**
 * first formal argument with a value
 * f <- function(x=2){}
 * 
 * @param sym name of the formal argument
 * @param expr expression to use for the value of the formal argument
 * @return the new built formal argument list
 */
static SEXP xxfirstformal1(SEXP sym, SEXP expr){
    SEXP ans;
    PROTECT(ans = FirstArg(expr, sym));
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
   return ans;
}

/**
 * Adds another formal argument name without value
 * 
 * @param formlist the list of formal arguments so far
 * @param sym name of the new formal argument
 * @param lloc location information
 * @return the modified formals list
 */
static SEXP xxaddformal0(SEXP formlist, SEXP sym, YYLTYPE *lloc) {
	
    SEXP ans;
    CheckFormalArgs(formlist, sym, lloc);
	PROTECT(ans = NextArg(formlist, R_MissingArg, sym));
    UNPROTECT_PTR(sym);
    UNPROTECT_PTR(formlist);
    return ans;
}

/**
 * Adds another formal argument, with the value
 *
 * @param formlist previously existing formal argument list
 * @param sym name of the new argument to add
 * @param expr expression to use as the value of the new argument
 * @param lloc location information
 * @return the formal argument list, with the new argument added
 */
static SEXP xxaddformal1(SEXP formlist, SEXP sym, SEXP expr, YYLTYPE *lloc)
{
    SEXP ans;
    CheckFormalArgs(formlist, sym, lloc);
	PROTECT(ans = NextArg(formlist, expr, sym));
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    UNPROTECT_PTR(formlist);
    return ans;
}


/**
 * Creates a formals argument list
 *
 * @param s the expression to use as the value of the formal argument
 * @param tag the name of the argument
 * @return the newly built formals argument list
 */
static SEXP FirstArg(SEXP s, SEXP tag){
    SEXP tmp;
    PROTECT(s);
    PROTECT(tag);
    PROTECT(tmp = NewList());
    tmp = GrowList(tmp, s);
    SET_TAG(CAR(tmp), tag);
    UNPROTECT(3);
    return tmp;
}

/**
 * Called to add a formal argument 
 *
 * @param l already existing formal argument list
 * @param s value of the argument
 * @param tag name of the arrgument
 */
static SEXP NextArg(SEXP l, SEXP s, SEXP tag) {
    PROTECT(tag);
    PROTECT(l);
    l = GrowList(l, s);
    SET_TAG(CAR(l), tag);
    UNPROTECT(2);
    return l;
} /*}}}*/

/*{{{ expression lists */

/**
 * Expression list
 * 
 * @param a1 the left brace
 * @param lloc location information for ??
 * @param a2 the expression list
 */
static SEXP xxexprlist(SEXP a1, SEXP a2) {
    SEXP ans;
    EatLines = 0;
    SET_TYPEOF(a2, LANGSXP);
	SETCAR(a2, a1);
	PROTECT(ans = a2);
    UNPROTECT_PTR(a2);
	return ans;
}

/**
 * Starts an expression list (after the left brace)
 * Creates a new list using NewList. 
 *
 * @return the newly created expression list
 */
static SEXP xxexprlist0(void) {
    SEXP ans;
    PROTECT(ans = NewList()); 
    return ans;
}

/**
 * Creates the first expression in an expression list
 * 
 * @param expr the first expression
 * @return an expression list with one expression (given by expr)
 */
static SEXP xxexprlist1(SEXP expr){
    SEXP ans,tmp;
    PROTECT(tmp = NewList());
	PROTECT(ans = GrowList(tmp, expr));
	UNPROTECT_PTR(tmp);
    UNPROTECT_PTR(expr);
    return ans;
}

/**
 * Adds an expression to an already existing expression list
 *
 * @param exprlist the current expression list
 * @param expr the expression to add
 * @return the modified expression list
 */
static SEXP xxexprlist2(SEXP exprlist, SEXP expr){
    SEXP ans;
    PROTECT(ans = GrowList(exprlist, expr));
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(exprlist);
    return ans;
}
/*}}}*/

/*{{{ subscripts */

/**
 * Single or double subscripts
 *
 * @param a1 expression before the square bracket
 * @param a2 either one square bracket or two
 * @param a3 the content
 *
 * @note
 * This should probably use CONS rather than LCONS, but
 * it shouldn't matter and we would rather not meddle
 * See PR#7055 
 */
static SEXP xxsubscript(SEXP a1, SEXP a2, SEXP a3){
    SEXP ans;
    PROTECT(ans = LCONS(a2, CONS(a1, CDR(a3))));
    UNPROTECT_PTR(a3);
    UNPROTECT_PTR(a1);
    return ans;
}


/*{{{ atomic subsets */
/**
 * Empty subset
 *
 * @return the subset
 */
static SEXP xxsub0(void){
    SEXP ans;
    PROTECT(ans = lang2(R_MissingArg,R_NilValue));
    return ans;
}

/**
 * subset with one expression
 *
 * @param expr expression that is inside the subset
 * @param lloc location information for the expression
 * @return the subset
 */
static SEXP xxsub1(SEXP expr, YYLTYPE *lloc){
    SEXP ans;
    PROTECT(ans = TagArg(expr, R_NilValue, lloc));
    UNPROTECT_PTR(expr);
    return ans;
}

/**
 * subset with a symbol or a character constant followed by the 
 * equal sign
 * 
 * examples: 
 * x[y=]
 * x['y'=]
 *
 * @param sym the symbol name (or character constant)
 * @param the location information of the symbol
 * @return the subset
 */
static SEXP xxsymsub0(SEXP sym, YYLTYPE *lloc){
    SEXP ans;
    PROTECT(ans = TagArg(R_MissingArg, sym, lloc));
    UNPROTECT_PTR(sym);
    return ans;
}

/**
 * Subset with exactly one expression. symbol name = expression
 *
 * examples: 
 * x[ y = 2 ]
 * x[ 'y' = 2 ]
 * 
 * @param sym symbol name (or character constant)
 * @param expr expression after the equal sign
 * @return the subset
 */
static SEXP xxsymsub1(SEXP sym, SEXP expr, YYLTYPE *lloc){
    SEXP ans;
    PROTECT(ans = TagArg(expr, sym, lloc));
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    return ans;
}

/**
 * Subset where the lhs of the subset is NULL and there is no rhs
 *
 * examples: 
 * x[ NULL = ]
 *
 * @param lloc location information for the NULL
 * @return the subset
 */
static SEXP xxnullsub0(YYLTYPE *lloc){
    SEXP ans;
    UNPROTECT_PTR(R_NilValue);
    PROTECT(ans = TagArg(R_MissingArg, install("NULL"), lloc));
    return ans;
}

/**
 * Subset where NULL is the lhs and there is a rhs
 * 
 * examples: 
 * x[ NULL = 2 ]
 *
 * @param expr expression to use at the rhs of the subset
 * @param lloc location information for the expression
 * @return the subset
 */
static SEXP xxnullsub1(SEXP expr, YYLTYPE *lloc) {
    SEXP ans = install("NULL");
    UNPROTECT_PTR(R_NilValue);
    PROTECT(ans = TagArg(expr, ans, lloc));
    UNPROTECT_PTR(expr);
    return ans;
}
/*}}}*/

/*{{{ subset lists */
/**
 * Subset list with exactly one subset
 * 
 * @param sub the subset to use
 * @return the subset list with this subset
 */ 
static SEXP xxsublist1(SEXP sub){
    SEXP ans;
    PROTECT(ans = FirstArg(CAR(sub),CADR(sub)));
    UNPROTECT_PTR(sub);
    return ans;
}

/**
 * Subset list with two or more subsets
 * 
 * @param sublist subset list so far
 * @param new subset to append
 * @return the subset list with the new subset added
 */ 
static SEXP xxsublist2(SEXP sublist, SEXP sub){
    SEXP ans;
    PROTECT(ans = NextArg(sublist, CAR(sub), CADR(sub)));
    UNPROTECT_PTR(sub);
    UNPROTECT_PTR(sublist);
    return ans;
}
/*}}}*/
/*}}}*/

/*{{{ Conditional things */
/**
 * Expression with round brackets
 *
 * This function has the side effect of setting the "EatLines"
 * variable to 1
 *
 * @param expr the expression 
 * @return the expression it was passed
 * 
 */ 
static SEXP xxcond(SEXP expr){
    EatLines = 1;
    return expr;
}

/**
 * expression within a if statement. 
 * 
 * This function has the side effect of setting the "EatLines"
 * variable to 1
 *
 * @param expr the expression inside the if call
 * @return the same expression
 */
static SEXP xxifcond(SEXP expr){
    EatLines = 1;
    return expr;
}

/**
 * if( cond ) expr
 *
 * @param ifsym the if expression
 * @param cond the content of condition
 * @param expr the expression in the "body" of the if
 * @return the expression surrounded by the if condition 
 */
static SEXP xxif(SEXP ifsym, SEXP cond, SEXP expr){
    SEXP ans;
    PROTECT(ans = lang3(ifsym, cond, expr));
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(cond);
	
    return ans;
}

/**
 * if( cond ) ifexpr else elseexpr
 * 
 * @param ifsym the if symbol
 * @param cond the condition
 * @param ifexpr the expression when the condition is TRUE
 * @param elseexpr the expressionn when the condition is FALSE
 * 
 */
static SEXP xxifelse(SEXP ifsym, SEXP cond, SEXP ifexpr, SEXP elseexpr){
    SEXP ans;
    PROTECT(ans = lang4(ifsym, cond, ifexpr, elseexpr));
	UNPROTECT_PTR(elseexpr);
    UNPROTECT_PTR(ifexpr);
    UNPROTECT_PTR(cond);
	return ans;
}

/**
 * content of a for loop 
 *
 * rule: 
 * '(' SYMBOL IN expr ')' 		{ $$ = xxforcond($2,$4); }
 *
 * @param sym the symbol used at the left of the in
 * @param expr the expression used at the right of the in
 * @return the content of the round bracket part of the for loop
 */
static SEXP xxforcond(SEXP sym, SEXP expr){
    SEXP ans;
    EatLines = 1;
    PROTECT(ans = LCONS(sym, expr));
    UNPROTECT_PTR(expr);
    UNPROTECT_PTR(sym);
    return ans;
}
/*}}}*/

/*{{{ Loops */
/** 
 * for loop
 * 
 * @param forsym the for symbol
 * @param forcond content of the condition (see xxforcond)
 * @param body body of the for loop
 * @return the whole for loop expression
 */
static SEXP xxfor(SEXP forsym, SEXP forcond, SEXP body){
    SEXP ans;
    PROTECT(ans = lang4(forsym, CAR(forcond), CDR(forcond), body));
    UNPROTECT_PTR(body);
    UNPROTECT_PTR(forcond);
   return ans;
}

/**
 * while loop
 * 
 * @param whilesym while symbol
 * @param cond condition of the while
 * @param body body of the while loop
 * @return the whole while expression
 */
static SEXP xxwhile(SEXP whilesym, SEXP cond, SEXP body){
    SEXP ans;
    PROTECT(ans = lang3(whilesym, cond, body));
	UNPROTECT_PTR(body);
    UNPROTECT_PTR(cond);
    return ans;
}

/**
 * the repeat loop
 * 
 * @param repeatsym the repeat symbol
 * @param body the body of the repeat
 * @return the whole repeat expression
 */
static SEXP xxrepeat(SEXP repeatsym, SEXP body){
    SEXP ans;
    PROTECT(ans = lang2(repeatsym, body));
    UNPROTECT_PTR(body);
    return ans;
}

/** 
 * next or break
 * 
 * @param keyword one of next or break
 * @return the keyword (passed through lang1)
 */ 
static SEXP xxnxtbrk(SEXP keyword){
    PROTECT(keyword = lang1(keyword));
	return keyword;
}
/*}}}*/

/*{{{ Functions */
/** 
 * function call
 * 
 * @param expr expression used as the calling function
 * @param args list of arguments
 * @return the whole function call
 */
static SEXP xxfuncall(SEXP expr, SEXP args){
	SEXP ans, sav_expr = expr;
    if (isString(expr)){
	    expr = install(CHAR(STRING_ELT(expr, 0)));
	}
	PROTECT(expr);
	if (length(CDR(args)) == 1 && CADR(args) == R_MissingArg && TAG(CDR(args)) == R_NilValue ){
	    ans = lang1(expr);
	} else {
	    ans = LCONS(expr, CDR(args));
	}
	UNPROTECT(1);
	PROTECT(ans);
    UNPROTECT_PTR(args);
    UNPROTECT_PTR(sav_expr);
	
    return ans;
}

/** 
 * Function definition
 *
 * @param fname the name of the function
 * @param formals the list of formal arguments
 * @param body the body of the function
 * @return the expression containing the function definition
 */
static SEXP xxdefun(SEXP fname, SEXP formals, SEXP body){
	SEXP ans;
    SEXP source;
    PROTECT(source = R_NilValue);
	PROTECT(ans = lang4(fname, CDR(formals), body, source));
	UNPROTECT_PTR(source);
    UNPROTECT_PTR(body);
    UNPROTECT_PTR(formals);
    
    return ans;
}
/*}}}*/

/*{{{ Operators unary and binary */
/**
 * Unary operator
 *
 * @param op operator
 * @param arg expression used as the argument of the operator
 * @return the operator applied to the expression
 */
static SEXP xxunary(SEXP op, SEXP arg){
    SEXP ans;
    PROTECT(ans = lang2(op, arg));
    UNPROTECT_PTR(arg);
    return ans;
}

/**
 * Binary operator
 * 
 * @param n1 expression before the binary operator
 * @param n2 the binary operator
 * @param n3 expression after the binary operator
 * @return the expression n1 n2 n3
 */
static SEXP xxbinary(SEXP n1, SEXP n2, SEXP n3){
    SEXP ans;
    PROTECT(ans = lang3(n1, n2, n3));
    UNPROTECT_PTR(n2);
    UNPROTECT_PTR(n3);
	
   return ans;
}
/*}}}*/

/**
 * Content of round brackets
 *
 * @param n1 the round bracket
 * @param n2 the expression inside
 * @return the n2 expression wrapped in brackets
 */
static SEXP xxparen(SEXP n1, SEXP n2){
    SEXP ans;
    PROTECT(ans = lang2(n1, n2));
	UNPROTECT_PTR(n2);
    return ans;
}

/**
 * Returns the value of the expression, called after '\n' or ';'
 *
 * @param v 
 * @param k 
 * @param lloc location information
 * @return 
 */        
static int xxvalue(SEXP v, int k) {
    if (k > 2) {
		UNPROTECT_PTR(v);
    }
	
	R_CurrentExpr = v;
    return k;
}

static SEXP mkString2(const char *s, int len){
    SEXP t;
    cetype_t enc = CE_NATIVE;

    if(known_to_be_latin1) {
		enc= CE_LATIN1;
	} else if(known_to_be_utf8) {
		enc = CE_UTF8;
	}

    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharLenCE(s, len, enc));
    UNPROTECT(1);
    return t;
}
/*}}} xx */

/*{{{ IfPush and ifpop */
static void IfPush(void) {
    if (*contextp==LBRACE || *contextp=='[' || *contextp=='('  || *contextp == 'i') {
		if(contextp - contextstack >= CONTEXTSTACK_SIZE){
		    error(_("contextstack overflow"));
		}
		*++contextp = 'i';
    }

}

static void ifpop(void){
    if (*contextp=='i')
	*contextp-- = 0;
}

/*}}}*/

/*{{{ typeofnext */
/**
 * Looks at the type of the next character
 *
 * @return 1 if the next character is a digit, 2 otherwise
 */
static int typeofnext(void) {
    int k, c;

    c = xxgetc();
    if (isdigit(c)){
		k = 1; 
	} else {
		k = 2;
	}
    xxungetc(c);
    return k;
}
/*}}}*/

/*{{{ nextchar */
/** 
 * Moves forward one character, checks that we get what we expect.
 * if not return 0 and move back to where it started
 * 
 * @param expect the character that is expected next
 * @return 1 if the the next character is the one we expect, otherwise 
 *    go back and return 0
 */
static int nextchar(int expect){
    int c = xxgetc();
    if (c == expect){
		return 1;
    } else {
		xxungetc(c);
	}
    return 0;
}
/*}}}*/

/*{{{ SkipComment */
/* Note that with interactive use, EOF cannot occur inside */
/* a comment.  However, semicolons inside comments make it */
/* appear that this does happen.  For this reason we use the */
/* special assignment EndOfFile=2 to indicate that this is */
/* going on.  This is detected and dealt with in Parse1Buffer. */

/**
 * Flush away comments. Keep going to the next character until the 
 * end of the line.
 *
 * This version differs from the one in the core R parser so that 
 * it records comments (via a call to record_). Also, the distinction
 * is made between comments and roxygen comments
 * 
 */
static int SkipComment(void){
    int c;
	/* locations before the # character was read */
	int _first_line   = yylloc.first_line   ;
	int _first_column = yylloc.first_column ;
	int _first_byte   = yylloc.first_byte   ;
	
	// we want to track down the character
	// __before__ the new line character
	int _last_line    = xxlineno ;
	int _last_column  = xxcolno ;
	int _last_byte    = xxbyteno ;
	int type = COMMENT ;
	
	// get first character of the comment so that we can check if this 
	// is a Roxygen comment
	c = xxgetc() ;
	if( c != '\n' && c != R_EOF ){
		_last_line = xxlineno ;
		_last_column = xxcolno ;
		_last_byte = xxbyteno ;
		if( c == '\'' ){
			type = ROXYGEN_COMMENT ;
		}
		while ((c = xxgetc()) != '\n' && c != R_EOF){
			_last_line = xxlineno ;
			_last_column = xxcolno ;
			_last_byte = xxbyteno ;
		}
	}
	if (c == R_EOF) {
		EndOfFile = 2;
	}
	incrementId( ) ;
	record_( _first_line,  _first_column, _first_byte, 
			_last_line, _last_column, _last_byte, 
			type, identifier ) ;
	nterminals++ ;
	return c ;
}
/*}}}*/

/*{{{ NumericValue*/
/**
 * Converts to a numeric value
 */ 
static int NumericValue(int c) {
    int seendot = (c == '.');
    int seenexp = 0;
    int last = c;
    int nd = 0;
    int asNumeric = 0;

    DECLARE_YYTEXT_BUFP(yyp);
    YYTEXT_PUSH(c, yyp);
    /* We don't care about other than ASCII digits */
    while (isdigit(c = xxgetc()) || c == '.' || c == 'e' || c == 'E' || c == 'x' || c == 'X' || c == 'L'){
		if (c == 'L') /* must be at the end.  Won't allow 1Le3 (at present). */
		    break;

		if (c == 'x' || c == 'X') {
		    if (last != '0') break;
		    YYTEXT_PUSH(c, yyp);
		    while(isdigit(c = xxgetc()) || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F') || c == '.') {
				YYTEXT_PUSH(c, yyp);
				nd++;
		    }
		    if (nd == 0) return ERROR;
		    if (c == 'p' || c == 'P') {
				YYTEXT_PUSH(c, yyp);
				c = xxgetc();
				if (!isdigit(c) && c != '+' && c != '-') return ERROR;
				if (c == '+' || c == '-') {
				    YYTEXT_PUSH(c, yyp);
				    c = xxgetc();
				}
				for(nd = 0; isdigit(c); c = xxgetc(), nd++){
				    YYTEXT_PUSH(c, yyp);
				}
				if (nd == 0) return ERROR;
		    }
		    break;
		}
		
		if (c == 'E' || c == 'e') {
		    if (seenexp)
				break;
		    seenexp = 1;
		    seendot = seendot == 1 ? seendot : 2;
		    YYTEXT_PUSH(c, yyp);
		    c = xxgetc();
		    if (!isdigit(c) && c != '+' && c != '-') return ERROR;
		    if (c == '+' || c == '-') {
				YYTEXT_PUSH(c, yyp);
				c = xxgetc();
				if (!isdigit(c)) return ERROR;
		    }
		}
		
		if (c == '.') {
		    if (seendot)
			break;
		    seendot = 1;
		}
		
		YYTEXT_PUSH(c, yyp);
		last = c;
    }
	
    YYTEXT_PUSH('\0', yyp);
    /* Make certain that things are okay. */
    if(c == 'L') {
		double a = R_atof(yytext_);
		int b = (int) a;
		/* We are asked to create an integer via the L, so we check that the
		   double and int values are the same. If not, this is a problem and we
		   will not lose information and so use the numeric value.
		*/
		if(a != (double) b) {
		    if(seendot == 1 && seenexp == 0){
			    warning(_("integer literal %sL contains decimal; using numeric value"), yytext_);
			} else {
			    warning(_("non-integer value %s qualified with L; using numeric value"), yytext_);
			}
		    asNumeric = 1;
		    seenexp = 1;
		}
    }

    if(c == 'i') {
		yylval = mkComplex(yytext_) ;
    } else if(c == 'L' && asNumeric == 0) {
		if(seendot == 1 && seenexp == 0)
		    warning(_("integer literal %sL contains unnecessary decimal point"), yytext_);
		yylval = mkInt(yytext_);
#if 0  /* do this to make 123 integer not double */
    } else if(!(seendot || seenexp)) {
		if(c != 'L') xxungetc(c);
		double a = R_atof(yytext_);
		int b = (int) a;
		yylval = (a != (double) b) ? mkFloat(yytext_) : mkInt(yytext_);
#endif
    } else {
		if(c != 'L')
		    xxungetc(c);
		yylval = mkFloat(yytext_) ;
    }

    PROTECT(yylval);
    return NUM_CONST;
}
/*}}}*/

/*{{{ SkipSpace */
/**
 * Keeping track of the last character of a SPACES token
 */
static int _space_last_line ;
static int _space_last_col  ;
static int _space_last_byte ;

/**
 * Reset the variables _space_last_line, _space_last_col, _space_last_byte
 * to current positions
 */ 
static void trackspaces( ){
	_space_last_line = xxlineno ;
	_space_last_col  = xxcolno ;
	_space_last_byte = xxbyteno ;    
}

/**
 * Skips any number of spaces. This implementation differs from the one
 * used in the standard R parser so that the first the first character 
 * of a token is within the token.
 *
 * The trackspaces function is called before each call to xxgetc so that 
 * the static variables _space_last_line, ... are reset to current 
 * information __before__ reading the character
 */ 
static int SkipSpace(void) {
    int c;
	trackspaces() ;
	
#ifdef Win32
    if(!mbcslocale) { /* 0xa0 is NBSP in all 8-bit Windows locales */
		while ((c = xxgetc()) == ' ' || c == '\t' || c == '\f' || (unsigned int) c == 0xa0){
			trackspaces() ;
		}
		return c;
    } else {
		int i, clen;
		wchar_t wc;
		while (1) {
		    c = xxgetc();
			if (c == ' ' || c == '\t' || c == '\f') continue;
		    if (c == '\n' || c == R_EOF) break;
		    if ((unsigned int) c < 0x80) break;
		    clen = mbcs_get_next(c, &wc);  /* always 2 */
		    if(! Ri18n_iswctype(wc, Ri18n_wctype("blank")) ) break;
		    for(i = 1; i < clen; i++) {
				c = xxgetc();
			}
			trackspaces() ;
		}
		return c;
    }
#endif

#if defined(SUPPORT_MBCS) && defined(__STDC_ISO_10646__)
    if(mbcslocale) { /* wctype functions need Unicode wchar_t */
		int i, clen;
		wchar_t wc;
		while (1) {
		    c = xxgetc();  
		    if (c == ' ' || c == '\t' || c == '\f') continue;
		    if (c == '\n' || c == R_EOF) break;
		    if ((unsigned int) c < 0x80) break;
		    clen = mbcs_get_next(c, &wc);
		    if(! Ri18n_iswctype(wc, Ri18n_wctype("blank")) ) break;
		    for(i = 1; i < clen; i++) {
				c = xxgetc();
			}
			trackspaces() ;
		}
    } else
#endif
		{
			while ((c = xxgetc()) == ' ' || c == '\t' || c == '\f'){
				trackspaces() ;
			}
		}   
	return c;
}

/**
 * Wrapper around SkipSpace. Calls SkipSpace and reset the first location
 * if any space was actually skipped. This allows to include the first character
 * of every token in the token (which the internal R parser does not)
 */
static int SkipSpace_(void){
	int c ;
	int _space_first_line = xxlineno ;
	int _space_first_byte = xxbyteno ;
	c = SkipSpace();
	
	// if we moved only one character, it means it was not a space
	if( _space_first_line == _space_last_line && _space_first_byte == _space_last_byte ){
		// no change needed
	} else {
		
		/* so that the SPACES consumes an identifier */
		incrementId( ) ;
		// record_( 
		// 	_space_first_line, _space_first_col, _space_first_byte, 
		// 	_space_last_line, _space_last_col, _space_last_byte,  
		// 	SPACES, identifier ) ;
		// nterminals++;
		setfirstloc( _space_last_line, _space_last_col, _space_last_byte );
	}
	
	return c ;
	
}

/*}}}*/

/*{{{ Special Symbols */
/* Syntactic Keywords + Symbolic Constants */
struct {
    char *name;
    int token;
}
static keywords[] = {
    { "NULL",	    NULL_CONST },
    { "NA",	    NUM_CONST  },
    { "TRUE",	    NUM_CONST  },
    { "FALSE",	    NUM_CONST  },
    { "Inf",	    NUM_CONST  },
    { "NaN",	    NUM_CONST  },
    { "NA_integer_", NUM_CONST  },
    { "NA_real_",    NUM_CONST  },
    { "NA_character_", NUM_CONST  },
    { "NA_complex_", NUM_CONST  },
    { "function",   FUNCTION   },
    { "while",	    WHILE      },
    { "repeat",	    REPEAT     },
    { "for",	    FOR	       },
    { "if",	    IF	       },
    { "in",	    IN	       },
    { "else",	    ELSE       },
    { "next",	    NEXT       },
    { "break",	    BREAK      },
    { "...",	    SYMBOL     },
    { 0,	    0	       }
};
/*}}}*/

/*{{{ Keyword lookup */
/**
 * 
 * @note KeywordLookup has side effects, it sets yylval
 */ 
static int KeywordLookup(const char *s) {
    int i;
    for (i = 0; keywords[i].name; i++) {
		if (strcmp(keywords[i].name, s) == 0) {
		    switch (keywords[i].token) {
		    	case NULL_CONST:
					PROTECT(yylval = R_NilValue);
					break;
		    	case NUM_CONST:
					switch(i) {
						case 1:
							PROTECT(yylval = mkNA());
							break;
						case 2:
							PROTECT(yylval = mkTrue());
							break;
						case 3:
							PROTECT(yylval = mkFalse());
							break;
						case 4:
							PROTECT(yylval = allocVector(REALSXP, 1));
							REAL(yylval)[0] = R_PosInf;
							break;
						case 5:
							PROTECT(yylval = allocVector(REALSXP, 1));
							REAL(yylval)[0] = R_NaN;
							break;
						case 6:
							PROTECT(yylval = allocVector(INTSXP, 1));
							INTEGER(yylval)[0] = NA_INTEGER;
							break;
						case 7:
							PROTECT(yylval = allocVector(REALSXP, 1));
							REAL(yylval)[0] = NA_REAL;
							break;
						case 8:
							PROTECT(yylval = allocVector(STRSXP, 1));
							SET_STRING_ELT(yylval, 0, NA_STRING);
							break;
						case 9:
							PROTECT(yylval = allocVector(CPLXSXP, 1));
							COMPLEX(yylval)[0].r = COMPLEX(yylval)[0].i = NA_REAL;
							break;
					}
					break;
		    	case FUNCTION:
		    	case WHILE:
		    	case REPEAT:
		    	case FOR:
		    	case IF:
		    	case NEXT:
		    	case BREAK:
					yylval = install(s);
					break;
		    	case IN:
		    	case ELSE:
					break;
		    	case SYMBOL:
					PROTECT(yylval = install(s));
					break;
		    }
		    return keywords[i].token;
		}
    }
    return 0;
}
/*}}}*/

/*{{{ mk* functions */
/**
 * Makes a COMPLEX (CPLXSXP) from the character string
 *
 * @param s the character string to convert to a complex
 */
SEXP mkComplex(const char *s ) {
    SEXP t = R_NilValue;
    double f;
	/* FIXME: make certain the value is legitimate. */
    f = R_atof(s); 

    t = allocVector(CPLXSXP, 1);
    COMPLEX(t)[0].r = 0;
    COMPLEX(t)[0].i = f;
    
    return t;
}

/**
 * Makes a missing logical value (LGLSXP, with NA_LOGICAL)
 */
SEXP mkNA(void){
    SEXP t = allocVector(LGLSXP, 1);
    LOGICAL(t)[0] = NA_LOGICAL;
    return t;
}

/**
 * Makes a TRUE logical value (LGLSXP, with 1)
 */
SEXP mkTrue(void){
    SEXP s = allocVector(LGLSXP, 1);
    LOGICAL(s)[0] = 1;
    return s;
}

/**
 * Makes a FALSE logical value (LGLSXP, with 0)
 */
SEXP mkFalse(void){
    SEXP s = allocVector(LGLSXP, 1);
    LOGICAL(s)[0] = 0;
    return s;
}

/**
 * Makes a float (ScalarReal) from the string
 */
SEXP mkFloat(const char *s) {
    return ScalarReal(R_atof(s));
}

/**
 * Makes a int (ScalarInteger) from ths string
 */
SEXP mkInt(const char *s){
    double f = R_atof(s);  /* or R_strtol? */
    return ScalarInteger((int) f);
}
/*}}}*/

/*{{{ TagArg */
/**
 * tags
 * Report an error when incorrect tag (using first line)
 */
static SEXP TagArg(SEXP arg, SEXP tag, YYLTYPE *lloc) {
    switch (TYPEOF(tag)) {
    	case STRSXP:
			tag = install(translateChar(STRING_ELT(tag, 0)));
    	case NILSXP:
    	case SYMSXP:
			return lang2(arg, tag);
    	default:
			error(_("incorrect tag type at line %d"), lloc->first_line); 
			return R_NilValue/* -Wall */;
    }
}
/*}}}*/

/*{{{ lexer */

/*{{{ yyerror */
/**
 * The error reporting function. calls whenever bison sees a token
 * that does not fit any parser rule
 *
 * @param s the error message 
 */
static void yyerror(char *s) {
    static const char *const yytname_translations[] =
    {
    /* the left column are strings coming from bison, the right
       column are translations for users.
       The first YYENGLISH from the right column are English to be translated,
       the rest are to be copied literally.  The #if 0 block below allows xgettext
       to see these.
    */
#define YYENGLISH 8
	"$undefined",	"input",
	"END_OF_INPUT",	"end of input",
	"ERROR",	"input",
	"STR_CONST",	"string constant",
	"NUM_CONST",	"numeric constant",
	"SYMBOL",	"symbol",
	"LEFT_ASSIGN",	"assignment",
	"'\\n'",	"end of line",
	"NULL_CONST",	"'NULL'",
	"FUNCTION",	"'function'",
	"EQ_ASSIGN",	"'='",
	"RIGHT_ASSIGN",	"'->'",
	"LBB",		"'[['",
	"FOR",		"'for'",
	"IN",		"'in'",
	"IF",		"'if'",
	"ELSE",		"'else'",
	"WHILE",	"'while'",
	"NEXT",		"'next'",
	"BREAK",	"'break'",
	"REPEAT",	"'repeat'",
	"GT",		"'>'",
	"GE",		"'>='",
	"LT",		"'<'",
	"LE",		"'<='",
	"EQ",		"'=='",
	"NE",		"'!='",
	"AND",		"'&'",
	"OR",		"'|'",
	"AND2",		"'&&'",
	"OR2",		"'||'",
	"NS_GET",	"'::'",
	"NS_GET_INT",	"':::'",
	0
    };
    static char const yyunexpected[] = "syntax error, unexpected ";
    static char const yyexpecting[] = ", expecting ";
    char *expecting;
#if 0
 /* these are just here to trigger the internationalization */
    _("input");
    _("end of input");
    _("string constant");
    _("numeric constant");
    _("symbol");
    _("assignment");
    _("end of line");
#endif

    H_ParseError     = yylloc.first_line;
    H_ParseErrorCol  = yylloc.first_column;
    
    if (!strncmp(s, yyunexpected, sizeof yyunexpected -1)) {
		int i;
		/* Edit the error message */
		expecting = strstr(s + sizeof yyunexpected -1, yyexpecting);
		if (expecting) {
			*expecting = '\0';
		}
		for (i = 0; yytname_translations[i]; i += 2) {
		    if (!strcmp(s + sizeof yyunexpected - 1, yytname_translations[i])) {
				sprintf(H_ParseErrorMsg, _("unexpected %s"),
				    i/2 < YYENGLISH ? _(yytname_translations[i+1])
						    : yytname_translations[i+1]);
				return;
		    }
		}
		sprintf(H_ParseErrorMsg, _("unexpected %s"), s + sizeof yyunexpected - 1);
    } else {
		strncpy(H_ParseErrorMsg, s, PARSE_ERROR_SIZE - 1);
    }
}
/*}}}*/

/*{{{ checkFormalArgs */
/**
 * checking formal arguments. Checks that the _new argument name
 * is not already used in the formal argument list. Generates
 * an error in that case.
 * 
 * Reports an error using the first line
 * @param formlist formal arguments list
 * @param _new name of a new symbol that is to be added to the 
 *        list of arguments
 * @param lloc location information
 * 
 */
static void CheckFormalArgs(SEXP formlist, SEXP _new, YYLTYPE *lloc) {
    while (formlist != R_NilValue) {
		if (TAG(formlist) == _new) {
		    error(_("Repeated formal argument '%s' on line %d"), CHAR(PRINTNAME(_new)),
									 lloc->first_line);
		}
		formlist = CDR(formlist);
    }
}
/*}}}*/


/* Strings may contain the standard ANSI escapes and octal */
/* specifications of the form \o, \oo or \ooo, where 'o' */
/* is an octal digit. */

#define STEXT_PUSH(c) do {                  \
	unsigned int nc = bp - stext;       \
	if (nc >= nstext - 1) {             \
	    char *old = stext;              \
	    nstext *= 2;                    \
	    stext = malloc(nstext);         \
	    if(!stext) error(_("unable to allocate buffer for long string at line %d"), xxlineno);\
	    memmove(stext, old, nc);        \
	    if(old != st0) free(old);	    \
	    bp = stext+nc; }		    \
	*bp++ = (c);                        \
} while(0)
	
/*{{{ mkStringUTF8 */
#ifdef USE_UTF8_IF_POSSIBLE
#define WTEXT_PUSH(c) do { if(wcnt < 1000) wcs[wcnt++] = c; } while(0)
 
/**
 * Turns the string into UTF-8
 *
 * @param wcs 
 * @param cnt FALSE if the quote character is ' or ", TRUE otherwise
 * @return the string as utf-8
 */
static SEXP mkStringUTF8(const wchar_t *wcs, int cnt) {
    SEXP t;
    char *s;
    int nb;

/* NB: cnt includes the terminator */
#ifdef Win32
    nb = cnt*4; /* UCS-2/UTF-16 so max 4 bytes per wchar_t */
#else
    nb = cnt*6;
#endif
    s = alloca(nb);
    R_CheckStack();
    memset(s, 0, nb); /* safety */
    wcstoutf8(s, wcs, nb);
    PROTECT(t = allocVector(STRSXP, 1));
    SET_STRING_ELT(t, 0, mkCharCE(s, CE_UTF8));
    UNPROTECT(1);
    return t;
}
#else
#define WTEXT_PUSH(c)
#endif
/*}}}*/

/*{{{ CTEXT_PUSH and CTEXT_POP */
#define CTEXT_PUSH(c) do { \
	if (ct - currtext >= 1000) {memmove(currtext, currtext+100, 901); memmove(currtext, "... ", 4); ct -= 100;} \
	*ct++ = (c); \
} while(0)
#define CTEXT_POP() ct--
/*}}}*/

/*{{{ StringValue */

/**
 * Returns the string value
 * 
 * @param c the quote character used to start the string
 * @param forSymbol 
 * @param the parser state after consuming all characters
 */
static int StringValue(int c, Rboolean forSymbol) {
	
    int quote = c;
    int have_warned = 0;
    char currtext[1010], *ct = currtext;
    char st0[MAXELTSIZE];
    unsigned int nstext = MAXELTSIZE;
    char *stext = st0, *bp = st0;

#ifdef USE_UTF8_IF_POSSIBLE
    int wcnt = 0;
    wchar_t wcs[1001];
    Rboolean use_wcs = FALSE;
#endif

    while ((c = xxgetc()) != R_EOF && c != quote) {
		CTEXT_PUSH(c);
		if (c == '\n') {
		    xxungetc(c);
		    /* Fix by Mark Bravington to allow multiline strings
		     * by pretending we've seen a backslash. Was:
		     * return ERROR;
		     */
		    c = '\\';
		}
		
		if (c == '\\') {
		    c = xxgetc(); CTEXT_PUSH(c);
		    if ('0' <= c && c <= '8') { 
				int octal = c - '0';
				if ('0' <= (c = xxgetc()) && c <= '8') {
				    CTEXT_PUSH(c);
				    octal = 8 * octal + c - '0';
				    if ('0' <= (c = xxgetc()) && c <= '8') {
						CTEXT_PUSH(c);
						octal = 8 * octal + c - '0';
				    } else {
						xxungetc(c);
						CTEXT_POP();
				    }
				} else {
				    xxungetc(c);
				    CTEXT_POP();
				}
				c = octal;
		    }
		    else if(c == 'x') {
				int val = 0; int i, ext;
				for(i = 0; i < 2; i++) {
				    c = xxgetc(); CTEXT_PUSH(c);
				    if(c >= '0' && c <= '9') ext = c - '0';
				    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
				    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
				    else {
						xxungetc(c);
						CTEXT_POP();
						if (i == 0) { /* was just \x */
						    if(R_WarnEscapes) {
								have_warned++;
								warningcall(R_NilValue, _("'\\x' used without hex digits"));
						    }
						    val = 'x';
						}
						break;
				    }
				    val = 16*val + ext;
				}
				c = val;
		    } else if(c == 'u') {
#ifndef SUPPORT_MBCS
				error(_("\\uxxxx sequences not supported (line %d)"), xxlineno);
#else
				unsigned int val = 0; int i, ext; size_t res;
				char buff[MB_CUR_MAX+1]; /* could be variable, and hence not legal C90 */
				Rboolean delim = FALSE;
				if((c = xxgetc()) == '{') {
				    delim = TRUE;
				    CTEXT_PUSH(c);
				} else {
					xxungetc(c);
				}
				for(i = 0; i < 4; i++) {
				    c = xxgetc(); CTEXT_PUSH(c);
				    if(c >= '0' && c <= '9') ext = c - '0';
				    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
				    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
				    else {
						xxungetc(c);
						CTEXT_POP();
						if (i == 0) { /* was just \x */
						    if(R_WarnEscapes) {
								have_warned++;
								warningcall(R_NilValue, _("\\u used without hex digits"));
						    }
						    val = 'u';
						}
						break;
				    }
				    val = 16*val + ext;
				}
				if(delim) {
				    if((c = xxgetc()) != '}')
					error(_("invalid \\u{xxxx} sequence (line %d)"), xxlineno);
				    else CTEXT_PUSH(c);
				}
				WTEXT_PUSH(val);
				res = ucstomb(buff, val);
				if((int) res <= 0) {
#ifdef USE_UTF8_IF_POSSIBLE
			    	if(!forSymbol) {
						use_wcs = TRUE;
			    	} else
#endif
				    {
						if(delim)
						    error(_("invalid \\u{xxxx} sequence (line %d)"), xxlineno);
						else
						    error(_("invalid \\uxxxx sequence (line %d)"), xxlineno);
				    }
				} else {
					for(i = 0; i <  res; i++) {
						STEXT_PUSH(buff[i]);
					}
				}
				continue;
#endif
			} else if(c == 'U') {
#ifndef SUPPORT_MBCS
				error(_("\\Uxxxxxxxx sequences not supported (line %d)"), xxlineno);
#else
				unsigned int val = 0; int i, ext; size_t res;
				char buff[MB_CUR_MAX+1]; /* could be variable, and hence not legal C90 */
				Rboolean delim = FALSE;
				if((c = xxgetc()) == '{') {
				    delim = TRUE;
				    CTEXT_PUSH(c);
				} else xxungetc(c);
				for(i = 0; i < 8; i++) {
				    c = xxgetc(); CTEXT_PUSH(c);
				    if(c >= '0' && c <= '9') ext = c - '0';
				    else if (c >= 'A' && c <= 'F') ext = c - 'A' + 10;
				    else if (c >= 'a' && c <= 'f') ext = c - 'a' + 10;
				    else {
						xxungetc(c);
						CTEXT_POP();
						if (i == 0) { /* was just \x */
						    if(R_WarnEscapes) {
								have_warned++;
								warningcall(R_NilValue, _("\\U used without hex digits"));
						    }
						    val = 'U';
						}
						break;
				    }
				    val = 16*val + ext;
				}
				if(delim) {
				    if((c = xxgetc()) != '}') {
						error(_("invalid \\U{xxxxxxxx} sequence (line %d)"), xxlineno);
				    } else { 
						CTEXT_PUSH(c);
					}
				}
				res = ucstomb(buff, val);
				if((int)res <= 0) {
				    if(delim) {
						error(_("invalid \\U{xxxxxxxx} sequence (line %d)"), xxlineno);
					} else {
						error(_("invalid \\Uxxxxxxxx sequence (line %d)"), xxlineno);
					}
				}
				for(i = 0; i <  res; i++) {
					STEXT_PUSH(buff[i]);
				}
				WTEXT_PUSH(val);
				continue;
#endif
		    }
		    else {
				switch (c) {
					case 'a':
					    c = '\a';
					    break;
					case 'b':
					    c = '\b';
					    break;
					case 'f':
					    c = '\f';
					    break;
					case 'n':
					    c = '\n';
					    break;
					case 'r':
					    c = '\r';
					    break;
					case 't':
					    c = '\t';
					    break;
					case 'v':
					    c = '\v';
					    break;
					case '\\':
					    c = '\\';
					    break;
					case '"':
					case '\'':
					case ' ':
					case '\n':
					    break;
					default:
					    if(R_WarnEscapes) {
							have_warned++;
							warningcall(R_NilValue, _("'\\%c' is an unrecognized escape in a character string"), c);
					    }
					    break;
				}
		    }
		}
#if defined(SUPPORT_MBCS)
		else if(mbcslocale) { 
			int i, clen;
			wchar_t wc = L'\0';
			/* We can't assume this is valid UTF-8 */
			clen = /* utf8locale ? utf8clen(c):*/ mbcs_get_next(c, &wc);
			WTEXT_PUSH(wc);
			for(i = 0; i < clen - 1; i++){
			    STEXT_PUSH(c);
			    c = xxgetc();
			    if (c == R_EOF) break;
			    CTEXT_PUSH(c);
			    if (c == '\n') {
				   xxungetc(c); CTEXT_POP();
				   c = '\\';
			    }
			}
			if (c == R_EOF) break;
			STEXT_PUSH(c);
			continue;
       }
#endif /* SUPPORT_MBCS */
		STEXT_PUSH(c);
#ifdef USE_UTF8_IF_POSSIBLE
		if ((unsigned int) c < 0x80) WTEXT_PUSH(c);
		else { /* have an 8-bit char in the current encoding */
		    wchar_t wc;
		    char s[2] = " ";
		    s[0] = c;
		    mbrtowc(&wc, s, 1, NULL);
		    WTEXT_PUSH(wc);
		}
#endif
    }
    STEXT_PUSH('\0');
    WTEXT_PUSH(0);
    if(forSymbol) {
		PROTECT(yylval = install(stext));
		if(stext != st0) free(stext);
		return SYMBOL;
    } else { 
#ifdef USE_UTF8_IF_POSSIBLE
		if(use_wcs) { 
		    if(wcnt < 1000){
				PROTECT(yylval = mkStringUTF8(wcs, wcnt)); /* include terminator */
		    } else {
				error(_("string at line %d containing Unicode escapes not in this locale\nis too long (max 1000 chars)"), xxlineno);
			}
		} else
#endif
	    PROTECT(yylval = mkString2(stext,  bp - stext - 1));
		if(stext != st0) free(stext);
		if(have_warned) {
		    *ct = '\0';
#ifdef ENABLE_NLS
	    	warningcall(R_NilValue,
				ngettext("unrecognized escape removed from \"%s\"",
					 "unrecognized escapes removed from \"%s\"",
					 have_warned),
				currtext);
#else
	    	warningcall(R_NilValue,
				"unrecognized escape(s) removed from \"%s\"", currtext);
#endif
		}
		return STR_CONST;
    }
}
/*}}}*/

/*{{{ isValidName */
/**
 * Checks validity of a symbol name
 * 
 * @return 1 if name is a valid name 0 otherwise 
 */
int isValidName(const char *name){
    const char *p = name;
    int i;

#ifdef SUPPORT_MBCS
    if(mbcslocale) {
		/* the only way to establish which chars are alpha etc is to
		   use the wchar variants */
		int n = strlen(name), used;
		wchar_t wc;
		used = Mbrtowc(&wc, p, n, NULL); p += used; n -= used;
		if(used == 0) return 0;
		if (wc != L'.' && !iswalpha(wc) ) return 0;
		if (wc == L'.') {
		    /* We don't care about other than ASCII digits */
		    if(isdigit(0xff & (int)*p)) return 0;
		    /* Mbrtowc(&wc, p, n, NULL); if(iswdigit(wc)) return 0; */
		}
		while((used = Mbrtowc(&wc, p, n, NULL))) {
		    if (!(iswalnum(wc) || wc == L'.' || wc == L'_')) break;
		    p += used; n -= used;
		}
		if (*p != '\0') return 0;
    } else
#endif
    {
		int c = 0xff & *p++;
		if (c != '.' && !isalpha(c) ) return 0;
		if (c == '.' && isdigit(0xff & (int)*p)) return 0;
		while ( c = 0xff & *p++, (isalnum(c) || c == '.' || c == '_') ) ;
		if (c != '\0') return 0;
    }

    if (strcmp(name, "...") == 0) return 1;

    for (i = 0; keywords[i].name != NULL; i++){
		if (strcmp(keywords[i].name, name) == 0) return 0;
	}
    return 1;
}
/*}}}*/

/*{{{ SpecialValue */
/**
 * Content of an operator, like %op% 
 * 
 * @param c
 * @return  
 */
static int SpecialValue(int c) {
	
	DECLARE_YYTEXT_BUFP(yyp);
    YYTEXT_PUSH(c, yyp);
	char *p = yytext_ ;
	*p='%';
	while ((c = xxgetc()) != R_EOF && c != '%') {
		if (c == '\n') {
		    xxungetc(c);
		    return ERROR;
		}
		YYTEXT_PUSH(c, yyp);
		*(p)++ = c ;
    }
    if (c == '%') {
		YYTEXT_PUSH(c, yyp);
		*(p)++ = '%' ;
	}
	*(p)++ = '\0' ;
	YYTEXT_PUSH('\0', yyp);
    yylval = install(yytext_);
    return SPECIAL;
}
/*}}}*/

/*{{{ SymbolValue */
static int SymbolValue(int c) {
	
    int kw;
    DECLARE_YYTEXT_BUFP(yyp);
#if defined(SUPPORT_MBCS)
    if(mbcslocale) {
		wchar_t wc; int i, clen;
		   /* We can't assume this is valid UTF-8 */
		clen = /* utf8locale ? utf8clen(c) :*/ mbcs_get_next(c, &wc);
		while(1) { 
		    /* at this point we have seen one char, so push its bytes
		       and get one more */
		    for(i = 0; i < clen; i++) {
				YYTEXT_PUSH(c, yyp);
				c = xxgetc();
		    }
		    if(c == R_EOF) break;
		    if(c == '.' || c == '_') {
				clen = 1;
				continue;
		    }
		    clen = mbcs_get_next(c, &wc);
		    if(!iswalnum(wc)) break;
		}
    } else
#endif
	{
		do {
		    YYTEXT_PUSH(c, yyp);
		} while ((c = xxgetc()) != R_EOF &&
			 (isalnum(c) || c == '.' || c == '_'));
    }
	xxungetc(c);
    YYTEXT_PUSH('\0', yyp);
    if ((kw = KeywordLookup(yytext_))) {
		return kw;
    }
    PROTECT(yylval = install(yytext_));
    return SYMBOL;
}
/*}}} */

/*{{{ token */

/** 
 * Split the input stream into tokens.
 * This is the lowest of the parsing levels.
 * 
 * @return the token type once characters are consumed
 */
static int token(void) {
	
    int c;
#if defined(SUPPORT_MBCS)
    wchar_t wc;
#endif

    if (SavedToken) {
		c = SavedToken;
		yylval = SavedLval;
		SavedLval = R_NilValue;
		SavedToken = 0;
		setfirstloc( xxlinesave, xxcolsave, xxbytesave) ;
		return c;
    }
	/* want to be able to go back one token */
    xxcharsave = xxcharcount; 

	/* Setting the yyloc to where we are before skipping spaces */ 
	setfirstloc( xxlineno, xxcolno, xxbyteno) ;

	/* eat any number of spaces */
	/* if we actually skip spaces, then yyloc will be altered */
	c = SkipSpace_();
	
	/* flush the comment */
	if (c == '#') {
		c = SkipComment();
		setfirstloc( xxlineno, xxcolno, xxbyteno) ;
	}
    
    if (c == R_EOF) {
		return END_OF_INPUT;
	}

    /* Either digits or symbols can start with a "." */
    /* so we need to decide which it is and jump to  */
    /* the correct spot. */
    if (c == '.' && typeofnext() >= 2) {
		goto symbol;
	}

    /* literal numbers */
    if (c == '.') {
		return NumericValue(c);
	}
	
    /* We don't care about other than ASCII digits */
    if (isdigit(c)){
		return NumericValue(c);
	}

    /* literal strings */
	if (c == '\"' || c == '\''){
		return StringValue(c, FALSE);
	}

    /* special functions */
    if (c == '%'){
		return SpecialValue(c);
	}

    /* functions, constants and variables */
    if (c == '`'){
		return StringValue(c, TRUE);
	}

	symbol:

		if (c == '.'){
			return SymbolValue(c);
		}
#if defined(SUPPORT_MBCS)
    	if(mbcslocale) {
			mbcs_get_next(c, &wc);
			if (iswalpha(wc)) {
				return SymbolValue(c);
			}
    	} else
#endif
		{ 
			if (isalpha(c)) {
				return SymbolValue(c);
			}
		}

    /* compound tokens */

    switch (c) {  
    	case '<':
			if (nextchar('=')) {
			    yylval = install("<=");
			    return LE;
			}
			if (nextchar('-')) {
			    yylval = install("<-");
			    return LEFT_ASSIGN;
			}
			if (nextchar('<')) {
			    if (nextchar('-')) {
					yylval = install("<<-");
					return LEFT_ASSIGN;
			    }
			    else
				return ERROR;
			}
			yylval = install("<");
			return LT;
    	case '-':
			if (nextchar('>')) {
			    if (nextchar('>')) {
					yylval = install("<<-");
					return RIGHT_ASSIGN;
			    }
			    else {
					yylval = install("<-");
					return RIGHT_ASSIGN;
			    }
			}
			yylval = install("-");
			return '-';
    	case '>':
			if (nextchar('=')) {
			    yylval = install(">=");
			    return GE;
			}
			yylval = install(">");
			return GT;
    	case '!':
			if (nextchar('=')) {
			    yylval = install("!=");
			    return NE;
			}
			yylval = install("!");
			return '!';
    	case '=':
			if (nextchar('=')) {
			    yylval = install("==");
			    return EQ;
			}
			yylval = install("=");
			return EQ_ASSIGN;
    	case ':':
			if (nextchar(':')) {
			    if (nextchar(':')) {
					yylval = install(":::");
					return NS_GET_INT;
			    } else {
					yylval = install("::");
					return NS_GET;
			    }
			}
			if (nextchar('=')) {
			    yylval = install(":=");
				colon = 1 ;
			    return LEFT_ASSIGN;
			}
			yylval = install(":");
			return ':';
    	case '&':
			if (nextchar('&')) {
			    yylval = install("&&");
			    return AND2;
			}
			yylval = install("&");
			return AND;
    	case '|':
			if (nextchar('|')) {
			    yylval = install("||");
			    return OR2;
			}
			yylval = install("|");
			return OR;
    	case LBRACE:
			yylval = install("{");
			return c;
    	case RBRACE:
			return c;
    	case '(':
			yylval = install("(");
			return c;
    	case ')':
			return c;
    	case '[':
			if (nextchar('[')) {
			    yylval = install("[[");
			    return LBB;
			}
			yylval = install("[");
			return c;
    	case ']':
			return c;
    	case '?':
			strcpy(yytext_, "?");
			yylval = install(yytext_);
			return c;
    	case '*':
			/* Replace ** by ^.  This has been here since 1998, but is
			   undocumented (at least in the obvious places).  It is in
			   the index of the Blue Book with a reference to p. 431, the
			   help for 'Deprecated'.  S-PLUS 6.2 still allowed this, so
			   presumably it was for compatibility with S. */
			if (nextchar('*'))
			    c='^';
			yytext_[0] = c;
			yytext_[1] = '\0';
			yylval = install(yytext_);
			return c;
    	case '+':
    	case '/':
    	case '^':
    	case '~':
    	case '$':
    	case '@':
			yytext_[0] = c;
			yytext_[1] = '\0';
			yylval = install(yytext_);
			return c;
    	default:
			return c;
    }
}

/**
 * Wrap around the token function. Returns the same result
 * but increments the identifier, after a call to token_, 
 * the identifier variable contains the id of the token
 * just returned
 *
 * @return the same as token
 */

static int token_(void){
	// capture the position before retrieving the token
	setfirstloc( xxlineno, xxcolno, xxbyteno ) ;
	
	// get the token
	int res = token( ) ;
	                       
	// capture the position after
	int _last_line = xxlineno ;
	int _last_col  = xxcolno ;
	int _last_byte = xxbyteno ;
	
	_current_token = res ;
	incrementId( ) ;
	yylloc.id = identifier ;
	
	// record the position
	if( res != '\n' ){
		record_( yylloc.first_line, yylloc.first_column, yylloc.first_byte, 
				_last_line, _last_col, _last_byte, 
				res, identifier ) ;
		nterminals++;
	}
	
	return res; 
}

/*}}}*/

/*{{{ setlastloc, setfirstloc */
/**
 * Sets the last elements of the yyloc structure with current 
 * information
 */
static void setlastloc(void) {
	yylloc.last_line = xxlineno;
    yylloc.last_column = xxcolno;
    yylloc.last_byte = xxbyteno;
}
/**
 * Sets the first elements of the yyloc structure with current 
 * information
 */
static void setfirstloc( int line, int column, int byte ){
	yylloc.first_line   = line;
    yylloc.first_column = column;
    yylloc.first_byte   = byte;
}

/*}}}*/ 

/*{{{ yylex */
/*----------------------------------------------------------------------------
 *
 *  The Lexical Analyzer:
 *
 *  Basic lexical analysis is performed by the following
 *  routines.  Input is read a line at a time, and, if the
 *  program is in batch mode, each input line is echoed to
 *  standard output after it is read.
 *
 *  The function yylex() scans the input, breaking it into
 *  tokens which are then passed to the parser.  The lexical
 *  analyser maintains a symbol table (in a very messy fashion).
 *
 *  The fact that if statements need to parse differently
 *  depending on whether the statement is being interpreted or
 *  part of the body of a function causes the need for ifpop
 *  and IfPush.  When an if statement is encountered an 'i' is
 *  pushed on a stack (provided there are parentheses active).
 *  At later points this 'i' needs to be popped off of the if
 *  stack.
 *
 */
  
/**
 * The lexical analyzer function. recognizes tokens from 
 * the input stream and returns them to the parser
 * 
 */
static int yylex(void){
	int tok;
	
	again:

		/* gets a token */
		tok = token_();
		
    	/* Newlines must be handled in a context */
    	/* sensitive way.  The following block of */
    	/* deals directly with newlines in the */
    	/* body of "if" statements. */
    	if (tok == '\n') {
    	
			if (EatLines || *contextp == '[' || *contextp == '(')
			    goto again;
    		
			/* The essence of this is that in the body of */
			/* an "if", any newline must be checked to */
			/* see if it is followed by an "else". */
			/* such newlines are discarded. */
    		
			if (*contextp == 'i') {
    		
			    /* Find the next non-newline token */
    		
			    while(tok == '\n'){
					tok = token_();
				}
    		
			    /* If we encounter "}", ")" or "]" then */
			    /* we know that all immediately preceding */
			    /* "if" bodies have been terminated. */
			    /* The corresponding "i" values are */
			    /* popped off the context stack. */
    		
			    if (tok == RBRACE || tok == ')' || tok == ']' ) {
					while (*contextp == 'i'){
					    ifpop();
					}
					*contextp-- = 0;
					setlastloc();
					return tok;
			    }
    		
			    /* When a "," is encountered, it terminates */
			    /* just the immediately preceding "if" body */
			    /* so we pop just a single "i" of the */
			    /* context stack. */
    		
			    if (tok == ',') {
					ifpop();
					setlastloc();
					return tok;
			    }
    		
			    /* Tricky! If we find an "else" we must */
			    /* ignore the preceding newline.  Any other */
			    /* token means that we must return the newline */
			    /* to terminate the "if" and "push back" that */
			    /* token so that we will obtain it on the next */
			    /* call to token.  In either case sensitivity */
			    /* is lost, so we pop the "i" from the context */
			    /* stack. */
    		
			    if(tok == ELSE) {
					EatLines = 1;
					ifpop();
					setlastloc();
					return ELSE;
			    } else {
					ifpop();
					SavedToken = tok;
					xxlinesave = yylloc.first_line;
					xxcolsave  = yylloc.first_column;
					xxbytesave = yylloc.first_byte;
					SavedLval = yylval;
					setlastloc();
					return '\n';
			    }
			} else {
			    setlastloc();
			    return '\n';
			}
    	}
    	
    	/* Additional context sensitivities */
    	
    	switch(tok) {
    	
			/* Any newlines immediately following the */
			/* the following tokens are discarded. The */
			/* expressions are clearly incomplete. */
    		
    		case '+':
    		case '-':
    		case '*':
    		case '/':
    		case '^':
    		case LT:
    		case LE:
    		case GE:
    		case GT:
    		case EQ:
    		case NE:
    		case OR:
    		case AND:
    		case OR2:
    		case AND2:
    		case SPECIAL:
    		case FUNCTION:
    		case WHILE:
    		case REPEAT:
    		case FOR:
    		case IN:
    		case '?':
    		case '!':
    		case '=':
    		case ':':
    		case '~':
    		case '$':
    		case '@':
    		case LEFT_ASSIGN:
    		case RIGHT_ASSIGN:
    		case EQ_ASSIGN:
				EatLines = 1;
				break;
    		
			/* Push any "if" statements found and */
			/* discard any immediately following newlines. */
    		
    		case IF:
				IfPush();
				EatLines = 1;
				break;
    		
			/* Terminate any immediately preceding "if" */
			/* statements and discard any immediately */
			/* following newlines. */
    		
    		case ELSE:
				ifpop();
				EatLines = 1;
				break;
    		
			/* These tokens terminate any immediately */
			/* preceding "if" statements. */
    		
    		case ';':
    		case ',':
				ifpop();
			break;
    		
			/* Any newlines following these tokens can */
			/* indicate the end of an expression. */
    		
    		case SYMBOL:
    		case STR_CONST:
    		case NUM_CONST:
    		case NULL_CONST:
    		case NEXT:
    		case BREAK:
				EatLines = 0;
				break;
    		
			/* Handle brackets, braces and parentheses */
    		
    		case LBB:
				if(contextp - contextstack >= CONTEXTSTACK_SIZE - 1)
				    error(_("contextstack overflow at line %d"), xxlineno);
				*++contextp = '[';
				*++contextp = '[';
				break;
    		
    		case '[':
				if(contextp - contextstack >= CONTEXTSTACK_SIZE)
				    error(_("contextstack overflow at line %d"), xxlineno);
				*++contextp = tok;
				break;
    		
    		case LBRACE:
				if(contextp - contextstack >= CONTEXTSTACK_SIZE)
				    error(_("contextstack overflow at line %d"), xxlineno);
				*++contextp = tok;
				EatLines = 1;
				break;
    		
    		case '(':
				if(contextp - contextstack >= CONTEXTSTACK_SIZE)
				    error(_("contextstack overflow at line %d"), xxlineno);
				*++contextp = tok;
				break;
    		
    		case ']':
				while (*contextp == 'i')
				    ifpop();
				*contextp-- = 0;
				EatLines = 0;
				break;
    		
    		case RBRACE:
				while (*contextp == 'i')
				    ifpop();
				*contextp-- = 0;
				break;
    		
    		case ')':
				while (*contextp == 'i')
				    ifpop();
				*contextp-- = 0;
				EatLines = 0;
				break;
    	
    	}
    	setlastloc();
    	return tok;
}
/*}}}*/

/*}}}*/

/*{{{ file_getc */
int file_getc(void){
    return _fgetc(fp_parse);
}
/*}}}*/

/*{{{ Parsing entry points */

/*{{{ HighlightParseContextInit */
static void HighlightParseContextInit(void) {
    H_ParseContextLast = 0;
    H_ParseContext[0] = '\0';
	colon = 0 ;
	
	/* starts the identifier counter*/
	initId();
	
	data_count = 0 ;
	data_size  = 0 ;
	id_size=0;
	nterminals = 0 ;
	PROTECT_WITH_INDEX( data = R_NilValue, &DATA_INDEX ) ;
	PROTECT_WITH_INDEX( ids = R_NilValue, &ID_INDEX ) ;
	growData( ) ;
	growID(15*NLINES);
	
}
/*}}}*/
           
/*{{{ ParseInit */
static void HighlightParseInit(void) {
    contextp = contextstack;
    *contextp = ' ';
    SavedToken = 0;
    SavedLval = R_NilValue;
    EatLines = 0;
    EndOfFile = 0;
    xxcharcount = 0;
    npush = 0;
}
/*}}}*/

/*{{{ R_Parse1 */
static SEXP Highlight_Parse1(ParseStatus *status) {
	
	int res = yyparse() ;
	switch(res) {
    	case 0:                     /* End of file */
			*status = PARSE_EOF;
			if (EndOfFile == 2) {
				*status = PARSE_INCOMPLETE;
			}
			break;
    	case 1:                     /* Syntax error / incomplete */
			*status = PARSE_ERROR;
			if (EndOfFile) {
				*status = PARSE_INCOMPLETE;
			}
			break;
    	case 2:                     /* Empty Line */
			*status = PARSE_NULL;
			break;
    	case 3:                     /* Valid expr '\n' terminated */
    	case 4:                     /* Valid expr ';' terminated */
			*status = PARSE_OK;
			break;
    }
    return R_CurrentExpr;
}
/*}}}*/

/*{{{ Highlight_Parse */
static SEXP Highlight_Parse(int n, ParseStatus *status, SEXP srcfile){
	
    // volatile int savestack;
	int i;
    SEXP t, rval;

    HighlightParseContextInit();
    // savestack = R_PPStackTop;
    PROTECT(t = NewList());
	
    xxlineno = 1;
    xxcolno = 0;
    xxbyteno = 0;
    
    for(i = 0; ; ) {
		if(n >= 0 && i >= n) break;
		HighlightParseInit();
		rval = Highlight_Parse1(status);
		
		switch(*status) {
			case PARSE_NULL:
			    break;
			case PARSE_OK:
			    t = GrowList(t, rval);
			    i++;
			    break;
			case PARSE_INCOMPLETE:
			case PARSE_ERROR:
				//  R_PPStackTop = savestack;
			    return R_NilValue;
			    break;
			case PARSE_EOF:
			    goto finish;
			    break;
		}
    }

finish:

    t = CDR(t);
    PROTECT( rval = allocVector(EXPRSXP, length(t)) );
    for (n = 0 ; n < LENGTH(rval) ; n++, t = CDR(t)){
		SET_VECTOR_ELT(rval, n, CAR(t));
	}
	finalizeData() ;
	setAttrib( rval, mkString( "data" ), data ) ;
	
	// R_PPStackTop = savestack;
    *status = PARSE_OK;
	
	UNPROTECT_PTR( data ) ;
	UNPROTECT_PTR( ids ) ;
	UNPROTECT( 2 ) ;  // t
	
    return rval;
}
/*}}}*/

/*{{{ Highlight_ParseFile */
attribute_hidden SEXP Highlight_ParseFile(FILE *fp, int n, ParseStatus *status, SEXP srcfile, int nl) {
    if( nl < 1000 ){
		NLINES = 1000 ;
	} else { 
		NLINES = nl ;
	}
	fp_parse = fp;
    ptr_getc = file_getc;
	// yydebug = 1 ;
	
    return Highlight_Parse(n, status, srcfile);
}
/*}}}*/

/*}}}*/

/*{{{ tracking things */

/*{{{ Keeping track of the id */
/**
 * Increments the token/grouping counter
 */
static void incrementId(void){
	identifier++; 
}

static void initId(void){
	identifier = 0 ;
}
/*}}}*/

/*{{{ Recording functions */
/**
 * Records location information about a symbol. The information is
 * used to fill the data 
 * 
 * @param first_line first line of the symbol
 * @param first_column first column of the symbol
 * @param first_byte first byte of the symbol
 * @param last_line last line of the symbol
 * @param last_column last column of the symbol
 * @param last_byte last byte of the symbol
 * @param token token type
 * @param id identifier for this token
 */
static void record_( int first_line, int first_column, int first_byte,                                    
	int last_line, int last_column, int last_byte, 
	int token, int id ){
       
	if( token == LEFT_ASSIGN && colon == 1){
		token = COLON_ASSIGN ;
		colon = 0 ;
	}
	
	// don't care about zero sized things
	if( first_line == last_line && first_byte == last_byte ) return ;
	
	_FIRST_LINE( data_count )   = first_line ;  
	_FIRST_COLUMN( data_count ) = first_column; 
	_FIRST_BYTE( data_count )   = first_byte;   
	_LAST_LINE( data_count )    = last_line;    
	_LAST_COLUMN( data_count )  = last_column;  
	_LAST_BYTE( data_count )    = last_byte;    
	_TOKEN( data_count )        = token;        
	_ID( data_count )           = id ;          
	_PARENT(data_count)         = 0 ; 
	
	// Rprintf("%d,%d,%d,%d,%d,%d,%d,%d,%d\n", 
	// 		_FIRST_LINE( data_count )  , 
	// 		_FIRST_COLUMN( data_count ), 
	// 		_FIRST_BYTE( data_count )  , 
	// 		_LAST_LINE( data_count )   , 
	// 		_LAST_COLUMN( data_count ) , 
	// 		_LAST_BYTE( data_count )   , 
	// 		_TOKEN( data_count )       , 
	// 		_ID( data_count )          ,            
	// 		_PARENT(data_count)         
	// 		) ;
	if( id > id_size ){
		growID(id) ;
	}
	ID_ID( id ) = data_count ; 
	
	data_count++ ;
	if( data_count == data_size ){
		growData( ) ;
	}
	
}

/**
 * records parent as the parent of all its childs. This grows the 
 * parents list with a new vector. The first element of the new 
 * vector is the parent id, and other elements are childs id
 *
 * @param parent id of the parent expression
 * @param childs array of location information for all child symbols
 * @param nchilds number of childs
 */
static void recordParents( int parent, yyltype * childs, int nchilds){
	
	if( parent > id_size ){
		growID(parent) ;
	}
	
	/* some of the childs might be an empty token (like cr)
	   which we do not want to track */
	int ii;    /* loop index */
	yyltype loc ;
	for( ii=0; ii<nchilds; ii++){
		loc = childs[ii] ;
		if( loc.first_line == loc.last_line && loc.first_byte == loc.last_byte ){
			continue ;
		}
		ID_PARENT( (childs[ii]).id ) = parent  ;
	}
	
}

/**
 * The token pointed by the location has the wrong token type, 
 * This updates the type
 *
 * @param loc location information for the token to track
 */ 
static void modif_token( yyltype* loc, int tok ){
	
	int id = loc->id ;
	if( tok == SYMBOL_FUNCTION_CALL ){
		// looking for first child of id
		int j = ID_ID( id ) ;
		int parent = id ;
	
		while( ID_PARENT( _ID(j) ) != parent ){
			j-- ;
		}
		if( _TOKEN(j) == SYMBOL ){
			_TOKEN(j) = SYMBOL_FUNCTION_CALL ;
		}
		
	} else{
		_TOKEN( ID_ID(id) ) = tok ;
	}
	
}
/*}}}*/

/*{{{ finalizeData */

static void finalizeData( ){
	
	int nloc = data_count ;
	
	SETLENGTH( data, data_count * 9 ) ;
	
	int maxId = _ID(nloc-1) ;
	int i, j, id ;
	int parent ; 
	
	/* attach comments to closest enclosing symbol */
	int comment_line, comment_first_byte, comment_last_byte ;
	int this_first_line, this_last_line, this_first_byte ;
	int orphan ;
	
	for( i=0; i<nloc; i++){
		if( _TOKEN(i) == COMMENT || _TOKEN(i) == ROXYGEN_COMMENT ){
			comment_line = _FIRST_LINE( i ) ;
			comment_first_byte = _FIRST_BYTE( i ) ;
			comment_last_byte  = _LAST_LINE( i ) ;
		
			orphan = 1 ;
			for( j=i+1; j<nloc; j++){
				this_first_line = _FIRST_LINE( j ) ;
				this_first_byte = _FIRST_BYTE( j ) ;
				this_last_line  = _LAST_LINE( j ) ;
				
				/* the comment needs to start after the current symbol */
				if( comment_line < this_first_line ) continue ;
				if( (comment_line == this_first_line) & (comment_first_byte < this_first_byte) ) continue ;
				
				/* the current symbol must finish after the comment */
				if( this_last_line <= comment_line ) continue ; 
				
				/* we have a match, record the parent and stop looking */
				ID_PARENT( _ID(i) ) = _ID(j) ;
				orphan = 0;
				break ;
			}
			if(orphan){
				ID_PARENT( _ID(i) ) = 0 ;
			}
		}
	}
	
	int idp;
	/* store parents in the data */
	for( i=0; i<nloc; i++){
		id = _ID(i);
		parent = ID_PARENT( id ) ;
		if( parent == 0 ){
			_PARENT(i)=parent;
			continue;
		}
		while( 1 ){
			idp = ID_ID( parent ) ;
			if( idp > 0 ) break ;
			if( parent == 0 ){
				break ;
			}
			parent = ID_PARENT( parent ) ;
		}
		_PARENT(i) = parent ;
	}

	/* now rework the parents of comments, we try to attach 
	comments that are not already attached (parent=0) to the next
	enclosing top-level expression */ 
	
	int token, token_j ;
	for( i=0; i<nloc; i++){
		token = _TOKEN(i); 
		if( ( token == COMMENT || token == ROXYGEN_COMMENT ) && _PARENT(i) == 0 ){
			for( j=i; j<nloc; j++){
				token_j = _TOKEN(j); 
				if( token_j == COMMENT || token_j == ROXYGEN_COMMENT ) continue ;
				if( _PARENT(j) != 0 ) continue ;
				_PARENT(i) = - _ID(j) ;
				break ;
			}
		}
	}
	
	SEXP dims ;
	PROTECT( dims = allocVector( INTSXP, 2 ) ) ;
	INTEGER(dims)[0] = 9 ;
	INTEGER(dims)[1] = data_count ;
	setAttrib( data, mkString( "dim" ), dims ) ;
	UNPROTECT(1) ; // dims

}
/*}}}*/

/*{{{ growData */
/**
 * Grows the data
 */
static void growData(){
	
	SEXP bigger ; 
	int current_data_size = data_size ;
	data_size += NLINES * 10 ;
	
	PROTECT( bigger = allocVector( INTSXP, data_size * 9 ) ) ; 
	int i,j,k;         
	if( current_data_size > 0 ){
		for( i=0,k=0; i<current_data_size; i++){
			for( j=0; j<9; j++,k++){
				INTEGER( bigger )[k] = INTEGER(data)[k] ;
			}
		}
	}
	REPROTECT( data = bigger, DATA_INDEX ) ;
	UNPROTECT( 1 ) ;
	
}
/*}}}*/

/*{{{ growID*/
/**
 * Grows the ids vector so that ID_ID(target) can be called
 */
static void growID( int target ){
	
	SEXP newid ;
	int current_id_size = id_size ;
	id_size = target + NLINES * 15 ;
	PROTECT( newid = allocVector( INTSXP, ( 1 + id_size ) * 2) ) ;
	int i=0,j,k=0;
	if( current_id_size > 0 ){ 
		for( ; i<(current_id_size+1); i++){
			for(j=0;j<2; j++,k++){
				INTEGER( newid )[k] = INTEGER( ids )[k] ;
			}
		}
	}
	for( ;i<(id_size+1);i++){
		for(j=0;j<2; j++,k++){
			INTEGER( newid )[k] = 0 ;
		}
	}
	REPROTECT( ids = newid, ID_INDEX ) ;
	UNPROTECT(1) ;
}
/*}}}*/

/*}}}*/

/*{{{ do_parser */
/** 
 * R interface : 
 *  highlight:::parser( file, encoding = "unknown" )
 *
 * Calls the Highlight_ParseFile function from gram.y -> gram.c
 */
SEXP attribute_hidden do_parser(SEXP args){
	
	/*{{{ declarations */
	SEXP result ;
    Rboolean old_latin1=known_to_be_latin1,
	old_utf8=known_to_be_utf8, allKnown = TRUE;
    const char *encoding;
	SEXP filename ;
    ParseStatus status;
	FILE *fp;
	/*}}}

	/*{{{ process arguments */
    
	filename = CADR(args) ;
	if(!isString(CADDR(args)) ){
		error(_("invalid '%s' value"), "encoding");
	}
	encoding = CHAR(STRING_ELT(CADDR(args), 0)); /* ASCII */
    known_to_be_latin1 = known_to_be_utf8 = FALSE;

	/* allow 'encoding' to override declaration on 'text'. */
    if(streql(encoding, "latin1")) {
		known_to_be_latin1 = TRUE;
		allKnown = FALSE;
    }
    if(streql(encoding, "UTF-8"))  {
		known_to_be_utf8 = TRUE;
		allKnown = FALSE;
    }
	
	/*}}}*/

	/*{{{ Try to open the file */
	const char* fname = CHAR(STRING_ELT(filename,0) ) ;
	if((fp = _fopen(R_ExpandFileName( fname ), "r")) == NULL){
		error(_("unable to open file to read"), 0);
	}
	int nl = nlines( fname ) ;
	
	/*}}}*/

	/*{{{ Call the parser */
	H_ParseError = 0;
    H_ParseErrorMsg[0] = '\0';
	PROTECT(result = Highlight_ParseFile(fp, -1, &status, filename, nl));
	if (status != PARSE_OK) {
		error("\n%s:%d:%d\n\t%s\n", fname, xxlineno, H_ParseErrorCol, H_ParseErrorMsg);
	}
	fclose( fp ) ;
	/*}}}*/
	
	/*{{{ reset encodings flags  */
    known_to_be_latin1 = old_latin1;
    known_to_be_utf8 = old_utf8;
	/*}}}*/
	
    UNPROTECT( 1 ) ;
    return result;
}
/*}}}*/


