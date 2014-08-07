#include "lex/lexical.h"
#include "yacc/rule.h"
#include "yacc/lalr1parser.h"

#include "lex/lextokens.h"

enum LEX_NONTERM{lexprogram=NONTERM_BASE,lexrequired,lexdeclareend,lexdeclare,lexdefine,
lexdeclarations,lexdeclaration,lexregexpr,
lexdefinitionor,lexdefinitions,lexdefinition,};

rule lexRules[]=
{
	{lexprogram,{lexrequired},"printf(\"accept!!\\n\");"},
	{lexprogram,{lexrequired,LEXICAL_PARTION},"printf(\"accept!!\\n\");"},

	{lexdeclare,{TERMEMPTY}},
	{lexdeclare,{lexdeclare,LEXICAL_LITLEFT},"{ofstream outf(\"_lblock1\",ios::app);\
scanner.writeLitBlock(outf);outf.close();}"},
	{lexdeclare,{lexdeclare,lexdeclarations}},
	{lexdeclarations,{TERMEMPTY}},
	{lexdeclarations,{lexdeclarations,lexdeclaration}},
	{lexdeclaration,{LEXICAL_IDENTIFIER,lexregexpr},"lex.auxNFAPart.addPart($1,$2);"},
	{lexregexpr,{LEXICAL_IDENTIFIER},"strcpy($$,$1);"},
	{lexregexpr,{LEXICAL_REGEXPR},"strcpy($$,$1);"},
	{lexdeclaration,{LEXICAL_STARTSTATEDEF,LEXICAL_IDENTIFIER},"lex.addStartState($2);"},

	{lexdefine,{LEXICAL_LITLEFT,lexdefinitions},"{ofstream outf(\"_lblock2\",ios::app);\
scanner.writeLitBlock(outf);outf.close();}"},
	{lexdefine,{lexdefinitions}},
	{lexdefinitions,{lexdefinition}},
	{lexdefinitions,{lexdefinitions,lexdefinition}},

	{lexdefinition,{lexdefinitionor,LEXICAL_ACTION},"for(int i=0;i<defExprs.exprcnt;i++)	lex.addTokenDef(defExprs.exprs[i],$2);\n"
	"defExprs.exprcnt=0;"},
	{lexdefinitionor,{lexdefinitionor,'|',lexregexpr},"defExprs.add($3);"},
	{lexdefinitionor,{lexregexpr},"defExprs.add($1);"},

	{lexdeclareend,{lexdeclare,LEXICAL_PARTION}},
	{lexrequired,{lexdeclareend,lexdefine}}
};
static Lexical lex;
void genlex()
{	

	lex.addStartState("STATE_ACTION");

	lex.auxNFAPart.addPart("D","[0-9]");
	lex.auxNFAPart.addPart("L","[a-zA-Z_]");
	lex.auxNFAPart.addPart("ID","{L}({L}|{D})*");
	lex.auxNFAPart.addPart("WS","[ \\r\\t\\n]+");
	lex.auxNFAPart.addPart("QUOTE","\\\"[^\"\\n]*\\\"");
	lex.auxNFAPart.addPart("SBRACKETS","\\\[[^\\\]\\n]*\\\]");

	lex.addTokenDef("/\\* ","skipComment();return 0;");

	lex.addTokenDef("%\\{","litstart=code; if(skipLitBlock()) return LEXICAL_LITLEFT;return -1;");//leave the job to parser
	lex.addTokenDef("%%{WS}","if(++partcnt==1) indef=true;else if(indef) writeLastBlock();\n"
		"return LEXICAL_PARTION;");
	lex.addTokenDef("%x","return LEXICAL_STARTSTATEDEF;");
	lex.addTokenDef("{ID}","strcpy(yylval.str,yytext);if(indef) BEGINSTATE STATE_ACTION;return LEXICAL_IDENTIFIER;");
	
	lex.addTokenDef("(([^ \\n\\t\\r\"[])|{QUOTE}|{SBRACKETS})+","strcpy(yylval.str,yytext);if(indef) BEGINSTATE STATE_ACTION;return LEXICAL_REGEXPR;");

	lex.addTokenDef("{WS}","return 0;");

	lex.addTokenDef("<STATE_ACTION>/\\*","skipComment();return 0;");

	lex.addTokenDef("<STATE_ACTION>{WS}","return 0;");

	lex.addTokenDef("<STATE_ACTION>\\|","BEGINSTATE INITIAL;return '|';");
	lex.addTokenDef("<STATE_ACTION>\\;","yylval.str[0]=';';yylval.str[1]=0;BEGINSTATE INITIAL;return LEXICAL_ACTION;");
	lex.addTokenDef("<STATE_ACTION>\\{{WS}*","if(!handleAction()) return -1;strcpy(yylval.str,yytext);BEGINSTATE INITIAL;return LEXICAL_ACTION;");	
	lex.addTokenDef("<STATE_ACTION>[^/|{; \\r\\t\\n]([^{;\\n]|{QUOTE})*[;\\n]","strcpy(yylval.str,yytext);BEGINSTATE INITIAL;return LEXICAL_ACTION;");	

	

	lex.buildTable();
	lex.generate();



	LALR1Parser lalr1;

// 	lalr1.addValType("int","ival");
// 	lalr1.addValType("char*","str");	
	lalr1.addValType("ival");
	lalr1.addValType("str");	

	lalr1.setValType(LEXICAL_ACTION,"str");
	lalr1.setValType(LEXICAL_REGEXPR,"str");
	lalr1.setValType(LEXICAL_IDENTIFIER,"str");
	lalr1.setValType(lexregexpr,"str");

	lalr1.readRules(lexRules,sizeof(lexRules)/sizeof(rule));
	lalr1.generate();
}
