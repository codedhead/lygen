#include "lex/lexical.h"
#include "yacc/rule.h"
#include "yacc/lalr1parser.h"

#include "yacc/yacctokens.h"

enum YACC_NONTERM{yaccprogram=NONTERM_BASE,yaccrequired,yaccdeclare,yaccdefine,
yaccdeclarations,yaccdeclaration,yaccdefinitionor,
yaccdefinitions,yaccdefinition,yaccprec,
yaccdefinitionpiece,yaccuniontypes,yaccdeclarationwithoutvaltype,
yaccnonterminals,yaccterminals,yaccsymbols,yaccvaltype,yacctypedterminals};

// rule Rules[]=
// {
// 	{yaccprogram,{yaccrequired}},
// 	{yaccprogram,{yaccrequired,YACC_PARTION}},
// 
// 	{yaccdeclare,{TERMEMPTY}},
// 	{yaccdeclare,{yaccdeclare,YACC_LITLEFT}},
// 	{yaccdeclare,{yaccdeclare,yaccdeclaration}},
// 
// 	{yaccdeclaration,{YACC_UNION,'{',yaccuniontypes,'}'}},
// 	////////extend for complex type,like struct _A *,and str[256]
// 	//no error checking here
// // 	{yaccuniontypes,{YACC_IDENTIFIER,YACC_IDENTIFIER,';'},
// // 	"parser.addValType($1,$2);"},
// // 	{yaccuniontypes,{yaccuniontypes,YACC_IDENTIFIER,YACC_IDENTIFIER,';'},
// // 	"parser.addValType($2,$3);"},
// 	{yaccuniontypes,{YACC_IDENTIFIER,';'},
// 	"parser.addValType($1);"},
// 	{yaccuniontypes,{yaccuniontypes,YACC_IDENTIFIER,';'},
// 	"parser.addValType($2);"},
// 
// 	{yaccdeclaration,{YACC_LEFT,yacctypedterminals},"curPrec++;"},
// 	{yaccdeclaration,{YACC_RIGHT,yacctypedterminals},"curPrec++;"},
// 	{yaccdeclaration,{YACC_NONASSOC,yacctypedterminals},"curPrec++;"},
// 	{yaccdeclaration,{YACC_TOKEN,yacctypedterminals}},
// 	{yaccdeclaration,{YACC_STARTRULE,YACC_IDENTIFIER},
// 	"{char* p=$2;\n"
// 		"if(symbolsMap.find(p)==symbolsMap.end())\n"
// 			"symbolsMap.insert(pair<string,int>(p,curNonTerm++));\n"
// 		"parser.setStartRule(symbolsMap[p]);}"},
// 
// 	{yacctypedterminals,{yaccdefaulttype,yaccterminals}},
// 	{yacctypedterminals,{YACC_VALTYPE,yaccterminals}},
// 
// 	{yaccdeclaration,{YACC_TYPE,yaccdefaulttype,yaccnonterminals}},
// 	{yaccdeclaration,{YACC_TYPE,YACC_VALTYPE,yaccnonterminals}},
// 
// 	//only used in %type
// 	{yaccnonterminals,{TERMEMPTY}},
// 	{yaccnonterminals,{yaccnonterminals,YACC_IDENTIFIER},
// 	"{char* p=$2;\n"
// 	"if(symbolsMap.find(p)==symbolsMap.end())\n"
// 	"symbolsMap.insert(pair<string,int>(p,curNonTerm++));\n"
// 	"parser.setValType(symbolsMap[p],$<str>0);}"},
// 
// 	//only used in token defs
// 	{yaccterminals,{TERMEMPTY}},
// 	{yaccterminals,{yaccterminals,YACC_IDENTIFIER},
// 	"{char* p=$2;\n"
// 	"if(symbolsMap.find(p)==symbolsMap.end())\n"
// 		"symbolsMap.insert(pair<string,int>(p,curTerm++));\n"
// 	"parser.setValType(symbolsMap[p],$<str>0);\n"
// 	"if($-1==TOKEN) parser.setPrec(symbolsMap[p],0),parser.setAssoc(symbolsMap[p],ASSOC_NON);\n"
// 	"else parser.setPrec(symbolsMap[p],curPrec),parser.setAssoc(symbolsMap[p],$-1);}"},
// 	{yaccterminals,{yaccterminals,YACC_CHAR},
// 	"{int p=$2;\n"
// 	"parser.setValType(p,$<str>0);\n"
// 	"if($-1==TOKEN) parser.setPrec(p,0),parser.setAssoc(p,ASSOC_NON);\n"
// 	"else parser.setPrec(p,curPrec),parser.setAssoc(p,$-1);}"},
// 
// //	{yaccdefaulttype,{TERMEMPTY},"strcpy($<str>$,parser.valTypes[0]);"},	
// 	{yaccdefaulttype,{TERMEMPTY},"strcpy($$,parser.valTypes[0]);"},	
// 
// 	{yaccdefine,{YACC_LITLEFT,yaccdefinitions}},
// 	{yaccdefine,{yaccdefinitions}},
// 	{yaccdefinitions,{yaccdefinition}},
// 	{yaccdefinitions,{yaccdefinitions,yaccdefinition}},
// 
// 	{yaccdefinitionpiece,{yaccsymbols,YACC_ACTION},"addingRule.setAction($2);"},
// 	{yaccdefinitionpiece,{yaccsymbols}},
// 
// 	//used in definition
// 	{yaccsymbols,{TERMEMPTY},"addingRule.clear();"},
// 	{yaccsymbols,{yaccsymbols,YACC_IDENTIFIER},
// 	"{char* p=$2;\n"
// 	"if(symbolsMap.find(p)==symbolsMap.end())\n"		
// 		"symbolsMap.insert(pair<string,int>(p,curNonTerm++));\n"
// 	"addingRule.addSymbol(symbolsMap[p]);}"},
// 	{yaccsymbols,{yaccsymbols,YACC_CHAR},"addingRule.addSymbol($2);"},
// // 	{yaccsymbols,{yaccsymbols,YACC_PREC,YACC_IDENTIFIER},
// // 	"{char* p=$3;\n"
// // 	"if(symbolsMap.find(p)==symbolsMap.end()) parseerror();\n"
// // 	"else addingRule.setPrec(parser.precTable[symbolsMap[p]]);and assoc}"},
// // 
// // 	{yaccsymbols,{yaccsymbols,YACC_PREC,YACC_CHAR},"addingRule.setPrec(parser.precTable[$3]);and assoc"},
// 	{yaccsymbols,{yaccsymbols,yaccprec}},
// 	{yaccprec,{YACC_PREC,YACC_IDENTIFIER},
// 	"{char* p=$2;\n"
// 	"if(symbolsMap.find(p)==symbolsMap.end()) parseerror();\n"
// 	"else{ addingRule.setPrec(parser.precTable[symbolsMap[p]]);addingRule.setAssoc(parser.assocTable[symbolsMap[p]]);}}"},
// 
// 	{yaccprec,{YACC_PREC,YACC_CHAR},"addingRule.setPrec(parser.precTable[$2]);addingRule.setAssoc(parser.assocTable[$2]);"},
// 
// 	{yaccdefinition,{YACC_IDENTIFIER,':',yaccdefinitionor,';'}},
// 	{yaccdefinitionor,{yaccdefinitionor,'|',yaccdefinitionpiece},
// 	"{char* p=$<str>-1;\n"
// 	"if(symbolsMap.find(p)==symbolsMap.end()) symbolsMap.insert(pair<string,int>(p,curNonTerm++));\n"
// 	"addingRule.setLHS(symbolsMap[p]);\n"
// 	"if(!addingRule.precSet) addingRule.setPrec(parser.precTable[addingRule.lastTerm]);\n"
// 	"parser.addRule(addingRule.r);}"},
// 	{yaccdefinitionor,{yaccdefinitionpiece},
// 	"{char* p=$<str>-1;\n"
// 	"if(symbolsMap.find(p)==symbolsMap.end()) symbolsMap.insert(pair<string,int>(p,curNonTerm++));\n"
// 	"addingRule.setLHS(symbolsMap[p]);\n"
// 	//and assoc
// 	"if(!addingRule.precSet) addingRule.setPrec(parser.precTable[addingRule.lastTerm]);\n"
// 	"parser.addRule(addingRule.r);}"},
// 
// 	{yaccrequired,{yaccdeclare,YACC_PARTION,yaccdefine}}
// };

rule yaccRules[]=
{
	{yaccprogram,{yaccrequired},"printf(\"accept!!\\n\");"},
	{yaccprogram,{yaccrequired,YACC_PARTION},"printf(\"accept!!\\n\");"},

	{yaccdeclare,{TERMEMPTY}},
	{yaccdeclare,{yaccdeclare,YACC_LITLEFT},"{ofstream outf(\"_yblock1\",ios::app);scanner.writeLitBlock(outf);outf.close();}"},
	{yaccdeclare,{yaccdeclare,yaccdeclaration}},

	{yaccdeclaration,{YACC_UNION,'{',yaccuniontypes,'}'}},
	{yaccuniontypes,{YACC_IDENTIFIER,';'},
	"parser.addValType($1);"},
	{yaccuniontypes,{yaccuniontypes,YACC_IDENTIFIER,';'},
	"parser.addValType($2);"},

	{yaccdeclaration,{YACC_LEFT,yacctypedterminals},"++curPrec;"},
	{yaccdeclaration,{YACC_RIGHT,yacctypedterminals},"++curPrec;"},
	{yaccdeclaration,{YACC_NONASSOC,yacctypedterminals},"++curPrec;"},
	{yaccdeclaration,{YACC_TOKEN,yacctypedterminals}},
	{yaccdeclaration,{YACC_STARTRULE,YACC_IDENTIFIER},
	"{char* p=$2;\n"
	"if(symbolsMap.find(p)==symbolsMap.end())\n"
	"symbolsMap.insert(pair<string,int>(p,curNonTerm++));\n"
	"parser.setStartSymbol(symbolsMap[p]);startSymbolSet=true;}"},

	//¿‡–Õ≤ª∆•≈‰
	{yacctypedterminals,{yaccvaltype,yaccterminals},"$$=0;"},

	{yaccvaltype,{TERMEMPTY},"strcpy($$,parser.valTypes[0]);"},	
	{yaccvaltype,{'<',YACC_IDENTIFIER,'>'},"strcpy($$,$2);"},

	{yaccdeclaration,{YACC_TYPE,yaccvaltype,yaccnonterminals}},

	//only used in %type
	{yaccnonterminals,{TERMEMPTY}},
	{yaccnonterminals,{yaccnonterminals,YACC_IDENTIFIER},
	"{char* p=$2;\n"
	"if(symbolsMap.find(p)==symbolsMap.end())\n"
	"{symbolsMap.insert(pair<string,int>(p,curNonTerm));\n"
	"parser.setValType(curNonTerm++,$<str>0);}\n"
	"else parser.setValType(symbolsMap[p],$<str>0);}"},

	//only used in token defs
	{yaccterminals,{TERMEMPTY}},
	{yaccterminals,{yaccterminals,YACC_IDENTIFIER},
	"{char* p=$2;\n"
	"if(symbolsMap.find(p)==symbolsMap.end())\n"
	"{symbolsMap.insert(pair<string,int>(p,curTerm++));\n"
	"parser.addTerminal(p);}"
	"parser.setValType(symbolsMap[p],$<str>0);\n"
	"int assotype=$-1;\n"
	"if(assotype==TOKEN) parser.setPrec(symbolsMap[p],0),parser.setAssoc(symbolsMap[p],ASSOC_NON);\n"
	"else parser.setPrec(symbolsMap[p],curPrec),parser.setAssoc(symbolsMap[p],assotype);}"},
	{yaccterminals,{yaccterminals,YACC_CHAR},
	"{int p=$2;\n"
	"parser.setValType(p,$<str>0);\n"
	"int assotype=$-1;\n"
	"if(assotype==TOKEN) parser.setPrec(p,0),parser.setAssoc(p,ASSOC_NON);\n"
	"else parser.setPrec(p,curPrec),parser.setAssoc(p,assotype);}"},

	//	{yaccdefaulttype,{TERMEMPTY},"strcpy($<str>$,parser.valTypes[0]);"},	


	{yaccdefine,{YACC_LITLEFT,yaccdefinitions},"{ofstream outf(\"_yblock2\",ios::app);scanner.writeLitBlock(outf);outf.close();}"},
	{yaccdefine,{yaccdefinitions}},
	{yaccdefinitions,{yaccdefinition}},
	{yaccdefinitions,{yaccdefinitions,yaccdefinition}},

	{yaccdefinitionpiece,{yaccsymbols,YACC_ACTION},"addingRule.setAction($2);"},
	{yaccdefinitionpiece,{yaccsymbols}},

	//used in definition
	{yaccsymbols,{TERMEMPTY},"addingRule.clear();"},
	{yaccsymbols,{yaccsymbols,YACC_IDENTIFIER},
	"{char* p=$2;\n"
	"if(symbolsMap.find(p)==symbolsMap.end())\n"		
	"symbolsMap.insert(pair<string,int>(p,curNonTerm++));\n"
	"addingRule.addSymbol(symbolsMap[p]);}"},
	{yaccsymbols,{yaccsymbols,YACC_CHAR},"addingRule.addSymbol($2);"},

	{yaccsymbols,{yaccsymbols,yaccprec}},
	{yaccprec,{YACC_PREC,YACC_IDENTIFIER},
	"{char* p=$2;\n"
	"if(symbolsMap.find(p)==symbolsMap.end()) parseerror(\"prec terminal symbol not found, skip setting.\");\n"
	"else{int c=symbolsMap[p];addingRule.setPrec(parser.precTable[c]);addingRule.setAssoc(parser.assocTable[c]);}}"},

	{yaccprec,{YACC_PREC,YACC_CHAR},"{int c=$2;addingRule.setPrec(parser.precTable[c]);addingRule.setAssoc(parser.assocTable[c]);}"},

	{yaccdefinition,{YACC_IDENTIFIER,':',yaccdefinitionor,';'},"if(firstNonTerm){firstNonTerm=false;if(!startSymbolSet){startSymbolSet=true;parser.setStartSymbol(symbolsMap[$1]);}}"},
	{yaccdefinitionor,{yaccdefinitionor,'|',yaccdefinitionpiece},
	"{char* p=$<str>-1;\n"
	"if(symbolsMap.find(p)==symbolsMap.end()) symbolsMap.insert(pair<string,int>(p,curNonTerm++));\n"
	"addingRule.setLHS(symbolsMap[p]);\n"
	"if(addingRule.r.bsize) addingRule.addSymbol(TERMEMPTY);\n"
	"if(!addingRule.precSet){addingRule.setPrec(parser.precTable[addingRule.lastTerm]);addingRule.setAssoc(parser.assocTable[addingRule.lastTerm]);}\n"
	"parser.addRule(addingRule.r);}"},
	{yaccdefinitionor,{yaccdefinitionpiece},
	"{char* p=$<str>-1;\n"
	"if(symbolsMap.find(p)==symbolsMap.end()) symbolsMap.insert(pair<string,int>(p,curNonTerm++));\n"
	"addingRule.setLHS(symbolsMap[p]);\n"
	//and assoc
	"if(!addingRule.precSet) {addingRule.setPrec(parser.precTable[addingRule.lastTerm]);addingRule.setAssoc(parser.assocTable[addingRule.lastTerm]);}\n"
	"parser.addRule(addingRule.r);}"},

	{yaccrequired,{yaccdeclare,YACC_PARTION,yaccdefine}}
};



static Lexical lex;
void genyacc()
{

	lex.addStartState("STATE_UNION");

	lex.auxNFAPart.addPart("D","[0-9]");
	lex.auxNFAPart.addPart("L","[a-zA-Z_]");
	lex.auxNFAPart.addPart("ID","{L}({L}|{D})*");
	lex.auxNFAPart.addPart("WS","[ \r\t\n]+");
	//	lex.auxNFAPart.addPart("SPACE","[ \r\t]");
	// 	lex.auxNFAPart.addPart("QUOTE","\\\"[^\"\\n]*\\\"");
	// 	lex.auxNFAPart.addPart("SBRACKETS","\\\[[^\\\]\\n]*\\\]");

	lex.addTokenDef("%\\{","litstart=code; if(skipLitBlock()) return YACC_LITLEFT;return -1;");//use as action too
	lex.addTokenDef("%%{WS}","if(++partcnt==2)writeLastBlock();return YACC_PARTION;");
	lex.addTokenDef("%union","BEGINSTATE STATE_UNION;return YACC_UNION;");
	lex.addTokenDef("%token","yylval.ival=TOKEN;return YACC_TOKEN;");
	lex.addTokenDef("%left","yylval.ival=ASSOC_LEFT;return YACC_LEFT;");
	lex.addTokenDef("%right","yylval.ival=ASSOC_RIGHT;return YACC_RIGHT;");
	lex.addTokenDef("%nonassoc","yylval.ival=ASSOC_NON;return YACC_NONASSOC;");
	lex.addTokenDef("%type","return YACC_TYPE;");
	lex.addTokenDef("%prec","return YACC_PREC;");
	lex.addTokenDef("%start","return YACC_STARTRULE;");

	lex.addTokenDef("{ID}","strcpy(yylval.str,yytext);return YACC_IDENTIFIER;");

	lex.addTokenDef("'\\\\?.'","if(yytext[0]=='\\\\') yylval.ival=handleTransChar(yytext[2]);else yylval.ival=yytext[1];return YACC_CHAR;");
	lex.addTokenDef(";","return ';';");
	lex.addTokenDef("\\<","return '<';");
	lex.addTokenDef("\\>","return '>';");
	lex.addTokenDef(":","return ':';");
	//introduce regular start state?
	lex.addTokenDef("\\|","return '|';");
	lex.addTokenDef("\\{{WS}?","handleAction();strcpy(yylval.str,yytext);return YACC_ACTION;");
	lex.addTokenDef("<STATE_UNION>\\{","return '{';");
	lex.addTokenDef("<STATE_UNION>;","return ';';");
	lex.addTokenDef("<STATE_UNION>\\}","BEGINSTATE INITIAL;return '}';");
	lex.addTokenDef("<STATE_UNION>{ID}","strcpy(yylval.str,yytext);return YACC_IDENTIFIER;");

	lex.addTokenDef("/\\*","skipComment();return 0;");

	lex.addTokenDef("{WS}","return 0;");
	lex.addTokenDef("<STATE_UNION>{WS}","return 0;");

	lex.buildTable();
	lex.generate();


	LALR1Parser lalr1;

	lalr1.addValType("ival");
	lalr1.addValType("str");

	//yaccdefaulttype int 0
	lalr1.setValType(yaccvaltype,"str");
	lalr1.setValType(YACC_ACTION,"str");
	lalr1.setValType(YACC_IDENTIFIER,"str");

	lalr1.readRules(yaccRules,sizeof(yaccRules)/sizeof(rule));

	lalr1.generate();
}
