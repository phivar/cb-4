%expect 0

%define api.parser.struct {Parser}
%define api.value.type {Value}
%define api.parser.check_debug { self.debug }

%define parse.error custom
%define parse.trace

%code use {
    // all use goes here
    use crate::{Token, C1Lexer as Lexer, Loc, Value};
}

%code parser_fields {
    errors: Vec<String>,
    /// Enables debug printing
    pub debug: bool,
}

%token
    AND           "&&"
    OR            "||"
    EQ            "=="
    NEQ           "!="
    LEQ           "<="
    GEQ           ">="
    LSS           "<"
    GRT           ">"
    KW_BOOLEAN    "bool"
    KW_DO         "do"
    KW_ELSE       "else"
    KW_FLOAT      "float"
    KW_FOR        "for"
    KW_IF         "if"
    KW_INT        "int"
    KW_PRINTF     "printf"
    KW_RETURN     "return"
    KW_VOID       "void"
    KW_WHILE      "while"
    CONST_INT     "integer literal"
    CONST_FLOAT   "float literal"
    CONST_BOOLEAN "boolean literal"
    CONST_STRING  "string literal"
    ID            "identifier"


// definition of association and precedence of operators
%left '+' '-' OR
%left '*' '/' AND
%nonassoc UMINUS

// workaround for handling dangling else
// LOWER_THAN_ELSE stands for a not existing else
%nonassoc LOWER_THAN_ELSE
%nonassoc KW_ELSE

%%

program: %empty {$$ = Value::None;}
	| program functiondefinition {$$ = Value::None;} 
    | program declassignment_mod {$$ = Value::None;}
    ;

functiondefinition : 
    type ID '(' opt_parameterlist ')' '{' statementlist '}'
    {$$ = Value::FuncDef;}
    ;

opt_parameterlist : %empty {$$ = Value::OptParamList;}
    | parameterlist {$$ = Value::OptParamList;}
    ;
parameterlist : type id {$$ = Value::None;}
    | parameterlist ',' type id {$$ = Value::None;}
    ; 

functioncall : id '(' opt_assignmentlist ')' {$$ = Value::FuncCall;}
    ;
opt_assignmentlist : %empty {$$ = Value::None;}
    | assignmentlist {$$ = Value::None;}
    ;
assignmentlist : assignment {$$ = Value::None;}
    | assignmentlist ',' assignment {$$ = Value::None;}
    ;
statementlist : %empty {$$ = Value::StmtList;}
    | statementlist block {$$ = Value::StmtList;}
    ;

block : '{' statementlist '}' {$$ = Value::Block;}
    | statement {$$ = Value::Block;}
    ;

statement : ifstatement {$$ = Value::Stmt;}
    | forstatement {$$ = Value::Stmt;}
    | whilestatement {$$ = Value::Stmt;}
    | returnstatement ';' {$$ = Value::Stmt;}
    | dowhilestatement ';' {$$ = Value::Stmt;}
    | printf ';' {$$ = Value::Stmt;}
    | declassignment ';' {$$ = Value::Stmt;}
    | statassignment ';' {$$ = Value::Stmt;}
    | functioncall ';' {$$ = Value::Stmt;}
    ;
statblock :'{' statementlist '}' {$$ = Value::StatBlock;}
    | statement {$$ = Value::StatBlock;}
    ;

ifstatement : 
    KW_IF '(' assignment ')' statblock %prec LOWER_THAN_ELSE 
    {$$ = Value::None;} 
    | KW_IF '(' assignment ')' statblock KW_ELSE statblock %prec KW_ELSE
    {$$ = Value::None;}
    ;
forstatement : 
    KW_FOR '(' stat_decl_assign ';' expr ';' statassignment ')' statblock {$$ = Value::None;}
    ;
stat_decl_assign : statassignment {$$ = Value::None;}
    | declassignment {$$ = Value::None;}
    ;
dowhilestatement : KW_DO statblock KW_WHILE '(' assignment ')' {$$ = Value::None;} 
    ;
whilestatement : KW_WHILE '(' assignment ')' statblock {$$ = Value::None;}
    ;
returnstatement : KW_RETURN {$$ = Value::None;}
    | KW_RETURN assignment {$$ = Value::None;}
    ;
printf : KW_PRINTF '(' const_str_assign ')' {$$ = Value::None;}
    ;
const_str_assign : assignment {$$ = Value::None;}
    | CONST_STRING {$$ = Value::None;}
    ;
declassignment_mod : type id ';' {$$ = Value::DeclAssign;}
    | type id '=' assignment ';' {$$ = Value::DeclAssign;}
    ;
declassignment : type id {$$ = Value::DeclAssign;}
    | type id '=' assignment {$$ = Value::DeclAssign;}
    ;
statassignment : id '=' assignment {$$ = Value::None;}
    ;
assignment : id '=' assignment {$$ = Value::None;}
    | expr {$$ = Value::None;}
    ;
expr : simpexpr {$$ = Value::None;}
    | cmp_expr {$$ = Value::None;}
    ;
cmp_expr : simpexpr EQ simpexpr {$$ = Value::None;}
    | simpexpr NEQ simpexpr {$$ = Value::None;}
    | simpexpr LEQ simpexpr {$$ = Value::None;}
    | simpexpr GEQ simpexpr {$$ = Value::None;}
    | simpexpr GRT simpexpr {$$ = Value::None;}
    | simpexpr LSS simpexpr {$$ = Value::None;}
    ;
simpexpr : term {$$ = Value::None;}
    |'-' term %prec UMINUS {$$ = Value::None;}
    | simpexpr '+' term {$$ = Value::None;}
    | simpexpr '-' term {$$ = Value::None;}
    | simpexpr OR term {$$ = Value::None;}
    ;
term : factor {$$ = Value::None;}
    | term '*' factor {$$ = Value::None;}
    | term '/' factor {$$ = Value::None;}
    | term AND factor {$$ = Value::None;}
    ;
factor : CONST_INT {$$ = Value::None;}
    | CONST_FLOAT {$$ = Value::None;}
    | CONST_BOOLEAN {$$ = Value::None;}
    | functioncall {$$ = Value::None;}
    | id {$$ = Value::None;}
    | '(' assignment ')' {$$ = Value::None;}
    ;

type : KW_BOOLEAN {$$ = Value::Type;}
    | KW_FLOAT {$$ = Value::Type;}
    | KW_INT {$$ = Value::Type;}
    | KW_VOID {$$ = Value::Type;}
    ;
id : ID {$$ = Value::Id;}
    ;

%%

impl Parser {
    /// "Sucess" status-code of the parser
    pub const ACCEPTED: i32 = -1;

    /// "Failure" status-code of the parser
    pub const ABORTED: i32 = -2;

    /// Constructor
    pub fn new(lexer: Lexer) -> Self {
        // This statement was added to manually remove a dead code warning for 'owned_value_at' which is auto-generated code
        Self::remove_dead_code_warning();
        Self {
            yy_error_verbose: true,
            yynerrs: 0,
            debug: false,
            yyerrstatus_: 0,
            yylexer: lexer,
            errors: Vec::new(),
        }
    }

    /// Wrapper around generated `parse` method that also
    /// extracts the `errors` field and returns it.
    pub fn do_parse(mut self) -> Vec<String> {
        self.parse();
        self.errors
    }

    /// Retrieve the next token from the lexer
    fn next_token(&mut self) -> Token {
        self.yylexer.yylex()
    }

    /// Print a syntax error and add it to the errors field
    fn report_syntax_error(&mut self, stack: &YYStack, yytoken: &SymbolKind, loc: YYLoc) {
        let token_name = yytoken.name();
        let error = format!("Unexpected token {} at {:?}", token_name, loc);
        eprintln!("Stack: {}\nError: {}", stack, error);
        self.errors.push(error);
    }

    /// Helper function that removes a dead code warning, which would otherwise interfere with the correction of a submitted
    /// solution
    fn remove_dead_code_warning() {
    	let mut stack = YYStack::new();
    	let yystate: i32 = 0;
    	let yylval: YYValue = YYValue::new_uninitialized();
    	let yylloc: YYLoc = YYLoc { begin: 0, end: 0 };
        stack.push(yystate, yylval.clone(), yylloc);
    	let _ = stack.owned_value_at(0);
    }
}

