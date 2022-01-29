#include <iostream>
#include <string>
#include <cstring>
#include <cstdio>
#include <ctype.h>
#include <cstdlib>
#include <vector>
#include <stack>
#include <algorithm>

using namespace std;

enum type_of_lex {
  LEX_NULL,                                                                                             /* 0*/
  LEX_AND, LEX_BEGIN, LEX_BOOL, LEX_STR, LEX_ELSE, LEX_END, LEX_IF, LEX_FALSE, LEX_INT,                 /* 9*/
  LEX_NOT, LEX_OR, LEX_PROGRAM, LEX_READ, LEX_THEN, LEX_TRUE, LEX_VAR, LEX_WHILE, LEX_WRITE, LEX_GOTO,  /*19*/
  LEX_PAROP, LEX_PARCL,                                                                                 /*21*/
  LEX_FIN,                                                                                              /*22*/
  LEX_SEMICOLON, LEX_COMMA, LEX_COLON, LEX_ASSIGN, LEX_LPAREN, LEX_RPAREN, LEX_EQ, LEX_LSS,             /*30*/
  LEX_GTR, LEX_MINUS, LEX_PLUS, LEX_TIMES, LEX_SLASH, LEX_LEQ, LEX_NEQ, LEX_GEQ, LEX_PERC,              /*39*/
  LEX_NUM,                                                                                              /*40*/
  LEX_ID,                                                                                               /*41*/
  LEX_STRNG,                                                                                            /*42*/
  LEX_MARK,                                                                                             /*43*/
  POLIZ_GO,                                                                                             /*44*/
  POLIZ_FGO,                                                                                            /*45*/
  POLIZ_LABEL,                                                                                          /*46*/
  POLIZ_ADDRESS,                                                                                        /*47*/
  POLIZ_POTEN_LABEL                                                                                     /*48*/
};

class Lex{
  type_of_lex t_lex;
  int v_lex;
public:
  Lex(type_of_lex t=LEX_NULL, int v=0): t_lex(t), v_lex(v){}
  type_of_lex get_type() const{
    return t_lex;
  }
  int get_value() const{
    return v_lex;
  }
  friend ostream & operator<< (ostream &s, Lex l);
};

class Ident{
    char *name;
    int value;
    type_of_lex type;
    bool declare;
    bool assign;
    bool is_mark;
    int poliz_number;
public:
    Ident(){
        declare=assign=is_mark=false;
    }
    Ident(const char* n){
        strcpy(name, n);
        declare=assign=is_mark=false;
    }
    char* get_name(){
        return name;
    }
    void put_name(const char* n){
        name = new char[strlen(n)+1];
        strcpy(name,n);
    }
    bool get_declare(){
        return declare;
    }
    void put_declare(){
        declare = true;
    }
    bool get_assign(){
        return assign;
    }
    void put_assign(){
        assign=true;
    }
    bool get_is_mark(){
        return is_mark;
    }
    void put_is_mark(){
        is_mark=true;
    }
    int get_poliz_number(){
        return poliz_number;
    }
    void put_poliz_number(int i){
        poliz_number=i;
    }
    type_of_lex get_type(){
        return type;
    }
    void put_type(type_of_lex t){
        type = t;
    }
    int get_value(){
        return value;
    }
    void put_value(int v){
        value = v;
    }
};

class Tabl_Ident{
    Ident *p;
    int size;
    int current;
public:
    Tabl_Ident(int max_size){
        size=max_size;
        p=new Ident[size];
        current = 1;
    }
    ~Tabl_Ident(){
        delete []p;
    }
    Ident& operator[](int k){
        return p[k];
    }
    int get_cur(){
        return current;
    }
    int put(const char* buf);
};

int Tabl_Ident::put(const char *buf){
    for (int j=1; j<current; ++j) {
        if (!strcmp(buf, p[j].get_name())) return j;
    }
    p[current].put_name(buf);
    ++current;
    return current-1;
}

class Tabl_String{
    char** str;
    int *str_len;
    int size;
    int current;
public:
    Tabl_String(int max_size){
        str=new char*[size=max_size];
        str_len=new int[size];
        current=1;
    }
    ~Tabl_String(){
        for (int i=0; i<current; i++) delete []str[i];
        delete []str;
        delete[]str_len;
    }
    char* operator[](int k){
        return str[k];
    }
    int get_str_len(int k){
        return str_len[k];
    }
    int get_curr(){
        return current;
    }
    int put(const char* buf);
    int put_new(const char* buf, int i);
    int str_cmp(int i, int j);
};

int Tabl_String::put(const char *buf){
    str_len[current]=strlen(buf)+1;
    str[current]=new char[str_len[current]];
    strcpy(str[current], buf);
    current++;
    return current-1;
}

int Tabl_String::put_new(const char *buf, int i){
    str_len[i]=strlen(buf)+1;
    delete []str[i];
    str[i]=new char[str_len[i]];
    strcpy(str[i], buf);
}

int Tabl_String::str_cmp(int i, int j){
    int mini;
    if (get_str_len(i)<get_str_len(j)) mini=i;
    else mini=j;
    for (int k=0; k<get_str_len(mini)-1; k++){
        if (str[i][k]<str[j][k]) return 2;
        if (str[i][k]>str[j][k]) return 1;
    }
    if (get_str_len(j)==get_str_len(i)) return 0;
    if (get_str_len(mini)==get_str_len(j)) return 1;
    else return 2;
}

Tabl_String TST(50);

class Scanner{
    enum state {H, IDENT, NUMB, COM, QUO, ALE, DELIM, NEQ};
    static type_of_lex words[];
    static type_of_lex dlms[];
    state CS;
    FILE *fp;
    char c;
    char buf[100];
    int buf_top;
    int opnum, clnum;
    void clear (){
        buf_top=0;
        for (int j=0; j<80; ++j) buf[j]='\0';
    }
    void add(){
        buf[buf_top++]=c;
    }
    int look(char *buf, const char **list){
        int i=0;
        while (list[i]){
            if (!strcmp(buf, list[i])) return i;
            i++;
        }
        return 0;
    }
    void gc(){
        c=fgetc (fp);
    }
    bool check_parath(){
        if (opnum<clnum) return false;
        else return true;
    }
public:
    static const char *TW[],*TD[];
    char get_char(){
        return c;
    }
    Lex get_lex();
    Scanner(const char * program){
        fp=fopen(program, "r");
        CS=H;
        clear();
        opnum=clnum=0;
        gc();
    }
};

const char * Scanner::TW[]={
    "",     // 0  позиция 0 не используется
    "and",  // 1
    "begin",  // 2
    "bool",   // 3
    "string",  // 4
    "else",  // 5
    "end",  // 6
    "if",  // 7
    "false",  // 8
    "int",  // 9
    "not",  // 10
    "or",  // 11
    "program", // 12
    "read",  // 13
    "then",  // 14
    "true",  // 15
    "var",  // 16
    "while",  // 17
    "write",  // 18
    "goto",  // 19
    "{",  // 20
    "}",  // 21
    NULL
};

const char * Scanner:: TD[]={
    "",     // 0  позиция 0 не используется
    "@",   // 1
    ";",   // 2
    ",",   // 3
    ":",   // 4
    "=",   // 5
    "(",   // 6
    ")",   // 7
    "==",   // 8
    "<",   // 9
    ">",   // 10
    "-",   // 11
    "+",   // 12
    "*",   // 13
    "/",   // 14
    "<=",   // 15
    "!=",   // 16
    ">=",   // 17
    "%",    // 18
    NULL
};

Tabl_Ident TID(100);

type_of_lex Scanner::words[]={
    LEX_NULL, LEX_AND, LEX_BEGIN,
    LEX_BOOL, LEX_STR, LEX_ELSE,
    LEX_END, LEX_IF, LEX_FALSE,
    LEX_INT, LEX_NOT, LEX_OR,
    LEX_PROGRAM, LEX_READ, LEX_THEN,
    LEX_TRUE, LEX_VAR, LEX_WHILE,
    LEX_WRITE, LEX_GOTO, LEX_PAROP,
    LEX_PARCL, LEX_NULL
};

type_of_lex Scanner::dlms[]={
    LEX_NULL, LEX_FIN, LEX_SEMICOLON,
    LEX_COMMA, LEX_COLON, LEX_ASSIGN,
    LEX_LPAREN, LEX_RPAREN, LEX_EQ,
    LEX_LSS, LEX_GTR, LEX_MINUS,
    LEX_PLUS, LEX_TIMES, LEX_SLASH,
    LEX_LEQ, LEX_NEQ, LEX_GEQ,
    LEX_PERC, LEX_NULL
};

Lex Scanner::get_lex(){
    int d, j, p;
    CS=H;
    do{
        switch (CS){
            case H:
                if (c ==' ' || c =='\n' || c=='\r' || c =='\t') gc();
                else if (isalpha(c)){
                    clear();
                    add();
                    gc();
                    CS=IDENT;
                }
                else if (isdigit(c)){
                    d=c-'0';
                    gc();
                    CS=NUMB;
                }
                else if (c=='/'){
                    clear();
                    add();
                    gc();
                    if (c=='*'){
                        clear();
                        gc();
                        CS=COM;
                    }
                    else{
                        j=look(buf, TD);
                        return Lex(dlms[j], j);
                    }
                }
                else if (c=='{'){
                    opnum++;
                    gc();
                    return Lex(LEX_PAROP, opnum);
                }
                else if (c=='}'){
                    clnum++;
                    if(!check_parath()) throw c;
                    gc();
                    return Lex(LEX_PARCL, clnum);
                }
                else if (c=='"'){
                    clear();
                    gc();
                    CS=QUO;
                }
                else if (c=='=' || c=='<' || c=='>'){
                    clear();
                    add();
                    gc();
                    CS=ALE;
                }
                else if (c=='@'){
                    if (opnum!=clnum) throw "Error: the number of '{' is not equal to the number of '}' ";
                    return Lex(LEX_FIN);
                }
                else if (c=='!'){
                    clear();
                    add();
                    gc();
                    CS=NEQ;
                }
                else CS=DELIM;
                break;
            case IDENT:
                if (isalpha(c) || isdigit(c)){
                    add();
                    gc();
                }
                else if (c==':'){
                    if (look(buf, TW)+look(buf, TD)) throw "That is a reserved name";
                    else{
                        gc();
                        j=TID.put(buf);
                        /*p=TID.get_cur();
                        if (((j=TID.put(buf))<p) && TID[j].get_declare()) throw "Mark has the same name as the other ID";
                        TID[j].put_is_mark();
                        TID[j].put_declare();*/
                        return Lex(LEX_MARK, j);
                    }
                }
                else if ((j=look(buf, TW))) return Lex(words[j], j);
                else{
                    j=TID.put(buf);
                    return Lex(LEX_ID, j);
                }
                break;
            case NUMB:
                if (isdigit(c)){
                    d=d*10+(c-'0');
                    gc();
                }
                else return Lex(LEX_NUM, d);
                break;
            case COM:
                if (c=='*'){
                    gc();
                    if (c=='/'){
                        gc();
                        CS=H;
                    }
                    else if (c=='@') throw c;
                    else gc();
                }
                else if (c=='@') throw c;
                else gc();
                break;
            case QUO:
                if (c=='"'){
                    j=TST.put(buf);
                    gc();
                    return Lex(LEX_STRNG, j);
                }
                else if (c!='@' && c!='\n'){
                    add();
                    gc();
                }
                else throw c;
                break;
            case ALE:
                if (c=='='){
                    add();
                    gc();
                    j=look(buf, TD);
                    return Lex(dlms[j], j);
                }
                else{
                    j=look(buf, TD);
                    return Lex(dlms[j], j);
                }
                break;
            case NEQ:
                if (c=='='){
                    add();
                    gc();
                    j=look(buf, TD);
                    return Lex(LEX_NEQ, j);
                }
                else throw '=';
                break;
            case DELIM:
                clear();
                add();
                j=look(buf, TD);
                if(j){
                    gc();
                    return Lex(dlms[j], j);
                }
                else throw c;
                break;
        }    // end switch
    }
    while(true);
}

ostream & operator<< (ostream &s, Lex l){
    string t, r="";
    if (l.t_lex<=21) t=Scanner::TW[l.t_lex];
    else if (l.t_lex>=22 && l.t_lex<=39) t=Scanner::TD[l.t_lex-21];
    else if (l.t_lex==40) t="NUMB";
    else if (l.t_lex==41) t=TID[l.v_lex].get_name();
    else if (l.t_lex==42) t=TST[l.v_lex];
    else if (l.t_lex==43) t=TID[l.v_lex].get_name();
    else if (l.t_lex==44) t="GO";
    else if (l.t_lex==45) t="FGO";
    else if (l.t_lex==46) t="LABEL";
    else if (l.t_lex==47){
        r="address of ";
        t=TID[l.v_lex].get_name();
    }
    else if (l.t_lex==48) t="POTEN_LABEL";
    else throw l;
    s << '(' << r << t << ',' << l.v_lex << ");" << endl;
    return s;
}

template <class T, int max_size > class Stack{
    T s[max_size];
    int top;
public:
    Stack(){top=0;}
    //~Stack(){delete []s;}
    void up_top(){top++;}
    void reset(){top=0;}
    void push(T i);
    T pop();
    bool is_empty(){return top==0;}
    bool is_full(){return top==max_size;}
};

template <class T, int max_size >

void Stack <T, max_size >::push(T i){
    if (!is_full()){
        s[top] = i;
        ++top;
    }
    else throw "Stack_is_full";
}

template <class T, int max_size >

T Stack <T, max_size >::pop(){
    if (!is_empty()){
        --top;
        return s[top];
    }
    else throw "Stack_is_empty";
}

class Poliz{
    Lex *l_array;
    int array_size;
    int curr;
public:
    Poliz(int s){
        l_array=new Lex[array_size=s];
        curr=0;
    }
    ~Poliz(){
        delete []l_array;
    }
    void put_lex(Lex l){
        l_array[curr++]=l;
    }
    void put_lex(Lex l, int i){
        if (i>=array_size) throw "Poliz: out of array";
        else l_array[i]=l;
    }
    void miss(){
        curr++;
    }
    int get_curr(){
        return curr;
    }
    Lex& operator[](int i){
        if (i>=array_size) throw "Poliz: out of array";
        else if (i>curr) throw "Poliz: indefinite element of array";
        else return l_array[i];
    }
    void print(){
        cout << "POLIZ:" << endl;
        for(int j=0; j<curr; j++) cout << j << ") " << l_array[j];
    }
};

class Parser {
    Lex curr_lex;
    type_of_lex c_type;
    int   c_val;
    Scanner  scan;
    Stack <int, 100> st_lvalue;
    Stack <type_of_lex, 100> st_ltype;
    void P();
    void DESs();
    void DES();
    void OPs();
    void OP();
    void PER();
    void CONST();
    void EXP();
    void EASS();
    void EOR();
    void EAND();
    void ECMP();
    void EMINPL();
    void ETIMSLAPER();
    /*void EXP1();
    void LEXP();
    void BOOL();
    void CMP();
    void CMPRBL();*/
    void init();
    void eq_type();
    void check_id();
    void check_id_assign();
    void check_mark();
    void check_op();
    void check_not();
    void mark_analysis();
    void gl(){
        curr_lex = scan.get_lex();
        c_type = curr_lex.get_type();
        c_val = curr_lex.get_value();
    }
public:
    Poliz internal_prog;
    Parser(const char *program) : scan(program), internal_prog(1000){}
    void  analyze();
};

void Parser::init(){
    int i;
    type_of_lex t=st_ltype.pop();
    while(!st_lvalue.is_empty()){
        i=st_lvalue.pop();
        if (TID[i].get_declare()) throw TID[i];
        else{
            TID[i].put_type(t);
            TID[i].put_declare();
        }
    }
    st_ltype.reset();
    st_lvalue.reset();
}

void Parser::eq_type(){
    int i, j;
    type_of_lex t2=st_ltype.pop(), t1=st_ltype.pop();
    switch(t1){
        case LEX_INT:
            if (t2!=LEX_NUM) throw t2;
            else{
                j=st_lvalue.pop();
                i=st_lvalue.pop();
                TID[i].put_value(j);
                st_lvalue.push(i);
            }
            break;
        case LEX_BOOL:
            if (t2!=LEX_TRUE && t2!=LEX_FALSE) throw t2;
            else{
                i=st_lvalue.pop();
                TID[i].put_value(t2);
                st_lvalue.push(i);
            }
            break;
        case LEX_STR:
            if (t2!=LEX_STRNG) throw t2;
            else{
                j=st_lvalue.pop();
                i=st_lvalue.pop();
                TID[i].put_value(j);
                st_lvalue.push(i);
            }
            break;
    }
    st_ltype.push(t1);
}

void Parser::check_id(){
    if (TID[c_val].get_declare()) st_ltype.push(TID[c_val].get_type());
    else throw TID[c_val];
}

void Parser::check_id_assign(){
    if (!TID[c_val].get_assign()) throw TID[c_val];
}

void Parser::check_mark(){
    if (!TID[c_val].get_is_mark() || !TID[c_val].get_declare()) throw "Error: there is no such mark";
}

void Parser::check_op(){
    type_of_lex t2=st_ltype.pop(), op=st_ltype.pop(), t1=st_ltype.pop();
    switch(t1){
        case LEX_INT:
            switch(op){
                case LEX_MINUS:
                case LEX_PLUS:
                case LEX_TIMES:
                case LEX_SLASH:
                case LEX_PERC:
                    if (t2==LEX_INT) st_ltype.push(LEX_INT);
                    else throw "Error: wrong types of operands in arithmetical operation";
                    break;
                case LEX_EQ:
                case LEX_NEQ:
                case LEX_GTR:
                case LEX_LSS:
                case LEX_GEQ:
                case LEX_LEQ:
                    if (t2==LEX_INT) st_ltype.push(LEX_BOOL);
                    else throw "Error: wrong types of operands in comparison";
                    break;
                case LEX_ASSIGN:
                    if (t2==LEX_INT) st_ltype.push(LEX_INT);
                    else throw "Error: wrong types of operands in assignment";
                    break;
                default:
                    throw "Error: impossible operation with integer";
                    break;
            }
            break;
        case LEX_STR:
            switch(op){
                case LEX_PLUS:
                    if (t2==LEX_STR) st_ltype.push(LEX_STR);
                    else throw "Error: wrong types of operands in string addition";
                    break;
                case LEX_EQ:
                case LEX_NEQ:
                case LEX_GTR:
                case LEX_LSS:
                    if (t2==LEX_STR) st_ltype.push(LEX_BOOL);
                    else throw "Error: wrong types of operands in string comparison";
                    break;
                case LEX_ASSIGN:
                    if (t2==LEX_STR) st_ltype.push(LEX_STR);
                    else throw "Error: wrong types of operands in string assignment";
                    break;
                default:
                    throw "Error: impossible operation with string";
                    break;
            }
            break;
        case LEX_BOOL:
            switch(op){
                case LEX_AND:
                case LEX_OR:
                    if (t2==LEX_BOOL) st_ltype.push(LEX_BOOL);
                    else throw "Error: wrong types of operands in boolean operations";
                    break;
                case LEX_ASSIGN:
                    if (t2==LEX_BOOL) st_ltype.push(LEX_BOOL);
                    else throw "Error: wrong types of operands in boolean assignment";
                    break;
                default:
                    throw "Error: impossible operation with boolean";
                    break;
            }
            break;
        default:
            throw "Error: there is no such type";
            break;
    }
    internal_prog.put_lex(Lex(op));
}

void Parser::check_not(){
    if (st_ltype.pop()==LEX_BOOL){
        st_ltype.push(LEX_BOOL);
        internal_prog.put_lex(Lex(LEX_NOT));
    }
    else throw "Error: wrong type in logical negotiation";
}

void Parser::mark_analysis(){
    for (int i=0; i<internal_prog.get_curr(); i++)
    {
        if (internal_prog[i].get_type()==POLIZ_POTEN_LABEL){
            if (!TID[internal_prog[i].get_value()].get_declare() || !TID[internal_prog[i].get_value()].get_is_mark()) throw "Error: there is no such mark";
            else internal_prog.put_lex(Lex(POLIZ_LABEL, TID[internal_prog[i].get_value()].get_poliz_number()), i);
        }
    }
}

void Parser::analyze(){
    gl();
    P();
    cout << endl << "Syntax is correct!!!" << endl;
}

void Parser::P(){
    if (c_type==LEX_PROGRAM) gl();
    else throw curr_lex;
    if (c_type==LEX_PAROP) gl();
    else throw curr_lex;
    DESs();
    OPs();
    if (c_type==LEX_PARCL) gl();
    else throw curr_lex;
    if (c_type!=LEX_FIN) throw curr_lex;
    else{
        internal_prog.put_lex(Lex(LEX_FIN));
        mark_analysis();
    }
}

void Parser::DESs(){
    if (c_type==LEX_INT || c_type==LEX_BOOL || c_type==LEX_STR){
        st_ltype.push(c_type);
        gl();
        DES();
    }
}

void Parser::DES(){
    PER();
    init();
    if (c_type==LEX_SEMICOLON){
        gl();
        DESs();
    }
    else throw curr_lex;
}

void Parser::PER(){
    int i;
    if (c_type==LEX_ID){
        st_lvalue.push(c_val);
        gl();
        if (c_type==LEX_ASSIGN){
            TID[i=st_lvalue.pop()].put_assign();
            st_lvalue.push(i);
            gl();
            CONST();
            eq_type();
            if (c_type==LEX_COMMA){
                gl();
                PER();
            }
        }
        else{
            if (c_type==LEX_COMMA){
                gl();
                PER();
            }
        }
    }
    else throw curr_lex;
}

void Parser::CONST(){
    if (c_type==LEX_MINUS || c_type==LEX_PLUS){
        st_ltype.push(c_type);
        gl();
        if (c_type==LEX_NUM){
            if (st_ltype.pop()==LEX_PLUS) st_lvalue.push(c_val);
            else st_lvalue.push(-c_val);
            st_ltype.push(c_type);
            gl();
        }
        else throw curr_lex;
    }
    else if (c_type==LEX_TRUE || c_type==LEX_FALSE){
        st_ltype.push(c_type);
        gl();
    }
    else if (c_type==LEX_STRNG){
        st_ltype.push(c_type);
        st_lvalue.push(c_val);
        gl();
    }
    else throw curr_lex;
}

void Parser::OPs(){
R:  if (c_type!=LEX_PARCL) OP();
    if (c_type!=LEX_PARCL) goto R;
}

void Parser::OP(){
    int i1, i2, i3, i4;
    if (c_type==LEX_IF){
        gl();
        if (c_type==LEX_LPAREN){
            gl();
            EXP();
            if (st_ltype.pop()!=LEX_BOOL) throw "Error: conditions can be only boolean";
            i1=internal_prog.get_curr();
            internal_prog.miss();
            internal_prog.put_lex(Lex(POLIZ_FGO));
            if (c_type==LEX_RPAREN){
                gl();
                OP();
                if (c_type==LEX_ELSE){
                    i2=internal_prog.get_curr();
                    internal_prog.miss();
                    internal_prog.put_lex(Lex(POLIZ_GO));
                    internal_prog.put_lex(Lex(POLIZ_LABEL, internal_prog.get_curr()), i1);
                    gl();
                    OP();
                    internal_prog.put_lex(Lex(POLIZ_LABEL, internal_prog.get_curr()), i2);
                }
                else internal_prog.put_lex(Lex(POLIZ_LABEL, internal_prog.get_curr()), i1);
            }
            else throw curr_lex;
        }
        else throw curr_lex;
    }
    else if (c_type==LEX_GOTO){
        gl();
        if (c_type==LEX_ID){
            internal_prog.put_lex(Lex(POLIZ_POTEN_LABEL, c_val));
            internal_prog.put_lex(Lex(POLIZ_GO));
            gl();
        }
        else throw curr_lex;
        if (c_type==LEX_SEMICOLON) gl();
        else throw curr_lex;
    }
    else if (c_type==LEX_WHILE){
        gl();
        if (c_type==LEX_LPAREN) gl();
        else throw curr_lex;
        i3=internal_prog.get_curr();
        EXP();
        if (st_ltype.pop()!=LEX_BOOL) throw "Error: conditions can be only boolean";
        i4=internal_prog.get_curr();
        internal_prog.miss();
        internal_prog.put_lex(Lex(POLIZ_FGO));
        if (c_type==LEX_RPAREN) gl();
        else throw curr_lex;
        OP();
        internal_prog.put_lex(Lex(POLIZ_LABEL, i3));
        internal_prog.put_lex(Lex(POLIZ_GO));
        internal_prog.put_lex(Lex(POLIZ_LABEL, internal_prog.get_curr()), i4);
    }
    else if (c_type==LEX_READ){
        gl();
        if (c_type==LEX_LPAREN) gl();
        else throw curr_lex;
        if (c_type==LEX_ID){
            check_id();
            if (st_ltype.pop()==LEX_BOOL) throw "Error: you can not read boolean value";
            else{
                internal_prog.put_lex(Lex(POLIZ_ADDRESS, c_val));
                gl();
            }
        }
        else throw curr_lex;
        if (c_type==LEX_RPAREN) gl();
        else throw curr_lex;
        if (c_type==LEX_SEMICOLON) gl();
        else throw curr_lex;
        internal_prog.put_lex(Lex(LEX_READ));
    }
    else if (c_type==LEX_WRITE){
        gl();
        if (c_type==LEX_LPAREN) gl();
        else throw curr_lex;
Rep:    EXP();
        internal_prog.put_lex(Lex(LEX_WRITE));
        if (c_type==LEX_COMMA){
            gl();
            goto Rep;
        }
        if (c_type==LEX_RPAREN) gl();
        else throw curr_lex;
        if (c_type==LEX_SEMICOLON) gl();
        else throw curr_lex;
    }
    else if (c_type==LEX_PAROP){
        gl();
        OPs();
        if (c_type==LEX_PARCL) gl();
        else throw curr_lex;
    }
    else if (c_type==LEX_MARK){
        if (TID[c_val].get_declare()) throw TID[c_val];
        else{
            TID[c_val].put_is_mark();
            TID[c_val].put_declare();
        }
        TID[c_val].put_poliz_number(internal_prog.get_curr());
        gl();
        OP();
    }
    else if (c_type==LEX_ID){
        check_id();
        internal_prog.put_lex(Lex(POLIZ_ADDRESS, c_val));
        gl();
        if (c_type==LEX_ASSIGN){
            st_ltype.push(c_type);
            gl();
            EXP();
            check_op();
            if (c_type==LEX_SEMICOLON) gl();
            else throw curr_lex;
        }
        else throw curr_lex;
    }
    else throw curr_lex;
}

void Parser::EXP(){
    EOR();
    while(c_type==LEX_OR){
        st_ltype.push(c_type);
        gl();
        EOR();
        check_op();
    }
}

void Parser::EOR(){
    EAND();
    while(c_type==LEX_AND){
        st_ltype.push(c_type);
        gl();
        EAND();
        check_op();
    }
}

void Parser::EAND(){
    ECMP();
    while(c_type==LEX_EQ || c_type==LEX_NEQ || c_type==LEX_GEQ || c_type==LEX_LEQ || c_type==LEX_GTR || c_type==LEX_LSS){
        st_ltype.push(c_type);
        gl();
        ECMP();
        check_op();
    }
}

void Parser::ECMP(){
    EMINPL();
    while(c_type==LEX_PLUS || c_type==LEX_MINUS){
        st_ltype.push(c_type);
        gl();
        EMINPL();
        check_op();
    }
}

void Parser::EMINPL(){
    ETIMSLAPER();
    while(c_type==LEX_TIMES || c_type==LEX_SLASH || c_type==LEX_PERC){
        st_ltype.push(c_type);
        gl();
        ETIMSLAPER();
        check_op();
    }
}

void Parser::ETIMSLAPER(){
    Lex l;
    switch(c_type){
        case LEX_ID:
            check_id();
            l=curr_lex;
            gl();
            if (c_type==LEX_ASSIGN) internal_prog.put_lex(Lex(POLIZ_ADDRESS, l.get_value()));
            else internal_prog.put_lex(l);
            break;
        case LEX_NUM:
            st_ltype.push(LEX_INT);
            internal_prog.put_lex(curr_lex);
            gl();
            break;
        case LEX_TRUE:
            st_ltype.push(LEX_BOOL);
            internal_prog.put_lex(Lex(LEX_TRUE, 1));
            gl();
            break;
        case LEX_FALSE:
            st_ltype.push(LEX_BOOL);
            internal_prog.put_lex(Lex(LEX_FALSE, 0));
            gl();
            break;
        case LEX_STRNG:
            st_ltype.push(LEX_STR);
            internal_prog.put_lex(curr_lex);
            gl();
            break;
        case LEX_NOT:
            gl();
            ETIMSLAPER();
            check_not();
            break;
        case LEX_LPAREN:
            gl();
            EXP();
            if (c_type==LEX_RPAREN) gl();
            else throw curr_lex;
            break;
        default:
            throw curr_lex;
            break;
    }
}

class Executer{
    Lex pc_el;
public:
    void execute(Poliz& prog);
};

void Executer::execute(Poliz& prog){
    Stack <int, 100> args;
    Stack <type_of_lex, 100> types;
    char *s, bufer[100];
    string str;
    Lex l1, l2;
    int i, j, i1, j1, k=1, index = 0, size = prog.get_curr();
    while(index<size){
        pc_el=prog[index];
        switch(pc_el.get_type()){
            case LEX_TRUE:
                args.push(1);
                types.push(LEX_BOOL);
                break;
            case LEX_FALSE:
                args.push(0);
                types.push(LEX_BOOL);
                break;
            case LEX_NUM:
                args.push(pc_el.get_value());
                types.push(LEX_INT);
                break;
            case LEX_STRNG:
                args.push(pc_el.get_value());
                types.push(LEX_STR);
                break;
            case POLIZ_ADDRESS:
                args.push(pc_el.get_value());
                types.push(POLIZ_ADDRESS);
                break;
            case POLIZ_LABEL:
                args.push(pc_el.get_value());
                types.push(POLIZ_LABEL);
                break;
            case LEX_ID:
                i=pc_el.get_value();
                if (TID[i].get_assign()){
                    switch(TID[i].get_type()){
                        case LEX_INT:
                            args.push(TID[i].get_value());
                            types.push(LEX_INT);
                            break;
                        case LEX_BOOL:
                            if (TID[i].get_value()==LEX_TRUE) args.push(1);
                            if (TID[i].get_value()==LEX_FALSE) args.push(0);
                            types.push(LEX_BOOL);
                            break;
                        case LEX_STR:
                            args.push(TID[i].get_value());
                            types.push(LEX_STR);
                            break;
                        default:
                            throw "Error: unknown type";
                            break;
                    }
                }
                else throw "Error: indefinite ID";
                break;
            case LEX_NOT:
                types.pop();
                if (args.pop()) args.push(0);
                else args.push(1);
                types.push(LEX_BOOL);
                break;
            case LEX_OR:
                i=args.pop();
                types.pop();
                types.pop();
                if(args.pop() || i) args.push(1);
                else args.push(0);
                types.push(LEX_BOOL);
                break;
            case LEX_AND:
                i=args.pop();
                types.pop();
                types.pop();
                if(args.pop() && i) args.push(1);
                else args.push(0);
                types.push(LEX_BOOL);
                break;
            case POLIZ_GO:
                types.pop();
                index=args.pop()-1;
                break;
            case POLIZ_FGO:
                types.pop();
                types.pop();
                i=args.pop();
                if (!args.pop()) index=i-1;
                break;
            case LEX_WRITE:
                switch(types.pop()){
                    case LEX_INT:
                        cout << args.pop() << endl;
                        break;
                    case LEX_BOOL:
                        if (args.pop()) cout << "true" << endl;
                        else cout << "false" << endl;
                        break;
                    case LEX_STR:
                        cout << TST[args.pop()] << endl;
                        break;
                    default:
                        throw "Error: unknown type";
                        break;
                }
                break;
            case LEX_READ:
                j=args.pop();
                switch(TID[j].get_type()){
                    case LEX_INT:
L:                      getline(cin, str);
                        if (str.size()==0) goto L;
                        i1=0;
                        if (str[0]=='-') k=-1;
                        else{
                            if (!isdigit(str[0])) throw "Error: number should be entered";
                            else i1+=str[0]-'0';
                        }
                        if (k==-1 && str.size()==1) throw "Error: number should be entered";
                        for (i=1; i<str.size(); i++){
                            if (!isdigit(str[i])) throw "Error: number should be entered";
                            else i1=10*i1+str[i]-'0';
                        }
                        TID[j].put_value(k*i1);
                        TID[j].put_assign();
                        break;
                    case LEX_STR:
                        getline(cin, str);
                        if (str.size()>100) throw "Error: so long string can not be entered";
                        s=new char[str.size()+1];
                        for (i=0; i<str.size(); i++) s[i]=str[i];
                        TST.put(s);
                        TID[j].put_value(TST.get_curr()-1);
                        TID[j].put_assign();
                        break;
                    default:
                        throw "Error: wrong type";
                        break;
                }
                break;
            case LEX_PLUS:
                i=args.pop();
                j=args.pop();
                i1=types.pop();
                j1=types.pop();
                if (j1==LEX_INT){
                    args.push(j+i);
                    types.push(LEX_INT);
                }
                else{
                    if (TST.get_str_len(j)+TST.get_str_len(i)>102) throw "Error: too long string";
                    for (int k=0; k<TST.get_str_len(j)-1; k++) bufer[k]=TST[j][k];
                    for (int k=0; k<TST.get_str_len(i); k++) bufer[k+TST.get_str_len(j)-1]=TST[i][k];
                    TST.put(bufer);
                    types.push(LEX_STR);
                    args.push(TST.get_curr()-1);
                }
                break;
            case LEX_TIMES:
                types.pop();
                types.pop();
                args.push(args.pop()*args.pop());
                types.push(LEX_INT);
                break;
            case LEX_MINUS:
                types.pop();
                types.pop();
                i=args.pop();
                args.push(args.pop()-i);
                types.push(LEX_INT);
                break;
            case LEX_SLASH:
                types.pop();
                types.pop();
                i=args.pop();
                if (i!=0) args.push(args.pop()/i);
                else throw "Error: division by zero";
                types.push(LEX_INT);
                break;
            case LEX_PERC:
                types.pop();
                types.pop();
                i=args.pop();
                if (i!=0) args.push(args.pop()%i);
                else throw "Error: division with remainder by zero";
                types.push(LEX_INT);
                break;
            case LEX_EQ:
                i1=types.pop();
                j1=types.pop();
                i=args.pop();
                j=args.pop();
                if (j1==LEX_INT){
                    if(j==i) args.push(1);
                    else args.push(0);
                }
                else{
                    switch(TST.str_cmp(j, i)){
                        case 0:
                            args.push(1);
                            break;
                        default:
                            args.push(0);
                            break;
                    }
                }
                types.push(LEX_BOOL);
                break;
            case LEX_NEQ:
                i1=types.pop();
                j1=types.pop();
                i=args.pop();
                j=args.pop();
                if (j1==LEX_INT){
                    if(j!=i) args.push(1);
                    else args.push(0);
                }
                else{
                    switch(TST.str_cmp(j, i)){
                        case 0:
                            args.push(0);
                            break;
                        default:
                            args.push(1);
                            break;
                    }
                }
                types.push(LEX_BOOL);
                break;
            case LEX_LSS:
                i1=types.pop();
                j1=types.pop();
                i=args.pop();
                j=args.pop();
                if (j1==LEX_INT){
                    if(j<i) args.push(1);
                    else args.push(0);
                }
                else{
                    switch(TST.str_cmp(j, i)){
                        case 2:
                            args.push(1);
                            break;
                        default:
                            args.push(0);
                            break;
                    }
                }
                types.push(LEX_BOOL);
                break;
            case LEX_GTR:
                i1=types.pop();
                j1=types.pop();
                i=args.pop();
                j=args.pop();
                if (j1==LEX_INT){
                    if(j>i) args.push(1);
                    else args.push(0);
                }
                else{
                    switch(TST.str_cmp(j, i)){
                        case 1:
                            args.push(1);
                            break;
                        default:
                            args.push(0);
                            break;
                    }
                }
                types.push(LEX_BOOL);
                break;
            case LEX_GEQ:
                types.pop();
                types.pop();
                i=args.pop();
                if(args.pop()>=i) args.push(1);
                else args.push(0);
                types.push(LEX_BOOL);
                break;
            case LEX_LEQ:
                types.pop();
                types.pop();
                i=args.pop();
                if(args.pop()<=i) args.push(1);
                else args.push(0);
                types.push(LEX_BOOL);
                break;
            case LEX_ASSIGN:
                i=args.pop();
                j=args.pop();
                i1=types.pop();
                j1=types.pop();
                if (i1==LEX_INT) TID[j].put_value(i);
                if (i1==LEX_BOOL){
                    if (i) TID[j].put_value(LEX_TRUE);
                    else TID[j].put_value(LEX_FALSE);
                }
                if (i1==LEX_STR) TID[j].put_value(i);
                TID[j].put_assign();
                break;
            case LEX_FIN:
                break;
            default:
                throw "Error: unexpected elem";
        }
        index++;
    }
    cout << "Finish of executing!!!" << endl;
}

class Interpretator{
    Parser pars;
    Executer E;
public:
    Interpretator(const char* program): pars(program){};
    void interpretation();
};

void Interpretator::interpretation(){
    pars.analyze();
    pars.internal_prog.print();
    E.execute(pars.internal_prog);
}

int main()
{
    try{
        //Lex l;
        //Scanner s("D:/prog.txt");
        //while(true) cout << s.get_lex();
        //while((l=s.get_lex()).get_type()!=LEX_FIN) cout << l;
        //Parser p("D:/prog.txt");
        //p.analyze();
        //cout << TID[6].get_value() << endl;
        //p.internal_prog.print();
        Interpretator i("./prog.txt");
        i.interpretation();
        return 0;
    }
    catch (char c){
        cout << "Error: unexpected symbol " << c << endl;
        return 1;
    }
    catch (Lex l){
        cout << "Error: unexpected lexem " << l << endl;
        return 1;
    }
    catch (const char* s){
        cout << s << endl;
        return 1;
    }
    catch (Ident i){
        cout << "Error: " << i.get_name() << " is declared twice or was not initialised or not declared at all" << endl;
        return 1;
    }
    catch (type_of_lex t){
        cout << "Error: " << t << " is wrong type" << endl;
        return 1;
    }
}
