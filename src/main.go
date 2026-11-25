package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// ---------- Lexer ----------

type Kind int

const (
	EOF Kind = iota
	INT
	FLOAT
	STRING
	IDENT
	KW
	OP
)

type Token struct {
	K    Kind
	S    string
	Line int
}

var keywords = map[string]bool{
	"fn": true, "let": true, "return": true, "if": true, "else": true,
	"while": true, "true": true, "false": true,
	"int": true, "float": true, "bool": true, "string": true,
}

var types = map[string]string{"int": "int", "float": "double", "bool": "bool", "string": "std::string"}

func fail(line int, msg string) { panic(fmt.Sprintf("ligne %d : %s", line, msg)) }

func isLetter(c byte) bool { return c == '_' || c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' }
func isDigit(c byte) bool  { return c >= '0' && c <= '9' }

func lex(src string) []Token {
	var toks []Token
	line, i := 1, 0
	for i < len(src) {
		c := src[i]
		switch {
		case c == '\n':
			line++
			i++
		case c == ' ' || c == '\t' || c == '\r':
			i++
		case c == '/' && i+1 < len(src) && src[i+1] == '/':
			for i < len(src) && src[i] != '\n' {
				i++
			}
		case isDigit(c):
			j := i
			for j < len(src) && isDigit(src[j]) {
				j++
			}
			k := INT
			if j+1 < len(src) && src[j] == '.' && isDigit(src[j+1]) {
				j++
				for j < len(src) && isDigit(src[j]) {
					j++
				}
				k = FLOAT
			}
			toks = append(toks, Token{k, src[i:j], line})
			i = j
		case c == '"':
			j := i + 1
			for j < len(src) && src[j] != '"' {
				if src[j] == '\\' {
					j++
				}
				j++
			}
			if j >= len(src) {
				fail(line, "chaîne non terminée")
			}
			toks = append(toks, Token{STRING, src[i+1 : j], line})
			i = j + 1
		case isLetter(c):
			j := i
			for j < len(src) && (isLetter(src[j]) || isDigit(src[j])) {
				j++
			}
			w := src[i:j]
			k := IDENT
			if keywords[w] {
				k = KW
			}
			toks = append(toks, Token{k, w, line})
			i = j
		default:
			if i+1 < len(src) {
				switch two := src[i : i+2]; two {
				case "->", "==", "!=", "<=", ">=", "&&", "||":
					toks = append(toks, Token{OP, two, line})
					i += 2
					continue
				}
			}
			if strings.IndexByte("+-*/%=<>(){},;:!", c) < 0 {
				fail(line, fmt.Sprintf("caractère inattendu : %c", c))
			}
			toks = append(toks, Token{OP, string(c), line})
			i++
		}
	}
	return append(toks, Token{EOF, "", line})
}

// ---------- AST ----------

type Expr struct {
	Kind string // int float str bool var bin un call
	Val  string
	Args []*Expr
}

type Stmt struct {
	Kind       string // let assign return if while expr
	Name, Type string
	E          *Expr
	Body, Else []*Stmt
}

type Func struct {
	Name   string
	Params [][2]string // nom, type
	Ret    string
	Body   []*Stmt
}

// ---------- Parser ----------

type Parser struct {
	toks []Token
	pos  int
}

func (p *Parser) peek() Token  { return p.toks[p.pos] }
func (p *Parser) peek2() Token { return p.toks[min(p.pos+1, len(p.toks)-1)] }
func (p *Parser) next() Token  { t := p.toks[p.pos]; p.pos++; return t }

func (p *Parser) is(s string) bool {
	t := p.peek()
	return (t.K == OP || t.K == KW) && t.S == s
}

func (p *Parser) expect(s string) {
	if !p.is(s) {
		fail(p.peek().Line, fmt.Sprintf("attendu '%s', trouvé '%s'", s, p.peek().S))
	}
	p.pos++
}

func (p *Parser) ident() string {
	t := p.next()
	if t.K != IDENT {
		fail(t.Line, fmt.Sprintf("identifiant attendu, trouvé '%s'", t.S))
	}
	return t.S
}

func (p *Parser) parseType() string {
	t := p.next()
	if _, ok := types[t.S]; !ok || t.K != KW {
		fail(t.Line, fmt.Sprintf("type attendu, trouvé '%s'", t.S))
	}
	return t.S
}

func (p *Parser) parseProgram() []*Func {
	var fs []*Func
	for p.peek().K != EOF {
		fs = append(fs, p.parseFunc())
	}
	return fs
}

func (p *Parser) parseFunc() *Func {
	p.expect("fn")
	f := &Func{Name: p.ident()}
	p.expect("(")
	for !p.is(")") {
		n := p.ident()
		p.expect(":")
		f.Params = append(f.Params, [2]string{n, p.parseType()})
		if !p.is(")") {
			p.expect(",")
		}
	}
	p.expect(")")
	if p.is("->") {
		p.next()
		f.Ret = p.parseType()
	}
	f.Body = p.parseBlock()
	return f
}

func (p *Parser) parseBlock() []*Stmt {
	p.expect("{")
	var b []*Stmt
	for !p.is("}") {
		if p.peek().K == EOF {
			fail(p.peek().Line, "'}' manquant")
		}
		b = append(b, p.parseStmt())
	}
	p.expect("}")
	return b
}

func (p *Parser) parseStmt() *Stmt {
	switch {
	case p.is("let"):
		p.next()
		s := &Stmt{Kind: "let", Name: p.ident()}
		if p.is(":") {
			p.next()
			s.Type = p.parseType()
		}
		p.expect("=")
		s.E = p.parseExpr(1)
		p.expect(";")
		return s
	case p.is("return"):
		p.next()
		s := &Stmt{Kind: "return"}
		if !p.is(";") {
			s.E = p.parseExpr(1)
		}
		p.expect(";")
		return s
	case p.is("if"):
		p.next()
		s := &Stmt{Kind: "if", E: p.parseExpr(1)}
		s.Body = p.parseBlock()
		if p.is("else") {
			p.next()
			if p.is("if") {
				s.Else = []*Stmt{p.parseStmt()}
			} else {
				s.Else = p.parseBlock()
			}
		}
		return s
	case p.is("while"):
		p.next()
		s := &Stmt{Kind: "while", E: p.parseExpr(1)}
		s.Body = p.parseBlock()
		return s
	case p.peek().K == IDENT && p.peek2().K == OP && p.peek2().S == "=":
		s := &Stmt{Kind: "assign", Name: p.ident()}
		p.expect("=")
		s.E = p.parseExpr(1)
		p.expect(";")
		return s
	}
	s := &Stmt{Kind: "expr", E: p.parseExpr(1)}
	p.expect(";")
	return s
}

var prec = map[string]int{
	"||": 1, "&&": 2, "==": 3, "!=": 3,
	"<": 4, ">": 4, "<=": 4, ">=": 4,
	"+": 5, "-": 5, "*": 6, "/": 6, "%": 6,
}

func (p *Parser) parseExpr(min int) *Expr {
	lhs := p.parseUnary()
	for {
		t := p.peek()
		pr, ok := prec[t.S]
		if t.K != OP || !ok || pr < min {
			return lhs
		}
		p.next()
		rhs := p.parseExpr(pr + 1)
		lhs = &Expr{Kind: "bin", Val: t.S, Args: []*Expr{lhs, rhs}}
	}
}

func (p *Parser) parseUnary() *Expr {
	if p.is("-") || p.is("!") {
		op := p.next().S
		return &Expr{Kind: "un", Val: op, Args: []*Expr{p.parseUnary()}}
	}
	return p.parsePrimary()
}

func (p *Parser) parsePrimary() *Expr {
	t := p.next()
	switch {
	case t.K == INT:
		return &Expr{Kind: "int", Val: t.S}
	case t.K == FLOAT:
		return &Expr{Kind: "float", Val: t.S}
	case t.K == STRING:
		return &Expr{Kind: "str", Val: t.S}
	case t.K == KW && (t.S == "true" || t.S == "false"):
		return &Expr{Kind: "bool", Val: t.S}
	case t.K == IDENT:
		if p.is("(") {
			p.next()
			c := &Expr{Kind: "call", Val: t.S}
			for !p.is(")") {
				c.Args = append(c.Args, p.parseExpr(1))
				if !p.is(")") {
					p.expect(",")
				}
			}
			p.expect(")")
			return c
		}
		return &Expr{Kind: "var", Val: t.S}
	case t.K == OP && t.S == "(":
		e := p.parseExpr(1)
		p.expect(")")
		return e
	}
	fail(t.Line, fmt.Sprintf("expression inattendue '%s'", t.S))
	return nil
}

// ---------- Codegen C++ ----------

func cty(t string) string {
	if t == "" {
		return "void"
	}
	return types[t]
}

func genExpr(e *Expr) string {
	switch e.Kind {
	case "int", "float", "bool", "var":
		return e.Val
	case "str":
		return `std::string("` + e.Val + `")`
	case "bin":
		return "(" + genExpr(e.Args[0]) + " " + e.Val + " " + genExpr(e.Args[1]) + ")"
	case "un":
		return "(" + e.Val + genExpr(e.Args[0]) + ")"
	case "call":
		var a []string
		for _, x := range e.Args {
			a = append(a, genExpr(x))
		}
		if e.Val == "print" && len(a) == 1 {
			return "(std::cout << " + a[0] + " << std::endl)"
		}
		return e.Val + "(" + strings.Join(a, ", ") + ")"
	}
	return ""
}

func genBlock(sb *strings.Builder, b []*Stmt, ind int) {
	for _, s := range b {
		genStmt(sb, s, ind)
	}
}

func genStmt(sb *strings.Builder, s *Stmt, ind int) {
	pad := strings.Repeat("  ", ind)
	switch s.Kind {
	case "let":
		t := "auto"
		if s.Type != "" {
			t = cty(s.Type)
		}
		fmt.Fprintf(sb, "%s%s %s = %s;\n", pad, t, s.Name, genExpr(s.E))
	case "assign":
		fmt.Fprintf(sb, "%s%s = %s;\n", pad, s.Name, genExpr(s.E))
	case "return":
		if s.E == nil {
			fmt.Fprintf(sb, "%sreturn;\n", pad)
		} else {
			fmt.Fprintf(sb, "%sreturn %s;\n", pad, genExpr(s.E))
		}
	case "expr":
		fmt.Fprintf(sb, "%s%s;\n", pad, genExpr(s.E))
	case "while":
		fmt.Fprintf(sb, "%swhile (%s) {\n", pad, genExpr(s.E))
		genBlock(sb, s.Body, ind+1)
		fmt.Fprintf(sb, "%s}\n", pad)
	case "if":
		fmt.Fprintf(sb, "%sif (%s) {\n", pad, genExpr(s.E))
		genBlock(sb, s.Body, ind+1)
		if len(s.Else) > 0 {
			fmt.Fprintf(sb, "%s} else {\n", pad)
			genBlock(sb, s.Else, ind+1)
		}
		fmt.Fprintf(sb, "%s}\n", pad)
	}
}

func sig(f *Func) string {
	var ps []string
	for _, p := range f.Params {
		ps = append(ps, cty(p[1])+" "+p[0])
	}
	return fmt.Sprintf("%s %s(%s)", cty(f.Ret), f.Name, strings.Join(ps, ", "))
}

func gen(fs []*Func) string {
	var sb strings.Builder
	sb.WriteString("#include <iostream>\n#include <string>\n\n")
	for _, f := range fs {
		if f.Name != "main" {
			sb.WriteString(sig(f) + ";\n")
		}
	}
	sb.WriteString("\n")
	for _, f := range fs {
		if f.Name == "main" {
			sb.WriteString("int main() {\n")
			genBlock(&sb, f.Body, 1)
			sb.WriteString("  return 0;\n}\n\n")
		} else {
			sb.WriteString(sig(f) + " {\n")
			genBlock(&sb, f.Body, 1)
			sb.WriteString("}\n\n")
		}
	}
	return sb.String()
}

// ---------- Main ----------

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "usage: ziz fichier.ziz")
		os.Exit(1)
	}
	defer func() {
		if r := recover(); r != nil {
			fmt.Fprintln(os.Stderr, "erreur :", r)
			os.Exit(1)
		}
	}()

	file := os.Args[1]
	src, err := os.ReadFile(file)
	if err != nil {
		panic(err.Error())
	}
	p := &Parser{toks: lex(string(src))}
	cpp := gen(p.parseProgram())

	base := strings.TrimSuffix(file, filepath.Ext(file))
	if err := os.WriteFile(base+".cpp", []byte(cpp), 0644); err != nil {
		panic(err.Error())
	}
	cmd := exec.Command("g++", "-O2", "-o", base, base+".cpp")
	cmd.Stdout, cmd.Stderr = os.Stdout, os.Stderr
	if err := cmd.Run(); err != nil {
		os.Exit(1)
	}
}
