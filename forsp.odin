package forsp

import "core:fmt"
import "core:log"
import "core:mem"
import "core:os"
import "core:strconv"
import "core:strings"

Nil :: bool
Atom :: string
Number :: i64
Pair :: struct {
	car, cdr: ^Obj,
}
Closure :: struct {
	body, env: ^Obj,
}
Primitive :: proc(env: Obj)

Obj :: union {
	Nil,
	Atom,
	Number,
	Pair,
	Closure,
	Primitive,
}

State :: struct {
	input:          string, // input data string used by read()
	pos:            u64, // input data position used by read()
	nil:            Obj, // nil: ()
	read_stack:     Obj, // defered obj to emit from read

	// Atoms
	interned_atoms: Obj, // interned atoms list
	atom_true:      Obj, // atom: t
	atom_quote:     Obj, // atom: quote
	atom_push:      Obj, // atom: push
	atom_pop:       Obj, // atom: pop

	// stack/env
	stack:          Obj, // top-of-stack (implemented with pairs)
	env:            Obj, // top-level / initial environment
}

state: State

nil_new :: proc() -> Obj {
	return Nil(true)
}

atom_new :: proc(str: string) -> Obj {
	return Atom(str)
}

number_new :: proc(n: i64) -> Obj {
	return Number(n)
}

pair_new :: proc(car, cdr: Obj) -> Obj {
	p := Pair{new(Obj), new(Obj)}
	p.car^ = car
	p.cdr^ = cdr
	return p
}

pair_destroy :: proc(p: ^Pair) {
	if p.car != nil {
		obj_destroy(p.car^)
		free(p.car)
		p.car = nil
	}

	if p.cdr != nil {
		obj_destroy(p.cdr^)
		free(p.cdr)
		p.cdr = nil
	}
}

closure_new :: proc(c: Closure) -> Obj {
	return c
}

closure_destroy :: proc(c: ^Closure) {
	if true {return}
	if c.env != nil {
		obj_destroy(c.env^)
		free(c.env)
		c.env = nil
	}

	if c.body != nil {
		obj_destroy(c.body^)
		free(c.body)
		c.body = nil
	}
}

primitive_new :: proc(f: proc(env: ^Obj)) -> Obj {
	return Primitive(f)
}

obj_new :: proc {
	nil_new,
	atom_new,
	number_new,
	pair_new,
	closure_new,
	primitive_new,
}

obj_delete :: proc(obj: Obj) {
	if obj == nil || obj == state.nil {
		return
	}

	switch &v in obj {
	case Nil, Atom, Number, Primitive:
	// nothing to delete/free
	case Closure:
	// closure_destroy(&v)
	case Pair:
	// pair_destroy(&v)
	}
}

obj_destroy :: proc {
	obj_delete,
	pair_destroy,
	closure_destroy,
}

assert :: proc(v: bool, msg: string) {
	if !v {
		fmt.panicf("ASSERT: %s", msg)
	}
}

assert_type :: proc(v: $V, $t: typeid, msg: string) {
	_, ok := v.(t)
	assert(ok, msg)
}

fail_type :: proc(v: any, $t: typeid, msg: string) {
	if is(v, t) {
		fail(msg)
	}
}

fail :: proc(msg: string) {
	fmt.panicf("FAIL: %s", msg)
}

failf :: proc(msg: string, args: ..any) {
	m := fmt.aprintf(msg, ..args) // I guess we just leak this...
	fmt.panicf("FAIL: %s", m)
}

is :: proc(v: $V, $t: typeid) -> bool {
	_, ok := v.(t)
	return ok
}

intern :: proc(atom_buf: string) -> Obj {
	for list := state.interned_atoms; list != state.nil; list = list.(Pair).cdr^ {
		assert_type(list, Pair, "state.interned_atoms must be Pairs")

		elem := list.(Pair).car
		assert_type(elem, Atom, "state.interned_atoms.car must be an Atom")
		if len(atom_buf) == len(elem.(Atom)) && atom_buf == elem.(Atom) {
			return elem^
		}
	}

	// not found, create a new one and push the front of the list
	atom := atom_new(atom_buf)
	state.interned_atoms = pair_new(atom, state.interned_atoms)

	return atom
}

car :: proc(obj: Obj) -> Obj {
	fail_type(obj, Pair, "Expected Pair to apply car() function")
	return obj.(Pair).car^
}

cdr :: proc(obj: Obj) -> Obj {
	fail_type(obj, Pair, "Expected Pair to apply cdr() function")
	return obj.(Pair).cdr^
}

obj_equal :: proc(a, b: Obj) -> bool {
	return a == b || (is(a, Number) && is(b, Number) && a.(Number) == b.(Number))
}

obj_i64 :: proc(a: Obj) -> i64 {
	return a.(Number) if is(a, Number) else 0
}

/*******************************************************************
 * Read
 ******************************************************************/

peek :: proc() -> u8 {
	return 0 if state.pos == u64(len(state.input)) else state.input[state.pos]
}

advance :: proc() {
	assert(peek() != 0, "cannot advance further")
	state.pos += 1
}

is_white :: proc(c: u8) -> bool {
	switch c {
	case ' ', '\t', '\n':
		return true
	}

	return false
}

is_directive :: proc(c: u8) -> bool {
	switch c {
	case '\'', '^', '$':
		return true
	}

	return false
}

is_punctuation :: proc(c: u8) -> bool {
	switch c {
	case 0, '(', ')', ';':
		return true
	}

	return true if is_white(c) || is_directive(c) else false
}

skip_white_and_comments :: proc() {
	c := peek()
	if c == 0 {return}

	// skip whitespace
	if is_white(c) {
		advance()
		skip_white_and_comments()
		return
	}

	// skip comment
	if c == ';' {
		advance()
		for {
			c = peek()
			if c == 0 {return}
			advance()
			if c == '\n' {break}
		}

		skip_white_and_comments()
		return
	}
}

read_list :: proc() -> Obj {
	if state.read_stack == nil {
		skip_white_and_comments()
		c := peek()
		if c == ')' {
			advance()
			return state.nil
		}
	}

	first := read()
	second := read_list()
	return pair_new(first, second)
}

parse_i64 :: proc(str: string) -> (i64, bool) {
	i, ok := strconv.parse_int(str, 10)
	return i64(i), ok
}

read_scalar :: proc() -> Obj {
	// otherwise, assume atom or number and read it
	start := state.pos
	for {
		if !is_punctuation(peek()) {
			advance()
			continue
		}

		break
	}

	str := state.input[start:state.pos]
	// is it a number?
	if n, ok := parse_i64(str); ok {
		return number_new(n)
	} else { 	// atom
		return intern(str)
	}
}

read :: proc() -> Obj {
	read_stack := state.read_stack
	if read_stack != nil {
		state.read_stack = cdr(read_stack)
		return car(read_stack)
	}

	skip_white_and_comments()

	c := peek()
	switch c {
	case 0:
		fail("End of input: could not read()")

	// A quote?
	case '\'':
		advance()
		return state.atom_quote

	// A push?
	case '^':
		advance()
		s: Obj
		s = pair_new(state.atom_push, s)
		s = pair_new(read_scalar(), s)
		s = pair_new(state.atom_quote, s)
		state.read_stack = s

		return read()

	// A pop?
	case '$':
		advance()
		s: Obj
		s = pair_new(state.atom_pop, s)
		s = pair_new(read_scalar(), s)
		s = pair_new(state.atom_quote, s)
		state.read_stack = s

		return read()

	// Read a list?
	case '(':
		advance()
		return read_list()

	}

	return read_scalar()
}

/*******************************************************************
 * Print
 ******************************************************************/

print_list_tail :: proc(obj: Obj) {
	if obj == state.nil {
		fmt.print(")")
		return
	}

	if is(obj, Pair) {
		fmt.print(" ")
		print_recurse(obj.(Pair).car^)
		print_list_tail(obj.(Pair).cdr^)
		return
	}

	fmt.print(" . ")
	print_recurse(obj)
	fmt.print(")")
}

print_recurse :: proc(obj: Obj) {
	if obj == state.nil {
		fmt.print("()")
		return
	}

	switch t in obj {
	case Nil, Atom, Number:
		fmt.print(t)
	case Pair:
		fmt.print("(")
		print_recurse(t.car^)
		print_list_tail(t.cdr^)

	case Closure:
		fmt.print("CLOSURE<")
		print_recurse(t.body^)
		fmt.printf(", %p>", t.env^)

	case Primitive:
		fmt.printf("PRIM<%p>", t)
	}
}

print :: proc(obj: Obj) {
	print_recurse(obj)
	fmt.println()
}

/*******************************************************************
 * Environment
 ******************************************************************/

// Environment is just a simple list of key-val (dotted) pairs

env_find :: proc(env, key: Obj) -> Obj {
	if !is(key, Atom) {fail("Expected 'key' to be an Atom in env_find()")}

	for v := env; v != state.nil; v = cdr(v) {
		kv := car(v)
		if key == car(kv) {
			return cdr(kv)
		}
	}

	failf("Failed to find key='%s' in environment", key.(Atom))
	return nil
}

env_define :: proc(env, key, val: Obj) -> Obj {
	return pair_new(pair_new(key, val), env)
}

env_define_prim :: proc(env: Obj, name: string, fn: proc(env: ^Obj)) -> Obj {
	return env_define(env, intern(name), primitive_new(fn))
}


/*******************************************************************
 * Value Stack Operations
 ******************************************************************/

push :: proc(obj: Obj) {
	state.stack = pair_new(obj, state.stack)
}

try_pop :: proc() -> (Obj, bool) {
	if state.stack == nil || state.stack == state.nil {
		return nil, false
	}

	o := car(state.stack)
	state.stack = cdr(state.stack)
	return o, true
}

pop :: proc() -> Obj {
	if ret, ok := try_pop(); ok {
		return ret
	} else {
		return nil
	}
}

/*******************************************************************
 * Eval
 ******************************************************************/

compute :: proc(comp: Obj, env: ^Obj) {
	when ODIN_DEBUG {
		fmt.print("compute: ")
		print(comp)
	}

	cmp := comp
	for cmp != state.nil {
		cmd := car(cmp)
		cmp = cdr(cmp)

		if cmd == state.atom_quote {
			if cmp == state.nil {fail("Expected data following a quote form")}
			push(car(cmp))
			cmp = cdr(cmp)

			continue
		}

		eval(cmd, env)
	}
}

eval :: proc(expr: Obj, env: ^Obj) {
	when ODIN_DEBUG {
		fmt.print("eval: ")
		print(expr)
	}

	if is(expr, Atom) {
		val := env_find(env^, expr)
		if is(val, Closure) {
			compute(val.(Closure).body^, val.(Closure).env)
		} else if is(val, Primitive) {
			val.(Primitive)(env^)
		} else {
			push(val)
		}
	} else if is(expr, Nil) || is(expr, Pair) {
		c := Closure{new(Obj), new(Obj)}
		c.body^ = expr
		c.env^ = env^

		push(closure_new(c))
	} else {
		push(expr)
	}
}

/*******************************************************************
 * Primitives
 ******************************************************************/

// Core primitives
prim_push :: proc(env: ^Obj) {
	a := pop()
	defer obj_destroy(a)

	push(env_find(env^, a))
}
prim_pop :: proc(env: ^Obj) {
	k, v := pop(), pop()
	env^ = env_define(env^, k, v)
}
prim_eq :: proc(_: ^Obj) {
	a, b := pop(), pop()
	defer {obj_destroy(a);obj_destroy(b)}

	push(obj_equal(a, b) ? state.atom_true : state.nil)
}
prim_cons :: proc(_: ^Obj) {
	a, b := pop(), pop()
	push(pair_new(a, b))
}
prim_car :: proc(_: ^Obj) {
	a := pop()
	defer obj_destroy(a)

	push(car(a))
}
prim_cdr :: proc(_: ^Obj) {
	a := pop()
	defer obj_destroy(a)

	push(cdr(a))
}
prim_cswap :: proc(_: ^Obj) {
	t := pop()
	defer obj_destroy(t)

	if (pop() == state.atom_true) {
		a, b := pop(), pop()
		push(a);push(b)
	}
}
prim_tag :: proc(_: ^Obj) {
	a := pop()
	defer obj_destroy(a)

	push(number_new(a.(Number)))
}
prim_read :: proc(_: ^Obj) {push(read())}
prim_print :: proc(_: ^Obj) {
	a := pop()

	print(a)
}

// Extra primitives
prim_stack :: proc(_: ^Obj) {push(state.stack)}
prim_env :: proc(env: ^Obj) {push(env^)}
prim_sub :: proc(_: ^Obj) {
	b, a := pop(), pop()
	defer {obj_destroy(a);obj_destroy(b)}

	push(number_new(obj_i64(a) - obj_i64(b)))
}
prim_mul :: proc(_: ^Obj) {
	b, a := pop(), pop()
	defer {obj_destroy(a);obj_destroy(b)}

	push(number_new(obj_i64(a) * obj_i64(b)))}
prim_nand :: proc(_: ^Obj) {
	b, a := pop(), pop()
	defer {obj_destroy(a);obj_destroy(b)}

	push(number_new(~(obj_i64(a) & obj_i64(b))))}
prim_lsh :: proc(_: ^Obj) {
	b, a := pop(), pop()
	defer {obj_destroy(a);obj_destroy(b)}

	push(number_new(obj_i64(a) << uint(obj_i64(b))))}
prim_rsh :: proc(_: ^Obj) {
	b, a := pop(), pop()
	defer {obj_destroy(a);obj_destroy(b)}

	push(number_new(obj_i64(a) >> uint(obj_i64(b))))}

when #config(USE_LOWLEVEL, false) {
	// Low-level primitives
	prim_ptr_state :: proc(_: ^Obj) {}
	prim_ptr_read :: proc(_: ^Obj) {}
	prim_ptr_write :: proc(_: ^Obj) {}
	prim_ptr_to_obj :: proc(_: ^Obj) {}
	prim_ptr_from_obj :: proc(_: ^Obj) {}
}

load_file :: proc(filename: string) -> (string, bool) {
	file, erno := os.open(filename)
	if erno != 0 {
		return "", false
	}
	defer os.close(file)

	b, ok := os.read_entire_file_from_handle(file)
	if !ok {
		return "", ok
	}
	defer delete(b)

	str, err := strings.clone_from_bytes(b)
	if err != nil {
		return "", false
	}

	return str, true
}

setup :: proc(filename: string) {
	state.input = load_file(filename) or_else panic("failed to load input file")
	state.pos = 0

	state.read_stack = nil
	state.nil = nil_new()

	state.interned_atoms = state.nil
	state.atom_true = intern("t")
	state.atom_quote = intern("quote")
	state.atom_push = intern("push")
	state.atom_pop = intern("pop")

	state.stack = state.nil

	env := state.nil

	// core primitives
	env = env_define_prim(env, "push", prim_push)
	env = env_define_prim(env, "pop", prim_pop)
	env = env_define_prim(env, "cons", prim_cons)
	env = env_define_prim(env, "car", prim_car)
	env = env_define_prim(env, "cdr", prim_cdr)
	env = env_define_prim(env, "eq", prim_eq)
	env = env_define_prim(env, "cswap", prim_cswap)
	env = env_define_prim(env, "tag", prim_tag)
	env = env_define_prim(env, "read", prim_read)
	env = env_define_prim(env, "print", prim_print)

	// extra primitives
	env = env_define_prim(env, "stack", prim_stack)
	env = env_define_prim(env, "env", prim_env)
	env = env_define_prim(env, "-", prim_sub)
	env = env_define_prim(env, "*", prim_mul)
	env = env_define_prim(env, "nand", prim_nand)
	env = env_define_prim(env, "<<", prim_lsh)
	env = env_define_prim(env, ">>", prim_rsh)

	// low-level primitives
	when #config(USE_LOWLEVEL, false) {
		env = env_define_prim(env, "ptr-state!", prim_ptr_state)
		env = env_define_prim(env, "ptr-read!", prim_ptr_read)
		env = env_define_prim(env, "ptr-write!", prim_ptr_write)
		env = env_define_prim(env, "ptr-to-obj!", prim_ptr_to_obj)
		env = env_define_prim(env, "ptr-from-obj!", prim_ptr_from_obj)
	}

	state.env = env
}

cleanup :: proc() {
	delete(state.input)
	obj_destroy(state.stack)
	obj_destroy(state.interned_atoms)
	obj_destroy(state.env)
	obj_destroy(state.atom_true)
	obj_destroy(state.atom_quote)
	obj_destroy(state.atom_push)
	obj_destroy(state.atom_pop)
	obj_destroy(state.nil)
}

main :: proc() {
	when ODIN_DEBUG {
		track: mem.Tracking_Allocator
		mem.tracking_allocator_init(&track, context.allocator)
		context.allocator = mem.tracking_allocator(&track)

		defer {
			if len(track.allocation_map) > 0 {
				fmt.eprintf("=== %v allocations not freed: ===\n", len(track.allocation_map))
				for _, entry in track.allocation_map {
					fmt.eprintf("- %v bytes @ %v\n", entry.size, entry.location)
				}
			}
			if len(track.bad_free_array) > 0 {
				fmt.eprintf("=== %v incorrect frees: ===\n", len(track.bad_free_array))
				for entry in track.bad_free_array {
					fmt.eprintf("- %p @ %v\n", entry.memory, entry.location)
				}
			}
			mem.tracking_allocator_destroy(&track)
		}

		l := log.create_console_logger()
		defer log.destroy_console_logger(l)
		context.logger = l
	}

	if len(os.args) != 2 {
		fmt.eprintf("usage: %s path\n", os.args[0])
		os.exit(1)
	}

	setup(os.args[1])
	defer cleanup()

	obj := read()
	defer obj_destroy(obj)

	compute(obj, &state.env)
}
