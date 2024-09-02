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
Primitive :: proc(env: ^Obj, allocator := context.allocator)

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
	nil:            ^Obj, // nil: ()
	read_stack:     ^Obj, // defered obj to emit from read

	// Atoms
	interned_atoms: ^Obj, // interned atoms list
	atom_true:      ^Obj, // atom: t
	atom_quote:     ^Obj, // atom: quote
	atom_push:      ^Obj, // atom: push
	atom_pop:       ^Obj, // atom: pop

	// stack/env
	stack:          ^Obj, // top-of-stack (implemented with pairs)
	env:            ^Obj, // top-level / initial environment
}

state: State

nil_new :: proc(allocator := context.allocator) -> ^Obj {
	o := new(Obj, allocator)
	o^ = Nil(true)
	return o
}

atom_new :: proc(str: string, allocator := context.allocator) -> ^Obj {
	o := new(Obj, allocator)
	o^ = Atom(str)
	return o
}

number_new :: proc(n: i64, allocator := context.allocator) -> ^Obj {
	o := new(Obj, allocator)
	o^ = Number(n)
	return o
}

pair_new :: proc(car, cdr: ^Obj, allocator := context.allocator) -> ^Obj {
	o := new(Obj, allocator)
	o^ = Pair{car, cdr}
	return o
}

pair_destroy :: proc(p: Pair, allocator := context.allocator) {
	obj_destroy(p.car, allocator)
	obj_destroy(p.cdr, allocator)
}

closure_new :: proc(c: Closure, allocator := context.allocator) -> ^Obj {
	o := new(Obj, allocator)
	o^ = c
	return o
}

closure_destroy :: proc(c: Closure, allocator := context.allocator) {
	obj_destroy(c.env, allocator)
	obj_destroy(c.body, allocator)
}

primitive_new :: proc(f: proc(env: ^^Obj, allocator := context.allocator), allocator := context.allocator) -> ^Obj {
	o := new(Obj, allocator)
	o^ = Primitive(f)
	return o
}

obj_new :: proc {
	nil_new,
	atom_new,
	number_new,
	pair_new,
	closure_new,
	primitive_new,
}

obj_delete :: proc(obj: ^Obj, allocator := context.allocator) {
	if obj == nil {
		return
	}

	switch &v in obj^ {
	case Nil, Atom, Number, Primitive:
	// nothing to delete/free except the outer obj
	case Closure:
		obj_destroy(v, allocator)
	case Pair:
		obj_destroy(v, allocator)
	}
}

obj_free :: proc(obj: ^^Obj, allocator := context.allocator) {
	if obj == nil || obj^ == nil{
		return
	}

	obj_destroy(obj^, allocator)
	free(obj^, allocator)
	obj^ = nil
}

obj_destroy :: proc {
	obj_delete,
	obj_free,
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

intern :: proc(atom_buf: string, allocator := context.allocator) -> ^Obj {
	for list := state.interned_atoms; list != state.nil; list = list.(Pair).cdr {
		assert_type(list, Pair, "state.interned_atoms must be Pairs")

		elem := list.(Pair).car
		assert_type(elem, Atom, "state.interned_atoms.car must be an Atom")
		if len(atom_buf) == len(elem.(Atom)) && atom_buf == elem.(Atom) {
			return elem
		}
	}

	// not found, create a new one and push the front of the list
	atom := obj_new(atom_buf, allocator)
	state.interned_atoms = obj_new(atom, state.interned_atoms, allocator)

	return atom
}

car :: proc(obj: ^Obj) -> ^Obj {
	fail_type(obj, Pair, "Expected Pair to apply car() function")
	return obj.(Pair).car
}

cdr :: proc(obj: ^Obj) -> ^Obj {
	fail_type(obj, Pair, "Expected Pair to apply cdr() function")
	return obj.(Pair).cdr
}

obj_equal :: proc(a, b: ^Obj) -> bool {
	return a == b || (is(a, Number) && is(b, Number) && a.(Number) == b.(Number))
}

obj_i64 :: proc(a: ^Obj) -> i64 {
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

read_list :: proc(allocator := context.allocator) -> ^Obj {
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
	return obj_new(first, second, allocator)
}

parse_i64 :: proc(str: string, allocator := context.allocator) -> (i64, bool) {
	i, ok := strconv.parse_int(str, 10)
	return i64(i), ok
}

read_scalar :: proc(allocator := context.allocator) -> ^Obj {
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
		return obj_new(n, allocator)
	} else { 	// atom
		return intern(str)
	}
}

read :: proc(allocator := context.allocator) -> ^Obj {
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
		s: ^Obj
		s = obj_new(state.atom_push, s, allocator)
		s = obj_new(read_scalar(), s, allocator)
		s = obj_new(state.atom_quote, s, allocator)
		state.read_stack = s

		return read()

	// A pop?
	case '$':
		advance()
		s: ^Obj
		s = obj_new(state.atom_pop, s, allocator)
		s = obj_new(read_scalar(), s, allocator)
		s = obj_new(state.atom_quote, s, allocator)
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

print_list_tail :: proc(obj: ^Obj) {
	if obj == state.nil {
		fmt.print(")")
		return
	}

	if is(obj, Pair) {
		fmt.print(" ")
		print_recurse(obj.(Pair).car)
		print_list_tail(obj.(Pair).cdr)
		return
	}

	fmt.print(" . ")
	print_recurse(obj)
	fmt.print(")")
}

print_recurse :: proc(obj: ^Obj) {
	if obj == state.nil {
		fmt.print("()")
		return
	}

	switch t in obj {
	case Nil, Atom, Number:
		fmt.print(t)
	case Pair:
		fmt.print("(")
		print_recurse(t.car)
		print_list_tail(t.cdr)

	case Closure:
		fmt.print("CLOSURE<")
		print_recurse(t.body)
		fmt.printf(", %p>", t.env)

	case Primitive:
		fmt.printf("PRIM<%p>", t)
	}
}

print :: proc(obj: ^Obj) {
	print_recurse(obj)
	fmt.println()
}

/*******************************************************************
 * Environment
 ******************************************************************/

// Environment is just a simple list of key-val (dotted) pairs

env_find :: proc(env, key: ^Obj) -> ^Obj {
	if !is(key, Atom) {fail("Expected 'key' to be an Atom in env_find()")}

	for v := env; v != state.nil; v = cdr(v) {
		kv := car(env)
		if key == car(kv) {
			return cdr(kv)
		}
	}

	failf("Failed to find key='%s' in environment", key.(Atom))
	return nil
}

env_define :: proc(env, key, val: ^Obj, allocator := context.allocator) -> ^Obj {
	return obj_new(obj_new(key, val, allocator), env, allocator)
}

env_define_prim :: proc(
	env: ^Obj,
	name: string,
	fn: proc(env: ^^Obj, allocator := context.allocator),
	allocator := context.allocator,
) -> ^Obj {
	return env_define(env, intern(name, allocator), obj_new(fn, allocator), allocator)
}


/*******************************************************************
 * Value Stack Operations
 ******************************************************************/

push :: proc(obj: ^Obj, allocator := context.allocator) {
	state.stack = obj_new(obj, state.stack, allocator)
}

try_pop :: proc() -> (^Obj, bool) {
	if state.stack == nil {
		return nil, false
	}

	o := car(state.stack)
	state.stack = cdr(state.stack)
	return o, true
}

pop :: proc() -> ^Obj {
	if ret, ok := try_pop(); ok {
		return ret
	} else {
		return nil
	}
}

/*******************************************************************
 * Eval
 ******************************************************************/

compute :: proc(comp, env: ^Obj) {}
eval :: proc(expr: ^Obj, env: ^^Obj) {}


/*******************************************************************
 * Primitives
 ******************************************************************/


// Core primitives
prim_push :: proc(env: ^^Obj, allocator := context.allocator) {push(env_find(env^, pop()))}
prim_pop :: proc(env: ^^Obj, allocator := context.allocator) {k := pop();v := pop();env^ = env_define(env^, k, v)}
prim_eq :: proc(_: ^^Obj, allocator := context.allocator) {push(obj_equal(pop(), pop()) ? state.atom_true : state.nil)}
prim_cons :: proc(_: ^^Obj, allocator := context.allocator) {a := pop()
	b := pop()
	push(obj_new(a, b, allocator))}
prim_car :: proc(_: ^^Obj, allocator := context.allocator) {push(car(pop()))}
prim_cdr :: proc(_: ^^Obj, allocator := context.allocator) {push(cdr(pop()))}
prim_cswap :: proc(_: ^^Obj, allocator := context.allocator) {
	if (pop() == state.atom_true) {a := pop()
		b := pop()
		push(a)
		push(b)
	}
}
prim_tag :: proc(_: ^^Obj, allocator := context.allocator) {push(
		obj_new(pop().(Number), allocator),
	)}
prim_read :: proc(_: ^^Obj, allocator := context.allocator) {push(read())}
prim_print :: proc(_: ^^Obj, allocator := context.allocator) {print(pop())}

// Extra primitives
prim_stack :: proc(_: ^^Obj, allocator := context.allocator) {push(state.stack)}
prim_env :: proc(env: ^^Obj, allocator := context.allocator) {push(env^)}
prim_sub :: proc(_: ^^Obj, allocator := context.allocator) {b := pop()
	a := pop()
	push(obj_new(obj_i64(a) - obj_i64(b), allocator))}
prim_mul :: proc(_: ^^Obj, allocator := context.allocator) {b := pop()
	a := pop()
	push(obj_new(obj_i64(a) * obj_i64(b), allocator))}
prim_nand :: proc(_: ^^Obj, allocator := context.allocator) {b := pop()
	a := pop()
	push(obj_new(~(obj_i64(a) & obj_i64(b)), allocator))}
prim_lsh :: proc(_: ^^Obj, allocator := context.allocator) {b := pop()
	a := pop()
	push(obj_new(obj_i64(a) << uint(obj_i64(b)), allocator))}
prim_rsh :: proc(_: ^^Obj, allocator := context.allocator) {b := pop()
	a := pop()
	push(obj_new(obj_i64(a) >> uint(obj_i64(b)), allocator))}

// when USE_LOWLEVEL {
// Low-level primitives
// prim_ptr_state :: proc(_: ^^Obj), allocator := context.allocator {push(obj_new(state, allocator))}
// prim_ptr_read :: proc(_: ^^Obj), allocator := context.allocator {push(obj_new(obj_i64(pop()), allocator))}
// prim_ptr_write :: proc(_: ^^Obj) {b := pop();a := pop();uintptr(obj_i64(a))^ = obj_i64(b)}
// prim_ptr_to_obj :: proc(_: ^^Obj) {push(obj_i64(pop()))}
// prim_ptr_from_obj :: proc(_: ^^Obj, allocator := context.allocator) {push(obj_new(pop(), allocator))}
// }

load_file :: proc(filename: string, allocator := context.allocator) -> (string, bool) {
	file, erno := os.open(filename)
	if erno != 0 {
		return "", false
	}
	defer os.close(file)

	b, ok := os.read_entire_file_from_handle(file, allocator)
	if !ok {
		return "", ok
	}
	defer delete(b, allocator)

	str, err := strings.clone_from_bytes(b, allocator)
	if err != nil {
		return "", false
	}

	return str, true
}

setup :: proc(filename: string, allocator := context.allocator) {
	state.input = load_file(filename, allocator) or_else panic("failed to load input file")
	state.pos = 0

	state.read_stack = nil
	state.nil = obj_new(allocator)

	state.interned_atoms = state.nil
	state.atom_true = intern("t", allocator)
	state.atom_quote = intern("quote", allocator)
	state.atom_push = intern("push", allocator)
	state.atom_pop = intern("pop", allocator)

	state.stack = state.nil

	env := state.nil

	// core primitives
	env = env_define_prim(env, "push", prim_push, allocator)
	env = env_define_prim(env, "pop", prim_pop, allocator)
	env = env_define_prim(env, "cons", prim_cons, allocator)
	env = env_define_prim(env, "car", prim_car, allocator)
	env = env_define_prim(env, "cdr", prim_cdr, allocator)
	env = env_define_prim(env, "eq", prim_eq, allocator)
	env = env_define_prim(env, "cswap", prim_cswap, allocator)
	env = env_define_prim(env, "tag", prim_tag, allocator)
	env = env_define_prim(env, "read", prim_read, allocator)
	env = env_define_prim(env, "print", prim_print, allocator)

	// extra primitives
	env = env_define_prim(env, "stack", prim_stack, allocator)
	env = env_define_prim(env, "env", prim_env, allocator)
	env = env_define_prim(env, "-", prim_sub, allocator)
	env = env_define_prim(env, "*", prim_mul, allocator)
	env = env_define_prim(env, "nand", prim_nand, allocator)
	env = env_define_prim(env, "<<", prim_lsh, allocator)
	env = env_define_prim(env, ">>", prim_rsh, allocator)

	// low-level primitives
	// env = env_define_prim(env, "ptr-state!", prim_ptr_state, allocator)
	// env = env_define_prim(env, "ptr-read!", prim_ptr_read, allocator)
	// env = env_define_prim(env, "ptr-write!", prim_ptr_write, allocator)
	// env = env_define_prim(env, "ptr-to-obj!", prim_ptr_to_obj, allocator)
	// env = env_define_prim(env, "ptr-from-obj!", prim_ptr_from_obj, allocator)

	state.env = env
}

cleanup :: proc(allocator := context.allocator) {
	delete(state.input, allocator)
	obj_destroy(state.nil, allocator)
	obj_destroy(&state.interned_atoms, allocator)
	obj_destroy(&state.atom_true, allocator)
	obj_destroy(&state.atom_quote, allocator)
	obj_destroy(&state.atom_push, allocator)
	obj_destroy(&state.atom_pop, allocator)
	obj_destroy(&state.stack, allocator)
	obj_destroy(&state.env, allocator)
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
	defer obj_destroy(&obj)

	compute(obj, state.env)
}
