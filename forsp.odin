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
Primitive :: proc(env: ^^Obj)

Obj_Value :: union {
	Nil,
	Atom,
	Number,
	Pair,
	Closure,
	Primitive,
}

Obj :: struct {
	value:  Obj_Value,
	marked: bool,
	next:   ^Obj,
}

Tags :: enum {
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

	// GC
	gc_objects:     ^Obj,
	gc_count:       int,
	gc_threshold:   int,
	gc_roots:       [dynamic]^^Obj,
}

state: State

/*******************************************************************
 * Error handling / assertions
 ******************************************************************/

assert :: proc(v: bool, msg: string) {
	if !v {
		fmt.panicf("ASSERT: %s", msg)
	}
}

fail :: proc(msg: string) {
	fmt.panicf("FAIL: %s", msg)
}

failf :: proc(msg: string, args: ..any) {
	m := fmt.aprintf(msg, ..args) // I guess we just leak this...
	defer delete(m)
	fail(m)
}

/*******************************************************************
 * GC
 ******************************************************************/

gc_mark :: proc(obj: ^Obj) {
	if obj == nil || obj.marked {
		return
	}

	obj.marked = true

	switch o in obj.value {
	case Nil:
	// no children

	case Atom:
	// no Obj children

	case Number:
	// no children

	case Pair:
		gc_mark(o.car)
		gc_mark(o.cdr)

	case Closure:
		gc_mark(o.body)
		gc_mark(o.env)

	case Primitive:
	// no Obj children
	}
}

gc_mark_roots :: proc() {
	gc_mark(state.nil)
	gc_mark(state.read_stack)

	gc_mark(state.interned_atoms)
	gc_mark(state.atom_true)
	gc_mark(state.atom_quote)
	gc_mark(state.atom_push)
	gc_mark(state.atom_pop)

	gc_mark(state.stack)
	gc_mark(state.env)

	for root in state.gc_roots {
		if root != nil {
			gc_mark(root^)
		}
	}
}

gc_free_obj :: proc(obj: ^Obj) {
	// If Atom strings are changed to be owned/cloned, free them here.
	//
	// switch o in obj.value {
	// case Atom:
	// 	mem.delete(string(o))
	// case:
	// }

	mem.free(obj)
}

gc_sweep :: proc() {
	link := &state.gc_objects

	for link^ != nil {
		obj := link^

		if !obj.marked {
			link^ = obj.next
			gc_free_obj(obj)
			state.gc_count -= 1
		} else {
			obj.marked = false
			link = &obj.next
		}
	}
}

gc_collect :: proc() {
	when ODIN_DEBUG {
		before := state.gc_count
		gc_mark_roots()
		gc_sweep()
		after := state.gc_count
		fmt.printf("gc: collected %d objects, %d remaining\n", before - after, after)
	} else {
		gc_mark_roots()
		gc_sweep()
	}

	state.gc_threshold = state.gc_count * 2
	if state.gc_threshold < 1024 {
		state.gc_threshold = 1024
	}
}

gc_push_root :: proc(slot: ^^Obj) {
	append(&state.gc_roots, slot)
}

gc_pop_root :: proc() {
	n := len(state.gc_roots)
	assert(n > 0, "GC root stack underflow")
	resize(&state.gc_roots, n - 1)
}

gc_free_all :: proc() {
	obj := state.gc_objects
	for obj != nil {
		next := obj.next
		gc_free_obj(obj)
		obj = next
	}

	state.gc_objects = nil
	state.gc_count = 0
}

/*******************************************************************
 * Object allocation / helpers
 ******************************************************************/

obj_alloc :: proc(value: Obj_Value) -> ^Obj {
	if state.gc_threshold > 0 && state.gc_count >= state.gc_threshold {
		gc_collect()
	}

	o := new(Obj)
	o.value = value
	o.marked = false
	o.next = state.gc_objects

	state.gc_objects = o
	state.gc_count += 1

	return o
}

nil_new :: proc() -> ^Obj {
	return obj_alloc(Nil(true))
}

atom_new :: proc(str: string) -> ^Obj {
	return obj_alloc(Atom(str))
}

number_new :: proc(n: i64) -> ^Obj {
	return obj_alloc(Number(n))
}

pair_new :: proc(p: Pair) -> ^Obj {
	return obj_alloc(p)
}

closure_new :: proc(c: Closure) -> ^Obj {
	return obj_alloc(c)
}

primitive_new :: proc(f: proc(env: ^^Obj)) -> ^Obj {
	return obj_alloc(Primitive(f))
}

obj_new :: proc {
	nil_new,
	atom_new,
	number_new,
	pair_new,
	closure_new,
	primitive_new,
}

obj_tag :: proc(o: ^Obj) -> Tags {
	switch _ in o.value {
	case Nil:
		return .Nil
	case Atom:
		return .Atom
	case Number:
		return .Number
	case Pair:
		return .Pair
	case Closure:
		return .Closure
	case Primitive:
		return .Primitive
	}

	return nil
}

is :: proc(v: ^Obj, $t: typeid) -> bool {
	if v == nil {
		return false
	}

	_, ok := v.value.(t)
	return ok
}

assert_type :: proc(v: ^Obj, $t: typeid, msg: string) {
	assert(is(v, t), msg)
}

fail_type :: proc(v: ^Obj, $t: typeid, msg: string) {
	if !is(v, t) {
		fail(msg)
	}
}

intern :: proc(atom_buf: string) -> ^Obj {
	for list := state.interned_atoms; list != state.nil; list = list.value.(Pair).cdr {
		assert_type(list, Pair, "state.interned_atoms must be Pairs")

		elem := list.value.(Pair).car
		assert_type(elem, Atom, "state.interned_atoms.car must be an Atom")
		if len(atom_buf) == len(elem.value.(Atom)) && atom_buf == elem.value.(Atom) {
			return elem
		}
	}

	atom := obj_new(atom_buf)
	gc_push_root(&atom)
	defer gc_pop_root()

	old_interned := state.interned_atoms
	gc_push_root(&old_interned)
	defer gc_pop_root()

	state.interned_atoms = obj_new(Pair{atom, old_interned})

	return atom
}

car :: proc(obj: ^Obj) -> ^Obj {
	fail_type(obj, Pair, "Expected Pair to apply car() function")
	return obj.value.(Pair).car
}

cdr :: proc(obj: ^Obj) -> ^Obj {
	fail_type(obj, Pair, "Expected Pair to apply cdr() function")
	return obj.value.(Pair).cdr
}

obj_equal :: proc(a, b: ^Obj) -> bool {
	return a == b || (is(a, Number) && is(b, Number) && a.value.(Number) == b.value.(Number))
}

obj_i64 :: proc(a: ^Obj) -> i64 {
	return a.value.(Number) if is(a, Number) else 0
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
	return c == ' ' || c == '\t' || c == '\n'
}

is_directive :: proc(c: u8) -> bool {
	return c == '\'' || c == '^' || c == '$'
}

is_punctuation :: proc(c: u8) -> bool {
	return c == 0 || is_white(c) || is_directive(c) || c == '(' || c == ')' || c == ';'
}

skip_white_and_comments :: proc() {
	c := peek()
	if c == 0 {
		return
	}

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
			if c == 0 {
				return
			}
			advance()
			if c == '\n' {
				break
			}
		}

		skip_white_and_comments()
		return
	}
}

read_list :: proc() -> ^Obj {
	if state.read_stack == nil {
		skip_white_and_comments()
		c := peek()
		if c == ')' {
			advance()
			return state.nil
		}
	}

	head := read()
	gc_push_root(&head)
	defer gc_pop_root()

	tail := read_list()
	gc_push_root(&tail)
	defer gc_pop_root()

	return obj_new(Pair{head, tail})
}

parse_i64 :: proc(str: string) -> (i64, bool) {
	i, ok := strconv.parse_int(str, 10)
	return i64(i), ok
}

read_scalar :: proc() -> ^Obj {
	// otherwise, assume atom or number and read it
	start := state.pos
	for !is_punctuation(peek()) {
		advance()
	}

	str := state.input[start:state.pos]
	// is it a number?
	if n, ok := parse_i64(str); ok {
		return obj_new(n)
	}

	// atom
	return intern(str)
}

read_directive :: proc(op: ^Obj) -> ^Obj {
	op_local := op
	gc_push_root(&op_local)
	defer gc_pop_root()

	s: ^Obj

	scalar := read_scalar()
	gc_push_root(&scalar)
	defer gc_pop_root()

	quote := state.atom_quote
	gc_push_root(&quote)
	defer gc_pop_root()

	s = obj_new(Pair{op, s})
	s = obj_new(Pair{scalar, s})
	s = obj_new(Pair{quote, s})

	state.read_stack = s
	return read()
}

read :: proc() -> ^Obj {
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
		return read_directive(state.atom_push)

	// A pop?
	case '$':
		advance()
		return read_directive(state.atom_pop)

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

	if o, ok := obj.value.(Pair); ok {
		fmt.print(" ")
		print_recurse(o.car)
		print_list_tail(o.cdr)
	} else {
		fmt.print(" . ")
		print_recurse(obj)
		fmt.print(")")
	}
}

print_recurse :: proc(obj: ^Obj) {
	if obj == state.nil {
		fmt.print("()")
		return
	}

	switch o in obj.value {
	case Nil: // do nothing

	case Atom:
		fmt.print(o)

	case Number:
		fmt.print(o)

	case Pair:
		fmt.print("(")
		print_recurse(o.car)
		print_list_tail(o.cdr)

	case Closure:
		fmt.print("CLOSURE<")
		print_recurse(o.body)
		fmt.printf(", %p>", o.env)

	case Primitive:
		fmt.printf("PRIM<%p>", o)
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
	if !is(key, Atom) {
		fail("Expected 'key' to be an Atom in env_find()")
	}

	for v := env; v != state.nil; v = cdr(v) {
		kv := car(v)
		if key == car(kv) {
			return cdr(kv)
		}
	}

	failf("Failed to find key='%s' in environment", key.value.(Atom))
	return nil
}

env_define :: proc(env, key, val: ^Obj) -> ^Obj {
	env_local := env
	gc_push_root(&env_local)
	defer gc_pop_root()

	key_local := key
	gc_push_root(&key_local)
	defer gc_pop_root()

	val_local := val
	gc_push_root(&val_local)
	defer gc_pop_root()

	kv := obj_new(Pair{key, val})
	gc_push_root(&kv)
	defer gc_pop_root()

	return obj_new(Pair{kv, env})
}

env_define_prim :: proc(env: ^Obj, name: string, fn: proc(env: ^^Obj)) -> ^Obj {
	env_local := env
	gc_push_root(&env_local)
	defer gc_pop_root()

	key := intern(name)
	gc_push_root(&key)
	defer gc_pop_root()

	val := obj_new(fn)
	gc_push_root(&val)
	defer gc_pop_root()

	return env_define(env, key, val)
}

/*******************************************************************
 * Value Stack Operations
 ******************************************************************/

push :: proc(obj: ^Obj) {
	obj_local := obj
	gc_push_root(&obj_local)
	defer gc_pop_root()

	old_stack := state.stack
	gc_push_root(&old_stack)
	defer gc_pop_root()

	state.stack = obj_new(Pair{obj, old_stack})
}

try_pop :: proc() -> (^Obj, bool) {
	if state.stack == nil || state.stack == state.nil {
		return nil, false
	}

	o := car(state.stack)
	state.stack = cdr(state.stack)
	return o, true
}

pop :: proc() -> ^Obj {
	if ret, ok := try_pop(); ok {
		return ret
	}

	fail("Value Stack Underflow")
	return nil
}

/*******************************************************************
 * Eval
 ******************************************************************/

compute :: proc(comp: ^Obj, env: ^Obj) {
	when ODIN_DEBUG {
		fmt.print("compute: ")
		print(comp)
	}

	comp_local := comp
	gc_push_root(&comp_local)
	defer gc_pop_root()

	env_local := env
	gc_push_root(&env_local)
	defer gc_pop_root()

	local_env := env
	gc_push_root(&local_env)
	defer gc_pop_root()

	cmp := comp
	gc_push_root(&cmp)
	defer gc_pop_root()

	for cmp != state.nil {
		cmd := car(cmp)
		gc_push_root(&cmd)
		defer gc_pop_root()

		cmp = cdr(cmp)

		if cmd == state.atom_quote {
			if cmp == state.nil {
				fail("Expected data following a quote form")
			}

			quoted := car(cmp)
			gc_push_root(&quoted)
			defer gc_pop_root()

			push(quoted)
			cmp = cdr(cmp)

			continue
		}

		eval(cmd, &local_env)
	}
}

eval :: proc(expr: ^Obj, env: ^^Obj) {
	when ODIN_DEBUG {
		fmt.print("eval: ")
		print(expr)
	}

	expr_local := expr
	gc_push_root(&expr_local)
	defer gc_pop_root()

	if is(expr, Atom) {
		val := env_find(env^, expr)
		gc_push_root(&val)
		defer gc_pop_root()

		if is(val, Closure) {
			compute(val.value.(Closure).body, val.value.(Closure).env)
		} else if is(val, Primitive) {
			val.value.(Primitive)(env)
		} else {
			push(val)
		}
	} else if is(expr, Nil) || is(expr, Pair) {
		body := expr
		gc_push_root(&body)
		defer gc_pop_root()

		captured_env := env^
		gc_push_root(&captured_env)
		defer gc_pop_root()

		closure := obj_new(Closure{body, captured_env})
		gc_push_root(&closure)
		defer gc_pop_root()

		push(closure)
	} else {
		push(expr)
	}
}

/*******************************************************************
 * Primitives
 ******************************************************************/

// Core primitives
prim_push :: proc(env: ^^Obj) {
	a := pop()
	gc_push_root(&a)
	defer gc_pop_root()

	push(env_find(env^, a))
}

prim_pop :: proc(env: ^^Obj) {
	k := pop()
	gc_push_root(&k)
	defer gc_pop_root()

	v := pop()
	gc_push_root(&v)
	defer gc_pop_root()

	e := env^
	gc_push_root(&e)
	defer gc_pop_root()

	env^ = env_define(e, k, v)
}

prim_eq :: proc(_: ^^Obj) {
	a := pop()
	gc_push_root(&a)
	defer gc_pop_root()

	b := pop()
	gc_push_root(&b)
	defer gc_pop_root()

	push(obj_equal(a, b) ? state.atom_true : state.nil)
}

prim_cons :: proc(_: ^^Obj) {
	a := pop()
	gc_push_root(&a)
	defer gc_pop_root()

	b := pop()
	gc_push_root(&b)
	defer gc_pop_root()

	p := obj_new(Pair{a, b})
	gc_push_root(&p)
	defer gc_pop_root()

	push(p)
}

prim_car :: proc(_: ^^Obj) {
	a := pop()
	gc_push_root(&a)
	defer gc_pop_root()

	push(car(a))
}

prim_cdr :: proc(_: ^^Obj) {
	a := pop()
	gc_push_root(&a)
	defer gc_pop_root()

	push(cdr(a))
}

prim_cswap :: proc(_: ^^Obj) {
	cond := pop()
	gc_push_root(&cond)
	defer gc_pop_root()

	if cond == state.atom_true {
		a := pop()
		gc_push_root(&a)
		defer gc_pop_root()

		b := pop()
		gc_push_root(&b)
		defer gc_pop_root()

		push(a)
		push(b)
	}
}

prim_tag :: proc(_: ^^Obj) {
	a := pop()
	gc_push_root(&a)
	defer gc_pop_root()

	push(obj_new(i64(obj_tag(a))))
}

prim_read :: proc(_: ^^Obj) {
	a := read()
	gc_push_root(&a)
	defer gc_pop_root()

	push(a)
}

prim_print :: proc(_: ^^Obj) {
	a := pop()
	gc_push_root(&a)
	defer gc_pop_root()

	print(a)
}

// Extra primitives
prim_stack :: proc(_: ^^Obj) {
	push(state.stack)
}

prim_env :: proc(env: ^^Obj) {
	push(env^)
}

prim_sub :: proc(_: ^^Obj) {
	b := pop()
	gc_push_root(&b)
	defer gc_pop_root()

	a := pop()
	gc_push_root(&a)
	defer gc_pop_root()

	push(obj_new(obj_i64(a) - obj_i64(b)))
}

prim_mul :: proc(_: ^^Obj) {
	b := pop()
	gc_push_root(&b)
	defer gc_pop_root()

	a := pop()
	gc_push_root(&a)
	defer gc_pop_root()

	push(obj_new(obj_i64(a) * obj_i64(b)))
}

prim_nand :: proc(_: ^^Obj) {
	b := pop()
	gc_push_root(&b)
	defer gc_pop_root()

	a := pop()
	gc_push_root(&a)
	defer gc_pop_root()

	push(obj_new(~(obj_i64(a) & obj_i64(b))))
}

prim_lsh :: proc(_: ^^Obj) {
	b := pop()
	gc_push_root(&b)
	defer gc_pop_root()

	a := pop()
	gc_push_root(&a)
	defer gc_pop_root()

	push(obj_new(obj_i64(a) << uint(obj_i64(b))))
}

prim_rsh :: proc(_: ^^Obj) {
	b := pop()
	gc_push_root(&b)
	defer gc_pop_root()

	a := pop()
	gc_push_root(&a)
	defer gc_pop_root()

	push(obj_new(obj_i64(a) >> uint(obj_i64(b))))
}

when #config(USE_LOWLEVEL, false) {
	// Low-level primitives
	prim_ptr_state :: proc(_: ^^Obj) {
		push(number_new(cast(i64)cast(uintptr)&state))
	}

	prim_ptr_read :: proc(_: ^^Obj) {
		a := cast(^i64)cast(uintptr)obj_i64(pop())
		push(number_new(a^))
	}

	prim_ptr_write :: proc(_: ^^Obj) {
		b := pop()
		gc_push_root(&b)
		defer gc_pop_root()

		a := cast(^i64)cast(uintptr)obj_i64(pop())
		a^ = obj_i64(b)
	}

	prim_ptr_to_obj :: proc(_: ^^Obj) {
		push(cast(^Obj)cast(uintptr)obj_i64(pop()))
	}

	prim_ptr_from_obj :: proc(_: ^^Obj) {
		push(number_new(cast(i64)cast(uintptr)pop()))
	}
}

load_file :: proc(filename: string) -> (string, bool) {
	file, erno := os.open(filename)
	if erno != 0 {
		return "", false
	}
	defer os.close(file)

	b, ferr := os.read_entire_file_from_file(file, context.temp_allocator)
	if ferr != nil {
		return "", false
	}
	// We clone so the input string is owned and can be freed later.
	// The temp_allocator memory is not released here (it will be freed
	// when the temp allocator is reset or at program exit).

	str, err := strings.clone_from_bytes(b, context.allocator)
	if err != nil {
		return "", false
	}

	return str, true
}

setup :: proc(filename: string) {
	input := load_file(filename) or_else panic("failed to load input file")
	setup_with_input(input)
}

setup_with_input :: proc(input: string) {
	state.input = input
	state.pos = 0

	// Disable automatic collection during bootstrap. Some permanent roots are
	// established gradually below.
	state.gc_objects = nil
	state.gc_count = 0
	state.gc_threshold = 0
	state.gc_roots = make([dynamic]^^Obj, 0, 32768)

	state.read_stack = nil
	state.nil = obj_new()

	state.interned_atoms = state.nil
	state.atom_true = intern("t")
	state.atom_quote = intern("quote")
	state.atom_push = intern("push")
	state.atom_pop = intern("pop")

	state.stack = state.nil

	env := state.nil
	gc_push_root(&env)
	defer gc_pop_root()

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

	// Enable automatic collection after bootstrap.
	state.gc_threshold = 1024
}

cleanup :: proc() {
	gc_free_all()
	delete(state.gc_roots)
	delete(state.input)
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
	gc_push_root(&obj)
	defer gc_pop_root()

	compute(obj, state.env)
}
