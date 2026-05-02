package forsp

import "core:fmt"
import "core:sync"
import "core:testing"

// Tests share the global `state` and must not run concurrently.
@(private="file")
test_mu: sync.Mutex

// Reset state to a clean initial state. Caller must hold test_mu.
_test_reset :: proc() {
	gc_free_all()
	delete(state.gc_roots)

	state = State{}
	state.gc_roots = make([dynamic]^^Obj)
}

// Simple init for tests that don't need full setup. Acquires test_mu;
// caller must defer _test_cleanup() to release it.
_test_init_minimal :: proc() {
	sync.lock(&test_mu)
	_test_reset()
	state.nil = obj_new()
	state.stack = state.nil
}

// Init with the standard atoms interned. Acquires test_mu.
_test_init_atoms :: proc() {
	sync.lock(&test_mu)
	_test_reset()
	state.nil = obj_new()
	state.interned_atoms = state.nil
	state.atom_true = intern("t")
	state.atom_quote = intern("quote")
	state.atom_push = intern("push")
	state.atom_pop = intern("pop")
	state.stack = state.nil
}

// Full setup including primitives + input. Acquires test_mu.
_test_init_full :: proc(input: string) {
	sync.lock(&test_mu)
	_test_reset()
	setup_with_input(input)
}

// Cleanup after test - free all GC objects and release the lock.
_test_cleanup :: proc() {
	gc_free_all()
	delete(state.gc_roots)
	state = State{}
	sync.unlock(&test_mu)
}

@(test)
test_is :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	{
		obj := obj_new()
		ok := is(obj, Nil)
		testing.expect(t, ok, "Expected is(Nil) to be true with Nil Obj")
		ok = is(obj, Atom)
		testing.expect(t, !ok, "Expected is(Atom) to be false with Nil Obj")
		ok = is(obj, Number)
		testing.expect(t, !ok, "Expected is(Number) to be false with Nil Obj")
		ok = is(obj, Pair)
		testing.expect(t, !ok, "Expected is(Pair) to be false with Nil Obj")
		ok = is(obj, Closure)
		testing.expect(t, !ok, "Expected is(Closure) to be false with Nil Obj")
		ok = is(obj, Primitive)
		testing.expect(t, !ok, "Expected is(Primitive) to be false with Nil Obj")
	}

	{
		obj := atom_new("test")
		ok := is(obj, Nil)
		testing.expect(t, !ok, "Expected is(Nil) to be false with Atom Obj")
		ok = is(obj, Atom)
		testing.expect(t, ok, "Expected is(Atom) to be true with Atom Obj")
		ok = is(obj, Number)
		testing.expect(t, !ok, "Expected is(Number) to be false with Atom Obj")
		ok = is(obj, Pair)
		testing.expect(t, !ok, "Expected is(Pair) to be false with Atom Obj")
		ok = is(obj, Closure)
		testing.expect(t, !ok, "Expected is(Closure) to be false with Atom Obj")
		ok = is(obj, Primitive)
		testing.expect(t, !ok, "Expected is(Primitive) to be false with Atom Obj")
	}

	{
		obj := number_new(5)
		ok := is(obj, Nil)
		testing.expect(t, !ok, "Expected is(Nil) to be false with Number Obj")
		ok = is(obj, Atom)
		testing.expect(t, !ok, "Expected is(Atom) to be false with Number Obj")
		ok = is(obj, Number)
		testing.expect(t, ok, "Expected is(Number) to be true with Number Obj")
		ok = is(obj, Pair)
		testing.expect(t, !ok, "Expected is(Pair) to be false with Number Obj")
		ok = is(obj, Closure)
		testing.expect(t, !ok, "Expected is(Closure) to be false with Number Obj")
		ok = is(obj, Primitive)
		testing.expect(t, !ok, "Expected is(Primitive) to be false with Number Obj")
	}

	{
		obj := pair_new(Pair{})
		ok := is(obj, Nil)
		testing.expect(t, !ok, "Expected is(Nil) to be false with Pair Obj")
		ok = is(obj, Atom)
		testing.expect(t, !ok, "Expected is(Atom) to be false with Pair Obj")
		ok = is(obj, Number)
		testing.expect(t, !ok, "Expected is(Number) to be false with Pair Obj")
		ok = is(obj, Pair)
		testing.expect(t, ok, "Expected is(Pair) to be true with Pair Obj")
		ok = is(obj, Closure)
		testing.expect(t, !ok, "Expected is(Closure) to be false with Pair Obj")
		ok = is(obj, Primitive)
		testing.expect(t, !ok, "Expected is(Primitive) to be false with Pair Obj")
	}
}

// =============================================================================
// Object Allocation
// =============================================================================

@(test)
test_obj_alloc :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	// Test allocating various types
	obj_nil := obj_new()
	testing.expect(t, obj_nil != nil, "obj_new(Nil) should return non-nil")
	testing.expect(t, is(obj_nil, Nil), "obj_new(Nil) should be Nil type")

	obj_num := number_new(42)
	testing.expect(t, obj_num != nil, "number_new should return non-nil")
	testing.expect(t, is(obj_num, Number), "number_new should be Number type")
	testing.expect(t, obj_num.value.(Number) == 42, "Number value should be 42")

	obj_pair := pair_new(Pair{obj_nil, obj_num})
	testing.expect(t, obj_pair != nil, "pair_new should return non-nil")
	testing.expect(t, is(obj_pair, Pair), "pair_new should be Pair type")

	// Verify object count increased
	testing.expect(t, state.gc_count >= 3, "Should have allocated at least 3 objects")
}

// =============================================================================
// Obj Tag
// =============================================================================

@(test)
test_obj_tag :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	nil_obj := obj_new()
	testing.expect(t, obj_tag(nil_obj) == .Nil, "Nil object should have tag Nil")

	num_obj := number_new(0)
	testing.expect(t, obj_tag(num_obj) == .Number, "Number object should have tag Number")

	pair_obj := pair_new(Pair{})
	testing.expect(t, obj_tag(pair_obj) == .Pair, "Pair object should have tag Pair")
}

// =============================================================================
// Obj Equal
// =============================================================================

@(test)
test_obj_equal :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	nil1 := obj_new()
	nil2 := obj_new()
	testing.expect(t, obj_equal(nil1, nil1), "Object should equal itself")
	testing.expect(t, !obj_equal(nil1, nil2), "Different Nil objects should not be equal")

	num1 := number_new(5)
	num2 := number_new(5)
	num3 := number_new(10)
	testing.expect(t, obj_equal(num1, num1), "Number should equal itself")
	testing.expect(t, obj_equal(num1, num2), "Equal numbers should be equal")
	testing.expect(t, !obj_equal(num1, num3), "Different numbers should not be equal")

	// Nil vs Number
	testing.expect(t, !obj_equal(nil1, num1), "Nil and Number should not be equal")
}

// =============================================================================
// Car and Cdr
// =============================================================================

@(test)
test_car_cdr :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	nil_obj := state.nil
	head := number_new(1)
	tail := number_new(2)
	pair := pair_new(Pair{head, tail})

	// Test car
	testing.expect(t, car(pair) == head, "car should return first element")

	// Test cdr
	testing.expect(t, cdr(pair) == tail, "cdr should return rest")

	// Test with longer list
	third := number_new(3)
	rest := pair_new(Pair{tail, third})
	full := pair_new(Pair{head, rest})
	testing.expect(t, car(full) == head, "car of full list should be head")
	testing.expect(t, car(cdr(full)) == tail, "car of cdr should be second element")
}

// =============================================================================
// GC Functions
// =============================================================================

@(test)
test_gc_mark :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	obj := pair_new(Pair{number_new(1), number_new(2)})
	testing.expect(t, !obj.marked, "Object should start unmarked")

	gc_mark(obj)
	testing.expect(t, obj.marked, "Object should be marked after gc_mark")

	// Marking again should be safe
	gc_mark(obj)
	testing.expect(t, obj.marked, "Object should remain marked")

	// Marking nil should be safe
	gc_mark(nil)
	testing.expect(t, true, "Marking nil should not crash")
}

@(test)
test_gc_push_pop_root :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	// Push some roots
	obj1 := number_new(1)
	obj2 := number_new(2)

	initial_roots := len(state.gc_roots)
	gc_push_root(&obj1)
	gc_push_root(&obj2)

	testing.expect(t, len(state.gc_roots) == initial_roots + 2, "Should have 2 more roots")

	gc_pop_root()
	testing.expect(t, len(state.gc_roots) == initial_roots + 1, "Should have 1 more root after pop")

	gc_pop_root()
	testing.expect(t, len(state.gc_roots) == initial_roots, "Should be back to initial root count")
}

@(test)
test_gc_mark_roots :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	// Create objects that should be roots
	nil_obj := state.nil
	stack_obj := number_new(100)

	state.stack = stack_obj

	// All should be unmarked
	testing.expect(t, !nil_obj.marked, "nil should start unmarked")
	testing.expect(t, !stack_obj.marked, "stack should start unmarked")

	gc_mark_roots()

	// All should now be marked
	testing.expect(t, nil_obj.marked, "nil should be marked")
	testing.expect(t, stack_obj.marked, "stack should be marked")
}

@(test)
test_gc_sweep :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	// Create some objects
	objs: [5]^Obj
	for i in 0..<5 {
		objs[i] = number_new(i64(i))
	}

	initial_count := state.gc_count

	// Mark some but not all
	objs[0].marked = true
	objs[2].marked = true
	objs[4].marked = true

	gc_sweep()

	// After sweep, unmarked objects are freed (at least 1, more if internals were allocated)
	testing.expect(t, state.gc_count < initial_count, "gc_sweep should free some objects")
}

// =============================================================================
// Stack Operations
// =============================================================================

@(test)
test_push_pop :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	// Initial stack should be nil
	testing.expect(t, state.stack == state.nil, "Initial stack should be nil")

	// Push some objects
	obj1 := number_new(1)
	obj2 := number_new(2)

	push(obj1)
	testing.expect(t, state.stack != state.nil, "Stack should not be nil after push")
	testing.expect(t, car(state.stack) == obj1, "Top of stack should be obj1")

	push(obj2)
	testing.expect(t, car(state.stack) == obj2, "Top of stack should be obj2 after second push")

	// Pop
	popped := pop()
	testing.expect(t, popped == obj2, "Popped object should be obj2")
	testing.expect(t, car(state.stack) == obj1, "Stack top should now be obj1")

	popped = pop()
	testing.expect(t, popped == obj1, "Popped object should be obj1")
	testing.expect(t, state.stack == state.nil, "Stack should be nil after popping all")
}

@(test)
test_try_pop :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	// Try pop on empty stack
	obj, ok := try_pop()
	testing.expect(t, obj == nil, "try_pop on empty stack should return nil")
	testing.expect(t, !ok, "try_pop on empty stack should return false")

	// Push then pop
	push(number_new(42))
	obj, ok = try_pop()
	testing.expect(t, obj != nil, "try_pop should return object")
	testing.expect(t, ok, "try_pop should return true")
	testing.expect(t, obj.value.(Number) == 42, "Popped value should be 42")
}

// =============================================================================
// Environment Functions
// =============================================================================

@(test)
test_env_define :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	key := atom_new("x")
	val := number_new(5)
	env := state.nil

	new_env := env_define(env, key, val)
	testing.expect(t, new_env != env, "env_define should return new environment")
	testing.expect(t, car(new_env) != nil, "New env should have a binding")

	// Find the binding
	found := env_find(new_env, key)
	testing.expect(t, found == val, "env_find should return defined value")
}

@(test)
test_env_find :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	// Define a simple environment
	key1 := atom_new("a")
	val1 := number_new(1)
	key2 := atom_new("b")
	val2 := number_new(2)

	env := state.nil
	env = env_define(env, key1, val1)
	env = env_define(env, key2, val2)

	// Find existing keys
	found1 := env_find(env, key1)
	testing.expect(t, found1 == val1, "Should find key1")

	found2 := env_find(env, key2)
	testing.expect(t, found2 == val2, "Should find key2")
}

// =============================================================================
// Read Helper Functions
// =============================================================================

@(test)
test_parsing_helpers :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	// Test is_white
	testing.expect(t, is_white(' '), "Space should be whitespace")
	testing.expect(t, is_white('\t'), "Tab should be whitespace")
	testing.expect(t, is_white('\n'), "Newline should be whitespace")
	testing.expect(t, !is_white('a'), "Letter should not be whitespace")
	testing.expect(t, !is_white('('), "Paren should not be whitespace")

	// Test is_directive
	testing.expect(t, is_directive('\''), "Quote should be directive")
	testing.expect(t, is_directive('^'), "Push should be directive")
	testing.expect(t, is_directive('$'), "Pop should be directive")
	testing.expect(t, !is_directive('a'), "Letter should not be directive")

	// Test is_punctuation
	testing.expect(t, is_punctuation(0), "Null should be punctuation")
	testing.expect(t, is_punctuation(' '), "Space should be punctuation")
	testing.expect(t, is_punctuation('('), "Open paren should be punctuation")
	testing.expect(t, is_punctuation(')'), "Close paren should be punctuation")
	testing.expect(t, is_punctuation(';'), "Semicolon should be punctuation")
	testing.expect(t, !is_punctuation('a'), "Letter should not be punctuation")
	testing.expect(t, !is_punctuation('1'), "Digit should not be punctuation")

	// Test parse_i64
	value, ok := parse_i64("42")
	testing.expect(t, ok, "parse_i64 should succeed for '42'")
	testing.expect(t, value == 42, "parse_i64('42') should return 42")

	value, ok = parse_i64("-123")
	testing.expect(t, ok, "parse_i64 should succeed for '-123'")
	testing.expect(t, value == -123, "parse_i64('-123') should return -123")

	_, ok = parse_i64("not_a_number")
	testing.expect(t, !ok, "parse_i64 should fail for 'not_a_number'")
}

@(test)
test_peek_advance :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	state.input = "hello"
	state.pos = 0

	// Test peek
	testing.expect(t, peek() == 'h', "Peek should return first character")

	// Test advance
	advance()
	testing.expect(t, peek() == 'e', "Peek should return 'e' after advance")

	advance()
	advance()
	advance()
	testing.expect(t, peek() == 'o', "Peek should return 'o' after 4 advances")

	advance()
	testing.expect(t, peek() == 0, "Peek should return 0 at end of string")
}

// =============================================================================
// State Initialization
// =============================================================================

@(test)
test_state_init :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	// Check state was properly initialized
	testing.expect(t, state.nil != nil, "state.nil should be initialized")
	testing.expect(t, state.stack == state.nil, "state.stack should be nil")
}

// =============================================================================
// Atom Creation (without intern)
// =============================================================================

@(test)
test_atom_new :: proc(t: ^testing.T) {
	_test_init_minimal()
	defer _test_cleanup()

	atom1 := atom_new("test_atom")
	testing.expect(t, atom1 != nil, "atom_new should return non-nil")
	testing.expect(t, is(atom1, Atom), "atom_new should return Atom type")

	atom2 := atom_new("another")
	testing.expect(t, atom2 != nil, "second atom_new should return non-nil")
	testing.expect(t, atom1 != atom2, "Different atoms should be different objects")
}