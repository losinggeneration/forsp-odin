package forsp

import "core:testing"

@(test)
test_is :: proc(t: ^testing.T) {
	{
		obj := obj_new()
		defer free(obj)
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
		obj := obj_new("test")
		defer free(obj)
		ok := is(obj, Nil)
		testing.expect(t, !ok, "Expected is(Nil) to be true with Atom Obj")
		ok = is(obj, Atom)
		testing.expect(t, ok, "Expected is(Atom) to be false with Atom Obj")
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
		obj := obj_new(5)
		defer free(obj)
		ok := is(obj, Nil)
		testing.expect(t, !ok, "Expected is(Nil) to be true with Number Obj")
		ok = is(obj, Atom)
		testing.expect(t, !ok, "Expected is(Atom) to be false with Number Obj")
		ok = is(obj, Number)
		testing.expect(t, ok, "Expected is(Number) to be false with Number Obj")
		ok = is(obj, Pair)
		testing.expect(t, !ok, "Expected is(Pair) to be false with Number Obj")
		ok = is(obj, Closure)
		testing.expect(t, !ok, "Expected is(Closure) to be false with Number Obj")
		ok = is(obj, Primitive)
		testing.expect(t, !ok, "Expected is(Primitive) to be false with Number Obj")
	}

	{
		obj := obj_new(Pair{})
		defer free(obj)
		ok := is(obj, Nil)
		testing.expect(t, !ok, "Expected is(Nil) to be true with Pair Obj")
		ok = is(obj, Atom)
		testing.expect(t, !ok, "Expected is(Atom) to be false with Pair Obj")
		ok = is(obj, Number)
		testing.expect(t, !ok, "Expected is(Number) to be false with Pair Obj")
		ok = is(obj, Pair)
		testing.expect(t, ok, "Expected is(Pair) to be false with Pair Obj")
		ok = is(obj, Closure)
		testing.expect(t, !ok, "Expected is(Closure) to be false with Pair Obj")
		ok = is(obj, Primitive)
		testing.expect(t, !ok, "Expected is(Primitive) to be false with Pair Obj")
	}

	{
		obj := obj_new(Closure{})
		defer free(obj)
		ok := is(obj, Nil)
		testing.expect(t, !ok, "Expected is(Nil) to be true with Closure Obj")
		ok = is(obj, Atom)
		testing.expect(t, !ok, "Expected is(Atom) to be false with Closure Obj")
		ok = is(obj, Number)
		testing.expect(t, !ok, "Expected is(Number) to be false with Closure Obj")
		ok = is(obj, Pair)
		testing.expect(t, !ok, "Expected is(Pair) to be false with Closure Obj")
		ok = is(obj, Closure)
		testing.expect(t, ok, "Expected is(Closure) to be false with Closure Obj")
		ok = is(obj, Primitive)
		testing.expect(t, !ok, "Expected is(Primitive) to be false with Closure Obj")
	}

	{
		obj := obj_new(proc(env: ^^Obj) {})
		defer free(obj)
		ok := is(obj, Nil)
		testing.expect(t, !ok, "Expected is(Nil) to be true with Primitive Obj")
		ok = is(obj, Atom)
		testing.expect(t, !ok, "Expected is(Atom) to be false with Primitive Obj")
		ok = is(obj, Number)
		testing.expect(t, !ok, "Expected is(Number) to be false with Primitive Obj")
		ok = is(obj, Pair)
		testing.expect(t, !ok, "Expected is(Pair) to be false with Primitive Obj")
		ok = is(obj, Closure)
		testing.expect(t, !ok, "Expected is(Closure) to be false with Primitive Obj")
		ok = is(obj, Primitive)
		testing.expect(t, ok, "Expected is(Primitive) to be false with Primitive Obj")
	}
}
