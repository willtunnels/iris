use crate::parse::parse_prog;

#[test]
fn test_parse_successful() {
    let prog = r"
        extern type Foo = crate::foo::Foo; // This is a comment
        extern type Bar<T> = crate::bar::Bar;

        // This is also a comment
        extern fn f(x: Foo, y: Bar<Foo>) = crate::f;
        extern fn g(x: Foo, y: Bar<Foo>) -> (i32, i32) = crate::g;

        fn h(x: i32) -> fn() -> fn(i32) -> i32 {
            let y = true;
            let z: i32 = (|x| x)(0i32);
            0i32
        }
    ";
    parse_prog(prog).unwrap();
}
